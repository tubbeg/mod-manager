#!/usr/bin/env bb

(ns app.manager
  (:require [babashka.cli :as cli]
            [clojure.string :as s]
            [app.config :refer []]
            [app.utility :refer [list-files
                                 mkdirCmd
                                 isZero
                                 notZero
                                 list-files-prefix
                                 copyDir
                                 list-files-skip-prefix
                                 compareFiles
                                 rmFile
                                 removeLastSlash
                                 diffDirectories
                                 writeToFile
                                 extract
                                 readFile
                                 directoryExists
                                 removeFileExtension]]
            [babashka.process :refer [shell process exec check]]
            [app.deploy :refer [deployMods purgeAllModFiles
                                listAllMods restoreFiles]]
            [app.config :refer [initialize
                                readFromConfig
                                createConfig]]))

(def input (cli/parse-args *command-line-args* ))

(def defaultConfigDir ".mm")
(def defaultConfigFile ".mm/config.edn")
(def defaultModFolder ".mm/mods")
;(def defaultExtension ".mm")
(def defaultSourceDirectory ".mm/source")
(def defaultDeployPath ".mm/deploy.edn")

(defn readDefaultConfig []
  (readFromConfig
   defaultConfigDir
   defaultConfigFile))

(defn writeDefaultConfig [content] 
  (writeToFile (str content) defaultConfigFile))

(defn isNilOrEmptyString [val]
  (or
   (= val nil)
   (= val "")))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn createModEntry [source name dir priority enable]
  (println "Creating entry for mod: " name)
  (println "with priority: " priority)
  {:name name
   :priority (parse-int priority) 
   :enabled enable 
   :path (str dir "/" name) 
   :source source})


(defn appendModEntry [config entry]
  (let [entries (-> config
                    :mods
                    :entries)
        newEntry (conj entries entry)]
     (assoc (:mods config) :entries newEntry)))

(defn hasMod [config name]
  (let [entries (-> config
                    :mods
                    :entries) 
        matches (filter
                 (fn [e] (= (:name e) name)) entries)
        nrOfMatches (count matches)]
    (> nrOfMatches 0)))


(defn _installMod [mod-path priority]
  (if (directoryExists defaultConfigDir) 
    (do 
      (println "installing:" mod-path) 
      (let [m  mod-path 
            n (removeFileExtension m) 
            c (readDefaultConfig) 
            dir (-> c 
                  :mods 
                  :dir) 
            fullpath (str dir "/" n)]  
        (if (hasMod c n)   
          (println "Error: mod already exists!")  
          (do  
            (mkdirCmd fullpath) 
            (extract m fullpath) 
            (let [e (createModEntry m n dir priority true)  
                  newEntries (appendModEntry c e)  
                  newConfig (-> (assoc c :mods newEntries)  
                                (assoc :numberOfMods 
                                       (+ (:numberOfMods c) 1)))]
              
              (println "new config is: " newConfig) 
              (writeToFile (str newConfig) defaultConfigFile) 
              (println "Succesfully installed mod at: " dir)))))) 
              (println "Error! Missing config directory!")))

(defn installMod [mod-path priority]
  (if (or (isNilOrEmptyString mod-path)
          (isNilOrEmptyString priority))
    (println "Error! Invalid name or priority!")
    (_installMod mod-path priority)))

(defn install [i]
  (let [mod (-> i
                :args
                (next)
                (first))
        prio (-> i
                 :args
                 (next)
                 (next)
                 (first))
        trimmed (s/trim mod)]
    (println "installing mod: " trimmed)
    (installMod trimmed prio)))


(defn compFiles [i]
  (let [from (-> i
                 :args
                 next
                 first
                 (list-files-skip-prefix))
        to (-> i
               :args
               next
               next
               first
               (list-files-skip-prefix))]
    (println "comparing: " to from)
    (println (compareFiles from to))))

(defn cleanDir []
  (println "You are about to delete your mod directory!")
  (println "Create BACKUP of any files you might need!")
  (println "Continue? (Y/n)")
  (when (= (read-line) "Y")
    (do
      (println "Removing .mm files...") 
      (shell "rm -rf .mm"))))


(defn searchMod [name coll]
  (filter #(= name (:name %)) coll))

(defn changeEntry [mod-name entry coll]
  (map #(if (= mod-name (:name %)) 
          entry 
          %)
       coll))

(defn get-mod [mod-name config]
  (let [entries (-> config
                    :mods
                    :entries)
        matches (searchMod mod-name entries)
        zero (isZero matches)]
    (if zero
      :error
      (first matches))))

(defn set-mod [mod-name enable priority config]
  (let [entries (-> config
                    :mods
                    :entries)
        dir (-> config
                :mods
                :dir)
        mod (get-mod mod-name config)
        modsConfig (:mods config) 
        newEntry (createModEntry  
                  (:source mod)  
                  mod-name 
                  dir 
                  priority 
                  enable) 
        newEntries (changeEntry mod-name newEntry entries) 
        newMods (assoc modsConfig :entries newEntries) 
        newConfig (assoc config :mods newMods)] 
    (if (or (= mod nil) (= mod :error))
      (println "Error in setting mod!")
      (do 
        (println "Created new config: " newConfig)
        (writeDefaultConfig newConfig)))))

(defn changeModEntry [i]
  (let [mod-name (-> i
                     :args
                     (next)
                     (first))
        enable (-> i
                   :args
                   (next)
                   (next)
                   (first)
                   (parse-boolean))
        prio (-> i
                 :args
                 (next)
                 (next)
                 (next)
                 (first))]
    
  (if (or (isNilOrEmptyString mod-name)
          (isNilOrEmptyString prio)
          (isNilOrEmptyString enable))
    (println "Invalid input!")
    (set-mod mod-name enable prio (readDefaultConfig)))))


(defn printStatus []
  (let [c (readDefaultConfig)
        entries (->> c
                    :mods
                    :entries
                    (sort-by :priority))]
    (println "Config file:" c)
    (println "")
    (println "Name\t\tPriority\t\tEnabled?")
    (loop [e entries]
      (let [f (first e)
            rem (next e)]
        (println (str (:name f)
                      "\t\t" (:priority f)
                      "\t\t" (:enabled f)))
        (if (isZero rem)
          (println (str "\n" "Done..."))
          (recur rem))))))

(defn filterInput [i]
 (cond 
   (= (:args i) ["help"]) (println "not yet implemented!")
   (= (:args i) ["status"]) (printStatus) 
   (= (first (:args i)) "init") (initialize
                                 i
                                 defaultConfigFile
                                 defaultConfigDir
                                 defaultSourceDirectory
                                 defaultModFolder) 
   (= (first (:args i)) "install") (install i)
   (= (:args i) ["restore"]) (restoreFiles (readDefaultConfig))
   (= (:args i) ["purge"]) (purgeAllModFiles
                            defaultConfigDir
                            defaultDeployPath)
   (= (:args i) ["list-mod"]) (listAllMods true (readDefaultConfig))
   (= (first (:args i)) "compare") (compFiles i)
   (= (:args i) ["clean"]) (cleanDir)
   (= (first (:args i)) "test") (println (next (:args i)))
   (= (first (:args i)) "set-mod") (changeModEntry i)
   (= (:args i) ["deploy"]) (deployMods 
                             defaultConfigDir
                             defaultConfigFile
                             defaultDeployPath)
   (= (first (:args i)) "list-files") (list-files (-> i
                                                      :args
                                                      next
                                                      first)
                                                  true
                                                  false)
   (= (:args i) ["diff"]) (diffDirectories "gameFolder" "modFolder1")
   :else (println "invalid argument: " (:args i))))

(filterInput input)