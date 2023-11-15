#!/usr/bin/env bb

(ns app.manager
  (:require [babashka.cli :as cli]
            [clojure.string :as s]
            [app.config :refer []]
            [app.utility :refer [mkdirCmd
                                 mountOverlay
                                 unmountOverlay
                                 isZero
                                 notZero
                                 writeToFile
                                 extract
                                 removeLastColon
                                 isNilOrEmptyString
                                 directoryExists
                                 removeFileExtension]]
            [babashka.process :refer [shell process exec check]]
            [app.deploy :refer [moveFilesInPriority]]
            [app.config :refer [initialize
                                readFromConfig]]))

(def input (cli/parse-args *command-line-args* ))

(def defaultConfigDir ".mm")
(def defaultOverlayName "myOverlay")
(def defaultConfigFile ".mm/config.edn")
(def defaultModFolder ".mm/mods")
(def defaultUpperDir ".mm/upper")
(def defaultWorkDir ".mm/work")

(defn readDefaultConfig []
  (readFromConfig
   defaultConfigDir
   defaultConfigFile))

(defn writeDefaultConfig [content] 
  (writeToFile (str content) defaultConfigFile))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn createModEntry [source name dir priority enable]
  (println "Creating entry for mod: " name)
  (println "with priority: " priority)
  {:name name
   ; priority is the load order currently
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

(defn filterEntriesByName [mod-name entries]
  (filter #(not= (:name %) mod-name) entries))

(defn remove-mod [mod-name config]
  (let [entries (-> config
                    :mods
                    :entries) 
        mod (get-mod mod-name config) 
        path (:path mod)
        modsConfig (:mods config)
        newEntries (filterEntriesByName mod-name entries)
        newMods (assoc modsConfig :entries newEntries)
        newConfig (assoc config :mods newMods)]
    (if (or (= mod nil) (= mod :error))
      (println "Error in removing mod!")
      (do
        (println "Created new config: " newConfig)
        (writeDefaultConfig newConfig)
        path))))

(defn removeModAndFiles [config i]
  (let [mod-name (-> i
                     :args
                     (next)
                     (first))] 
    (if (isNilOrEmptyString mod-name) 
      (println "Invalid name!") 
      (do 
        (println "Removing files from: " mod-name) 
        (println "Continue? (Y/n)") 
        (if (= (read-line) "Y") 
          (->> 
           (remove-mod mod-name config) 
           (shell "rm -rf")) 
          (println "Aborted"))))))

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


(defn firstArg [i match]
  (if
   (and (not= i nil) (not= match nil)) 
    (let [f (first (:args i))] 
      (if (not= f nil) 
        (= (s/trim f) match) 
        false)) 
    false))

(def errorPath {:error :missing-paths})

(defn filterDisabledMods [entries]
  (filter #(:enabled %) entries))

(defn buildDir2 [entries]
   (if (notZero entries) 
     (if (= (count entries) 1) 
       (let [e (first entries)]
         (println "Only one entry:")
         (println "first entry is is" e)
         (if (:enabled e)
           (:path e)
           errorPath))
       (->> (filterDisabledMods entries) 
            (map #(str (:path %) ":"))
            (s/join) 
            (removeLastColon)))
     errorPath))

(defn mount-mods [config]
  (let [entries (-> config
                    :mods
                    :entries)
        game-path (-> config
                      :game-path)
        lowerdir (buildDir2 entries)
        upperdir (-> config
                     :upper-dir)
        work (-> config
                 :work-dir)
        name (-> config
                 :overlay-name)]
    (mkdirCmd upperdir)
    (mkdirCmd work)
    (if (isNilOrEmptyString lowerdir) 
      (println "Error! Lower dir missing!") 
      (do 
        (println "Lower dir: " lowerdir) 
        (println "Upper dir: " upperdir) 
        (println "Work dir: " work) 
        (println "Overlay name: " name) 
        (println "Destination: " game-path) 
        (mountOverlay name upperdir lowerdir work game-path) 
        (moveFilesInPriority config) 
        (println "Done!")))))

(defn unmount-mods [config] 
  (unmountOverlay (:overlay-name config))
  (println "Removing files from: " (:work-dir config)) 
  (println "Removing files from: " (:upper-dir config))
  (println "Continue? (Y/n)")
  (when (= (read-line) "Y")  
    (shell "rm -rf" (:work-dir config))
    (shell "rm -rf" (:upper-dir config))
    (println "Done...")))

(defn filterInput [i] 
  (cond  
    (firstArg i "help") (println "not yet implemented!") 
    (firstArg i "status") (printStatus) 
    ;-------
    ;run mount and unmount commands with
    ;sudo -E env "PATH=$PATH" /path/to/script mount
    ;otherwise it will fail
    (firstArg i "mount") (mount-mods (readDefaultConfig)) 
    (firstArg i "unmount") (unmount-mods (readDefaultConfig))
    ;-------
    (firstArg i "init") (initialize  
                         i  
                         defaultConfigFile  
                         defaultConfigDir 
                         defaultModFolder 
                         defaultUpperDir 
                         defaultWorkDir
                         defaultOverlayName) 
    (firstArg i "install") (install i) 
    (firstArg i "clean") (cleanDir) 
    (firstArg i "remove-mod") (removeModAndFiles (readDefaultConfig) i) 
    (firstArg i "set-mod") (changeModEntry i) 
    :else (println "invalid argument: " (:args i))))



(filterInput input)

