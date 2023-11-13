#!/usr/bin/env bb

(ns app.manager
  (:require [babashka.cli :as cli]
            [clojure.edn :as edn]
            [clojure.string :as s]
            [app.utility :refer [list-files
                                 mkdirCmd
                                 copyDirNoReplace
                                 list-files-skip-prefix
                                 compareFiles
                                 diffDirectories
                                 writeToFile
                                 extract
                                 readFile
                                 directoryExists
                                 removeFileExtension]]
            [babashka.process :refer [shell process exec check]]))

(def input (cli/parse-args *command-line-args* ))

(def defaultConfigDir ".mm")
(def defaultConfigFile ".mm/config.edn")
(def defaultModFolder ".mm/mods")
(def defaultExtension ".mm")

(defn createHashKey [s]
  (->>
   (hash s)
   (str "path") 
   (keyword)))

(defn createOrigin [path]
  (let [f (list-files path false true)]
    (if (< (count f) 1) 
      (println "No files found!") 
      (loop [files f 
             origin {}]
        (println "files is" files "and path is" path)
        (let [first (first files)
              k (createHashKey first)
              rem (next files) 
              newOrigin (assoc origin k :unchanged)]
          ;(println "neworigin is" newOrigin)
          (if (< (count files) 1) 
            origin 
            (recur rem newOrigin)))))))

(defn createOriginTracker [mod path]
  (println "creating origin file from mod:" mod "at path: " path)
  (let [origin (createOrigin (str defaultModFolder "/" mod))]
    (writeToFile (str origin) path)))

(defn createConfig [gamePath]
  (if (and (not (= gamePath "")) (not (= gamePath nil))) 
    (if (not (directoryExists defaultConfigDir)) 
      (do   
        (mkdirCmd defaultConfigDir) 
        (let [conf {:game-path gamePath    
                    :numberOfMods 0 
                    :mods {:dir defaultModFolder 
                           :entries []}}] 
          (-> conf    
              str 
              (writeToFile defaultConfigFile)))) 
      (println "Error: .mm config already exists!"))
    (println "Invalid path!")))

(defn readFromConfig []
  (if (directoryExists defaultConfigDir) 
    (->
     (readFile defaultConfigFile)  
     (edn/read-string))
    (do
     (println "Error! Missing config directory!") 
     {:error :missing-directory!})))

(defn printStatus []
  (->> (readFromConfig)
      (println "status: ")))

(defn createModEntry [mod n dir priority]
  (let [entry {:name n
               :priority priority
               :enabled true
               :path (str dir "/" n)
               :source mod}]
    entry))


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


(defn installMod [mod-path]
  (if (directoryExists defaultConfigDir) 
  (do
    (println "installing: " mod-path) 
    (let [m (first mod-path)
          n (removeFileExtension m) 
          c (readFromConfig)
          dir (-> c 
                  :mods 
                  :dir)
          fullpath (str dir "/" n)] 
      (if (hasMod c n)  
        (println "Error: mod already exists!") 
        (do 
          (mkdirCmd fullpath)
          (extract m fullpath)
          ;(createOriginTracker n (str ".mm/" n "-origin.edn"))
          (let [prio (:numberOfMods c) 
                e (createModEntry m n dir prio) 
                newEntries (appendModEntry c e) 
                newConfig (-> (assoc c :mods newEntries) 
                              (assoc :numberOfMods
                                     (+ (:numberOfMods c) 1)))]
            
            (println "new config is: " newConfig) 
            (writeToFile (str newConfig) defaultConfigFile)
            (println "Succesfully installed mod at: " dir))))))
    (println "Error! Missing config directory!")))



(defn findIdenticalFiles []
  )

(defn deployMods [ext]
  (if (not (directoryExists defaultConfigDir))
    (println "Error! Missing config directory!")
    (let [config (readFromConfig)
          game-dir (:game-path config)
          entries (-> config
                      :mods
                      :entries)]
      (if (< (count entries) 1) 
        (println "Error! No mods to deploy!") 
        (loop [e (sort-by :priority entries)] 
          (let [m (first e) 
                remainder (next e)
                p (:path m)]
            (copyDirNoReplace (str p "/") game-dir ext)
            (if (< (count remainder) 1)
              (println "Finished deploying")
              (recur remainder))))))))

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


(defn filterExtension [files ext]
  )

(defn removeWithExt [ext]
  (let [c (readFromConfig)
        path (:game-path c)
        files (list-files path false true)]
    ))

(defn filterInput [i]
 (cond 
   (= (:args i) ["help"]) (println "not yet implemented!")
   (= (:args i) ["status"]) (printStatus) 
   (= (first (:args i)) "init") (-> i
                                   :args
                                   next
                                   first
                                   createConfig) 
   (= (first (:args i)) "install") (-> i
                                      :args
                                      next
                                      installMod)
   (= (:args i) ["purge"]) (println "not yet implemented")
   (= (first (:args i)) "compare") (compFiles i)
   (= (:args i) ["clean"]) (shell "rm -rf .mm")
   (= (:args i) ["enable"]) (println "not yet implemented") 
   (= (:args i) ["disable"]) (println "not yet implemented")
   (= (first (:args i)) ["set-priority"]) (println "not yet implemented")
   (= (:args i) ["deploy"]) (deployMods defaultExtension)
   (= (first (:args i)) "list-files") (list-files (-> i
                                                      :args
                                                      next
                                                      first)
                                                  true
                                                  false)
   (= (:args i) ["diff"]) (diffDirectories "gameFolder" "modFolder1")
   :else (println "invalid argument: " (:args i))))

(filterInput input)


(def test1 ".mm/mods/test/Generated/Public/Shared/Assets/Characters/_Models/Dragonborn/_Female/Resources/DGB_F_ARM_FlamingFist_Leather_Body.gr2")

(def test2 ".mm3/mods/test/Generated/Public/Shared/Assets/Characters/_Models/Dragonborn/_Female/Resources/DGB_F_ARM_FlamingFist_Leather_Body.gr2")

(createHashKey test1)

(s/split "./modFolder1/file1.txt" #"./modFolder1")