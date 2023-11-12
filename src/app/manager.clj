#!/usr/bin/env bb

(ns app.manager
  (:require [babashka.cli :as cli]
            [clojure.edn :as edn]
            [clojure.string :as s]
            [app.utility :refer [mkdirCmd
                                 rsyncDirCmd
                                 rmDir
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

(defn createConfig [gamePath]
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
    (println "Error: .mm config already exists!")))

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
          (let [prio (:numberOfMods c) 
                e (createModEntry m n dir prio) 
                newEntries (appendModEntry c e) 
                newConfig (-> (assoc c :mods newEntries) 
                              (assoc :numberOfMods
                                     (+ (:numberOfMods c) 1)))]
            
            (println "new config is: " newConfig) 
            (writeToFile (str newConfig) defaultConfigFile)
            (println "Succesfully installed mod at: " dir))))))
    (println "Error! Missing config directory!") ))

(def entries 
  [{:name "test",
    :priority 0, 
    :enabled true, 
    :path ".mm/mods/test", 
    :source "test.zip"}
   {:name "test2",
    :priority 3,
    :enabled true,
    :path ".mm/mods/test",
    :source "test.zip"}
   {:name "test2",
    :priority 2,
    :enabled true,
    :path ".mm/mods/test",
    :source "test.zip"} 
   ])

(sort-by :priority entries)
 (sort-by :rank [{:rank 2} {:rank 3} {:rank 1}])

(defn deployMods []
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
            (rsyncDirCmd (str p "/") game-dir)
            (if (< (count remainder) 1)
              (println "Finished deploying")
              (recur remainder))))))))


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
   (= (:args i) ["clean"]) (shell "rm -rf .mm")
   (= (:args i) ["enable"]) (println "not yet implemented") 
   (= (:args i) ["disable"]) (println "not yet implemented")
   (= (first (:args i)) ["set-priority"]) (println "not yet implemented")
   (= (:args i) ["deploy"]) (deployMods) 
   :else (println "invalid argument: " (:args i))))

(filterInput input)
