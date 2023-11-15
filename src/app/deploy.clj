(ns app.deploy
  (:require 
   [app.utility :refer [notZero isZero 
                        removePrefix 
                        list-files-prefix 
                        list-files 
                        removeLastSlash
                        rsyncDirCmd]]
   [app.config :refer [readFromConfig ]]
   [clojure.edn :as edn]
   [clojure.string :as s]))


(defn listAllModFiles [mod-names mods-path]
  (when (notZero mod-names)
    (loop [m mod-names
           coll []]
      (let [firstMod (first m)
            remaining (next m)
            files (-> (list-files-prefix mods-path firstMod)
                      (concat coll)
                      (into []))]
        (if (notZero remaining)
          (recur remaining files)
          files)))))

(defn equalPathsPrefix [p1 p1Prefix p2 p2Prefix]
  (let [c1 (removePrefix p1Prefix p1)
        c2 (removePrefix p2Prefix p2)]
    ;(println "Comparing: " c1 "and" c2)
    (= c1 c2)))

(defn hasConflict [mod-file mod-path listOfFiles]
  (if (notZero listOfFiles) 
    (let [filteredFiles (filter #(equalPathsPrefix  
                                  mod-file  
                                  mod-path 
                                  (:full-path %)  
                                  (:prefix %)) listOfFiles) 
          n (notZero filteredFiles)]
      n) 
    false))


(defn copyToDir [entries dir]
  (if (notZero entries)
    (loop [e entries]
      (let [entry (first e)
            rem (next e)
            path (-> entry
                     :path
                     (removeLastSlash)
                     (str "/"))] 
        (println "Copying dir: " path)
        (rsyncDirCmd path dir)
        (if (notZero rem)
          (recur rem)
          :done))) 
    (println "Error! Missing files!")))

(defn moveFilesInPriority [config]
  ;1. gather entries
  ;2. copy each file in order to work-dir
  ;3. move final result to upper 
  (let [entries (-> config 
                    :mods 
                    :entries) 
        work-dir (-> config 
                     :work-dir
                     (removeLastSlash)
                     (str "/")) 
        upper (-> config 
                  :upper-dir)
        ordered (sort-by :priority entries)
        ] 
    (copyToDir ordered work-dir)
    (println "Syncing files with upper directory...")
    (rsyncDirCmd work-dir upper)))