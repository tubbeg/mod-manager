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


(defn copyToDir [entries dir]
  (if (notZero entries)
    (loop [e entries]
      (let [entry (first e)
            rem (next e)
            path (-> entry
                     :path
                     (removeLastSlash)
                     (str "/"))
            enabled (-> entry
                        :enabled)] 
        (when enabled
         (println "Copying dir: " path) 
          (rsyncDirCmd path dir))
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
                  :upper-dir
                  (removeLastSlash)
                  (str "/"))
        ordered (sort-by :priority entries)
        ] 
    (copyToDir ordered upper)
    ;(println "Syncing files with upper directory...")
    ;(rsyncDirCmd work-dir upper)
    ))