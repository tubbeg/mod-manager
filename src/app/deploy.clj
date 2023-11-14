(ns app.deploy
  (:require 
   [app.utility :refer [mkdirCmd directoryExists readFile 
                        writeToFile notZero isZero copyDir
                        copyFile removePrefix
                        removeLastSlash list-files-prefix 
                        list-files rmFile rsyncDirCmd]]
   [app.config :refer [readFromConfig ]]
   [clojure.edn :as edn]
   [clojure.string :as s]))

(defn addPrefix [coll prefix]
  (if (notZero coll)
    (loop [c coll
           files []]
      (let [firstFile (first c)
            remainder (next c)
            newfile (str prefix firstFile)
            newFiles (conj files newfile)]
        (if (notZero remainder)
          (recur remainder newFiles)
          newFiles)))
    (println "Error! Empty collection: " coll)))

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

(defn listAllMods [verbose config]
  (let [mods-path (-> config
                      :mods
                      :dir)
        game-path (-> config
                      :game-path
                      (removeLastSlash))
        allMods (->> config
                     :mods
                     :entries
                     (map #(:name %)))
        files (listAllModFiles allMods mods-path)
        game-files (addPrefix files game-path)]
    ;(println files)
    (when verbose
      (println "Game files:" game-files)
      (println "Mods: " allMods))
    game-files))


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
    ;(println "Found filtered files: " filteredFiles) 
    ;  (when n 
    ;    (println "File:" mod-file "has a conflict!")) 
      n) 
    false))

(defn addEntryToDeployList [list mod-path file priority]
  (conj list {:prefix mod-path
              :priority priority
              :full-path file}))

(defn addNonConflictFiles [mod-files prio mod-path listOfFiles] 
  (when (notZero mod-files)  
    (loop [mf mod-files  
           lof listOfFiles] 
      (let [file (first mf)  
            rem (next mf) 
            l (addEntryToDeployList lof mod-path file prio)  
            hasConf (hasConflict file mod-path listOfFiles)] 
        (cond  
          (and (not hasConf) (notZero rem)) (recur rem l)
          (and hasConf (notZero rem))  (recur rem lof)
          (and (not hasConf) (isZero rem)) l
          :else lof)))))

(defn createDeployList [config]
  (let [e (->> config 
               :mods 
               :entries 
               (sort-by :priority))]
    (loop [entries e
           allModFiles []]
      (let [mod (first entries)
            rem (next entries)
            mod-prefix (:path mod)
            enabled (:enabled mod)
            prio (:priority mod)
            files (list-files (:path mod) false true)
            updateModFiles (addNonConflictFiles
                            files
                            prio
                            mod-prefix
                            allModFiles)]
        (cond 
          (and enabled (isZero rem)) updateModFiles
          (and enabled (notZero rem)) (recur rem updateModFiles)
          (and (not enabled) (notZero rem)) (recur rem allModFiles)
          :else allModFiles)))))


(defn deployMods [config-dir config-file deploy-path]
  (if (not (directoryExists config-dir))
    (println "Error! Missing config directory!")
    (let [config (readFromConfig config-dir config-file)
          deployList (createDeployList config)] 
      (if (isZero deployList)
        (println "Error! No mods to deploy!")
        (loop [e (sort-by :priority deployList)
               updatedDeploy []]
          (let [m (first e)
                remainder (next e)
                p (:full-path m)
                r (copyFile p (:prefix m) config false)
                updatedEntry (assoc m :destination r)
                updatedDeplList (conj updatedDeploy updatedEntry)]
            (if (isZero remainder)
              (do
                (writeToFile (str updatedDeplList) deploy-path)
                (println "Deploy list available at:" deploy-path)
                (println "Finished deploying"))
              (recur remainder updatedDeplList))))))))


(defn readDeployList [file config-dir]
  (if (directoryExists config-dir)
    (->
     (readFile file)
     (edn/read-string))
    (do
      (println "Error! Missing config directory!")
      {:error :missing-directory!})))

(defn getAllGamePathsFromDeploy [deploy]
  (if (or (= deploy nil) (= deploy {:error :missing-directory!})) 
    (do 
      (println "Something went wrong!") 
      :error) 
    (map #(:destination %) deploy)))

(defn purgeAllModFiles [config-dir deploy-path]
  (let [deploy (readDeployList deploy-path config-dir)
        paths (getAllGamePathsFromDeploy deploy)]
    (if (and (not (= paths :error))
             (not (= paths nil))
             (notZero paths)) 
      (do  
        (println "Detected following files:" paths) 
        (println "You are about to delete these files.") 
        (println "Continue? (Y/n)")
        (when (= (read-line) "Y")  
          (loop [p paths]  
            (let [f (first p)  
                  rem (next p)]  
              (rmFile f)  
              (if (isZero rem)  
                (println "Done...")  
                (recur rem)))))) 
      (println "No files detected!"))))

(defn restoreFiles [config]
  (let [source (:source-dir config)
        game-path (:game-path config)]
  (rsyncDirCmd (str source "/") game-path)))