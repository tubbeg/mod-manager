(ns app.mount
  (:require
   [babashka.process :refer [shell process exec check]]
   [clojure.string :as s]
   [app.deploy :refer [moveFilesInPriority]]
   [app.utility :refer [unmountOverlay
                        mkdirCmd
                        notZero
                        isNilOrEmptyString
                        removeLastColon
                        addBackSlashBeforeWhiteSpace
                        mountOverlay]]))

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
          (addBackSlashBeforeWhiteSpace (:path e))
          errorPath))
      (->> (filterDisabledMods entries)
           (map
            #(str
              (addBackSlashBeforeWhiteSpace (:path %))
              ":"))
           (s/join)
           (removeLastColon)))
    errorPath))

(defn unmount-mods [config]
  (let [res (unmountOverlay (:overlay-name config))
        isNoFailure (= res :ok)]
    (if isNoFailure
      (do
        (println "Removing files from: " (:work-dir config))
        (println "Removing files from: " (:upper-dir config))
        (println "Continue? (Y/n)")
        (when (= (read-line) "Y")
          (shell "rm -rf" (:work-dir config))
          (shell "rm -rf" (:upper-dir config))
          (println "Done...")))
      (println "Something went wrong..."))))


(defn start-mt [name lower upper work merge config]
  (println "Lower dir: " lower)
  (println "Upper dir: " upper)
  (println "Work dir: " work)
  (println "Overlay name: " name)
  (println "Destination: " merge)
  (let [res (mountOverlay name
                          upper
                          lower
                          work
                          merge)]
    (when (= res :ok)
      (moveFilesInPriority config)
      (println "Done!"))))

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
    (cond
      (= lowerdir errorPath) (println "No mods available!")
      (not (isNilOrEmptyString lowerdir)) (start-mt
                                           name
                                           lowerdir
                                           upperdir
                                           work
                                           game-path
                                           config)
      :else (println "Error! Lower dir missing!"))))