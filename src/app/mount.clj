(ns app.mount
  (:require
   [babashka.process :refer [shell process exec check]]
   [clojure.string :as s]
   [app.config]
   [app.deploy :refer [moveFilesInPriority]]
   [app.utility :refer [unmountOverlay
                        writeToFile
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
        (if (:enabled e)
          (str "\"" (:path e) "\"")
          errorPath))
      (->> (filterDisabledMods entries)
           (map
            #(str
              (str "\"" (:path %) "\"")
              ":"))
           (s/join)
           (removeLastColon)))
    errorPath))

(defn unmount-mods [config config-path]
  (if (= (:deployed config) true)
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
            (-> config
                (assoc :deployed false)
                (str)
                (writeToFile config-path))
            (println "Done...")))
        (println "Something went wrong...")))
    (println "Your overlay is not mounted!")))



(defn start-mt [name lower upper work merge config file]
  (println "Lower dir: " lower)
  (println "Upper dir: " upper)
  (println "Work dir: " work)
  (println "Overlay name: " name)
  (println "Destination: " merge)
  (let [res (mountOverlay name
                          upper
                          lower
                          work
                          merge)
        newConfig (assoc config :deployed true)]
    (when (= res :ok)
      (moveFilesInPriority config)
      (writeToFile (str newConfig) file)
      (println "Done!"))))

(defn mount-mods [config config-path]
  (let [isMounted (-> config
                      :deployed)
        entries (-> config
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
      isMounted (println "Your overlay is already mounted!")
      (= lowerdir errorPath) (println "No mods available!")
      (not (isNilOrEmptyString lowerdir)) (start-mt
                                           name
                                           lowerdir
                                           upperdir
                                           work
                                           game-path
                                           config
                                           config-path)
      :else (println "Error! Lower dir missing!"))))