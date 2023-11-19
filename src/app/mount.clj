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

(defn unmount-mods [config config-path]
  (if (= (:deployed config) true)
    (do
      (println "Detected upper directory: " (:upper-dir config))
      (println "Do you want to unmount? (Y/n)")
      (when (= (read-line) "Y")
        (if (= (unmountOverlay (:overlay-name config)) :ok)
          (do
            (println "Removing upper directory...")
            (shell "rm -rf " (-> config
                                 :upper-dir))
            (-> config
                (assoc :deployed false)
                (str)
                (writeToFile config-path))
            (println "Done...")) 
          (println "Something went wrong..."))))
    (println "Your overlay is not mounted!")))



(defn start-mt [name lower upper work merge config file]
  (println "Lower dir: " lower)
  (println "Overlay name: " name)
  (println "Destination: " merge)
  (println "Creating upper dir: " upper)
  (mkdirCmd upper)
  (let [res (mountOverlay name
                          lower
                          upper
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
        game-path (-> config
                      :game-path)
        lowerdir (-> config
                     :lower-dir)
        workdir (-> config
                     :work-dir)
        upperdir (-> config
                    :upper-dir)
        name (-> config
                 :overlay-name)]
    (cond
      isMounted (println "Your overlay is already mounted!")
      (= lowerdir errorPath) (println "No mods available!")
      (not (isNilOrEmptyString lowerdir)) (start-mt
                                           name
                                           lowerdir
                                           upperdir
                                           workdir
                                           game-path
                                           config
                                           config-path)
      :else (println "Error! Lower dir missing!"))))