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
      (println "Detected dir: " (:lower-dir config))
      (println "Do you want to unmount? (Y/n)")
      (when (= (read-line) "Y")
        (if (= (unmountOverlay (:overlay-name config)) :ok)
          (do
            (println "Removing lower directory files...")
            (shell "rm -rf" (:lower-dir config))
            (-> config
                (assoc :deployed false)
                (str)
                (writeToFile config-path))
            (println "Done...")) 
          (println "Something went wrong..."))))
    (println "Your overlay is not mounted!")))



(defn start-mt [name lower merge config file]
  (println "Lower dir: " lower)
  (println "Overlay name: " name)
  (println "Destination: " merge)
  (mkdirCmd lower) 
  (moveFilesInPriority config)
  (let [res (mountOverlay name
                          lower
                          merge)
        newConfig (assoc config :deployed true)]
    (when (= res :ok)
      (writeToFile (str newConfig) file)
      (println "Done!"))))

(defn mount-mods [config config-path]
  (let [isMounted (-> config
                      :deployed)
        game-path (-> config
                      :game-path)
        lowerdir (-> config
                     :lower-dir)
        name (-> config
                 :overlay-name)]
    (cond
      isMounted (println "Your overlay is already mounted!")
      (= lowerdir errorPath) (println "No mods available!")
      (not (isNilOrEmptyString lowerdir)) (start-mt
                                           name
                                           lowerdir
                                           game-path
                                           config
                                           config-path)
      :else (println "Error! Lower dir missing!"))))