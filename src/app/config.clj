(ns app.config
  (:require 
   [babashka.process :refer [shell process exec]] 
   [clojure.edn :as edn]
   [app.utility :refer [mkdirCmd directoryExists readFile
                        writeToFile isNilOrEmptyString]]))


(defn createConfig
  [gamePath config config-dir mod-dir lower name]
  (if (not (directoryExists config-dir))
    (do
      (mkdirCmd config-dir)
      (let [conf {:game-path gamePath
                  :overlay-name name
                  :deployed false
                  :numberOfMods 0
                  :lower-dir lower
                  :mods {:dir mod-dir
                         :entries []}}]
        (-> conf
            str
            (writeToFile config))))
    (println "Error: .mm config already exists!")))

(defn readFromConfig [config-dir config-file]
  (if (directoryExists config-dir)
    (->
     (readFile config-file)
     (edn/read-string))
    (do
      (println "Error! Missing config directory!")
      {:error :missing-directory!})))

(defn initialize
  [path config config-dir mod-dir lower name]
  (if (isNilOrEmptyString path)
    (println "Invalid path: " path)
    (createConfig path config config-dir
                  mod-dir lower name)))