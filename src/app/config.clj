(ns app.config
  (:require 
   [babashka.process :refer [shell process exec]] 
   [clojure.edn :as edn]
   [app.utility :refer [mkdirCmd directoryExists readFile
                        writeToFile]]))


(defn createConfig
  [gamePath config config-dir source-dir mod-dir]
  (if (and (not= gamePath "") (not= gamePath nil))
    (if (not (directoryExists config-dir))
      (do
        (mkdirCmd config-dir)
        (mkdirCmd source-dir)
        (let [conf {:game-path gamePath
                    :numberOfMods 0
                    :source-dir source-dir
                    :mods {:dir mod-dir
                           :entries []}}]
          (-> conf
              str
              (writeToFile config))))
      (println "Error: .mm config already exists!"))
    (println "Invalid path!")))

(defn readFromConfig [config-dir config-file]
  (if (directoryExists config-dir)
    (->
     (readFile config-file)
     (edn/read-string))
    (do
      (println "Error! Missing config directory!")
      {:error :missing-directory!})))

(defn initialize
  [i config config-dir source-dir mod-dir]
  (-> i
      :args
      (next)
      (first)
      (createConfig config config-dir source-dir mod-dir)))