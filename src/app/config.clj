(ns app.config
  (:require 
   [babashka.process :refer [shell process exec]] 
   [clojure.edn :as edn]
   [app.utility :refer [mkdirCmd directoryExists readFile
                        writeToFile]]))


(defn createConfig
  [gamePath config config-dir mod-dir upper work name]
  (if (and (not= gamePath "") (not= gamePath nil))
    (if (not (directoryExists config-dir))
      (do
        (mkdirCmd config-dir)
        (mkdirCmd work)
        (mkdirCmd upper)
        (let [conf {:game-path gamePath
                    :overlay-name name
                    :deployed false
                    :numberOfMods 0
                    :work-dir work
                    :upper-dir upper
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
  [i config config-dir mod-dir upper work name]
  (-> i
      :args
      (next)
      (first)
      (createConfig config config-dir mod-dir upper work name)))