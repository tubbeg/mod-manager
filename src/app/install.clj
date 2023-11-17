(ns app.install
  (:require [app.utility :refer [removeFileExtension
                                 isNilOrEmptyString
                                 extract
                                 mkdirCmd
                                 directoryExists
                                 writeToFile
                                 isFile]]))
(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn createModEntry [source name dir priority enable]
  (println "Creating entry for mod: " name)
  (println "with priority: " priority)
  {:name name
   ; priority is the load order currently
   :priority priority
   :enabled enable
   :hash (hash name)
   :path (str dir "/" name)
   :source source})


(defn appendModEntry [config entry]
  (let [entries (-> config
                    :mods
                    :entries)
        newEntry (conj entries entry)]
    (assoc (:mods config) :entries newEntry)))

(defn hasMod [config name]
  (let [entries (-> config
                    :mods
                    :entries)
        matches (filter
                 (fn [e] (= (:hash e) (hash name))) entries)
        nrOfMatches (count matches)]
    (> nrOfMatches 0)))


(defn handleTemplate [config template]
  (println "Not yet implemented!"))

(defn _installMod [mod-path config priority config-file-path]
  (println "installing:" mod-path)
  (let [m  mod-path
        n (removeFileExtension m)
        c config

        dir (-> c
                :mods
                :dir)
        fullpath (str dir "/" n)]
    (if (hasMod c n)
      (println "Error: mod already exists!")
      (do
        (mkdirCmd fullpath)
        (extract m fullpath)
        (let [e (createModEntry m n dir priority true)
              newEntries (appendModEntry c e)
              newConfig (-> (assoc c :mods newEntries)
                            (assoc :numberOfMods
                                   (+ (:numberOfMods c) 1)))]

          (println "new config is: " newConfig)
          (writeToFile (str newConfig) config-file-path)
          (println "Succesfully installed mod at: " dir)
          (println "Running template")
          (handleTemplate nil nil))))))

(defn installMod [mod-path priority config config-file-path]
  (if (or (isNilOrEmptyString mod-path)
          (isNilOrEmptyString priority)
          (not (isFile mod-path)))
    (println "Error! Invalid name or priority!"
             "Install using: 'mm install <filename> <priority>'"
             "Verify that your config file is valid.")
    (_installMod mod-path config priority config-file-path)))

