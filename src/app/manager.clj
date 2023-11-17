#!/usr/bin/env bb

(ns app.manager
  (:require [babashka.cli :as cli]
            [clojure.string :as s]
            [app.config :refer []]
            [app.utility :refer [mkdirCmd
                                 mountOverlay
                                 unmountOverlay
                                 isZero
                                 readFile
                                 notZero
                                 writeToFile
                                 extract
                                 removeLastColon
                                 isNilOrEmptyString
                                 addBackSlashBeforeWhiteSpace
                                 directoryExists
                                 isFile
                                 removeFileExtension]]
            [babashka.process :refer [shell process exec check]]
            [app.deploy :refer [moveFilesInPriority]]
            [app.config :refer [initialize
                                readFromConfig]]))



(def defaultConfigDir ".mm")
(def defaultOverlayName "myOverlay")
(def defaultConfigFile ".mm/config.edn")
(def defaultModFolder ".mm/mods")
(def defaultUpperDir ".mm/upper")
(def defaultWorkDir ".mm/work")

(defn readDefaultConfig []
  (readFromConfig
   defaultConfigDir
   defaultConfigFile))

(defn writeDefaultConfig [content]
  (writeToFile (str content) defaultConfigFile))

(defn parse-int [s]
  (Integer/parseInt (re-find #"\A-?\d+" s)))

(defn selectPath [alt current]
  (if (or (isNilOrEmptyString alt)
          (not (directoryExists alt)))
    current
    alt))

(defn createModEntry [source name dir priority enable alt-path]
  (println "Creating entry for mod: " name)
  (println "with priority: " priority)
  {:name name
   ; priority is the load order currently
   :priority priority
   :alt-deploy-path (selectPath alt-path :none)
   :enabled enable
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
                 (fn [e] (= (:name e) name)) entries)
        nrOfMatches (count matches)]
    (> nrOfMatches 0)))


(defn handleTemplate [config template]
  (println "Not yet implemented!"))

(defn _installMod [mod-path priority]
  (println "installing:" mod-path)
  (let [m  mod-path
        n (removeFileExtension m)
        c (readDefaultConfig)

        dir (-> c
                :mods
                :dir)
        fullpath (str dir "/" n)]
    (if (hasMod c n)
      (println "Error: mod already exists!")
      (do
        (mkdirCmd fullpath)
        (extract m fullpath)
        (let [e (createModEntry m n dir priority true :none)
              newEntries (appendModEntry c e)
              newConfig (-> (assoc c :mods newEntries)
                            (assoc :numberOfMods
                                   (+ (:numberOfMods c) 1)))]

          (println "new config is: " newConfig)
          (writeToFile (str newConfig) defaultConfigFile)
          (println "Succesfully installed mod at: " dir)
          (println "Running template")
          (handleTemplate nil nil))))))

(defn installMod [mod-path priority]
  (if (or (isNilOrEmptyString mod-path)
          (isNilOrEmptyString priority)
          (not (directoryExists defaultConfigDir))
          (not (isFile mod-path)))
    (println "Error! Invalid name or priority!"
             "Install using with 'mm install <filename> <priority>'"
             "Verify that your config file is valid.")
    (_installMod mod-path priority)))

(defn install [i]
  (let [mod (-> i
                :opts
                :path)
        prio (-> i
                 :opts
                 :priority)
        trimmed (s/trim mod)]
    (println "installing mod: " trimmed)
    (installMod trimmed prio)))

(defn cleanDir [args]
  (println "You are about to delete your mod directory!")
  (println "Create BACKUP of any files you might need!")
  (println "Continue? (Y/n)")
  (when (= (read-line) "Y")
    (do
      (println "Removing .mm files...")
      (shell "rm -rf .mm"))))


(defn searchMod [name coll]
  (filter #(= name (:name %)) coll))

(defn changeEntry [mod-name entry coll]
  (map #(if (= mod-name (:name %))
          entry
          %)
       coll))

(defn get-mod [mod-name config]
  (let [entries (-> config
                    :mods
                    :entries)
        matches (searchMod mod-name entries)
        zero (isZero matches)]
    (if zero
      :error
      (first matches))))

(defn filterEntriesByName [mod-name entries]
  (filter #(not= (:name %) mod-name) entries))

(defn remove-mod [mod-name config]
  (let [entries (-> config
                    :mods
                    :entries)
        mod (get-mod mod-name config)
        path (:path mod)
        modsConfig (:mods config)
        newEntries (filterEntriesByName mod-name entries)
        newMods (assoc modsConfig :entries newEntries)
        newConfig (assoc config :mods newMods)]
    (if (or (= mod nil) (= mod :error))
      (println "Error in removing mod!")
      (do
        (println "Created new config: " newConfig)
        (writeDefaultConfig newConfig)
        path))))

(defn removeModAndFiles [args]
  (let [mod-name (-> args
                     :opts
                     :name)
        config (readDefaultConfig)]
    (if (isNilOrEmptyString mod-name)
      (println "Invalid name!")
      (do
        (println "Removing files from: " mod-name)
        (println "Continue? (Y/n)")
        (if (= (read-line) "Y")
          (->>
           (remove-mod mod-name config)
           (shell "rm -rf"))
          (println "Aborted"))))))



(defn set-mod [mod-name enable priority alt-path config]
  (let [entries (-> config
                    :mods
                    :entries)
        dir (-> config
                :mods
                :dir)
        mod (get-mod mod-name config)
        modsConfig (:mods config)
        newEntry (createModEntry
                  (:source mod)
                  mod-name
                  dir
                  priority
                  enable
                  alt-path)
        newEntries (changeEntry mod-name newEntry entries)
        newMods (assoc modsConfig :entries newEntries)
        newConfig (assoc config :mods newMods)]
    (if (or (= mod nil) (= mod :error))
      (println "Error in setting mod!")
      (do
        (println "Created new config: " newConfig)
        (writeDefaultConfig newConfig)))))

(defn changeModEntry [args]
  (println "Args:" args)
  (let [mod-name (-> args
                     :opts
                     :name)
        enable (-> args
                   :opts
                   :enable)
        prio (-> args
                 :opts
                 :priority)
        new-path (-> args
                     :opts
                     :new-path)]
    (if (or (isNilOrEmptyString mod-name)
            (isNilOrEmptyString prio)
            (isNilOrEmptyString enable))
      (println "Invalid input!")
      (set-mod mod-name enable prio new-path (readDefaultConfig)))))




(defn printStatus [args]
  (let [tabs "\t\t\t\t"
        c (readDefaultConfig)
        entries (->> c
                     :mods
                     :entries
                     (sort-by :priority))]
    (println "Config file:" c)
    (println "")
    (println "Name" tabs "Priority" tabs "Enabled?")
    (loop [e entries]
      (let [f (first e)
            rem (next e)]
        (println (str (:name f)
                      tabs (:priority f)
                      tabs (:enabled f)))
        (if (isZero rem)
          (println (str "\n" "Done..."))
          (recur rem))))))


(defn firstArg [i match]
  (if
   (and (not= i nil) (not= match nil))
    (let [f (first (:args i))]
      (if (not= f nil)
        (= (s/trim f) match)
        false))
    false))

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

(defn mount-mods [args]
  (let [config (readDefaultConfig)
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
    (if (isNilOrEmptyString lowerdir)
      (println "Error! Lower dir missing!")
      (do
        (println "Lower dir: " lowerdir)
        (println "Upper dir: " upperdir)
        (println "Work dir: " work)
        (println "Overlay name: " name)
        (println "Destination: " game-path)
        (let [res (mountOverlay name
                                upperdir
                                lowerdir
                                work
                                game-path)]
          (when (= res :ok)
            (moveFilesInPriority config)
            (println "Done!")))))))


(defn quickHelp [cmdArgs]
  (println "Initialize a .mm directory using 'mm init $MY_PATH'")
  (println "Files will be installed to your .mm directory (.mm/mods)")
  (println "Check status with 'mm status'")
  (println "Install mods using 'mm install /path/to/mod.zip 100'")
  (println "the last number is the priority (load order)")
  (println "Priority order, and enabling and disabling mods can")
  (println "be done using 'mm set-mod myModName false 123'")
  (println "Remove mods using 'mm remove-mod myModName'")
  (println "mount using 'mm mount'")
  (println "unmount using 'mm unmount'")
  (println "NOTE! Mounting/unmounting requires a superuser!")
  (println "You should mount with the following command:")
  (println "'sudo -E env \"PATH=$PATH\" mm mount'"))

(defn unmount-mods [args]
  (let [config (readDefaultConfig)
        res (unmountOverlay (:overlay-name config))
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


(defn initCmd [m]
  (let [path (-> m
                 :opts
                 :path)]
    (initialize path
                defaultConfigFile
                defaultConfigDir
                defaultModFolder
                defaultUpperDir
                defaultWorkDir
                defaultOverlayName)))

(def table
  [{:cmds ["mount"]   :fn mount-mods}
   {:cmds ["unmount"] :fn unmount-mods}
   {:cmds ["status"]   :fn printStatus}
   {:cmds ["init"] :fn initCmd :args->opts [:path]}
   {:cmds ["install"] :fn install :args->opts [:path :priority]}
   {:cmds ["set-mod"] :fn changeModEntry :args->opts
    [:name :enable :priority :new-path]}
   {:cmds ["remove-mod"] :fn removeModAndFiles :args->opts [:name]}
   {:cmds ["clean"] :fn cleanDir}
   {:cmds ["help"] :fn quickHelp}
   {:cmds []       :fn quickHelp}])

(def cmdLineArgs *command-line-args*)
(defn start-mod-manager []
  (cli/dispatch table cmdLineArgs {:coerce {:priority :long}}))

(start-mod-manager)



