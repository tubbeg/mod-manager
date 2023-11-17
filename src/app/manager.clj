#!/usr/bin/env bb

(ns app.manager
  (:require [babashka.cli :as cli]
            [clojure.string :as s]
            [app.utility :refer [isZero
                                 notZero
                                 writeToFile
                                 isNilOrEmptyString
                                 directoryExists
                                 isFile]]
            [babashka.process :refer [shell
                                      process
                                      exec
                                      check]]
            [app.mount :refer [unmount-mods
                               mount-mods]]
            [app.config :refer [initialize
                                readFromConfig]]
            [app.install :refer [installMod
                                 createModEntry]]))

(def defaultConfigDir ".mm")
(def defaultOverlayName "mm-overlay")
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

(defn install [i]
  (let [mod (-> i
                :opts
                :path)
        prio (-> i
                 :opts
                 :priority)
        trimmed (s/trim mod)]
    (println "installing mod: " trimmed)
    (installMod trimmed prio (readDefaultConfig)
                defaultConfigFile)))

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



(defn set-mod [mod-name enable priority config]
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
                  enable)
        newEntries (changeEntry mod-name newEntry entries)
        newMods (assoc modsConfig :entries newEntries)
        newConfig (assoc config :mods newMods)]
    (if (or (= mod nil) (= mod :error))
      (println "Error in setting mod!")
      (do
        (println "Created new config: " newConfig)
        (writeDefaultConfig newConfig)))))

(defn changeModEntry [args]
  (let [mod-name (-> args
                     :opts
                     :name)
        enable (-> args
                   :opts
                   :enable)
        prio (-> args
                 :opts
                 :priority)]
    (if (or (isNilOrEmptyString mod-name)
            (isNilOrEmptyString prio)
            (isNilOrEmptyString enable))
      (println "Invalid input!")
      (set-mod mod-name enable prio
               (readDefaultConfig)))))




(defn printStatus [args]
  (let [tabs "\t\t\t\t"
        c (readDefaultConfig)
        entries (->> c
                     :mods
                     :entries
                     (sort-by :priority))]
    (println "Config file:" c)
    (println "")
    (println "Name" tabs "Priority (load order)" tabs "Enabled?")
    (loop [e entries]
      (let [f (first e)
            rem (next e)]
        (println (str (:name f)
                      tabs (:priority f)
                      tabs (:enabled f)))
        (if (isZero rem)
          (println (str "\n" "Done..."))
          (recur rem))))))

(defn mountMods [args]
  (mount-mods (readDefaultConfig) defaultConfigFile))


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


(defn createUniqueName [def s]
  (str def "_" (hash s)))

(defn initCmd [m]
  (let [path (-> m
                 :opts
                 :path)
        name (createUniqueName
              defaultOverlayName path)]
    (initialize path
                defaultConfigFile
                defaultConfigDir
                defaultModFolder
                defaultUpperDir
                defaultWorkDir
                name)))

(defn unmount [args]
  (unmount-mods (readDefaultConfig) defaultConfigFile))

(def table
  [{:cmds ["mount"]   :fn mountMods}
   {:cmds ["unmount"] :fn unmount}
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



