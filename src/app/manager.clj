#!/usr/bin/env unshare -rm bb

(ns app.manager
  (:require [babashka.cli :as cli]
            [app.utility :refer [mkdirCmd rmDir
                                touchCmd overlayCmd
                                 ls-and-grep
                                 example]]
            [babashka.process :refer [shell process exec check]]))

(def cli-options {:port {:default 80 :coerce :long} 
                  :status {:coerce :boolean}
                  :help {:coerce :boolean}})

(def input (cli/parse-args *command-line-args* {:spec cli-options}))


(def defaultOverlayName "myModManagerOverlay")
(def workDir "myWork")
(def upperDir "myUpper")
(def lowerDir "myLower")
(def newFile (str lowerDir "/newfile.txt"))
(def mergeDir "myMerge")


(def exampleConfig
  {:game-path "/skyrim"
   :mods {:1 "path/to/mod"}
   :upper "myUpper"
   :work "myWork"})

(defn createConfig [gamePath]
  (mkdirCmd workDir)
  (mkdirCmd upperDir)
  {:game-path gamePath ;merge dir
   :mods {} ;lower dir
   :upper upperDir
   :work lowerDir})

(defn readModsFromConfig []
  ())

(defn mergeModDir [mods]
  ;joins all mod paths
  (if (< (count mods) 2) 
    (first (vals mods))
    (let [v (vals mods) 
          s (map (fn [e] (str e ":")) v)] 
      (apply str s))))

(mergeModDir {:1 "blalba" :2 "dfkopkf" :3 "sdasd"})
(mergeModDir {:1 "blalba"})

(defn mountOverlay [config]
  ;s is shell
  (println "'sudo -E env \"PATH=$PATH\" ./manager.clj mount'")
  (println "mounting overlay: " config)
  (overlayCmd
   ;s
   defaultOverlayName 
   (mergeModDir (:mods config))
   upperDir
   workDir
   (:game-path config)))


(defn installMod [args]
  (println "installing: " (-> args
                              next))
  (println "not yet implemented"))

(defn filterInput [i]
 (cond
  (= (:args i) ["status"]) (println "not yet implemented")
  (= (:args i) ["init"]) (println "not yet implemented")
  (= (first (:args i)) "install") (installMod (:args i))
  (= (:args i) ["enable"]) (println "not yet implemented")
  (= (:args i) ["disable"]) (println "not yet implemented")
  (= (:args i) ["mount"]) (mountOverlay :notyetimplemented)
  :else (println "invalid argument: " (:args i))))

(filterInput input)



(def defaultConfig
  {:mods {}})

(defn tryMistake []
  (try
    (shell "ls2")  
    (catch Exception e
           (println "incorrect cmd: " e))))

;(tryMistake)


(defn createDirectories []
  (mkdirCmd workDir)
  (mkdirCmd upperDir)
  (mkdirCmd lowerDir)
  (touchCmd newFile)
  (mkdirCmd mergeDir))

(defn cleanup []
  (rmDir workDir)
  (rmDir upperDir)
  (rmDir lowerDir)
  (rmDir mergeDir))


;example : mount -t overlay myModManagerOverlay -o lowerdir=myLower,upperdir=myUpper,workdir=myWork myMerge

(comment (defn ls-and-grep [search]
  (-> (process "ls")
      (process {:out :string} "grep" search)
      deref :out)))


(defn testUnshare []
  (println "unsharing")
  ;(mountOverlay {:mods {:1 lowerDir} :game-path mergeDir})
  (shell example)
  )
  
;(createDirectories)
;(shell "whoami")
;(ls-and-grep "my")
;(testUnshare)
(cleanup)