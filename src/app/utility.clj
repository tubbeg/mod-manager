(ns app.utility
  (:require 
   [babashka.process :refer [shell process exec]] 
   [clojure.java.io :as io]  
   [clojure.string :as s]))
 

(defn directoryExists [path]
  ;(println "checking directory: " path)
  (.isDirectory (io/file path))) 

 (defn ls-and-grep [search]
  (-> (process "ls")
      (process {:out :string} "grep" search)
      deref :out))
 
 ; note! this will replace any file!!
 (defn extract [file dir]
   (if (directoryExists dir)
    (try
     (shell "7za" "x" "-y" (str "-o" dir) file)
     (catch Exception e
       (println "Failure! Exception: " e)))
     (println "missing directory!")))

 (defn readFile [file]
   (slurp file))

 ;note: overwrites any contents
 (defn writeToFile [contents filepath]
   (spit filepath contents))

(defn rsyncDirCmd [from to]
  (shell "rsync" "-a" from to))

(defn cpCmd [from to]
  (shell "cp" from to))

(defn mkdirCmd [path]
  (shell "mkdir -p " path))

(defn rmDir [path]
  (shell "rm -rf " path))

(defn touchCmd [file]
  (shell "touch " file))

(defn unmountCmd [dir]
  (shell "unmount " dir))


; this does not cover all cases
(defn removeFileExtension [s] 
  (first (s/split s #"\.")))


