(ns app.utility
  (:require 
   [babashka.process :refer [shell process exec]] 
   [clojure.java.io :as io]  
   [clojure.string :as s]))
 
 (defn diffDirectories [dir1 dir2]
  (println
   (-> (process {:out :string} "diff -qrs" dir1 dir2)
       deref :out)))

 (defn list-files [path print split]
   ;(println "path is " path)
   (let [s (str "find " path " -type f")
         f (-> (process {:out :string} s)
               deref :out)]
     (when print
       (println "f is" f)) 
     (if split 
       (s/split-lines f) 
       f)))
 
 (defn splitPrefix [str prefix]
   (let [splt (s/split str (re-pattern prefix))]
     (first (next splt))))
 
 (defn list-files-skip-prefix [path]
   (let [f (list-files path false true)
         l (map (fn [e] (splitPrefix e path)) f)]
     l))
 
(defn directoryExists [path]
  ;(println "checking directory: " path)
  (.isDirectory (io/file path))) 
 
 (defn fileExists [path] 
   (.exists (io/file path)))

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
 
 (defn cpCmd [from to]
   (shell "cp" from to))
 
 (defn mvCmd [from to]
   (shell "mv" from to))
 
 (defn mkdirCmd [path]
   (shell "mkdir -p " path))
 
 (defn rmDir [path]
   (shell "rm -rf " path))
 
 (defn touchCmd [file]
   (shell "touch " file))
 
 (defn unmountCmd [dir]
   (shell "unmount " dir))

 ;note: overwrites any contents
 (defn writeToFile [contents filepath]
   (spit filepath contents))

(defn rsyncDirCmd [from to]
  (shell "rsync" "-a" from to))
 
(defn collHasPath [path coll]
  (let [res (filter (fn [e] (= e path)) coll)]
    ;(println "finding" path "in" coll)
    ;(println "res is" res)
    res))
 
(defn compareFiles [from to]
  (if (> (count from) 0)
   (loop [f from
          common []]
    (let [_first (first f)
          rem (next f)
          hasRemainder (> (count rem) 0)
          matches (collHasPath _first to)
          hasMatch (> (count matches) 0)]
      ;(println "has match is" hasMatch "file: " _first)
      (cond
        (and hasRemainder hasMatch) (recur rem (conj common _first))
        hasRemainder (recur rem common)
        (and (not hasRemainder) hasMatch) (conj common _first)
        :else common)))
    (println "No files detected!")))


(defn renameFiles [common-files ext prefix]
  (println "Renaming files: " common-files)
  (when (> (count common-files) 0)
    (loop [files common-files]
      (let [f (first files)
            rem (next files)
            continue (> (count rem) 0)
            mvFrom (str prefix f)
            mvTo (str prefix f ext)]
        (println "Changing file name: " mvFrom "to" mvTo)
        (mvCmd mvFrom mvTo)
        (if continue
          (recur rem)
          :done)))))


(defn copyDirNoReplace [from to ext]  
  (let [to-files (list-files-skip-prefix to) 
        from-file (list-files-skip-prefix from) 
        commonFiles (compareFiles to-files from-file) 
        hasCommon (> (count commonFiles) 0)]
    ;(println "from" from-file)
    ;(println "to" to-files)
    ;(println "common files" commonFiles)
    (when hasCommon 
      (do 
        (println "has common files!" (count commonFiles)) 
        (renameFiles commonFiles ext to))) 
    (rsyncDirCmd from to)))



; this does not cover all cases
(defn removeFileExtension [s] 
  (first (s/split s #"\.")))

