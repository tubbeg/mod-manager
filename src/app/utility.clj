(ns app.utility
  (:require 
   [babashka.process :refer [shell process exec]] 
   [clojure.java.io :as io]  
   [clojure.string :as s]))
 
(defn isZero [coll]
  (< (count coll) 1))

(defn removeLastSlash [s]
  (let [index (s/last-index-of s "/")
        isNil (= index nil)
        len (- (count s) 1)
        lenNotEqualsIndex (not= index len)
        regex (-> (str ".{" index "}"))]
    (if (or isNil lenNotEqualsIndex)
      s
      (-> regex
          (re-pattern)
          (re-seq s)
          (first)))))

(defn removePrefix [prefix s]
  (s/replace-first s prefix ""))

(defn switchPrefix [s new old verbose isFile]
  (when verbose 
    (println "Switching prefix on: " s) 
    (println "old: "  old) 
    (println "new: " new))
  (let [o (removeLastSlash old)
        n (removeLastSlash new)
        newS (->> (str s "/") 
                  (removePrefix o) 
                  (str n))]
    (when verbose 
      (println "resulting string: " newS))
    (if isFile
      (removeLastSlash newS)
      newS)))

(def test ".mm/mods/modFolder1/file1.txt")
(def p1 ".mm/mods/modFolder1")
(def p2 "gameFolder/")

(defn notZero [coll]
  (> (count coll) 0))
 
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
 
(defn list-files-prefix [path prefix] 
  (let [f (list-files path false true) 
        l (map (fn [e] (splitPrefix e prefix)) f)] 
    l))
 
 (defn list-files-skip-prefix [path]
   (list-files-prefix path path))
 
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
 
 (defn rmFile [path]
   (shell "rm" path))
 
 (defn touchCmd [file]
   (shell "touch " file))
 
 (defn unmountCmd [dir]
   (shell "unmount " dir))

 ;note: overwrites any contents
 (defn writeToFile [contents filepath]
   (spit filepath contents))

(defn rsyncDirCmd [from to]
  (shell "rsync" "-a" from to))
 
(defn rsyncCreateMissingDirectories [from to]
  (shell "rsync" "-a" "--mkpath" from to))
 
(defn collHasPath [path coll]
  (let [res (filter (fn [e] (= e path)) coll)]
    res))
 
(defn compareFiles [from to]
  (if (notZero from)
   (loop [f from
          common []]
    (let [_first (first f)
          rem (next f)
          hasRemainder (notZero rem)
          matches (collHasPath _first to)
          hasMatch (notZero matches)]
      (cond
        (and hasRemainder hasMatch) (recur rem (conj common _first))
        hasRemainder (recur rem common)
        (and (not hasRemainder) hasMatch) (conj common _first)
        :else common)))
    (println "No files detected!")))


(defn renameFiles [common-files ext prefix]
  (println "Renaming files: " common-files)
  (when (notZero common-files)
    (loop [files common-files]
      (let [f (first files)
            rem (next files)
            continue (notZero rem)
            mvFrom (str prefix f)
            mvTo (str prefix f ext)] 
        (println "Changing file name: " mvFrom "to" mvTo) 
        (mvCmd mvFrom mvTo)
        (if continue
          (recur rem)
          :done)))))

(defn moveFiles [common-files dest prefix] 
  (println "Moving original files to source: " common-files)
  (when (notZero common-files)
     (loop [files common-files]
       (let [f (first files)
             rem (next files)
             mvFrom (str prefix f)]
         (println "File is: " f)
         (println "Moving file: " mvFrom "to" dest)
         (mvCmd mvFrom dest)
         (if (notZero rem)
           (recur rem)
           :done)))))

(defn copyDir [from to replace dest] 
  (println "source is" from "destination is" to) 
  (when (not replace) 
    (let [to-files (list-files-skip-prefix to) 
          from-file (list-files-skip-prefix from) 
          commonFiles (compareFiles from-file to-files) 
          hasCommon (notZero commonFiles)] 
      (when hasCommon
        (moveFiles commonFiles dest to)))) 
  (rsyncDirCmd from to))



(defn copyFile [file mod config replace]
  (let [source (:source-dir config)
        game-path (:game-path config)
        moveFrom (switchPrefix file game-path mod false true)
        srcDest (switchPrefix
                 moveFrom
                 source
                 game-path
                 false
                 true)]
    (when (and (not replace) (fileExists moveFrom)) 
      (do 
        (println "Moving file: " moveFrom "to" srcDest) 
        (rsyncCreateMissingDirectories moveFrom srcDest))) 
    (println "Copying" file "to" moveFrom) 
    (rsyncCreateMissingDirectories file moveFrom) 
    moveFrom))

(defn getFileExtension [s] 
  (println "Not yet implemented!"))

(defn removeFileExtension [s]
  (let [index (s/last-index-of s ".")
        regex (-> (str ".{" index "}"))]
    (if (= index nil)
      (do (println "No file extension detected!")
          s) 
      (-> regex 
          (re-pattern) 
          (re-seq s)
          (first)))))



