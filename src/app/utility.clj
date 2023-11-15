(ns app.utility
  (:require 
   [babashka.process :refer [shell process exec]] 
   [clojure.java.io :as io]  
   [clojure.string :as s]
   [clojure.edn :as edn]))

(defn isNilOrEmptyString [s]
  (or
   (= s nil)
   (= s "")))

(defn addBackSlashBeforeWhiteSpace [s]
  (if (isNilOrEmptyString s)
    {:error :invalid-arg}
    (let [r (s/replace s #"[\ `]" "\\\\$0")]
      (if (isNilOrEmptyString r)
        {:error :white-space}
        r))))
 
(defn isZero [coll]
  (< (count coll) 1))

(defn removeLastSymbol [s symb]
  (let [index (s/last-index-of s symb)
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

(defn removeLastSlash [s]
  (removeLastSymbol s "./"))

(defn removeLastColon [s]
  (removeLastSymbol s ":"))

(removeLastColon "sdasasda/\"//&&!I))sda:::12ur r,.r e8  32718:7d:")

(defn removePrefix [prefix s]
  (s/replace-first s prefix ""))

(defn unmountOverlay [name]
  (println "Removing mount: " name)
  (try
    (do
      (shell "umount " name) 
      (println "Done...")) 
  (catch Exception e
    (println "Exception: " e))))

(defn printErrorMessage [e] 
  (println "Encountered exception: " e "\n")
  (println "Check that the directory paths are correct!")
  (println "Verify that you are a superuser.")
  (println (str "You can run this" 
                " script using: 'sudo -E env \"PATH=$PATH\""
                " /path/to/script mount'")))


(defn mountOverlay [name upper lower work m]
  (let [merge (str "'" m "'")
        lowerdir (str "lowerdir=" (str (addBackSlashBeforeWhiteSpace m) "/") ":" lower ",")
        upperdir (str "upperdir=" upper ",")
        work (str "workdir=" work)
        dirs (str lowerdir upperdir work)
        full (str "mount -t overlay " name " -o " dirs " " merge)]
    (println "command is:\n" full)
    (println "You are about to mount an overlay to"
             merge "with name" name "\n"  
             "\nThis requires superuser permissions\n")
    (println "Do you want to continue? (Y/n)") 
    (if (= (read-line) "Y")
      (try 
        (do 
          (shell full) 
          (println "Done...")
          :ok) 
        (catch Exception e
          (printErrorMessage e)
          :error))
      (println "Mount canceled"))))


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

(defn isOriginal [rename file1 file2]
  (and
   (= rename :not-applicable)
   (= file1 file2)))

    
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



