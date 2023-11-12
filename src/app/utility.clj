(ns app.utility
  (:require
  [babashka.process :refer [shell process exec]]))

(def example "mount -t overlay myModManagerOverlay -o lowerdir=myLower,upperdir=myUpper,workdir=myWork myMerge")


(defn ls-and-grep [search]
 (-> (process "ls")
     (process {:out :string} "grep" search) deref :out))

(defn overlayCmd [name lower upper work merge]
  ; s = shell
  ;s(println "shell is" s)
  (let [mystring (str
                  "mount -t overlay "
                  name
                  " -o "
                  "lowerdir=" lower
                  ","
                  "upperdir=" upper
                  ","
                  "workdir=" work
                  " "  merge)]
    (println "cmd is " mystring)
    (try
      (shell example)
      (catch Exception e
        (println "exception: " e)))
    ))

(defn mkdirCmd [path]
  (shell "mkdir -p " path))

(defn rmDir [path]
  (shell "rm -rf " path))

(defn touchCmd [file]
  (shell "touch " file))

(defn unmountCmd [dir]
  (shell "unmount " dir))
