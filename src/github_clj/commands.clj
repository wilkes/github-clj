(ns github-clj.commands
  (:use github-clj
        [clojure.contrib.pprint :only [pprint]]
        [clojure.contrib.shell-out :only [sh]])
  (:import [java.io File]))

(defn clone-repo [repo dir]
  (sh "git" "clone"
      (format "git@github.com:%s/%s.git"
              (:owner repo) (:name repo))
      :dir dir
      :return-map true))

(defn pull-repo [repo dir]
  (sh "git" "pull" "--rebase"
      :dir (File. dir (:name repo))
      :return-map true))

(defn sync-repo [repo dir]
  (if (.exists (File. dir (:name repo)))
    (pull-repo repo dir)
    (clone-repo repo dir)))

(defn ensure-dir-exists [dir]
  (let [file (if (isa? (class dir) File)
               dir
               (File. dir))]
    (when-not (.exists file)
      (.mkdirs file))))

(defn sync-user-repos
  "Clone or pull and rebase all the repositories of a user (defaults to *login*)
   to the given directory"
  ([dir]
     (sync-user-repos (login) dir))
  ([user dir]
     (ensure-dir-exists dir)
     (doseq [repo (list-all-repos user) :let [result (sync-repo repo dir)]]
       (print "Syncing" (:name repo) "... ")
       (println (if (= 0 (:exit result))
                (:out result)
                (:err result))))))

(defn remove-collaborator-from-all-repos [repo-owner collaborator]
  (doseq [repo (map :name (list-all-repos repo-owner))]
    (remove-collaborator repo collaborator)))

(defn add-collaborator-to-all-repos [repo-owner collaborator]
  (doseq [repo (map :name (list-all-repos repo-owner))]
    (add-collaborator repo collaborator)))