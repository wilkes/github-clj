(ns github-clj
  (:use [clojure.contrib.json.read :only [read-json-string]]
        [clojure.contrib.def :only [defvar]]
        [clojure.http.client :only [url-encode request]]
        [clojure.contrib.str-utils :only [re-sub re-gsub]]))

(def base-url "https://github.com/api/v2/json/")

(defvar *login*)
(defvar *token*)

(defmacro as-user [login token & body]
  `(binding [*login* ~login *token* ~token]
     ~@body))

(defn auth-info []  {"login" *login* "token" *token*})

(defn key-fn [s]
  #(get % s))

(defn apply-to-keys [f m]
  (let [map-over-list (fn [l] (map (fn [x]
                                     (if (map? x)
                                       (apply-to-keys f x)
                                       x))
                                   l))
        g (fn [x]
            (cond
             (map? x) (apply-to-keys f x)
             (or (list? x) (vector? x)) (map-over-list x)
             :default x))]
    (reduce (fn [r [k v]] (conj r {(f k) (g v)}))
            {}
            m)))

(defn stringify-keys [m]
  (apply-to-keys (fn [k] (if (keyword? k) (name k) (str k))) m))

(defn keywordify-keys [m]
  (apply-to-keys keyword m))

(defn sub-action [action]
  (re-sub #"\*login\*" *login* action))

(defn parse-url-str [s]
  [(re-gsub #":[a-z]+" "%s" s)
   (vec (map (comp symbol second) (re-seq #":([a-z]+)" s)))])

(defn parse-response
  ([response]
     (parse-response response (comp keywordify-keys read-json-string)))
  ([response body-reader]
     (let [body (apply str (interpose "\n" (:body-seq response)))
           code (:code response)]
       (if (= 200 code)
         (try
          (body-reader body)
          (catch Exception e
            (throw (Exception. (str "Unable to parse:\n" body "\n\n")
                               e))))
         (throw (Exception. (str code " " (:msg response))))))))

(defn make-key-mapper [f keys]
  (fn [m]
    (reduce (fn [result k]
              (conj result (if-let [v (get m k)]
                             {(f k) v}
                             nil)))
            {}
            keys)))

(defn nested [& keys]
  (make-key-mapper #(str "values[" (name %) "]") keys))

(defn flat [& keys]
  (make-key-mapper identity keys))

(defn do-get [action]
  (parse-response (request (str base-url action "?" (url-encode (auth-info))))))

(defn do-post [action & [params]]
  (parse-response (request (str base-url action)
                           "POST"
                           nil
                           nil
                           (if (map? params)
                             (stringify-keys (merge params (auth-info)))
                             (str params "&" (url-encode (auth-info)))))))

(defmacro GET [name url-pattern]
  (let [[action-base action-params] (parse-url-str url-pattern)]
      `(defn ~name [~@action-params]
         (do-get (apply format ~action-base
                        (doall (map url-encode ~action-params)))))))

(defmacro POST [name url-pattern & [keys-fn]]
  (let [[action-base params] (parse-url-str url-pattern)
        keys-fn (or keys-fn identity)]
    `(defn ~name [~@params & [form-params#]]
       (do-post (apply format (sub-action ~action-base)
                       (doall (map url-encode ~params)))
                (~keys-fn form-params#)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(GET   search-for-users  "user/search/:q")
(GET   get-user-info     "user/show/:user")
(POST  update-user-info  "user/show/*login*" (nested :name :email :blog
                                                     :company :location))
(GET   list-following    "user/show/:user/following")
(GET   list-followers    "user/show/:user/followers")
(GET   watched-repos     "repos/watched/:user")
(GET   list-keys         "user/keys")
(POST  add-key           "user/key/add"      (flat :title :key))
(POST  remove-key        "user/key/remove"   (flat :id))
(GET   list-emails       "user/emails")
(comment
  (POST add-email         "user/email/add"    (flat :emails))
  (POST remove-email      "user/email/remove" (flat :emails)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repositories API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(GET   search-repos         "repos/search/:q")
(GET   get-repo-info        "repos/show/:user/:repo")
(POST  update-repo-info     "repos/show/:user/:repo" (nested :description
                                                             :homepage
                                                             :has_wiki
                                                             :has_issues
                                                             :has_downloads))
(GET   list-all-repos       "repos/show/:user")
(POST  watch-repo           "repos/watch/:user/:repo")
(POST  unwatch-repo         "repos/unwatch/:user/:repo")
(POST  fork-repo            "repos/fork/:user/:repo")
(POST  create-repo          "repos/create" (flat :name
                                                 :description
                                                 :homepage
                                                 :public))
(POST  delete-repo          "repos/delete/:repo")
(POST  set-private-repo     "repos/set/private/:repo")
(POST  set-public-repo      "repos/set/public/:repo")
(GET   list-deploy-keys     "repos/keys/:repo")
(POST  add-deploy-key       "repos/key/:repo/add" (flat :title :key))
(POST  remove-deploy-key    "repos/key/:repo/remove" (flat :id))
(GET   list-collaborators   "repos/show/:user/:repo/collaborators")
(POST  add-collaborator     "repos/collaborators/:repo/add/:user")
(POST  remove-collaborator  "repos/collaborators/:repo/remove/:user")
(GET   pushable-repos       "repos/pushable")
(GET   repos-network        "repos/show/:user/:repo/network")
(GET   repos-languages      "repos/show/:user/:repo/languages")
(GET   repos-tags           "repos/show/:user/:repo/tags")
(GET   repos-branches       "repos/show/:user/:repo/branches")

(defn remove-collab-from-all-projects [collab-user repo-owner]
  (doseq [repo-name (map :name (list-all-repos repo-owner))]
    (remove-collaborator repo-name collab-user)
    (println "removed " collab-user " from " repo-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commits API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(GET list-commits "commits/list/:user_id/:repository/:branch")
(GET list-file-commits "commits/list/:user_id/:repository/:branch/:path")
(GET commit-info "commits/show/:user_id/:repository/:sha")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Issues API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(GET  search-issues "issues/search/:user/:repo/:state/:search_term")
(GET  list-project-issues "issues/list/:user/:repo/:state")
(GET  show-issue "issues/show/:user/:repo/:number")
(GET  list-issue-comments "issues/comments/:user/:repo/:number")
(POST open-issue "issues/open/:user/:repo" (flat :title :body))
(POST close-issue "issues/close/:user/:repo/:number")
(POST reopen-issue "issues/reopen/:user/:repo/:number")
(POST edit-issue "issues/edit/:user/:repo/:number" (flat :title :body))
(GET  list-labels "issues/labels/:user/:repo")
(POST add-label "issues/label/add/:user/:repo/:label/:number")
(POST remove-label "issues/label/add/:user/:repo/:label/:number")
(POST comment-on-issue "issues/comment/:user/:repo/:id" (flat :comment))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn network-meta [user repo]
  (parse-response
   (request (str (format "https://github.com/%s/%s/network_meta" user repo)
                 "?" (url-encode (auth-info))))))

(defn network-data-chunk [user repo nethash]
  (parse-response
   (request (str (format "https://github.com/%s/%s/network_data_chunk?" user repo)
                 "?" (url-encode (merge {"nethash" nethash} (auth-info)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(GET tree-contents "tree/show/:user/:repo/:tree_sha")
(GET blob-contents-by-tree "blob/show/:user/:repo/:tree_sha/:path")
(GET list-blobs "blob/all/:user/:repo/:tree_sha")

(defn blob-contents [user repo sha]
  (parse-response
   (request (str base-url
                 (format "blob/show/%s/%s/%s" user repo sha)
                 "?" (url-encode (auth-info))))
   identity))

