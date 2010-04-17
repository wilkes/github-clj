(ns github-clj
  (:use [clojure.contrib.json.read :only [read-json-string]]
        [clojure.contrib.def :only [defvar]]
        [clojure.http.client :only [url-encode request]]
        [clojure.contrib.str-utils :only [re-sub]]))

(def base-url "http://github.com/api/v2/json/")

(defvar *login*)
(defvar *token*)

(defmacro as-user [login token & body]
  `(binding [*login* ~login *token* ~token]
     ~@body))

(defn auth-info []  {"login" *login* "token" *token*})

(defn key-fn [s]
  #(get % s))

(defn stringify-keys [map]
  (reduce (fn [r [k v]] (conj r {(name k) v}))))

(defn sub-action [action]
  (re-sub #"\*login\*" *login* action))

(defn parse-response [response]
  (let [[body] (:body-seq response)
        code (:code response)]
    (if (= 200 code)
      (read-json-string body)
      (throw (Exception. (str code " " (:msg response)))))))

(defn do-get [action]
  (parse-response (request (str base-url action "?" (url-encode (auth-info))))))

(defn do-post [action & [params]]
  (parse-response (request (str base-url action)
                           "POST"
                           nil
                           nil
                           (if (map? params)
                             (merge params (auth-info))
                             (str params "&" (url-encode (auth-info)))))))

(defmacro GET [name action-base action-params]
  `(defn ~name [~@action-params]
     (let [escaped-params# (doall (map url-encode [~@action-params]))]
       (do-get (apply format ~action-base escaped-params#)))))

(defmacro POST [name action-base params]
  `(defn ~name [~@params]
     (let [action-params# [~(butlast params)]
           form-params# ~(last params)
           escaped-params# (doall (map url-encode action-params#))]
       (do-post (apply format (sub-action ~action-base) escaped-params#)
                form-params#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(GET   search-for-users  "user/search/%s"          [s])
(GET   get-user-info     "user/show/%s"            [username])
(POST  update-user-info  "user/show/*login*"       [map-info])
(GET   list-following    "user/show/%s/following"  [username])
(GET   list-followers    "user/show/%s/followers"  [username])
(GET   watched-repos     "repos/watched/%s"        [username])
(GET   list-keys         "user/keys"               [])
(POST  add-key           "user/key/add"            [key-map])
(POST  remove-key        "user/key/remove"         [key-map])
(GET   list-emails       "user/emails"             [])
(comment
  (POST add-email         "user/email/add"         [email-map])
  (POST remove-email      "user/email/remove"      [email-map]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repositories API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(GET   search-repos         "repos/search/%s"                   [s])
(GET   get-repo-info        "repos/show/%s/%s"                  [username repo-name])
(POST  update-repo-info     "repos/show/%s/%s"                  [username repo-name map-info])
(GET   list-all-repos       "repos/show/%s"                     [username])
(POST  watch-repo           "repos/watch/%s/%s"                 [username repo-name])
(POST  unwatch-repo         "repos/unwatch/%s/%s"               [username repo-name])
(POST  fork-repo            "repos/fork/%s/%s"                  [username repo-name])
(POST  create-repo          "repos/create"                      [repo-map])
(POST  delete-repo          "repos/delete/%s"                   [repo-name])
(POST  set-private-repo     "repos/set/private/%s"              [repo-map])
(POST  set-public-repo      "repos/set/public/%s"               [repo-name])
(GET   list-deploy-keys     "repos/keys/%s"                     [repo-name])
(POST  add-deploy-key       "repos/key/%s/add"                  [key-map])
(POST  remove-deploy-key    "repos/key/%s/add"                  [key-map])
(GET   list-collaborators   "repos/show/%s/%s/collaborators"    [username repo-name])
(POST  add-collaborator     "repos/collaborators/%s/add/%s"     [repo-name username])
(POST  remove-collaborator  "repos/collaborators/%s/remove/%s"  [repo-name username])
(GET   pushable-repos       "repos/pushable"                    [])
(GET   repos-network        "repos/show/%s/%s/network"          [repo-name username])
(GET   repos-languages      "repos/show/%s/%s/languages"        [repo-name username])
(GET   repos-tags           "repos/show/%s/%s/tags"             [repo-name username])
(GET   repos-branches       "repos/show/%s/%s/branches"         [repo-name username])

(defn remove-collab-from-all-projects [collab-user repo-owner]
  (doseq [repo-name (map (key-fn "name") (list-all-repos repo-owner))]
    (remove-collaborator repo-name collab-user)
    (println "removed " collab-user " from " repo-name)))
