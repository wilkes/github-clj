;; The MIT License
;;  
;; Copyright (c) 2010 Wilkes Joiner
;;  
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;  
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;  
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(ns github-clj
  (:use [clojure.contrib.json.read :only [read-json-string]]
        [clojure.contrib.def :only [defvar]]
        [clojure.http.client :only [url-encode request]]
        [clojure.contrib.str-utils :only [re-sub re-gsub]]))

(def base-url "https://github.com/api/v2/json/")

(defvar *login*)
(defvar *token*)

(defn auth-info []  {"login" *login* "token" *token*})

(defn map-keys [f m]
  (let [listish? #(or (list? %) (vector? %))
        val-fn (fn [v]
                 (cond
                  (map? v) (map-keys f v)
                  (listish? v) (map #(if (map? %) (map-keys f %) %) v)
                  :default v))
        reduce-fn  (fn [r [k v]]
                     (conj r {(f k) (val-fn v)}))]
    (reduce reduce-fn {} m)))

(defn stringify-keys [m]
  (map-keys #(if (keyword? %) (name %) (str %)) m))

(defn keywordify-keys [m]
  (map-keys keyword m))

(defn sub-action [action]
  (re-sub #"\*login\*" *login* action))

(defn parse-url-str [s]
  [(re-gsub #":[a-z]+" "%s" s)
   (vec (map (comp symbol second) (re-seq #":([a-z]+)" s)))])

(defn parse-json [body]
  (try
   (let [result (keywordify-keys (read-json-string body))
         ks (keys result)]
     (if (= 1 (count ks))
       ((first ks) result)
       result))
   (catch Exception e
     (throw (Exception. (str "Unable to parse:\n" body "\n\n")
                        e)))))

(defn parse-response
  ([response]
     (parse-response response parse-json))
  ([response body-reader]
     (let [body (apply str (interpose "\n" (:body-seq response)))
           code (:code response)]
       (if (= 200 code)
         (body-reader body)
         (throw (Exception. (str code " " (:msg response))))))))

(defn make-key-mapper [f keys]
  (fn [m]
    (let [reduce-fn (fn [result k]
                    (conj result (if-let [v (get m k)]
                                   {(f k) v}
                                   nil)))]
      (reduce reduce-fn {} keys))))

(defn nested [& keys]
  (make-key-mapper #(str "values[" (name %) "]") keys))

(defn flat [& keys]
  (make-key-mapper identity keys))

(defn do-get [action]
  (parse-response (request (str base-url action "?" (url-encode (auth-info))))))

(defn do-post [action & [params]]
  (let [post-data (if (map? params)
                    (stringify-keys (merge params (auth-info)))
                    (str params "&" (url-encode (auth-info))))
        uri (str base-url action)]
    (parse-response (request uri "POST" nil nil post-data))))

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

(defmacro as-user [login token & body]
  `(binding [*login* ~login *token* ~token]
     ~@body))

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
(GET   repo-network        "repos/show/:user/:repo/network")
(GET   repo-languages      "repos/show/:user/:repo/languages")
(GET   repo-tags           "repos/show/:user/:repo/tags")
(GET   repo-branches       "repos/show/:user/:repo/branches")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commits API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(GET  list-commits       "commits/list/:user_id/:repository/:branch")
(GET  list-file-commits  "commits/list/:user_id/:repository/:branch/:path")
(GET  commit-info        "commits/show/:user_id/:repository/:sha")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Issues API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(GET  search-issues        "issues/search/:user/:repo/:state/:search_term")
(GET  list-project-issues  "issues/list/:user/:repo/:state")
(GET  show-issue           "issues/show/:user/:repo/:number")
(GET  list-issue-comments  "issues/comments/:user/:repo/:number")
(POST open-issue           "issues/open/:user/:repo" (flat :title :body))
(POST close-issue          "issues/close/:user/:repo/:number")
(POST reopen-issue         "issues/reopen/:user/:repo/:number")
(POST edit-issue           "issues/edit/:user/:repo/:number" (flat :title :body))
(GET  list-labels          "issues/labels/:user/:repo")
(POST add-label            "issues/label/add/:user/:repo/:label/:number")
(POST remove-label         "issues/label/add/:user/:repo/:label/:number")
(POST comment-on-issue     "issues/comment/:user/:repo/:id" (flat :comment))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Network API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn network-api-call [url-fmt params user repo]
  (parse-response (request (str (format url-fmt user repo) "?" params))))

(defn network-meta [user repo]
  (network-api-call "https://github.com/%s/%s/network_meta"
                    (url-encode (auth-info))
                    user repo))

(defn network-data-chunk [user repo nethash]
  (network-api-call "https://github.com/%s/%s/network_data_chunk?"
                    (url-encode (merge {"nethash" nethash} (auth-info)))
                    user repo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Object API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(GET tree-contents          "tree/show/:user/:repo/:tree_sha")
(GET blob-contents-by-tree  "blob/show/:user/:repo/:tree_sha/:path")
(GET list-blobs             "blob/all/:user/:repo/:tree_sha")

(defn blob-contents [user repo sha]
  (let [url (str base-url
                 (format "blob/show/%s/%s/%s" user repo sha)
                 "?" (url-encode (auth-info)))]
    (parse-response (request url) identity)))
