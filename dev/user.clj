(ns user
  (:require [cats.core :as m]))
(require '[free-monad.core :refer :all])
(require '[clojure.string :as str])
(require '[clj-http.client :as client])
(require '[clojure.data.json :as json])
(require '[cats.core :as m])

(defeffect Tell [text])
(defeffect Ask [question])
(defeffect SearchMovies [title])

(def pgr (m/mlet [name (Ask. "Tell me a joke")
                  _    [(Tell. (str "Your joke: " name))
                        (Tell. "Bye")]]
                 (m/return name)))

(def url "https://api.themoviedb.org/3/search/movie?api_key=b5efb835d4322ffdd907d19fe581fc48")
(def fake-db (atom {:movies [{:title "Harry Potter" :vote_count 1000}
                             {:title "The Lord of the Rings" :vote_count 1500}
                             {:title "Star Wars" :vote_count 1100}]}))

(defimpl inMemmoryImpl
  (Tell [t] (println t))
  (Ask [q] (do (println q)
               (read-line)))

  (SearchMovies [title]
    (filter #(str/includes? (:title %) title) (-> @fake-db :movies))))

(defn adapt-result [res]
  (-> res
      :body
      (json/read-str :key-fn keyword)
      :results
      (->> (map #(select-keys % [:title :vote_count])))))

(defimpl movieDbImpl
  (Tell [t] (println t))
  (Ask [q] (do (println q)
               (read-line)))

  (SearchMovies [title]
    (-> (str url "&query=" title)
        client/get
        adapt-result)))

(def get-most-popular #(sort-by :vote_count > %))
(defn format-list [list] (str/join "\n" (map :title list)))

(def movie-search
  (m/mlet [title    (Ask. "Give me a movie title")
           movies   (SearchMovies. title)
           most-pop (pure (get-most-popular movies))
           _        (Tell. "\nMost popular\n")
           _        (Tell. (format-list most-pop))]))
(comment
  (run-free movie-search inMemmoryImpl)
  (run-free movie-search movieDbImpl))

(def name-pgr
  (m/mlet [first-name (Ask. "What's your first name")
           last-name  (Ask. "What's your last name")
           full-name  (pure (str first-name " " last-name))
           _          (Tell. (str "Full name: " full-name))]))

(comment
  (defn purchase!
    [purchase account-id db logger]
    (let [account     (one-account account-id db)]
      (if (authorize? purchase account)
        (do
          (db/insert-transaction! purchase account db)
          (logger/log "Authorized purchase" purchase logger)
          {:authorized true})
        (do
          (logger/log "Denied purchase" purchase logger)
          {:authorized false})))))

(defeffect insert-transaction [transaction account])
(defeffect log [reason any])
(defeffect find-account [account-id])

(defn authorize? [purchase account] (< (:amount purchase) 100))

(defn purchase
  [purchase account-id]
  (m/mlet [account     (->find-account account-id)
           authorized? (pure (authorize? purchase account))
           _           (if authorized?
                         [(->insert-transaction purchase account)
                          (->log "Authorized purchase" purchase)]
                         (->log "Denied purchase" purchase))]
     (m/return {:authorized authorized?})))

(defn test-valid-purchase []
  (def effects (atom []))
  (defimpl test-impl
    (insert-transaction
     [{:keys [transaction account] :as effect}]
     (swap! effects conj effect)
     {:id (:id account)})

    (log
     [{:keys [reason any] :as effect}]
     (swap! effects conj effect)
     nil)

    (find-account
     [{:keys [account-id] :as effect}]
     (swap! effects conj effect)
     {:id account-id}))

  (reset! effects [])
  (assert (run-free (purchase {:amount 50} 1) test-impl) {:authorized true})
  (assert (= @effects [(->find-account 1)
                       (->insert-transaction {:amount 50} {:id 1})
                       (->log "Authorized purchase" {:amount 50})])))

;; (defn test-invalid-purchase []
;;   (def effects (atom []))
;;   (defimpl testImpl
;;     (InsertTransaction [transaction account]
;;                        (swap! effects conj (->InsertTransaction transaction account))
;;                        {:id (:id account)})

;;     (Log [reason any]
;;          (swap! effects conj (->Log reason any))
;;          nil)

;;     (FindAccount [account-id]
;;                  (swap! effects conj (->FindAccount account-id))
;;                  {:id account-id}))


;;   (assert (run-free (purchase {:amount 150} 1) testImpl) {:authorized false})
;;   (assert (= @effects [(->FindAccount 1)
;;                        (->Log "Denied purchase" {:amount 150})])))
