(ns user)
(require '[free-monad.core :refer :all])
(require '[clojure.string :as str])
(require '[clj-http.client :as client])
(require '[clojure.data.json :as json])

(def pgr (m/mlet [name (Ask. "Tell me a joke")
                  _    [(Tell. (str "Your joke: " name))
                        (Tell. "Bye")]]
                 (m/return name)))

(defcmd Tell [text])

(defcmd Ask [question])

(defcmd SearchMovies [title])


(def url "https://api.themoviedb.org/3/search/movie?api_key=b5efb835d4322ffdd907d19fe581fc48")
(def fake-db (atom {:movies [{:title "Harry Potter" :vote_count 1000}
                             {:title "The Lord of the Rings" :vote_count 1500}
                             {:title "Star Wars" :vote_count 1100}]}))

(defimpl testImpl
  (Tell [t] (println t))
  (Ask [q] (do (println q)
               (read-line)))

  (SearchMovies [title]
    [{:title "Harry Potter" :vote_count 10}
     {:title "The Lord of the Rings" :vote_count 20}]))

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

(def program1
  (Bind. (Ask. "What's your name")
         (fn [name]
           (Bind. (Tell. (str "Hi, " name))
                  (fn [_] (Tell. "I'm you computer, and I'm alive!"))))))

(def program3
  (m/mlet [name (Ask. "What's your name")
           _    (Tell. (str "Hi, " name))
           _    (Tell. "I'm your computer, and I'm aliiiiiive!")
           resp (Ask. "Are you a computer too (s/n)?")
           const (Pure. "const")
           _    (Tell. const)
           _    (if (= resp "s")
                  (Tell. "Let's make a revolution")
                  (Tell. "We are coming for you!"))]))

(def get-most-popular #(sort-by :vote_count > %))
(defn format-list [list] (str/join "\n" (map :title list)))

(def movie-search
  (m/mlet [title    (Ask. "Give me a movie title")
           movies   (SearchMovies. title)
           most-pop (Pure. (get-most-popular movies))
           _        (Tell. "\nMost popular\n")
           _        (Tell. (format-list most-pop))]))
(comment
  (run-free movie-search inMemmoryImpl)
  (run-free movie-search movieDbImpl))

(def name-pgr
  (m/mlet [first-name (Ask. "What's your first name")
           last-name  (Ask. "What's your last name")
           full-name  (Pure. (str first-name " " last-name))
           _          (Tell. (str "Full name: " full-name))]))

(comment
  (defn purchase!
    [purchase account-id db logger]
    (let [account     (one-account account-id db)]
      (if (authorize? purchase account)
        (do
          (db/insert-transaction! purchase account db)
          (logger/log "Authorized transaction" transaction logger)
          {:authorized true})
        (do
          (logger/log "Denied transaction" transaction logger)
          {:authorized false})))))
