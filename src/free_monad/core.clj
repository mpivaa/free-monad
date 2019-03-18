(ns hex.core
  (:require [clojure.string :as str]))
(require '[clj-http.client :as client])
(require '[clojure.data.json :as json])
(require '[cats.core :as m])
(require '[cats.protocols :as p])

(defrecord Bind [val f]
  p/Contextual
  (-get-context [_] free))

(defrecord Pure [val]
  p/Contextual
  (-get-context [_] free))

(defrecord Tell [text]
  p/Contextual
  (-get-context [_] free))

(defrecord Ask [question]
  p/Contextual
  (-get-context [_] free))

(defrecord SearchMovies [title]
  p/Contextual
  (-get-context [_] free))

(def free
  (reify
    p/Context

    p/Functor
    (-fmap [_ f mv]
      (m/bind mv #(Pure. (f %))))

    p/Monad
    (-mreturn [_ v]
      (Pure. v))

    (-mbind [_ mv f]
      (let [{val :val g :f} mv]
        (condp = (type mv)
          Pure (f val)
          Bind (Bind. val (comp #(m/bind % f) g))
          (Bind. mv f))))))

(def pgr (m/mlet [name (Ask. "Tell me a joke")
                  _    [(Tell. (str "Your joke: " name))
                        (Tell. "Bye")]]
                 (m/return name)))

(defmacro defimpl [name & impls]
  (let [forms (mapv (fn [[type bindings & body]]
                        [type `(fn [~@bindings] ~@body)])
                    impls)]
    `(def ~name ~forms)))

(defn run-expr
  [cmd impl]
  (if-let [cmd-impl (some #(when (= (type cmd) (first %))
                              (second %))
                          impl)]
    (apply cmd-impl (vals cmd))
    (throw (Exception. (str "Implementation not defined for " (type cmd))))))

(defn run-free
  [{:keys [val f] :as free} impl]
  (condp = (type free)
    nil nil
    Pure val
    Bind (-> (if (vector? val)
               (mapv #(run-expr % impl) val)
               (run-expr val impl))
             f
             (run-free impl))))

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
