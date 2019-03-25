(ns free-monad.core
  (:require [clojure.string :as str]
            [cats.core :as m]
            [cats.protocols :as p]
            [clojure.pprint :as pprint]))

(declare free)

(defrecord Bind [val f]
  p/Contextual
  (-get-context [_] free))

(defrecord Pure [val]
  p/Contextual
  (-get-context [_] free))

(def pure ->Pure)
(def bind ->Bind)

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
      (condp = (type mv)
        Pure (f (:val mv))
        Bind (Bind. (:val mv) (comp #(m/bind % f) (:f mv)))
        (Bind. mv f)))))

(defmacro defeffect [name bindings]
  `(do
     (defrecord ~name ~bindings
      p/Contextual
      (-get-context [_] free))
     (def ~(symbol (str name "!")) ~(symbol (str "->" name)))))

(defn build-impl [bindings impls]
  (mapv (fn [[t bindings & body]]
          [t `(fn ~t [~@bindings] ~@body)])
        impls))

(defmacro defimpl [name bindings & forms]
  (let [impl (build-impl bindings forms)]
    `(defn ~name ~bindings ~impl)))

(defn run-effect
  [effect impl]
  (if-let [effect-impl (some #(when (= (type effect) (first %))
                                    (second %))
                          impl)]
    (apply effect-impl effect (vals effect))
    (throw (Exception. (str "Implementation not defined for " (type effect))))))

(defn run-program
  [{:keys [val f] :as free} impl]
  (condp = (type free)
    nil nil
    Pure val
    Bind (-> (if (vector? val)
               (mapv #(run-program % impl) val)
               (run-program val impl))
             f
             (run-program impl))
    (run-effect free impl)))

(defmacro defprogram [name args bindings & body]
  `(defn ~name ~args
     (m/mlet ~bindings ~@body)))

(defn log-effect! [effects effect]
  (swap! effects conj effect))

(defn tapd [m] (Bind. m (fn [v] (prn v) (Pure. v))))

(defn logged-impl [impl]
  (let [effects (atom [])
        new-impl (mapv (fn [[t f]]
                         [t (fn [e & args]
                             (log-effect! effects e)
                             (apply f e args))])
                       impl)]
    [effects new-impl]))
