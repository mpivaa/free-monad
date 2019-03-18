(ns free-monad.core
  (:require [clojure.string :as str]
            [cats.core :as m]
            [cats.protocols :as p]))

(declare free)

(defrecord Bind [val f]
  p/Contextual
  (-get-context [_] free))

(defrecord Pure [val]
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

(defmacro defcmd [name bindings]
  `(defrecord ~name ~bindings
     p/Contextual
     (-get-context [_] free)))

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
