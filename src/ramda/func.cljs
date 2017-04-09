(ns ramda.func
  (:refer-clojure :exclude [identity]))

(defn always [v]
  (fn [] v))

(def identity clojure.core/identity)
