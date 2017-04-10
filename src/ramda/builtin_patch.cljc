(ns ramda.builtin-patch
  (:require [ramda.protocols :as p]
            [ramda.utils :as u :include-macros true]))

(defn patch-collection [type]
  (extend-type type
    p/Functor
    (p/fl-map [this f]
      (map f this))

    p/Foldable
    (p/fl-reduce [this f x]
      (reduce f x this))

    p/Semigroup
    (p/fl-concat [this a]
      (concat this a))

    p/Setoid
    (p/fl-equals [this a]
      (= this a))

    p/Apply
    (p/fl-ap [this a]
      (reduce
       (fn [acc f]
         (p/fl-concat acc (p/fl-map a this)))
       (empty this)
       a))

    p/Chain
    (p/fl-chain [this f]
      (mapcat f this))))

(defn patch-vector []
  #?(:cljs (do
             (patch-collection PersistentVector))
     :clj (do
            (patch-collection clojure.lang.PersistentVector))))

(defn patch-list []
  #?(:cljs (do
             (patch-collection List)
             (patch-collection LazySeq))
     :clj (do
            (patch-collection clojure.lang.ASeq))))

(defn patch-set []
  #?(:cljs (do
             (patch-collection PersistentHashSet)
             (patch-collection PersistentTreeSet))
     :clj (do
            (patch-collection clojure.lang.APersistentSet))))

(defn patch-number []
  (extend-type #?(:cljs js/Number :clj Number)
    p/Ord
    (p/fl-lte [this a]
      (<= this a))))

(defn patch-all []
  (patch-vector)
  (patch-list)
  (patch-set)
  (patch-number))
