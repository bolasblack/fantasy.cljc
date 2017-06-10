(ns fantasy.builtin-patch
  (:require [fantasy.protocols :as p]
            [fantasy.standard-func :as sf]
            [fantasy.builtin-patch.macros :as macros :include-macros true]))

(defn patch-vector []
  #?(:cljs (do
             (macros/patch-collection-type PersistentVector []))
     :clj (do
            (macros/patch-collection-type clojure.lang.PersistentVector []))))

(defn patch-list []
  #?(:cljs (do
             (macros/patch-collection-type List '())
             (macros/patch-collection-type EmptyList '())
             (macros/patch-collection-type LazySeq (lazy-seq)))
     :clj (do
            (macros/patch-collection-type clojure.lang.ASeq '())
            (macros/patch-collection-type clojure.lang.PersistentList$EmptyList '())
            (macros/patch-collection-type clojure.lang.LazySeq (lazy-seq)))))

(defn patch-set []
  #?(:cljs (do
             (macros/patch-collection-type PersistentHashSet #{} :wrapper set)
             (macros/patch-collection-type PersistentTreeSet #{} :wrapper set))
     :clj (do
            (macros/patch-collection-type clojure.lang.APersistentSet #{} :wrapper set))))

(defn patch-hash []
  #?(:cljs (do
             (macros/patch-hash-type PersistentArrayMap {})
             (macros/patch-hash-type PersistentHashMap {})
             (macros/patch-hash-type PersistentTreeMap {}))
     :clj (do
            (macros/patch-hash-type clojure.lang.PersistentArrayMap {})
            (macros/patch-hash-type clojure.lang.PersistentHashMap {})
            (macros/patch-hash-type clojure.lang.PersistentTreeMap {}))))

(defn patch-string []
  (defmethod sf/empty #?(:cljs js/String :clj java.lang.String) [type]
    "")

  (defmethod sf/zero #?(:cljs js/String :clj java.lang.String) [type]
    "")

  (extend-type #?(:cljs js/String :clj java.lang.String)
    p/Semigroup
    (p/concat [this that]
      (str this that))

    p/Monoid))

(defn patch-number []
  (defmethod sf/zero #?(:cljs js/Number :clj java.lang.Number) [type]
    0)

  (extend-type #?(:cljs js/Number :clj java.lang.Number)
    p/Ord
    (p/lte [this that]
      (<= this that))

    p/Plus))

(defn patch-all []
  (patch-vector)
  (patch-list)
  (patch-set)
  (patch-hash)
  (patch-string)
  (patch-number))
