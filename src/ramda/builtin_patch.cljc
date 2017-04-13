(ns ramda.builtin-patch
  (:require [ramda.protocols :as p]
            [ramda.standard-func :as sf]
            [ramda.utils :as u :include-macros true]))

(defn patch-collection [type emptied]
  (defmethod sf/empty type [type]
    emptied)

  (defmethod sf/zero type [type]
    emptied)

  (defmethod sf/of type [type-rep value]
    (conj (sf/zero type-rep) value))

  (defmethod sf/chain-rec type [type-rep f x]
    (loop [todo [x]
           done []]
      (if (> (count todo) 0)
        (let [result (f u/chain-rec-next u/chain-rec-done (first todo))
              {:keys [more-todo more-done]} (->> result
                                                 (group-by #(if (:next? %) :more-todo :more-done))
                                                 (map (fn [[key results]] [key (map #(.-value %) results)]))
                                                 (into {}))]
          (recur (concat more-todo (next todo))
                 (concat done more-done)))
        done)))

  (extend-type type
    p/Setoid
    (p/fl-equals [this that]
      (= this that))

    p/Functor
    (p/fl-map [this f]
      (map f this))

    p/Foldable
    (p/fl-reduce [this f x]
      (reduce f x this))

    p/Semigroup
    (p/fl-concat [this a]
      (concat this a))

    p/Alt
    (p/fl-alt [this a]
      (concat this a))

    p/Apply
    (p/fl-ap [this a]
      (reduce
       (fn [acc f]
         (p/fl-concat acc (p/fl-map this f)))
       (empty this)
       a))

    p/Chain
    (p/fl-chain [this f]
      (mapcat f this))

    p/Extend
    (p/fl-extend [this f]
      (concat (empty this) (f this)))

    p/Applicative

    p/Monoid

    p/Plus

    p/ChainRec))

(defn patch-vector []
  #?(:cljs (do
             (patch-collection PersistentVector []))
     :clj (do
            (patch-collection clojure.lang.PersistentVector []))))

(defn patch-list []
  #?(:cljs (do
             (patch-collection List '())
             (patch-collection EmptyList '())
             (patch-collection LazySeq (lazy-seq)))
     :clj (do
            (patch-collection clojure.lang.ASeq '()))))

(defn patch-set []
  #?(:cljs (do
             (patch-collection PersistentHashSet #{})
             (patch-collection PersistentTreeSet #{}))
     :clj (do
            (patch-collection clojure.lang.APersistentSet #{}))))

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
