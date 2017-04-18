(ns ramda.builtin-patch
  (:require [ramda.protocols :as p]
            [ramda.standard-func :as sf]
            [ramda.utils :as u :include-macros true]))

(defn patch-collection [type emptied & {:keys [wrapper] :or {wrapper identity}}]
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
    (p/equals [this that]
      (= this that))

    p/Functor
    (p/map [this f]
      (wrapper (map f this)))

    p/Foldable
    (p/reduce [this f x]
      (reduce f x this))

    p/Semigroup
    (p/concat [this that]
      (wrapper (concat this that)))

    p/Alt
    (p/alt [this that]
      (wrapper (concat this that)))

    p/Apply
    (p/ap [this that]
      (wrapper (reduce
                (fn [acc f]
                  (p/concat acc (p/map this f)))
                (empty this)
                that)))

    p/Chain
    (p/chain [this f]
      (wrapper (mapcat f this)))

    p/Extend
    (p/extend [this f]
      (wrapper (conj (empty this) (f this))))

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
             (patch-collection PersistentHashSet #{} :wrapper set)
             (patch-collection PersistentTreeSet #{} :wrapper set))
     :clj (do
            (patch-collection clojure.lang.APersistentSet #{} :wrapper set))))

(defn patch-hash
  ([type emptied]
   (defmethod sf/zero type [type]
     emptied)

   (defmethod sf/empty type [type]
     emptied)

   (extend-type type
     p/Setoid
     (p/equals [this that]
       (= this that))

     p/Semigroup
     (p/concat [this that]
       (merge this that))

     p/Functor
     (p/map [this f]
       (into emptied (map (fn [[key value]]
                            [key (f value)])
                          this)))

     p/Apply
     (p/ap [this that]
       (into emptied (map (fn [[key value]]
                            [key ((get that key identity) value)])
                          this)))

     p/Alt
     (p/alt [this that]
       (p/concat this that))

     p/Foldable
     (p/reduce [this f x]
       (reduce f x this))

     p/Plus

     p/Monoid))

  ([]
   #?(:cljs (do
              (patch-hash PersistentArrayMap {})
              (patch-hash PersistentHashMap {}))
      :clj (do
             (patch-hash clojure.lang.PersistentArrayMap {})
             (patch-hash clojure.lang.PersistentHashMap {})))))

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
  (patch-number)
  (patch-string))
