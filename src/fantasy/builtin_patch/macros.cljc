(ns fantasy.builtin-patch.macros
  (:require [fantasy.protocols :as p]
            [fantasy.standard-func :as sf]
            [fantasy.utils :as u]))

(defn coll-chain-rec [type-rep f x]
   (loop [todo [x]
          done []]
     (if (> (count todo) 0)
       (let [result (f u/chain-rec-next u/chain-rec-done (first todo))
             {:keys [more-todo more-done]} (->> result
                                                (group-by #(if (:next? %) :more-todo :more-done))
                                                (map (fn [[key results]]
                                                       [key (map #(.-value %) results)]))
                                                (into {}))]
         (recur (concat more-todo (next todo))
                (concat done more-done)))
       done)))

(defmacro patch-collection-type [type emptied & {:keys [wrapper] :or {wrapper 'identity}}]
  `(do
     (defmethod sf/empty ~type [type#] ~emptied)
     (defmethod sf/zero ~type [type#] ~emptied)
     (defmethod sf/of ~type [type-rep# value#] (conj (sf/zero type-rep#) value#))
     (defmethod sf/chain-rec ~type [type-rep# f# x#] (coll-chain-rec type-rep# f# x#))

     (extend-type ~type
       p/Setoid
       (p/-equals [this# that#]
         (= this# that#))

       p/Functor
       (p/map [this# f#]
         (~wrapper (map f# this#)))

       p/Foldable
       (p/reduce [this# f# x#]
         (reduce f# x# this#))

       p/Semigroup
       (p/concat [this# that#]
         (~wrapper (concat this# that#)))

       p/Alt
       (p/alt [this# that#]
         (~wrapper (concat this# that#)))

       p/Apply
       (p/ap [this# that#]
         (~wrapper (reduce
                    (fn [acc# f#]
                      (p/concat acc# (p/map this# f#)))
                    (empty this#)
                    that#)))

       p/Chain
       (p/chain [this# f#]
         (~wrapper (mapcat f# this#)))

       p/Extend
       (p/extend [this# f#]
         (~wrapper (conj (empty this#) (f# this#))))

       p/Applicative

       p/Monoid

       p/Plus

       p/ChainRec)))

(defmacro patch-hash-type [type emptied]
  `(do
     (defmethod sf/zero ~type [type#] ~emptied)
     (defmethod sf/empty ~type [type#] ~emptied)

     (extend-type ~type
       p/Setoid
       (p/-equals [this# that#]
         (= this# that#))

       p/Semigroup
       (p/concat [this# that#]
         (merge this# that#))

       p/Functor
       (p/map [this# f#]
         (into ~emptied (map (fn [[key# value#]]
                               [key# (f# value#)])
                             this#)))

       p/Apply
       (p/ap [this# that#]
         (into ~emptied (map (fn [[key# value#]]
                               [key# ((get that# key# identity) value#)])
                             this#)))

       p/Alt
       (p/alt [this# that#]
         (p/concat this# that#))

       p/Foldable
       (p/reduce [this# f# x#]
         (reduce f# x# this#))

       p/Plus

       p/Monoid)))
