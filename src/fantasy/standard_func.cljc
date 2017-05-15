(ns fantasy.standard-func
  (:refer-clojure :exclude [map reduce concat empty extend])
  (:require [fantasy.curry :as curry-ns :include-macros true]
            [fantasy.utils :as u :include-macros true]
            [fantasy.funcs :as funcs]
            [fantasy.protocols :as p]))

(defn- compose-apply
  ([applyer args]
   (clojure.core/reduce #(applyer %1 %2) (reverse args)))
  ([applyer x args]
   (clojure.core/reduce #(applyer %1 %2) x (reverse args))))

(defn- bool-compose-apply [applyer args]
  (clojure.core/reduce #(if (applyer %1 %2) %1 (reduced false)) args))

(defn- pre-satisfies? [type a]
  #?(:clj (satisfies? type a)
     :cljs true))

(defn- pre-satisfies-every? [type as]
  (every? #(pre-satisfies? type %) as))

(defn- transducable [protocol applyer args]
  (cond
    (and (> (count args) 1)
         (every? u/invoke? (butlast args))
         (pre-satisfies? protocol (last args)))
    (let [fns (butlast args)
          a (last args)]
      (compose-apply applyer a fns))

    (every? u/invoke? args)
    (fn [a] (compose-apply applyer a args))

    :else
    (u/throw-error "arguments not valid")))

(defmulti of (fn [a v]
               (if (and (map? a) (:fl-of? a))
                 (:type a)
                 a)))

(defmulti chain-rec (funcs/n-ary 1 identity))

(defmulti empty (funcs/n-ary 1 identity))

(defmulti zero (funcs/n-ary 1 identity))

(defn map
  "(map ...f) -> (fn [functor] (... (map f2 (map f1 functor))))
   (map ...f Functor) -> (... (map f2 (map f1 Functor)))"
  [& args]
  (transducable p/Functor p/map args))

(defn ap
  "(ap ...Apply) -> (... (ap a3 (ap a2 a1)))"
  [& applies]
  {:pre [(pre-satisfies-every? p/Apply applies)]}
  (compose-apply p/ap applies))

(defn reduce [f x a]
  {:pre [(u/invoke? f)
         (pre-satisfies? p/Foldable a)]}
  (p/reduce a f x))

(defn concat
  "(concat ...Semigroup) -> (... (concat a3 (concat a2 a1)))"
  [& semigroups]
  {:pre [(pre-satisfies-every? p/Semigroup semigroups)]}
  (compose-apply p/concat semigroups))

(defn equals
  "(equals ...Setoid) -> (... (if (equals a3 (if (equals a2 a1) a1 false)) a1 false)"
  [& setoids]
  {:pre [(pre-satisfies-every? p/Setoid setoids)]}
  (bool-compose-apply p/-equals setoids))

(defn alt
  "(alt ...Alt) -> (... (alt a3 (alt a2 a1)))"
  [& alts]
  {:pre [(pre-satisfies-every? p/Alt alts)]}
  (compose-apply p/alt alts))

(defn bimap [f1 f2 a]
  {:pre [(u/invoke? f1)
         (u/invoke? f2)
         (pre-satisfies? p/Bifunctor a)]}
  (p/bimap a f1 f2))

(defn extend
  "(extend ...f Extend) -> (... (extend f2 (extend f1 Extend)))"
  [& args]
  (transducable p/Extend p/extend args))

(defn promap [f1 f2 a]
  {:pre [(u/invoke? f1)
         (u/invoke? f2)
         (pre-satisfies? p/Profunctor a)]}
  (p/promap a f1 f2))

(defn traverse [type-rep f a]
  {:pre [(u/invoke? f)
         (pre-satisfies? p/Traversable a)]}
  (p/traverse a type-rep f))

(defn lte
  "(lte ...Ord) -> (... (if (lte a3 (if (lte a2 a1) a1 false)) a1 false)"
  [& ords]
  {:pre [(pre-satisfies-every? p/Ord ords)]}
  (bool-compose-apply p/lte ords))

(defn chain
  "(chain ...f Chain) -> (... (chain f2 (chain f1 Chain)))"
  [& args]
  (transducable p/Chain p/chain args))

(defn extract [a]
  {:pre [(pre-satisfies? p/Comonad a)]}
  (p/extract a))
