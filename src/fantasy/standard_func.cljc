(ns fantasy.standard-func
  (:refer-clojure :exclude [map reduce concat empty extend])
  (:require [fantasy.curry :as curry-ns :include-macros true]
            [fantasy.utils :as u]
            [fantasy.funcs :as funcs]
            [fantasy.protocols :as p]))

(defmulti of (fn [a v]
               (if (and (map? a) (:fl-of? a))
                 (:type a)
                 a)))

(defmulti chain-rec (funcs/n-ary 1 identity))

(defmulti empty (funcs/n-ary 1 identity))

(defmulti zero (funcs/n-ary 1 identity))

(defn map [f a]
  {:pre [(u/invoke? f)
         (satisfies? p/Functor a)]}
  (p/map a f))

(defn ap [a b]
  {:pre [(satisfies? p/Apply a)
         (satisfies? p/Apply b)]}
  (p/ap b a))

(defn reduce [f x a]
  {:pre [(u/invoke? f)
         (satisfies? p/Foldable a)]}
  (p/reduce a f x))

(defn concat [a b]
  {:pre [(satisfies? p/Semigroup a)
         (satisfies? p/Semigroup b)]}
  (p/concat b a))

(defn equals [a b]
  {:pre [(satisfies? p/Setoid a)
         (satisfies? p/Setoid b)]}
  (p/-equals b a))

(defn alt [a b]
  {:pre [(satisfies? p/Setoid a)
         (satisfies? p/Setoid b)]}
  (p/alt b a))

(defn bimap [f1 f2 a]
  {:pre [(u/invoke? f1)
         (u/invoke? f2)
         (satisfies? p/Bifunctor a)]}
  (p/bimap a f1 f2))

(defn extend [f a]
  {:pre [(u/invoke? f)
         (satisfies? p/Extend a)]}
  (p/extend a f))

(defn promap [f1 f2 a]
  {:pre [(u/invoke? f1)
         (u/invoke? f2)
         (satisfies? p/Profunctor a)]}
  (p/promap a f1 f2))

(defn traverse [type-rep f a]
  {:pre [(u/invoke? f)
         (satisfies? p/Traversable a)]}
  (p/traverse a type-rep f))

(defn lte [a b]
  {:pre [(satisfies? p/Ord a)
         (satisfies? p/Ord b)]}
  (p/lte b a))

(defn chain [f a]
  {:pre [(u/invoke? f)
         (satisfies? p/Chain a)]}
  (p/chain a f))

(defn extract [a]
  {:pre [(satisfies? p/Comonad a)]}
  (p/extract a))
