(ns ramda.standard-func
  (:refer-clojure :exclude [map reduce concat empty])
  (:require [ramda.curry :as curry-ns :include-macros true :refer-macros [defcurry]]
            [ramda.utils :as u]
            [ramda.protocols :as p]))

(defmulti of identity)

(defmulti chain-rec identity)

(defmulti empty identity)

(defmulti zero identity)

(defcurry map [f a]
  {:pre [(u/invoke? f) (satisfies? p/Functor a)]}
  (p/map a f))

(defcurry ap [a b]
  {:pre [(satisfies? p/Apply a) (satisfies? p/Apply b)]}
  (p/ap b a))

(defcurry reduce [f x a]
  {:pre [(u/invoke? f) (satisfies? p/Foldable a)]}
  (p/reduce a f x))

(defcurry concat [a b]
  {:pre [(satisfies? p/Semigroup a)
         (satisfies? p/Semigroup b)]}
  (p/concat b a))

(defcurry equals [a b]
  {:pre [(satisfies? p/Setoid a)
         (satisfies? p/Setoid b)]}
  (p/equals b a))

(defcurry alt [a b]
  {:pre [(satisfies? p/Setoid a)
         (satisfies? p/Setoid b)]}
  (p/alt b a))

(defcurry bimap [f1 f2 a]
  {:pre [(u/invoke? f1)
         (u/invoke? f2)
         (satisfies? p/Bifunctor a)]}
  (p/bimap a f1 f2))

(defcurry extend [f a]
  {:pre [(u/invoke? f)
         (satisfies? p/Extend a)]}
  (p/extend a f))

(defcurry promap [f1 f2 a]
  {:pre [(u/invoke? f1)
         (u/invoke? f2)
         (satisfies? p/Profunctor a)]}
  (p/promap a f1 f2))

(defcurry traverse [type-rep f a]
  {:pre [(u/invoke? f)
         (satisfies? p/Traversable a)]}
  (p/traverse a type-rep f))

(defcurry lte [a b]
  {:pre [(satisfies? p/Ord a)
         (satisfies? p/Ord b)]}
  (p/lte b a))

(defcurry chain [f a]
  {:pre [(u/invoke? f)
         (satisfies? p/Chain a)]}
  (p/chain a f))

(defcurry extract [a]
  {:pre [(satisfies? p/Comonad a)]}
  (p/extract a))
