(ns ramda.standard-func
  (:refer-clojure :exclude [map reduce concat])
  (:require [ramda.curry :as curry-ns :include-macros true :refer-macros [defcurry]]
            [ramda.protocols :as p]))

(defmulti of identity)

(defmulti chain-rec identity)

(defcurry map [f a]
  {:pre [(fn? f) (satisfies? p/Functor a)]}
  (p/fl-map a f))

(defcurry ap [a b]
  {:pre [(satisfies? p/Apply a) (satisfies? p/Apply b)]}
  (p/fl-ap b a))

(defcurry reduce [f x a]
  {:pre [(fn? f) (satisfies? p/Foldable a)]}
  (p/fl-reduce a f x))

(defcurry concat [a b]
  {:pre [(satisfies? p/Semigroup a)
         (satisfies? p/Semigroup b)]}
  (p/fl-concat b a))

(defcurry equals [a b]
  {:pre [(satisfies? p/Setoid a)
         (satisfies? p/Setoid b)]}
  (p/fl-equals b a))

(defcurry alt [a b]
  {:pre [(satisfies? p/Setoid a)
         (satisfies? p/Setoid b)]}
  (p/fl-alt b a))

(defcurry bimap [f1 f2 a]
  {:pre [(fn? f1)
         (fn? f2)
         (satisfies? p/Bifunctor a)]}
  (p/fl-bimap a f1 f2))

(defcurry extend [f a]
  {:pre [(fn? f)
         (satisfies? p/Extend a)]}
  (p/fl-extend a f))

(defcurry promap [f1 f2 a]
  {:pre [(fn? f1)
         (fn? f2)
         (satisfies? p/Profunctor a)]}
  (p/fl-promap a f1 f2))

(defcurry traverse [f1 f2 a]
  {:pre [(fn? f1)
         (fn? f2)
         (satisfies? p/Traversable a)]}
  (p/fl-traverse a f1 f2))

(defcurry lte [a b]
  {:pre [(satisfies? p/Ord a)
         (satisfies? p/Ord b)]}
  (p/fl-lte b a))

(defcurry chain [f a]
  {:pre [(fn? f)
         (satisfies? p/Chain a)]}
  (p/fl-chain a f))

(defcurry extract [a]
  {:pre [(satisfies? p/Comonad a)]}
  (p/fl-extract a))
