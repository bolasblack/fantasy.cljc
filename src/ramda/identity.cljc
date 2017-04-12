(ns ramda.identity
  (:require [ramda.protocols :as p]
            [ramda.standard-func :as standard-fn]
            [ramda.multimethods :as m]
            [ramda.maybe :as maybe-ns]
            [ramda.utils :as u :include-macros true :refer-macros [defpr]]))

(deftype Identity [value]
  IEquiv
  (-equiv [this a]
    (u/equals this a))

  p/Functor
  (p/fl-map [this f]
    (Identity. (f (.-value this))))

  p/Foldable
  (p/fl-reduce [this f x]
    (f x (.-value this)))

  p/Semigroup
  (p/fl-concat [this a]
    (Identity. (p/fl-concat (.-value this) (.-value a))))

  p/Setoid
  (p/fl-equals [this a]
    (= (.-value this) (.-value a)))

  p/Apply
  (p/fl-ap [this a]
    (p/fl-map this (.-value a)))

  p/Extend
  (p/fl-extend [this f]
    (Identity. (f this)))

  p/Traversable
  (p/fl-traverse [this type-rep f]
    (p/fl-map (f (.-value this)) #(Identity. %)))

  p/Applicative

  p/Chain
  (p/fl-chain [this f]
    (f (.-value this)))

  p/Comonad
  (p/fl-extract [this]
    (.-value this))

  p/Monad)

(defpr [Identity] [this]
  (str "(Identity. " (.-value this) ")"))

(defmethod m/to-maybe Identity [a]
  (if (nil? (.-value a))
    maybe-ns/nothing
    (maybe-ns/just (.-value a))))

(defmethod standard-fn/of Identity [type value]
  (Identity. value))
