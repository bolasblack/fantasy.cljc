(ns ramda.identity
  (:require [ramda.protocols :as p]
            [ramda.standard-func :as standard-fn]
            [ramda.multimethods :as m]
            [ramda.maybe :as maybe-ns]
            [ramda.utils :as u :include-macros true :refer-macros [defpr]]))

(deftype Identity [value]
  IEquiv
  (-equiv [this that]
    (u/equals this that))

  p/Functor
  (p/map [this f]
    (Identity. (f (.-value this))))

  p/Foldable
  (p/reduce [this f x]
    (f x (.-value this)))

  p/Semigroup
  (p/concat [this that]
    (Identity. (p/concat (.-value this) (.-value that))))

  p/Setoid
  (p/equals [this that]
    (= (.-value this) (.-value that)))

  p/Apply
  (p/ap [this that]
    (p/map this (.-value that)))

  p/Extend
  (p/extend [this f]
    (Identity. (f this)))

  p/Traversable
  (p/traverse [this type-rep f]
    (p/map (f (.-value this)) #(Identity. %)))

  p/Applicative

  p/Chain
  (p/chain [this f]
    (f (.-value this)))

  p/Comonad
  (p/extract [this]
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
