(ns fantasy.identity
  (:require [fantasy.protocols :as p]
            [fantasy.standard-func :as standard-fn]
            [fantasy.funcs :as funcs]
            [fantasy.maybe :as maybe-ns]
            [fantasy.utils :as u :include-macros true]))

(deftype Identity [value]
  #?@(:cljs [IEquiv (-equiv [this that] (p/-equals this that))]
      :clj [Object (equals [this that] (p/-equals this that))])

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
  (p/-equals [this that]
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

  p/Monad

  p/Printable
  (p/-repr [this]
    (str "(Identity. " (.-value this) ")")))

(u/make-printable Identity)

(defmethod funcs/to-maybe Identity [a]
  (if (nil? (.-value a))
    maybe-ns/nothing
    (maybe-ns/just (.-value a))))

(defmethod standard-fn/of Identity [type value]
  (Identity. value))
