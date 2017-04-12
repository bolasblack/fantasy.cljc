(ns ramda.either
  (:require [ramda.protocols :as p]
            [ramda.standard-func :as standard-fn]
            [ramda.multimethods :as m]
            [ramda.utils :as u :include-macros true :refer-macros [defpr extend-types]]))

(defprotocol Either
  (left? [this])
  (right? [this]))

(deftype Left [value]
  Either
  (left? [this] true)
  (right? [this] false))

(deftype Right [value]
  Either
  (left? [this] false)
  (right? [this] true))

(extend-types
 [Left Right]

 IEquiv
 (-equiv [this a]
         (p/fl-equals this a))

 p/Functor
 (p/fl-map [this f]
           (if (right? this)
             (Right. (f (.-value this)))
             this))

 p/Bifunctor
 (p/fl-bimap [this f1 f2]
             (if (right? this)
               (Right. (f2 (.-value this)))
               (Left. (f1 (.-value this)))))

 p/Apply
 (p/fl-ap [this that]
          (if (right? that)
            (p/fl-map this (.-value that))
            that))

 p/Extend
 (p/fl-extend [this f]
              (if (right? this)
                (Right. (f this))
                this))

 p/Chain
 (p/fl-chain [this f]
             (if (right? this)
               (f (.-value this))
               this))

 p/Foldable
 (p/fl-reduce [this f x]
              (if (right? this)
                (f x (.-value this))
                x))

 p/Traversable
 (p/fl-traverse [this type-rep f]
                (if (right? this)
                  (p/fl-map (f (.-value this)) #(Right. %))
                  (standard-fn/of type-rep this)))

 p/Alt
 (p/fl-alt [this that]
           (if (right? this)
             this
             that))

 p/Semigroup
 (p/fl-concat [this that]
              (if (left? this)
                (if (left? that) (Left. (p/fl-concat (.-value this) (.-value that))) that)
                (if (left? that) this (Right. (p/fl-concat (.-value this) (.-value that))))))

 p/Setoid
 (p/fl-equals [this a]
              (u/equals this a))

 p/Applicative

 p/Monad

 p/ChainRec)

(defpr [Left Right] [this]
  (str (if (left? this) "(Left. " "(Right. ") (.-value this) ")"))

(defn left [value]
  (Left. value))

(defn right [value]
  (Right. value))

(defmethod m/from Left [a]
  (.-value a))

(defmethod m/from Right [a]
  (.-value a))

(defmethod standard-fn/of Either [type value]
  (right value))

;; https://github.com/ramda/ramda-fantasy/blob/723b3f71d676f6e69764e56f15e98ff7e3039d53/src/Either.js#L71
(defmethod standard-fn/chain-rec Either [type f i]
  (loop [state (u/chain-rec-next i)]
    (if (:next? state)
      (let [result (f u/chain-rec-next u/chain-rec-done (:value state))]
        (if (left? result)
          result
          (recur (.-value result))))
      (right (:value state)))))
