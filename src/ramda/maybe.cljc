(ns ramda.maybe
  (:require [ramda.protocols :as p]
            [ramda.multimethods :as m]
            [ramda.utils :as u :include-macros true :refer-macros [defpr extend-types]]))

(defprotocol Maybe
  (just? [this])
  (nothing? [this])
  (get-or-else [this a]))

(deftype Just [value]
  Maybe
  (just? [this] true)
  (nothing? [this] false)

  p/Functor
  (p/fl-map [this f]
    (Just. (f (.-value this))))

  p/Foldable
  (p/fl-reduce [this f x]
    (f x (.-value this)))

  p/Apply
  (p/fl-ap [this that]
    (if (just? that)
      (p/fl-map this (.-value that))
      that))

  p/Chain
  (p/fl-chain [this f]
    (f (.-value this))))

(deftype Nothing []
  #?(:cljs
     IFn (-invoke [this] this))

  Maybe
  (just? [this] false)
  (nothing? [this] true)

  p/Functor
  (p/fl-map [this f]
    this)

  p/Foldable
  (p/fl-reduce [this f x]
    x)

  p/Apply
  (p/fl-ap [this a]
    this)

  p/Chain
  (p/fl-chain [this f]
    this))

(extend-types
 [Just Nothing]

 IEquiv
 (-equiv [this a]
         (u/equals this a))

 p/Setoid
 (p/fl-equals [this a]
              (u/equals this a))

 p/Applicative

 p/Monad

 p/ChainRec)

(defpr [Just Nothing] [this]
  (if (nothing? this) "(Nothing. )" (str "(Just. " (.-value this) ")")))

(def nothing (Nothing.))

(defn just [value]
  (Just. value))

(defmethod m/from Just [a b]
  (.-value a))

(defmethod m/from Nothing [a b]
  b)

(defmethod m/of Maybe [type value]
  (just value))

;; https://github.com/ramda/ramda-fantasy/blob/723b3f71d676f6e69764e56f15e98ff7e3039d53/src/Maybe.js#L92
(defmethod m/chain-rec Maybe [type f i]
  (loop [state (u/chain-rec-next i)]
    (if (:next? state)
      (let [result (f u/chain-rec-next u/chain-rec-done (:value state))]
        (if (nothing? result)
          result
          (recur (.-value result))))
      (just (:value state)))))
