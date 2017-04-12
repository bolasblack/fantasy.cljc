(ns ramda.maybe
  (:require [ramda.protocols :as p]
            [ramda.standard-func :as standard-fn]
            [ramda.multimethods :as m]
            [ramda.utils :as u :include-macros true :refer-macros [defpr extend-types]]))

(defprotocol Maybe
  (just? [this])
  (nothing? [this]))

(deftype Just [value]
  Maybe
  (just? [this] true)
  (nothing? [this] false))

(deftype Nothing []
  #?(:cljs
     IFn (-invoke [this] this))

  Maybe
  (just? [this] false)
  (nothing? [this] true))

(extend-types
 [Just Nothing]

 IEquiv
 (-equiv [this a]
         (p/fl-equals this a))

 p/Alt
 (p/fl-alt [this a]
   (if (just? this) this a))

 p/Functor
 (p/fl-map [this f]
           (if (just? this)
             (Just. (f (.-value this)))
             this))

 p/Foldable
 (p/fl-reduce [this f x]
              (if (just? this)
                (f x (.-value this))
                x))

 p/Apply
 (p/fl-ap [this a]
          (if (just? a)
            (p/fl-map this (.-value a))
            a))

 p/Chain
 (p/fl-chain [this f]
             (if (just? this)
               (f (.-value this))
               this))

 p/Traversable
 (p/fl-traverse [this type-rep f]
                (if (just? this)
                  (p/fl-map (f (.-value this)) #(Just. %))
                  (standard-fn/of type-rep this)))

 p/Extend
 (p/fl-extend [this f]
              (if (just? this)
                (Just. (f this))
                this))

 p/Semigroup
 (p/fl-concat [this that]
              (if (nothing? this)
                that
                (if (nothing? that)
                  this
                  (Just. (p/fl-concat (.-value this) (.-value that))))))

 p/Setoid
 (p/fl-equals [this a]
              (if (nothing? this)
                (= (type a) Nothing)
                (u/equals this a)))

 p/Applicative

 p/Monad

 p/ChainRec

 p/Monoid

 p/Plus

 p/Alternative)

(defpr [Just Nothing] [this]
  (if (nothing? this) "(Nothing. )" (str "(Just. " (.-value this) ")")))

(def nothing (Nothing.))

(defn just [value]
  (Just. value))

(defmethod m/from Just [a b]
  (.-value a))

(defmethod m/from Nothing [a b]
  b)

(defmethod standard-fn/of Maybe [type value]
  (just value))

;; https://github.com/ramda/ramda-fantasy/blob/723b3f71d676f6e69764e56f15e98ff7e3039d53/src/Maybe.js#L92
(defmethod standard-fn/chain-rec Maybe [type f i]
  (loop [state (u/chain-rec-next i)]
    (if (:next? state)
      (let [result (f u/chain-rec-next u/chain-rec-done (:value state))]
        (if (nothing? result)
          result
          (recur (.-value result))))
      (just (:value state)))))

(defmethod standard-fn/zero Maybe [type]
  nothing)

(defmethod standard-fn/empty Maybe [type]
  nothing)
