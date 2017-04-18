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
  Maybe
  (just? [this] false)
  (nothing? [this] true))

(extend-types
 [Just Nothing]

 IEquiv
 (-equiv [this that]
         (p/equals this that))

 p/Alt
 (p/alt [this that]
        (if (just? this) this that))

 p/Functor
 (p/map [this f]
        (if (just? this)
          (Just. (f (.-value this)))
          this))

 p/Foldable
 (p/reduce [this f x]
           (if (just? this)
             (f x (.-value this))
             x))

 p/Apply
 (p/ap [this that]
       (if (just? that)
         (p/map this (.-value that))
         that))

 p/Chain
 (p/chain [this f]
          (if (just? this)
            (f (.-value this))
            this))

 p/Traversable
 (p/traverse [this type-rep f]
             (if (just? this)
               (p/map (f (.-value this)) #(Just. %))
               (standard-fn/of type-rep this)))

 p/Extend
 (p/extend [this f]
           (if (just? this)
             (Just. (f this))
             this))

 p/Semigroup
 (p/concat [this that]
           (if (nothing? this)
             that
             (if (nothing? that)
               this
               (Just. (p/concat (.-value this) (.-value that))))))

 p/Setoid
 (p/equals [this that]
           (if (nothing? this)
             (= (type that) Nothing)
             (u/equals this that)))

 p/Comonad
 (p/extract [this]
            (if (nothing? this)
              nil
              (.-value this)))

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

(defn from-maybe [a b]
  {:pre [(satisfies? Maybe a)]}
  (if (just? a) (.-value a) b))

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
