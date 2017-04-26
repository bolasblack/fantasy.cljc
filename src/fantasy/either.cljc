(ns fantasy.either
  (:require [fantasy.protocols :as p]
            [fantasy.standard-func :as standard-fn]
            [fantasy.utils :as u :refer [defpr extend-types] :include-macros true :refer-macros [defpr extend-types]]))

(defprotocol Either
  (left? [this])
  (right? [this]))

(deftype Left [value]
  Either
  (left? [this] true)
  (right? [this] false)

  #?@(:clj [Object (equals [this that] (p/-equals this that))]))

(deftype Right [value]
  Either
  (left? [this] false)
  (right? [this] true)

  #?@(:clj [Object (equals [this that] (p/-equals this that))]))

(extend-types
 [Left Right]

 #?@(:cljs [IEquiv (-equiv [this that] (p/-equals this that))])

 p/Functor
 (p/map [this f]
        (if (right? this)
          (Right. (f (.-value this)))
          this))

 p/Bifunctor
 (p/bimap [this f1 f2]
          (if (right? this)
            (Right. (f2 (.-value this)))
            (Left. (f1 (.-value this)))))

 p/Apply
 (p/ap [this that]
       (if (right? that)
         (p/map this (.-value that))
         that))

 p/Extend
 (p/extend [this f]
   (if (right? this)
     (Right. (f this))
     this))

 p/Chain
 (p/chain [this f]
          (if (right? this)
            (f (.-value this))
            this))

 p/Foldable
 (p/reduce [this f x]
           (if (right? this)
             (f x (.-value this))
             x))

 p/Traversable
 (p/traverse [this type-rep f]
             (if (right? this)
               (p/map (f (.-value this)) #(Right. %))
               (standard-fn/of type-rep this)))

 p/Alt
 (p/alt [this that]
        (if (right? this)
          this
          that))

 p/Semigroup
 (p/concat [this that]
           (if (left? this)
             (if (left? that) (Left. (p/concat (.-value this) (.-value that))) that)
             (if (left? that) this (Right. (p/concat (.-value this) (.-value that))))))

 p/Setoid
 (p/-equals [this that]
            (u/equals this that))

 p/Comonad
 (p/extract [this]
            (.-value this))

 p/Applicative

 p/Monad

 p/ChainRec)

(defpr [Left Right] [this]
  (str (if (left? this) "(Left. " "(Right. ") (.-value this) ")"))

(defn left [value]
  (Left. value))

(defn right [value]
  (Right. value))

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
