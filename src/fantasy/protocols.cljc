(ns fantasy.protocols
  (:refer-clojure :exclude [map reduce concat empty]))

(defprotocol Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#functor"
  (map [this f]))

(defprotocol Foldable
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#foldable"
  (reduce [this f x]))

(defprotocol Semigroup
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#semigroup"
  (concat [this that]))

(defprotocol Setoid
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#setoid"
  (equals [this that]))

(defprotocol Alt ; extends Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#alt"
  (alt [this that]))

(defprotocol Apply ; extends Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#apply"
  (ap [this that]))

(defprotocol Bifunctor ; extends Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#bifunctor"
  (bimap [this f1 f2]))

(defprotocol Extend ; extends Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#extend"
  (extend [this f]))

(defprotocol Profunctor ; extends Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#profunctor"
  (promap [this f1 f2]))

(defprotocol Traversable ; extends Functor Foldable
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#traversable"
  (traverse [this type-rep f]))

(defprotocol Monoid ; extends Semigroup
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#monoid")
;; (empty [])

(defprotocol Ord ; extends Setoid
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#ord"
  (lte [this that]))

(defprotocol Plus ; extends Alt
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#plus")
;; (zero [])

(defprotocol Applicative ; extends Apply
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#applicative")
;; (of [])

(defprotocol Chain ; extends Apply
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#chain"
  (chain [this f]))

(defprotocol Comonad ; extends Extend
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#comonad"
  (extract [this]))

(defprotocol Alternative ; extends Applicative Plus
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#alternative")

(defprotocol Monad ; extends Applicative Chain
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#monad")

(defprotocol ChainRec ; extends Chain
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#chainRec")
