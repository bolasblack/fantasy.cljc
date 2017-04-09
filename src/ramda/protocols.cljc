(ns ramda.protocols)

(defprotocol Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#functor"
  (fl-map [this f]))

(defprotocol Foldable
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#foldable"
  (fl-reduce [this f x]))

(defprotocol Semigroup
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#semigroup"
  (fl-concat [this a]))

(defprotocol Setoid
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#setoid"
  (fl-equals [this a]))

(defprotocol Alt ; extends Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#alt"
  (fl-alt [this a]))

(defprotocol Apply ; extends Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#apply"
  (fl-ap [this a]))

(defprotocol Bifunctor ; extends Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#bifunctor"
  (fl-bimap [this f1 f2]))

(defprotocol Extend ; extends Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#extend"
  (fl-extend [this f]))

(defprotocol Profunctor ; extends Functor
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#profunctor"
  (fl-promap [this f1 f2]))

(defprotocol Traversable ; extends Functor Foldable
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#traversable"
  (fl-traverse [this f1 f2]))

(defprotocol Monoid ; extends Semigroup
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#monoid")
;; (empty [])

(defprotocol Ord ; extends Setoid
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#ord"
  (fl-lte [this a]))

(defprotocol Plus ; extends Alt
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#plus")
;; (zero [])

(defprotocol Applicative ; extends Apply
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#applicative")

(defprotocol Chain ; extends Apply
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#chain"
  (fl-chain [this f]))

(defprotocol Comonad ; extends Extend
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#comonad"
  (fl-extract [this]))

(defprotocol Alternative ; extends Applicative Plus
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#alternative")

(defprotocol Monad ; extends Applicative Chain
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#monad")

(defprotocol ChainRec ; extends Chain
  "https://github.com/fantasyland/fantasy-land/blob/master/README.md#chainRec")
