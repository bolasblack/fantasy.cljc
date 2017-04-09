(ns ramda.core
  (:refer-clojure :exclude [map reduce concat])
  (:require [ramda.curry :as curry-ns :include-macros true :refer-macros [defcurry]]
            [ramda.func :as func-ns]
            [ramda.protocols :as p]
            [ramda.either :as either]
            [ramda.maybe :as maybe]
            [ramda.multimethods :as multimethods]
            [ramda.utils :as u :include-macros true]))

(u/pull-to-ns {curry-ns [curry __]
               func-ns [always]
               either [Either Left Right left right]
               maybe [Maybe Just Nothing just nothing]
               multimethods [of chain-rec from]})

(defcurry map [f a]
  {:pre [(or (and (fn? f) (satisfies? p/Functor a))
             (and (fn? f) (coll? a)))]}
  (if (satisfies? p/Functor a)
    (p/fl-map a f)
    (clojure.core/map f a)))

(defcurry ap [a b]
  {:pre [(or (and (satisfies? p/Apply a) (satisfies? p/Apply b))
             (and (coll? a) (every? fn? a) (coll? b)))]}
  (if (and (satisfies? p/Apply a) (satisfies? p/Apply b))
    (p/fl-ap a b)
    (apply clojure.core/concat (map #(map % b) a))))

(defcurry reduce [f x a]
  {:pre [(or (and (fn? f) (satisfies? p/Foldable a))
             (and (fn? f) (coll? a)))]}
  (if (satisfies? p/Foldable a)
    (p/fl-reduce a f x)
    (clojure.core/reduce f x a)))

(defcurry concat [a b]
  {:pre [(or (and (satisfies? p/Semigroup a) (satisfies? p/Semigroup b))
             (and (coll? a) (coll? b)))]}
  (if (satisfies? p/Semigroup b)
    (p/fl-concat b a)
    (clojure.core/concat a b)))

(defcurry equals [a b]
  (if (and (satisfies? p/Setoid a)
           (satisfies? p/Setoid b))
    (p/fl-equals b a)
    (= a b)))

(defcurry alt [a b]
  {:pre [(satisfies? p/Setoid a)
         (satisfies? p/Setoid b)]}
  (p/fl-alt b a))

(defcurry bimap [f1 f2 a]
  {:pre [(fn? f1)
         (fn? f2)
         (satisfies? p/Bifunctor a)]}
  (p/fl-bimap a f1 f2))

(defcurry extend [f a]
  {:pre [(fn? f)
         (satisfies? p/Extend a)]}
  (p/fl-extend a f))

(defcurry promap [f1 f2 a]
  {:pre [(fn? f1)
         (fn? f2)
         (satisfies? p/Profunctor a)]}
  (p/fl-promap a f1 f2))

(defcurry traverse [f1 f2 a]
  {:pre [(fn? f1)
         (fn? f2)
         (satisfies? p/Traversable a)]}
  (p/fl-traverse a f1 f2))

(defcurry lte [a b]
  {:pre [(satisfies? p/Ord a)
         (satisfies? p/Ord b)]}
  (p/fl-lte b a))

(defcurry chain [f a]
  {:pre [(fn? f)
         (satisfies? p/Chain a)]}
  (p/fl-chain a f))

(defcurry extract [a]
  {:pre [(satisfies? p/Comonad a)]}
  (p/fl-extract a))

(comment
  (cljs.nodejs/enable-util-print!)

  (defn -main [& args]
    (try
      (println (satisfies? Either (left 123)))
      (catch js/Error e
        (println (.-stack e)))))

  (set! *main-cli-fn* -main))
