(ns ramda.laws
  (:require [ramda.core :as R]))



(defn setoid-reflexivity [a]
  (= (R/equals a a)))

(defn setoid-symmetry [a b]
  (= (R/equals b a)
     (R/equals a b)))

(defn setoid-transitivity [a b c]
  (= (and (R/equals b a) (R/equals c b))
     (R/equals a c)))



(defn semigroup-associative [a b c]
  (= (-> a (R/concat b) (R/concat c))
     (->> c (R/concat b) (R/concat a))))



(defn functor-identity [a]
  (= (R/map identity a)
     a))

(defn functor-composition [a f g]
  (= (R/map (fn [x] (f (g x))) a)
     (->> a
          (R/map g)
          (R/map f))))



(defn apply-composition [v u a]
  (= (->> v (R/ap u) (R/ap a))
     (-> (R/map (fn [f] (fn [g] (fn [x] (f (g x))))) a)
         (R/ap u)
         (R/ap v))))



(defn applicative-identity [v of]
  (= (-> (of (fn [x] x))
         (R/ap v))
     v))

(defn applicative-homomorphism [x f of]
  (= (R/ap (of f) (of x))
     (of (f x))))

(defn applicative-interchange [y u of]
  (= (R/ap u (of y))
     (R/ap (of #(% y)) u)))



(defn chain-associativity [m f g]
  (= (->> m (R/chain f) (R/chain g))
     (R/chain #(R/chain g (f %)) m)))



(defn chain-rec-equivalence [chain-rec p d n x]
  (= (chain-rec
      (fn [next done v]
        (if (p v)
          (R/map done (d v))
          (R/map next (n v))))
      x)
     ((fn step [v]
        (if (p v)
          (d v)
          (R/chain step (n v))))
      x)))

(defn chain-rec-stacksafe [chain-rec of]
  (= (chain-rec
      (fn [next done n]
        (if (= n 0)
          (of (done "DONE"))
          (of (next (- n 1)))))
      100000)
     (of "DONE")))



(defn extend-associative [w g f]
  (= (->> w (R/extend g) (R/extend f))
     (R/extend (fn [_w] (f (R/extend g _w))) w)))



(defn bifunctor-identity [a]
  (= (R/bimap identity identity a)
     a))

(defn bifunctor-composition [p f g h i]
  (= (R/bimap
      (fn [a] (f (g a)))
      (fn [a] (h (i a)))
      p)
     (->> p
          (R/bimap g i)
          (R/bimap f h))))



(defn foldable [u f x]
  (= (->> (R/reduce (fn [acc x] (R/concat acc [x])) [] u)
          (R/reduce f x))
     (R/reduce f x u)))
