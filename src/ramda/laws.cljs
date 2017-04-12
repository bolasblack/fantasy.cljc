(ns ramda.laws
  (:require [ramda.core :as R]
            [ramda.compose :as C :include-macros true]))



(defn setoid-reflexivity [a]
  (= (R/equals a a)))

(defn setoid-symmetry [a b]
  (= (R/equals b a)
     (R/equals a b)))

(defn setoid-transitivity [a b c]
  (= (and (R/equals b a) (R/equals c b))
     (R/equals a c)))



(defn semigroup-associativity [a b c]
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



(defn monad-left-identity [a f of]
  (= (R/chain f (of a))
     (f a)))

(defn monad-right-identity [m of]
  (= (R/chain of m)
     m))



(defn traversable-naturality [t u type-rep-f type-rep-g]
  (= (t (R/traverse type-rep-f identity u))
     (R/traverse type-rep-g t u)))

(defn traversable-identity [u type-rep-f]
  (= (R/traverse type-rep-f #(R/of type-rep-f %) u)
     (R/of type-rep-f u)))

(defn traversable-composition [u type-rep-f type-rep-g]
  (let [Compose (C/defcompose type-rep-f type-rep-g)]
    (= (R/traverse Compose #(new Compose %) u)
       (new Compose (->> u
                         (R/traverse type-rep-f identity)
                         (R/map #(R/traverse type-rep-g identity %)))))))



(defn monoid-left-identity [m type-rep]
  (= (->> type-rep (R/empty) (R/concat m))
     m))

(defn monoid-right-identity [m type-rep]
  (= (R/concat (R/empty type-rep) m)
     m))



(defn alt-associativity [a b c]
  (= (->> a (R/alt b) (R/alt c))
     (R/alt (R/alt c b) a)))

(defn alt-distributivity [a b f]
  (= (->> a (R/alt b) (R/map f))
     (->> a (R/map f) (R/alt (R/map f b)))))



(defn plus-left-identity [x type-rep]
  (= (R/alt x (R/zero type-rep))
     x))

(defn plus-right-identity [x type-rep]
  (= (R/alt (R/zero type-rep) x)
     x))

(defn plus-annihilation [type-rep f]
  (= (R/map f (R/zero type-rep))
     (R/zero type-rep)))



(defn extend-associativity [w g f]
  (= (->> w (R/extend g) (R/extend f))
     (R/extend (fn [_w] (f (R/extend g _w))) w)))
