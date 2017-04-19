(ns fantasy.laws
  (:require [fantasy.core :as f]
            [fantasy.compose :as C :include-macros true]))



(defn setoid-reflexivity [a]
  (= (f/equals a a)))

(defn setoid-symmetry [a b]
  (= (f/equals b a)
     (f/equals a b)))

(defn setoid-transitivity [a b c]
  (= (and (f/equals b a) (f/equals c b))
     (f/equals a c)))



(defn semigroup-associativity [a b c]
  (= (-> a (f/concat b) (f/concat c))
     (->> c (f/concat b) (f/concat a))))



(defn functor-identity [a]
  (= (f/map identity a)
     a))

(defn functor-composition [a f g]
  (= (f/map (fn [x] (f (g x))) a)
     (->> a
          (f/map g)
          (f/map f))))



(defn apply-composition [v u a]
  (= (->> v (f/ap u) (f/ap a))
     (-> (f/map (fn [f] (fn [g] (fn [x] (f (g x))))) a)
         (f/ap u)
         (f/ap v))))



(defn applicative-identity [v of]
  (= (-> (of (fn [x] x))
         (f/ap v))
     v))

(defn applicative-homomorphism [x f of]
  (= (f/ap (of f) (of x))
     (of (f x))))

(defn applicative-interchange [y u of]
  (= (f/ap u (of y))
     (f/ap (of #(% y)) u)))



(defn chain-associativity [m f g]
  (= (->> m (f/chain f) (f/chain g))
     (f/chain #(f/chain g (f %)) m)))



(defn chain-rec-equivalence [chain-rec p d n x]
  (= (chain-rec
      (fn [next done v]
        (if (p v)
          (f/map done (d v))
          (f/map next (n v))))
      x)
     ((fn step [v]
        (if (p v)
          (d v)
          (f/chain step (n v))))
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
  (= (f/bimap identity identity a)
     a))

(defn bifunctor-composition [p f g h i]
  (= (f/bimap
      (fn [a] (f (g a)))
      (fn [a] (h (i a)))
      p)
     (->> p
          (f/bimap g i)
          (f/bimap f h))))



(defn foldable [u f x]
  (= (->> (f/reduce (fn [acc x] (f/concat acc [x])) [] u)
          (f/reduce f x))
     (f/reduce f x u)))



(defn monad-left-identity [a f of]
  (= (f/chain f (of a))
     (f a)))

(defn monad-right-identity [m of]
  (= (f/chain of m)
     m))



(defn traversable-naturality [t u type-rep-f type-rep-g]
  (= (t (f/traverse type-rep-f identity u))
     (f/traverse type-rep-g t u)))

(defn traversable-identity [u type-rep-f]
  (= (f/traverse type-rep-f #(f/of type-rep-f %) u)
     (f/of type-rep-f u)))

(defn traversable-composition [u type-rep-f type-rep-g]
  (let [Compose (C/defcompose type-rep-f type-rep-g)]
    (= (f/traverse Compose #(new Compose %) u)
       (new Compose (->> u
                         (f/traverse type-rep-f identity)
                         (f/map #(f/traverse type-rep-g identity %)))))))



(defn monoid-left-identity [m type-rep]
  (= (->> type-rep (f/empty) (f/concat m))
     m))

(defn monoid-right-identity [m type-rep]
  (= (f/concat (f/empty type-rep) m)
     m))



(defn alt-associativity [a b c]
  (= (->> a (f/alt b) (f/alt c))
     (f/alt (f/alt c b) a)))

(defn alt-distributivity [a b f]
  (= (->> a (f/alt b) (f/map f))
     (->> a (f/map f) (f/alt (f/map f b)))))



(defn plus-left-identity [x type-rep]
  (= (f/alt x (f/zero type-rep))
     x))

(defn plus-right-identity [x type-rep]
  (= (f/alt (f/zero type-rep) x)
     x))

(defn plus-annihilation [type-rep f]
  (= (f/map f (f/zero type-rep))
     (f/zero type-rep)))



(defn extend-associativity [w g f]
  (= (->> w (f/extend g) (f/extend f))
     (f/extend (fn [_w] (f (f/extend g _w))) w)))
