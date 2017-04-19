(ns fantasy.either.laws-test
  (:require [fantasy.core :as f]
            [fantasy.laws :as laws]
            [clojure.test :refer [deftest testing are is]]))

(deftest Either
  (testing "Setoid"
    (testing "reflexivity"
      (is (laws/setoid-reflexivity (f/left 1)))
      (is (laws/setoid-reflexivity (f/right 1))))
    (testing "symmetry"
      (is (laws/setoid-symmetry (f/left 1) (f/left 1)))
      (is (laws/setoid-symmetry (f/right 1) (f/right 1))))
    (testing "transitivity"
      (is (laws/setoid-transitivity (f/left 1) (f/left 1) (f/left 1)))
      (is (laws/setoid-transitivity (f/right 1) (f/right 1) (f/right 1)))))

  (testing "Functor"
    (testing "identity"
      (is (laws/functor-identity (f/left 1)))
      (is (laws/functor-identity (f/right 1))))
    (testing "composition"
      (is (laws/functor-composition (f/left 1) #(+ % 1) #(+ % 100)))
      (is (laws/functor-composition (f/right 1) #(+ % 1) #(+ % 100)))))

  (testing "Apply"
    (testing "composition"
      (is (laws/apply-composition (f/left 1) (f/left #(+ % 1)) (f/left #(+ % 2))))
      (is (laws/apply-composition (f/right 1) (f/right #(+ % 1)) (f/right #(+ % 2))))))

  (testing "Applicative"
    (testing "identity"
      (is (laws/applicative-identity (f/left 1) #(f/of f/Either %)))
      (is (laws/applicative-identity (f/right 1) #(f/of f/Either %))))
    (testing "homomorphism"
      (is (laws/applicative-homomorphism (f/left 1) #(+ % 1) #(f/of f/Either %)))
      (is (laws/applicative-homomorphism (f/right 1) #(+ % 1) #(f/of f/Either %))))
    (testing "interchange"
      (is (laws/applicative-interchange 2 (f/left #(+ % 1)) #(f/of f/Either %)))
      (is (laws/applicative-interchange 2 (f/right #(+ % 1)) #(f/of f/Either %)))))

  (testing "Chain"
    (testing "associativity"
      (is (laws/chain-associativity (f/left 1) #(f/left (+ % 1)) #(f/left (+ % 2))))
      (is (laws/chain-associativity (f/right 1) #(f/right (+ % 1)) #(f/right (+ % 2))))))

  (testing "ChainRec"
    (testing "equivalence"
      (let [predicate #(> (count %) 5)
            initial [1]
            done #(f/of f/Either %)
            next #(f/of f/Either (concat % [1]))]
        (is (laws/chain-rec-equivalence #(f/chain-rec f/Either %1 %2) predicate done next initial))))

    (testing "stacksafe"
      (is (laws/chain-rec-stacksafe #(f/chain-rec f/Either %1 %2) #(f/of f/Either %))))

    (testing "responds to failure immediately"
      (is (= (f/chain-rec f/Either #(f/left "ERROR") 100)
             (f/left "ERROR"))))

    (testing "responds to failure on next step"
      (is (= (f/chain-rec
              f/Either
              (fn [next done n]
                (if (= n 0) (f/left "ERROR") (f/right (next (- n 1)))))
              100)
             (f/left "ERROR")))))

  (testing "Monad"
    (testing "left-identity"
      (is (laws/monad-left-identity (f/left 1) #(+ 1 %) #(f/of f/Either %)))
      (is (laws/monad-left-identity (f/right 1) #(+ 1 %) #(f/of f/Either %))))
    (testing "right-identity"
      (is (laws/monad-right-identity (f/left 1) #(f/of f/Either %)))
      (is (laws/monad-right-identity (f/right 1) #(f/of f/Either %)))))

  (testing "Extend"
    (testing "associativity"
      (is (laws/extend-associativity
           (f/left 1)
           #(f/left (f/map (fn [a] (+ a 2)) %))
           #(f/left (f/map (fn [a] (+ a 1)) %))))
      (is (laws/extend-associativity
           (f/right 1)
           #(f/right (f/map (fn [a] (+ a 2)) %))
           #(f/right (f/map (fn [a] (+ a 1)) %))))))

  (testing "Bifunctor"
    (testing "identity"
      (is (laws/bifunctor-identity (f/left 1)))
      (is (laws/bifunctor-identity (f/right 1))))
    (testing "composition"
      (is (laws/bifunctor-composition (f/left 1) #(+ % 1) #(+ % 10) #(+ % 100) #(+ % 1000)))
      (is (laws/bifunctor-composition (f/right 1) #(+ % 1) #(+ % 10) #(+ % 100) #(+ % 1000)))))

  (testing "Semigroup"
    (testing "associativity"
      (is (laws/semigroup-associativity (f/left [1]) (f/left [2]) (f/left [3])))
      (is (laws/semigroup-associativity (f/right [1]) (f/right [2]) (f/right [3])))))

  (testing "Alt"
    (testing "associativity"
      (is (laws/alt-associativity (f/left 1) (f/left 2) (f/left 3)))
      (is (laws/alt-associativity (f/right 1) (f/right 2) (f/right 3))))
    (testing "distributivity"
      (is (laws/alt-distributivity (f/left 1) (f/left 2) #(+ 1 %)))
      (is (laws/alt-distributivity (f/right 1) (f/right 2) #(+ 1 %)))))

  (testing "Foldable"
    (is (laws/foldable (f/left [1 2 3 4]) + 0))
    (is (laws/foldable (f/right [1 2 3 4]) + 0)))

  (testing "Traversable"
    (testing "naturality"
      (is (laws/traversable-naturality f/to-maybe (f/left (f/Identity. 1)) f/Identity f/Maybe))
      (is (laws/traversable-naturality f/to-maybe (f/right (f/Identity. 1)) f/Identity f/Maybe)))
    (testing "identity"
      (is (laws/traversable-identity (f/left 1) f/Identity))
      (is (laws/traversable-identity (f/right 1) f/Identity)))
    (testing "composition"
      (is (laws/traversable-composition (f/left (f/Identity. (f/left 1))) f/Identity f/Maybe))
      (is (laws/traversable-composition (f/right (f/Identity. (f/right 1))) f/Identity f/Maybe))))

  (testing "Comonad"
    (is (= 1 (f/extract (f/left 1))))
    (is (= 1 (f/extract (f/right 1))))))
