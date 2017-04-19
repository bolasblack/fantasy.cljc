(ns fantasy.maybe.laws-test
  (:require [fantasy.core :as f]
            [fantasy.laws :as laws]
            [clojure.test :refer [deftest testing are is]]))

(deftest Maybe
  (testing "Setoid"
    (testing "reflexivity"
      (is (laws/setoid-reflexivity f/nothing))
      (is (laws/setoid-reflexivity (f/just 1))))
    (testing "symmetry"
      (is (laws/setoid-symmetry f/nothing f/nothing))
      (is (laws/setoid-symmetry (f/just 1) (f/just 1))))
    (testing "transitivity"
      (is (laws/setoid-transitivity f/nothing f/nothing f/nothing))
      (is (laws/setoid-transitivity (f/just 1) (f/just 1) (f/just 1)))))

  (testing "Semigroup"
    (testing "associativity"
      (is (laws/semigroup-associativity f/nothing f/nothing f/nothing))
      (is (laws/semigroup-associativity (f/just [1]) (f/just [2]) (f/just [3])))))

  (testing "Functor"
    (testing "identity"
      (is (laws/functor-identity f/nothing))
      (is (laws/functor-identity (f/just 1))))
    (testing "composition"
      (is (laws/functor-composition f/nothing #(+ % 1) #(+ % 100)))
      (is (laws/functor-composition (f/just 1) #(+ % 1) #(+ % 100)))))

  (testing "Apply"
    (testing "composition"
      (is (laws/apply-composition f/nothing f/nothing f/nothing))
      (is (laws/apply-composition (f/just 1) (f/just #(+ % 1)) (f/just #(+ % 2))))))

  (testing "Applicative"
    (testing "identity"
      (is (laws/applicative-identity f/nothing #(f/of f/Maybe %)))
      (is (laws/applicative-identity (f/just 1) #(f/of f/Maybe %))))
    (testing "homomorphism"
      (is (laws/applicative-homomorphism f/nothing #(+ % 1) #(f/of f/Maybe %)))
      (is (laws/applicative-homomorphism (f/just 1) #(+ % 1) #(f/of f/Maybe %))))
    (testing "interchange"
      (is (laws/applicative-interchange 2 f/nothing #(f/of f/Maybe %)))
      (is (laws/applicative-interchange 2 (f/just #(+ % 1)) #(f/of f/Maybe %)))))

  (testing "Chain"
    (testing "associativity"
      (is (laws/chain-associativity f/nothing (fn [] f/nothing) (fn [] f/nothing)))
      (is (laws/chain-associativity (f/just 1) #(f/just (+ % 1)) #(f/just (+ % 2))))))

  (testing "Foldable"
    (is (laws/foldable f/nothing + 0))
    (is (laws/foldable (f/just [1 2 3 4]) + 0)))

  (testing "ChainRec"
    (testing "equivalence"
      (let [predicate #(> (count %) 5)
            initial [1]
            done #(f/of f/Maybe %)
            next #(f/of f/Maybe (concat % [1]))]
        (is (laws/chain-rec-equivalence #(f/chain-rec f/Maybe %1 %2) predicate done next initial))))

    (testing "stacksafe"
      (is (laws/chain-rec-stacksafe #(f/chain-rec f/Maybe %1 %2) #(f/of f/Maybe %))))

    (testing "responds to failure immediately"
      (is (= (f/chain-rec f/Maybe (fn [] f/nothing) 100)
             f/nothing)))

    (testing "responds to failure on next step"
      (is (= (f/chain-rec
              f/Maybe
              (fn [next done n]
                (if (= n 0) f/nothing (f/just (next (- n 1)))))
              100)
             f/nothing))))

  (testing "Monad"
    (testing "left-identity"
      (is (laws/monad-left-identity f/nothing #(+ 1 %) #(f/of f/Maybe %)))
      (is (laws/monad-left-identity (f/just 1) #(+ 1 %) #(f/of f/Maybe %))))

    (testing "right-identity"
      (is (laws/monad-right-identity f/nothing #(f/of f/Maybe %)))
      (is (laws/monad-right-identity (f/just 1) #(f/of f/Maybe %)))))

  (testing "Traversable"
    (testing "naturality"
      (is (laws/traversable-naturality f/to-maybe f/nothing f/Identity f/Maybe))
      (is (laws/traversable-naturality f/to-maybe (f/just (f/Identity. 1)) f/Identity f/Maybe)))

    (testing "identity"
      (is (laws/traversable-identity f/nothing f/Identity))
      (is (laws/traversable-identity (f/just 1) f/Identity)))

    (testing "composition"
      (is (laws/traversable-composition f/nothing f/Identity f/Maybe))
      (is (laws/traversable-composition (f/just (f/Identity. (f/just 1))) f/Identity f/Maybe))))

  (testing "Monoid"
    (testing "left-identity"
      (is (laws/monoid-left-identity f/nothing f/Maybe))
      (is (laws/monoid-left-identity (f/just 1) f/Maybe)))

    (testing "right-identity"
      (is (laws/monoid-right-identity f/nothing f/Maybe))
      (is (laws/monoid-right-identity (f/just 1) f/Maybe))))

  (testing "Alt"
    (testing "associativity"
      (is (laws/alt-associativity f/nothing f/nothing f/nothing))
      (is (laws/alt-associativity (f/just 1) (f/just 2) (f/just 3))))

    (testing "distributivity"
      (is (laws/alt-distributivity f/nothing f/nothing #(+ 1 %)))
      (is (laws/alt-distributivity (f/just 1) (f/just 2) #(+ 1 %)))))

  (testing "Plus"
    (testing "left-identity"
      (is (laws/plus-left-identity f/nothing f/Maybe))
      (is (laws/plus-left-identity (f/just 1) f/Maybe)))

    (testing "right-identity"
      (is (laws/plus-right-identity f/nothing f/Maybe))
      (is (laws/plus-right-identity (f/just 1) f/Maybe)))

    (testing "annihilation"
      (is (laws/plus-annihilation f/Maybe #(+ 1 %)))))

  (testing "extend-associativity"
    (testing "associativity"
      (is (laws/extend-associativity f/nothing #(+ 1 (.-value %)) #(* (.-value %) (.-value %))))
      (is (laws/extend-associativity (f/just 2) #(+ 1 (.-value %)) #(* (.-value %) (.-value %))))))

  (testing "Comonad"
    (is (= nil (f/extract f/nothing)))
    (is (= 1 (f/extract (f/just 1))))))
