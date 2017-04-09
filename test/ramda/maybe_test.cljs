(ns ramda.maybe-test
  (:require [ramda.core :as R]
            [ramda.laws :as laws]
            [clojure.test :refer [deftest testing are is]]))

(deftest Maybe
  (testing "Setoid"
    (testing "reflexivity"
      (is (laws/setoid-reflexivity R/nothing))
      (is (laws/setoid-reflexivity (R/just 1))))
    (testing "symmetry"
      (is (laws/setoid-symmetry R/nothing R/nothing))
      (is (laws/setoid-symmetry (R/just 1) (R/just 1))))
    (testing "transitivity"
      (is (laws/setoid-transitivity R/nothing R/nothing R/nothing))
      (is (laws/setoid-transitivity (R/just 1) (R/just 1) (R/just 1)))))

  (testing "Functor"
    (testing "identity"
      (is (laws/functor-identity R/nothing))
      (is (laws/functor-identity (R/just 1))))
    (testing "composition"
      (is (laws/functor-composition R/nothing #(+ % 1) #(+ % 100)))
      (is (laws/functor-composition (R/just 1) #(+ % 1) #(+ % 100)))))

  (testing "Apply"
    (testing "composition"
      (is (laws/apply-composition R/nothing R/nothing R/nothing))
      (is (laws/apply-composition (R/just 1) (R/just #(+ % 1)) (R/just #(+ % 2))))))

  (testing "Applicative"
    (testing "identity"
      (is (laws/applicative-identity R/nothing #(R/of R/Maybe %)))
      (is (laws/applicative-identity (R/just 1) #(R/of R/Maybe %))))
    (testing "homomorphism"
      (is (laws/applicative-homomorphism R/nothing #(+ % 1) #(R/of R/Maybe %)))
      (is (laws/applicative-homomorphism (R/just 1) #(+ % 1) #(R/of R/Maybe %))))
    (testing "interchange"
      (is (laws/applicative-interchange 2 R/nothing #(R/of R/Maybe %)))
      (is (laws/applicative-interchange 2 (R/just #(+ % 1)) #(R/of R/Maybe %)))))

  (testing "Chain"
    (testing "associativity"
      (is (laws/chain-associativity R/nothing (fn [] R/nothing) (fn [] R/nothing)))
      (is (laws/chain-associativity (R/just 1) #(R/just (+ % 1)) #(R/just (+ % 2))))))

  (testing "Foldable"
    (is (laws/foldable R/nothing + 0))
    (is (laws/foldable (R/just [1 2 3 4]) + 0)))

  (testing "ChainRec"
    (testing "equivalence"
      (let [predicate #(> (count %) 5)
            initial [1]
            done #(R/of R/Maybe %)
            next #(R/of R/Maybe (concat % [1]))]
        (is (laws/chain-rec-equivalence #(R/chain-rec R/Maybe %1 %2) predicate done next initial))))

    (testing "stacksafe"
      (is (laws/chain-rec-stacksafe #(R/chain-rec R/Maybe %1 %2) #(R/of R/Maybe %))))

    (testing "responds to failure immediately"
      (is (= (R/chain-rec R/Maybe (fn [] R/nothing) 100)
             R/nothing)))

    (testing "responds to failure on next step"
      (is (= (R/chain-rec
              R/Maybe
              (fn [next done n]
                (if (= n 0) R/nothing (R/just (next (- n 1)))))
              100)
             R/nothing))))

  (testing "Monad"))
