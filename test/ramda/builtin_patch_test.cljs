(ns ramda.builtin-patch-test
  (:require [ramda.core :as R]
            [ramda.laws :as laws]
            [clojure.test :refer [deftest testing are is]]))

(deftest patch-collection
  (testing "Setoid"
    (testing "reflexivity"
      (is (laws/setoid-reflexivity [1])))
    (testing "symmetry"
      (is (laws/setoid-symmetry [1] [1])))
    (testing "transitivity"
      (is (laws/setoid-transitivity [1] [1] [1]))))

  (testing "Semigroup"
    (testing "associativity"
      (is (laws/semigroup-associativity [1] [2] [3]))))

  (testing "Functor"
    (testing "identity"
      (is (laws/functor-identity [1])))
    (testing "composition"
      (is (laws/functor-composition [1] #(+ % 1) #(+ % 100)))))

  (testing "Apply"
    (testing "composition"
      (is (laws/apply-composition [1] [#(+ % 1)] [#(+ % 2)]))))

  (testing "Applicative"
    (testing "identity"
      (is (laws/applicative-identity [1] #(R/of (type []) %))))
    (testing "homomorphism"
      (is (laws/applicative-homomorphism [1] #(+ % 1) #(R/of (type []) %))))
    (testing "interchange"
      (is (laws/applicative-interchange 2 [#(+ % 1)] #(R/of (type []) %)))))

  (testing "Chain"
    (testing "associativity"
      (is (laws/chain-associativity [1] (fn [n] [(+ n 1)]) (fn [n] [(+ n 2)])))))

  (testing "Foldable"
    (is (laws/foldable [1 2 3 4] + 0)))

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

  (testing "Monad"
    (testing "left-identity"
      (is (laws/monad-left-identity [1] #(R/of (type []) (+ 1 %)) #(R/of (type []) %))))

    (testing "right-identity"
      (is (laws/monad-right-identity [1] #(R/of (type []) %)))))

  (testing "Monoid"
    (testing "left-identity"
      (is (laws/monoid-left-identity [1] (type []))))

    (testing "right-identity"
      (is (laws/monoid-right-identity [1] (type [])))))

  (testing "Alt"
    (testing "associativity"
      (is (laws/alt-associativity [1] [2] [3])))

    (testing "distributivity"
      (is (laws/alt-distributivity [1] [2] #(+ 1 %)))))

  (testing "Plus"
    (testing "left-identity"
      (is (laws/plus-left-identity [1] (type []))))

    (testing "right-identity"
      (is (laws/plus-right-identity [1] (type []))))

    (testing "annihilation"
      (is (laws/plus-annihilation (type []) #(+ 1 %)))))

  (testing "Extend"
    (testing "associativity"
      (is (laws/extend-associativity [2] (fn [n] [(+ 1 (first n))]) (fn [n] [(* (first n) (first n))]))))))
