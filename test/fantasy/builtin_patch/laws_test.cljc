(ns fantasy.builtin-patch.laws-test
  (:require [fantasy.core :as f]
            [fantasy.laws :as laws]
            [clojure.test :refer [deftest testing is]]))

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
      (is (laws/applicative-identity [1] #(f/of (type []) %))))
    (testing "homomorphism"
      (is (laws/applicative-homomorphism 1 #(+ % 1) #(f/of (type []) %))))
    (testing "interchange"
      (is (laws/applicative-interchange 2 [#(+ % 1)] #(f/of (type []) %)))))

  (testing "Chain"
    (testing "associativity"
      (is (laws/chain-associativity [1] (fn [n] [(+ n 1)]) (fn [n] [(+ n 2)])))))

  (testing "Foldable"
    (is (laws/foldable [1 2 3 4] + 0)))

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
      (is (= (f/chain-rec f/Maybe (fn [next done n] f/nothing) 100)
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
      (is (laws/monad-left-identity 1 #(f/of (type []) (+ 1 %)) #(f/of (type []) %))))

    (testing "right-identity"
      (is (laws/monad-right-identity [1] #(f/of (type []) %)))))

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
      (is (laws/extend-associativity [2]
                                     (fn [n] (+ 1 (first n)))
                                     (fn [n] (* (first n) (first n))))))))


(deftest patch-hash
  (testing "Setoid"
    (testing "reflexivity"
      (is (laws/setoid-reflexivity {:a 1})))
    (testing "symmetry"
      (is (laws/setoid-symmetry {:a 1} {:a 1})))
    (testing "transitivity"
      (is (laws/setoid-transitivity {:a 1} {:a 1} {:a 1}))))

  (testing "Semigroup"
    (testing "associativity"
      (is (laws/semigroup-associativity {:a 1} {:a 2} {:a 3}))))

  (testing "Functor"
    (testing "identity"
      (is (laws/functor-identity {:a 1})))
    (testing "composition"
      (is (laws/functor-composition {:a 1} #(+ % 1) #(+ % 100)))))

  (testing "Apply"
    (testing "composition"
      (is (laws/apply-composition {:a 1 :b 2} {:a #(+ % 1) :b #(+ % 2)} {:a #(+ % 2) :b #(+ % 3)}))))

  (testing "Foldable"
    (is (laws/foldable {:a 1 :b 2 :c 3} (fn [memo [key value]] (assoc memo key (+ 1 value))) {})))

  (testing "Monoid"
    (testing "left-identity"
      (is (laws/monoid-left-identity {:a 1} (type {}))))

    (testing "right-identity"
      (is (laws/monoid-right-identity {:a 1} (type {})))))

  (testing "Plus"
    (testing "left-identity"
      (is (laws/plus-left-identity {:a 1} (type {}))))

    (testing "right-identity"
      (is (laws/plus-right-identity {:a 1} (type {}))))

    (testing "annihilation"
      (is (laws/plus-annihilation (type {}) #(+ 1 (last %)))))))
