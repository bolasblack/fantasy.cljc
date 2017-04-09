(ns ramda.either-test
  (:require [ramda.core :as R]
            [ramda.laws :as laws]
            [clojure.test :refer [deftest testing are is]]))

(deftest Either
  (testing "Setoid"
    (testing "reflexivity"
      (is (laws/setoid-reflexivity (R/left 1)))
      (is (laws/setoid-reflexivity (R/right 1))))
    (testing "symmetry"
      (is (laws/setoid-symmetry (R/left 1) (R/left 1)))
      (is (laws/setoid-symmetry (R/right 1) (R/right 1))))
    (testing "transitivity"
      (is (laws/setoid-transitivity (R/left 1) (R/left 1) (R/left 1)))
      (is (laws/setoid-transitivity (R/right 1) (R/right 1) (R/right 1)))))

  (testing "Functor"
    (testing "identity"
      (is (laws/functor-identity (R/left 1)))
      (is (laws/functor-identity (R/right 1))))
    (testing "composition"
      (is (laws/functor-composition (R/left 1) #(+ % 1) #(+ % 100)))
      (is (laws/functor-composition (R/right 1) #(+ % 1) #(+ % 100)))))

  (testing "Apply"
    (testing "composition"
      (is (laws/apply-composition (R/left 1) (R/left #(+ % 1)) (R/left #(+ % 2))))
      (is (laws/apply-composition (R/right 1) (R/right #(+ % 1)) (R/right #(+ % 2))))))

  (testing "Applicative"
    (testing "identity"
      (is (laws/applicative-identity (R/left 1) #(R/of R/Either %)))
      (is (laws/applicative-identity (R/right 1) #(R/of R/Either %))))
    (testing "homomorphism"
      (is (laws/applicative-homomorphism (R/left 1) #(+ % 1) #(R/of R/Either %)))
      (is (laws/applicative-homomorphism (R/right 1) #(+ % 1) #(R/of R/Either %))))
    (testing "interchange"
      (is (laws/applicative-interchange 2 (R/left #(+ % 1)) #(R/of R/Either %)))
      (is (laws/applicative-interchange 2 (R/right #(+ % 1)) #(R/of R/Either %)))))

  (testing "Chain"
    (testing "associativity"
      (is (laws/chain-associativity (R/left 1) #(R/left (+ % 1)) #(R/left (+ % 2))))
      (is (laws/chain-associativity (R/right 1) #(R/right (+ % 1)) #(R/right (+ % 2))))))

  (testing "ChainRec"
    (testing "equivalence"
      (let [predicate #(> (count %) 5)
            initial [1]
            done #(R/of R/Either %)
            next #(R/of R/Either (concat % [1]))]
        (is (laws/chain-rec-equivalence #(R/chain-rec R/Either %1 %2) predicate done next initial))))

    (testing "stacksafe"
      (is (laws/chain-rec-stacksafe #(R/chain-rec R/Either %1 %2) #(R/of R/Either %))))

    (testing "responds to failure immediately"
      (is (= (R/chain-rec R/Either #(R/left "ERROR") 100)
             (R/left "ERROR"))))

    (testing "responds to failure on next step"
      (is (= (R/chain-rec
              R/Either
              (fn [next done n]
                (if (= n 0) (R/left "ERROR") (R/right (next (- n 1)))))
              100)
             (R/left "ERROR")))))

  (testing "Monad")

  (testing "Extend"
    (testing "extend-associative"
      (is (laws/extend-associative
           (R/left 1)
           #(R/left (R/map (fn [a] (+ a 2)) %))
           #(R/left (R/map (fn [a] (+ a 1)) %))))
      (is (laws/extend-associative
           (R/right 1)
           #(R/right (R/map (fn [a] (+ a 2)) %))
           #(R/right (R/map (fn [a] (+ a 1)) %))))))

  (testing "Bifunctor"
    (testing "identity"
      (is (laws/bifunctor-identity (R/left 1)))
      (is (laws/bifunctor-identity (R/right 1))))
    (testing "composition"
      (is (laws/bifunctor-composition (R/left 1) #(+ % 1) #(+ % 10) #(+ % 100) #(+ % 1000)))
      (is (laws/bifunctor-composition (R/right 1) #(+ % 1) #(+ % 10) #(+ % 100) #(+ % 1000))))))
