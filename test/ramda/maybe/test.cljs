(ns ramda.maybe.test
  (:require [ramda.core :as R]
            [clojure.test :refer [deftest testing are is]]))

(deftest Maybe
  (testing "helper methods"
    (are [x y] (= x y)
      (R/just 1) (R/Just. 1)
      (R/from-maybe (R/just 1) 2) 1
      (R/from-maybe R/nothing 2) 2))

  (testing "just?"
    (is (R/just? (R/just 1)))
    (is (not (R/just? R/nothing))))

  (testing "nothing?"
    (is (not (R/nothing? (R/just 1))))
    (is (R/nothing? R/nothing)))

  (testing "of"
    (is (= (R/of R/Maybe 1) (R/just 1))))

  (testing "zero"
    (is (= (R/zero R/Maybe) R/nothing)))

  (testing "empty"
    (is (= (R/empty R/Maybe) R/nothing)))

  (testing "equals"
    (is (R/equals R/nothing R/nothing))
    (is (R/equals (R/just 1) (R/just 1)))
    (is (not (R/equals R/nothing (R/just 1))))
    (is (not (R/equals (R/just 1) (R/just 2)))))

  (testing "map"
    (are [x y] (= x y)
      (R/map inc R/nothing) R/nothing
      (R/map inc (R/just 2)) (R/just 3)))

  (testing "ap"
    (are [x y] (= x y)
      (R/ap R/nothing R/nothing) R/nothing
      (R/ap (R/just inc) R/nothing) R/nothing
      (R/ap R/nothing (R/just 2)) R/nothing
      (R/ap (R/just inc) (R/just 2)) (R/just 3)))

  (testing "extend"
    (are [x y] (= x y)
      (R/extend #(+ 1 (.-value %)) R/nothing) R/nothing
      (R/extend #(+ 1 (.-value %)) (R/just 2)) (R/just 3)))

  (testing "chain"
    (are [x y] (= x y)
      (R/chain #(R/just (inc %)) R/nothing) R/nothing
      (R/chain (fn [] R/nothing) (R/right 2)) R/nothing
      (R/chain #(R/just (inc %)) (R/just 2)) (R/just 3)))

  (testing "traverse"
    (are [x y] (= x y)
      (R/traverse R/Maybe #(R/right %) R/nothing) (R/just R/nothing)
      (R/traverse R/Maybe #(R/right %) (R/just 1)) (R/right (R/just 1))))

  (testing "reduce"
    (are [x y] (= x y)
      (R/reduce #(- %1 %2) 42 R/nothing) 42
      (R/reduce #(- %1 %2) 42 (R/just 1)) 41))

  (testing "alt"
    (are [x y] (= x y)
      (R/alt R/nothing R/nothing) R/nothing
      (R/alt (R/just 1) R/nothing) (R/just 1)
      (R/alt R/nothing (R/just 1)) (R/just 1)
      (R/alt (R/just 2) (R/just 1)) (R/just 1)))

  (testing "concat"
    (are [x y z] (= (R/concat x y) z)
      R/nothing R/nothing R/nothing
      (R/just "abc") R/nothing (R/just "abc")
      R/nothing (R/just "abc") (R/just "abc")
      (R/just "def") (R/just "abc") (R/just "abcdef")))

  (testing "extract"
    (are [x y] (= (R/extract x) y)
      R/nothing nil
      (R/just 1) 1)))
