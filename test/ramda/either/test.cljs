(ns ramda.either.test
  (:require [ramda.core :as R]
            [clojure.test :refer [deftest testing are is]]))

(deftest Either
  (testing "helper methods"
    (are [x y] (= x y)
      (R/left 1) (R/Left. 1)
      (R/right 1) (R/Right. 1)))

  (testing "left?"
    (is (R/left? (R/left 1)))
    (is (not (R/left? (R/right 1)))))

  (testing "right?"
    (is (not (R/right? (R/left 1))))
    (is (R/right? (R/right 1))))

  (testing "of"
    (is (= (R/of R/Either 1) (R/right 1))))

  (testing "equals"
    (is (R/equals (R/left 1) (R/left 1)))
    (is (not (R/equals (R/left 1) (R/right 1))))
    (is (not (R/equals (R/left 1) (R/left 2))))
    (is (R/equals (R/right 1) (R/right 1)))
    (is (not (R/equals (R/right 1) (R/left 1))))
    (is (not (R/equals (R/right 1) (R/right 2)))))

  (testing "map"
    (are [x y] (= x y)
      (R/map inc (R/left 2)) (R/left 2)
      (R/map inc (R/right 2)) (R/right 3)))

  (testing "bimap"
    (are [x y] (= x y)
      (R/bimap inc dec (R/left 2)) (R/left 3)
      (R/bimap inc dec (R/right 2)) (R/right 1)))

  (testing "ap"
    (are [x y] (= x y)
      (R/ap (R/left 1) (R/left 2)) (R/left 1)
      (R/ap (R/left 1) (R/right 2)) (R/left 1)

      (R/ap (R/right inc) (R/left 2)) (R/left 2)
      (R/ap (R/right inc) (R/right 2)) (R/right 3)))

  (testing "extend"
    (are [x y] (= x y)
      (R/extend #(+ 1 (.-value %)) (R/left 2)) (R/left 2)
      (R/extend #(+ 1 (.-value %)) (R/right 2)) (R/right 3)))

  (testing "chain"
    (are [x y] (= x y)
      (R/chain #(R/right (inc %)) (R/left 2)) (R/left 2)
      (R/chain #(R/left (inc %)) (R/right 2)) (R/left 3)
      (R/chain #(R/right (inc %)) (R/right 2)) (R/right 3)))

  (testing "reduce"
    (are [x y] (= x y)
      (R/reduce #(- %1 %2) 42 (R/left 1)) 42
      (R/reduce #(- %1 %2) 42 (R/right 1)) 41))

  (testing "traverse"
    (are [x y] (= x y)
      (R/traverse R/Either #(R/just %) (R/left 1)) (R/right (R/left 1))
      (R/traverse R/Either #(R/just %) (R/right 1)) (R/just (R/right 1))))

  (testing "alt"
    (are [x y] (= x y)
      (R/alt (R/left 1) (R/left 2)) (R/left 1)
      (R/alt (R/left 1) (R/right 2)) (R/right 2)
      (R/alt (R/right 1) (R/left 2)) (R/right 1)
      (R/alt (R/right 1) (R/right 2)) (R/right 2)))

  (testing "concat"
    (are [x y z] (= (R/concat x y) z)
      (R/left "def") (R/left "abc") (R/left "abcdef")
      (R/right "def") (R/left "abc") (R/right "def")
      (R/right "def") (R/right "abc") (R/right "abcdef")
      (R/left "def") (R/right "abc") (R/right "abc")))

  (testing "extract"
    (are [x y] (= (R/extract x) y)
      (R/left 1) 1
      (R/right 1) 1)))
