(ns fantasy.either.test
  (:require [fantasy.core :as f]
            [clojure.test :refer [deftest testing are is]])
  (:import #?(:clj [fantasy.either Left Right])))

(deftest Either
  (testing "helper methods"
    (are [x y] (= x y)
      #?@(:cljs [(f/left 1) (f/Left. 1)
                 (f/right 1) (f/Right. 1)]
          :clj [(f/left 1) (Left. 1)
                (f/right 1) (Right. 1)])))

  (testing "left?"
    (is (f/left? (f/left 1)))
    (is (not (f/left? (f/right 1)))))

  (testing "right?"
    (is (not (f/right? (f/left 1))))
    (is (f/right? (f/right 1))))

  (testing "of"
    (is (= (f/of f/Either 1) (f/right 1))))

  (testing "equals"
    (is (f/equals (f/left 1) (f/left 1)))
    (is (not (f/equals (f/left 1) (f/left 2))))
    (is (f/equals (f/right 1) (f/right 1)))
    (is (not (f/equals (f/right 1) (f/right 2))))
    (is (not (f/equals (f/left 1) (f/right 1)))))

  (testing "map"
    (are [x y] (= x y)
      (f/map inc (f/left 2)) (f/left 2)
      (f/map inc (f/right 2)) (f/right 3)))

  (testing "bimap"
    (are [x y] (= x y)
      (f/bimap inc dec (f/left 2)) (f/left 3)
      (f/bimap inc dec (f/right 2)) (f/right 1)))

  (testing "ap"
    (are [x y] (= x y)
      (f/ap (f/left 1) (f/left 2)) (f/left 1)
      (f/ap (f/left 1) (f/right 2)) (f/left 1)

      (f/ap (f/right inc) (f/left 2)) (f/left 2)
      (f/ap (f/right inc) (f/right 2)) (f/right 3)))

  (testing "extend"
    (are [x y] (= x y)
      (f/extend #(+ 1 (.-value %)) (f/left 2)) (f/left 2)
      (f/extend #(+ 1 (.-value %)) (f/right 2)) (f/right 3)))

  (testing "chain"
    (are [x y] (= x y)
      (f/chain #(f/right (inc %)) (f/left 2)) (f/left 2)
      (f/chain #(f/left (inc %)) (f/right 2)) (f/left 3)
      (f/chain #(f/right (inc %)) (f/right 2)) (f/right 3)))

  (testing "reduce"
    (are [x y] (= x y)
      (f/reduce #(- %1 %2) 42 (f/left 1)) 42
      (f/reduce #(- %1 %2) 42 (f/right 1)) 41))

  (testing "traverse"
    (are [x y] (= x y)
      (f/traverse f/Either #(f/just %) (f/left 1)) (f/right (f/left 1))
      (f/traverse f/Either #(f/just %) (f/right 1)) (f/just (f/right 1))))

  (testing "alt"
    (are [x y] (= x y)
      (f/alt (f/left 1) (f/left 2)) (f/left 1)
      (f/alt (f/left 1) (f/right 2)) (f/right 2)
      (f/alt (f/right 1) (f/left 2)) (f/right 1)
      (f/alt (f/right 1) (f/right 2)) (f/right 2)))

  (testing "concat"
    (are [x y z] (= (f/concat x y) z)
      (f/left "def") (f/left "abc") (f/left "abcdef")
      (f/right "def") (f/left "abc") (f/right "def")
      (f/right "def") (f/right "abc") (f/right "abcdef")
      (f/left "def") (f/right "abc") (f/right "abc")))

  (testing "extract"
    (are [x y] (= (f/extract x) y)
      (f/left 1) 1
      (f/right 1) 1)))
