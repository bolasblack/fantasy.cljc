(ns fantasy.maybe.test
  (:require [fantasy.core :as f]
            [clojure.test :refer [deftest testing are is]])
  (:import #?(:clj [fantasy.maybe Just Nothing])))

(deftest Maybe
  (testing "helper methods"
    (are [x y] (= x y)
      #?@(:cljs [(f/just 1) (f/Just. 1)
                 f/nothing (f/Nothing.)]
          :clj [(f/just 1) (Just. 1)
                f/nothing (Nothing.)])
      (f/just 1) (f/just 1)
      f/nothing f/nothing
      (f/from-maybe (f/just 1) 2) 1
      (f/from-maybe f/nothing 2) 2))

  (testing "just?"
    (is (f/just? (f/just 1)))
    (is (not (f/just? f/nothing))))

  (testing "nothing?"
    (is (not (f/nothing? (f/just 1))))
    (is (f/nothing? f/nothing)))

  (testing "of"
    (is (= (f/of f/Maybe 1) (f/just 1))))

  (testing "zero"
    (is (= (f/zero f/Maybe) f/nothing)))

  (testing "empty"
    (is (= (f/empty f/Maybe) f/nothing)))

  (testing "equals"
    (is (f/equals f/nothing f/nothing))
    (is (f/equals (f/just 1) (f/just 1)))
    (is (not (f/equals f/nothing (f/just 1))))
    (is (not (f/equals (f/just 1) (f/just 2)))))

  (testing "map"
    (are [x y] (= x y)
      (f/map inc f/nothing) f/nothing
      (f/map inc (f/just 2)) (f/just 3)))

  (testing "ap"
    (are [x y] (= x y)
      (f/ap f/nothing f/nothing) f/nothing
      (f/ap (f/just inc) f/nothing) f/nothing
      (f/ap f/nothing (f/just 2)) f/nothing
      (f/ap (f/just inc) (f/just 2)) (f/just 3)))

  (testing "extend"
    (are [x y] (= x y)
      (f/extend #(+ 1 (.-value %)) f/nothing) f/nothing
      (f/extend #(+ 1 (.-value %)) (f/just 2)) (f/just 3)))

  (testing "chain"
    (are [x y] (= x y)
      (f/chain #(f/just (inc %)) f/nothing) f/nothing
      (f/chain (fn [a] f/nothing) (f/right 2)) f/nothing
      (f/chain #(f/just (inc %)) (f/just 2)) (f/just 3)))

  (testing "traverse"
    (are [x y] (= x y)
      (f/traverse f/Maybe #(f/right %) f/nothing) (f/just f/nothing)
      (f/traverse f/Maybe #(f/right %) (f/just 1)) (f/right (f/just 1))))

  (testing "reduce"
    (are [x y] (= x y)
      (f/reduce #(- %1 %2) 42 f/nothing) 42
      (f/reduce #(- %1 %2) 42 (f/just 1)) 41))

  (testing "alt"
    (are [x y] (= x y)
      (f/alt f/nothing f/nothing) f/nothing
      (f/alt (f/just 1) f/nothing) (f/just 1)
      (f/alt f/nothing (f/just 1)) (f/just 1)
      (f/alt (f/just 2) (f/just 1)) (f/just 1)))

  (testing "concat"
    (are [x y z] (= (f/concat x y) z)
      f/nothing f/nothing f/nothing
      (f/just "abc") f/nothing (f/just "abc")
      f/nothing (f/just "abc") (f/just "abc")
      (f/just "def") (f/just "abc") (f/just "abcdef")))

  (testing "extract"
    (are [x y] (= (f/extract x) y)
      f/nothing nil
      (f/just 1) 1)))
