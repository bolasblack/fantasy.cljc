(ns fantasy.standard-func-test
  (:refer-clojure :exclude [map reduce concat empty extend])
  (:require [fantasy.core :as f]
            [fantasy.utils :as u :include-macros true]
            [clojure.test :refer [deftest is]]))

(deftest map
  (is (= (f/map #(+ 1 %) #(+ 2 %) #(+ 3 %) (f/right 1))
         (f/map #(+ 1 %) (f/map #(+ 2 %) (f/map #(+ 3 %) (f/right 1))))
         (f/right 7))))

(deftest ap
  (is (= (f/ap (f/right #(+ 1 %)) (f/right #(+ 2 %)) (f/right 1))
         (f/ap (f/right #(+ 1 %)) (f/ap (f/right #(+ 2 %)) (f/right 1)))
         (f/right 4))))

(deftest reduce)

(deftest concat
  (is (= (f/concat (f/right [1]) (f/right [2 3]) (f/right [4 5 6]))
         (f/concat (f/right [1]) (f/concat (f/right [2 3]) (f/right [4 5 6])))
         (f/right [4 5 6 2 3 1]))))

(deftest equals
  (is (= (f/equals (f/right 2) (f/right 1) (f/right 1))
         (if (not (f/equals (f/right 1) (f/right 1)))
           false
           (f/equals (f/right 1) (f/right 2)))
         false)))

(deftest alt
  (is (= (f/alt (f/right 1) (f/right 2) (f/left 3))
         (f/alt (f/right 1) (f/alt (f/right 2) (f/left 3)))
         (f/right 2))))

(deftest bimap)

(deftest extend
  (is (= (f/extend #(+ 1 (.-value %)) #(+ 2 (.-value %)) #(+ 3 (.-value %)) (f/right 1))
         (f/extend #(+ 1 (.-value %)) (f/extend #(+ 2 (.-value %)) (f/extend #(+ 3 (.-value %)) (f/right 1))))
         (f/right 7))))

(deftest promap)

(deftest traverse)

(deftest lte
  (is (= (f/lte 2 1 1)
         (if (not (f/lte 1 1))
           false
           (f/lte 2 1))
         false)))

(deftest chain
  (is (= (f/chain #(f/right (+ 1 %)) #(f/right (+ 2 %)) (f/right 1))
         (f/chain #(f/right (+ 1 %)) (f/chain #(f/right (+ 2 %)) (f/right 1)))
         (f/right 4))))

(deftest extract)
