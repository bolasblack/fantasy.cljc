(ns fantasy.builtin-patch.test
  (:refer-clojure :exclude [map reduce concat empty])
  (:require [fantasy.core :as f]
            [clojure.test :refer [deftest testing are is]]))

(defn duplicate [type-rep]
  (fn [x] (f/concat (f/of type-rep x) (f/of type-rep x))))

(deftest equals)

(deftest concat
  (are [x y z] (= (f/concat x y) z)
    "" "" ""
    "" "abc" "abc"
    "abc" "" "abc"
    "def" "abc" "abcdef"

    [] [] []
    [1 2 3] [] [1 2 3]
    [] [1 2 3] [1 2 3]
    [4 5 6] [1 2 3] [1 2 3 4 5 6]

    '() '() '()
    '(1 2 3) '() '(1 2 3)
    '() '(1 2 3) '(1 2 3)
    '(4 5 6) '(1 2 3) '(1 2 3 4 5 6)

    #{} #{} #{}
    #{1 2 3} #{} #{1 2 3}
    #{} #{1 2 3} #{1 2 3}
    #{4 5 6} #{1 2 3} #{1 2 3 4 5 6}

    {} {} {}
    {} {:x 1 :y 2} {:x 1 :y 2}
    {:x 1 :y 2} {} {:x 1 :y 2}
    {:y 3 :z 4} {:x 1 :y 2} {:x 1 :y 3 :z 4}))

(deftest empty
  (are [x y] (= (f/empty (type x)) y)
    [1] []
    '(1) '()
    #{1} #{}
    {:a 1} {}
    "1" ""))

(deftest map
  (are [x y] (= (f/map inc x) y)
    [] []
    [1 2 3] [2 3 4]

    '() '()
    '(1 2 3) '(2 3 4)

    #{} #{}
    #{1 2 3} #{2 3 4}

    {} {}
    {:x 1 :y 2} {:x 2 :y 3}))

(deftest ap
  (are [x y z] (= (f/ap x y) z)
    [] [] []
    [] [1 2 3] []
    [inc] [] []
    [inc] [1 2 3] [2 3 4]

    '() '() '()
    '() '(1 2 3) '()
    (list inc) '() '()
    (list inc) '(1 2 3) '(2 3 4)

    #{} #{} #{}
    #{} #{1 2 3} #{}
    #{inc} #{} #{}
    #{inc} #{1 2 3} #{2 3 4}

    {} {} {}
    {} {:x 1 :y 2} {:x 1 :y 2}
    {:x inc} {} {}
    {:x inc} {:x 1} {:x 2}
    {:x inc :y dec} {:x 1 :y 2} {:x 2 :y 1}
    {:x inc :y dec :z mod} {:w 4 :x 1 :y 2} {:w 4 :x 2 :y 1}))

(deftest of
  (are [x y z] (= (f/of (type x) y) z)
    [] 1 [1]
    '() 1 '(1)
    #{} 1 #{1}))

(deftest chain
  (are [x y z] (= (f/chain x y) z)
    (duplicate (type [])) [] []
    (duplicate (type [])) [1 2 3] [1 1 2 2 3 3]

    (duplicate (type '())) '() '()
    (duplicate (type '())) '(1 2 3) '(1 1 2 2 3 3)

    (fn [x] #{x (inc x)}) #{} #{}
    (fn [x] #{x (inc x)}) #{1 3 5} #{1 2 3 4 5 6}))

(deftest chain-rec)

(deftest alt
  (are [x y z] (= (f/alt x y) z)
    [] [] []
    [1 2 3] [] [1 2 3]
    [] [1 2 3] [1 2 3]
    [4 5 6] [1 2 3] [1 2 3 4 5 6]

    '() '() '()
    '(1 2 3) '() '(1 2 3)
    '() '(1 2 3) '(1 2 3)
    '(4 5 6) '(1 2 3) '(1 2 3 4 5 6)

    #{} #{} #{}
    #{1 2 3} #{} #{1 2 3}
    #{} #{1 2 3} #{1 2 3}
    #{4 5 6} #{1 2 3} #{1 2 3 4 5 6}

    {} {} {}
    {} {:x 1 :y 2} {:x 1 :y 2}
    {:x 1 :y 2} {} {:x 1 :y 2}
    {:y 3 :z 4} {:x 1 :y 2} {:x 1 :y 3 :z 4}))

(deftest zero
  (are [x y] (= (f/zero (type x) x) y)
    [1] []
    '(1) '()
    #{1} #{}
    {:a 1} {}
    "1" ""
    3 0))

(deftest reduce
  (are [w x y z] (= (f/reduce w x y) z)
    f/concat "x" [] "x"
    f/concat "x" ["a" "b" "c"] "cbax"

    f/concat "x" '() "x"
    f/concat "x" '("a" "b" "c") "cbax"

    f/concat "x" #{} "x"
    f/concat "x" #{"a" "b" "c"} "cbax"

    f/concat "x" {} "x"
    (fn [memo [key value]] (f/concat value memo)) "x" {:x "a" :y "b" :z "c"} "xabc"))

(deftest extend
  (are [x y z] (= (f/extend x y) z)
    count [] [0]
    count [1 2 3] [3]

    count '() '(0)
    count '(1 2 3) '(3)

    count #{} #{0}
    count #{1 2 3} #{3}))
