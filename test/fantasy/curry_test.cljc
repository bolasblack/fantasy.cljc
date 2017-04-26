(ns fantasy.curry-test
  (:require [fantasy.curry :as f]
            [fantasy.utils :as u :include-macros true]
            [clojure.test :refer [deftest is]]))

(defn test-curry-fn [curry-fn]
  (let [f (curry-fn (fn [a b c] [a b c]))
        _ f/__]
    (is (= [1 2 3] ((f) 1 2 3)))
    (is (= [1 2 3] ((f 1) 2 3)))
    (is (= [1 2 3] ((f 1 2) 3)))
    (is (= [1 2 3] (f 1 2 3)))

    (is (= [1 2 3] ((f _ 2 3) 1)))
    (is (= [1 2 3] ((f 1 _ 3) 2)))
    (is (= [1 2 3] ((f 1 2 _) 3)))

    (is (= [1 2 3] (((f 1 _ _) 2) 3)))
    (is (= [1 2 3] (((f _ 2 _) 1) 3)))
    (is (= [1 2 3] (((f _ _ 3) 1) 2)))

    (is (= [1 2 3] ((f 1 _ _) 2 3)))
    (is (= [1 2 3] ((f _ 2 _) 1 3)))
    (is (= [1 2 3] ((f _ _ 3) 1 2)))

    (is (= [1 2 3] (((f 1 _ _) _ 3) 2)))
    (is (= [1 2 3] (((f _ 2 _) _ 3) 1)))
    (is (= [1 2 3] (((f _ _ 3) _ 2) 1)))

    (is (= [1 2 3] (((f _ _ _) _ _) 1 2 3)))
    (is (= [1 2 3] ((((((f _ _ _) 1 _ _) _ _) 2 _) _) 3)))))

(deftest curry
  (test-curry-fn f/curry)
  (is (= [4 5 6] ((f/curry (fn [a] [4 5 6])) 1)))
  (u/if-cljs
   (is (thrown-with-msg? js/Error
                         (js/RegExp. "curry called with multiple arglist function [^]+, use \\(curry arity f\\) instead")
                         (f/curry (fn ([a] [a]) ([a b] [a b])))))
   (is (thrown-with-msg? RuntimeException
                         #"curry called with multiple arglist function .+, use \(curry arity f\) instead"
                         (f/curry (fn ([a] [a]) ([a b] [a b])))))))

(deftest curry-n
  #?(:cljs (test-curry-fn (fn [f] (f/curry-n (f/arity f) [] f)))
     ;; the way access private var learn from
     ;;   http://dev.clojure.org/display/community/Library+Coding+Standards
     :clj (test-curry-fn (fn [f] (@#'f/curry-n (@#'f/arity f) [] f)))))
