(ns ramda.curry-test
  (:require [cljs.nodejs :as nodejs]
            [ramda.curry :as R]
            [clojure.test :refer [deftest are is run-tests]]))

(defn test-curry-fn [curry-fn]
  (let [f (curry-fn (fn [a b c] [a b c]))
        _ R/__]
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
  (test-curry-fn R/curry)
  (is (= [4 5 6] ((R/curry (fn [] [4 5 6])) 1)))
  (is (thrown-with-msg? js/Error #"curry called with multiple arglist function, use \(curry arity f\) instead"
                        (R/curry (fn ([a] [a]) ([a b] [a b]))))))

(deftest curry-n
  (test-curry-fn (fn [f] (R/curry-n (R/arity f) [] f))))

(nodejs/enable-util-print!)

(defn -main [& args]
  (try
    (run-tests)
    (catch js/Error e
      (println (.-stack e)))))

(set! *main-cli-fn* -main)
