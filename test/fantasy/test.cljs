(ns fantasy.test
  (:require [cljs.nodejs :as nodejs]
            [clojure.test :refer [run-all-tests run-tests]]
            [fantasy.curry-test :as curry-test]
            [fantasy.either.test :as either-test]
            [fantasy.either.laws-test :as either-laws-test]
            [fantasy.maybe.test :as maybe-test]
            [fantasy.maybe.laws-test :as maybe-laws-test]
            [fantasy.builtin-patch.test :as builtin-patch-test]
            [fantasy.builtin-patch.laws-test :as builtin-patch-laws-test]))

(nodejs/enable-util-print!)

(defn -main [& args]
  (try
    (run-all-tests #"fantasy\..*(\-|\.)test")
    (catch js/Error e
      (println (.-stack e)))))

(set! *main-cli-fn* -main)
