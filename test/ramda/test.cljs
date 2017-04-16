(ns ramda.test
  (:require [cljs.nodejs :as nodejs]
            [clojure.test :refer [run-all-tests run-tests]]
            [ramda.curry-test :as curry-test]
            [ramda.either.test :as either-test]
            [ramda.either.laws-test :as either-laws-test]
            [ramda.maybe.test :as maybe-test]
            [ramda.maybe.laws-test :as maybe-laws-test]
            [ramda.builtin-patch.test :as builtin-patch-test]
            [ramda.builtin-patch.laws-test :as builtin-patch-laws-test]))

(nodejs/enable-util-print!)

(defn -main [& args]
  (try
    (run-all-tests #"ramda\..*(\-|\.)test")
    (catch js/Error e
      (println (.-stack e)))))

(set! *main-cli-fn* -main)
