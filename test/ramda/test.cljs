(ns ramda.test
  (:require [cljs.nodejs :as nodejs]
            [clojure.test :refer [run-all-tests run-tests]]
            [ramda.curry-test :as curry-test]
            [ramda.either.laws-test :as either-test]
            [ramda.maybe.laws-test :as maybe-test]
            [ramda.builtin-patch.laws-test :as builtin-patch-test]))

(nodejs/enable-util-print!)

(defn -main [& args]
  (try
    (run-all-tests #"ramda\..*\-test")
    (catch js/Error e
      (println (.-stack e)))))

(set! *main-cli-fn* -main)
