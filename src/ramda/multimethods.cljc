(ns ramda.multimethods)

(defmulti from (fn [a] (type a)))
