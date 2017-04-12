(ns ramda.multimethods)

(defmulti from (fn [a] (type a)))

(defmulti to-maybe (fn [a] (type a)))
