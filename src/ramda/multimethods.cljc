(ns ramda.multimethods)

(defmulti to-maybe (fn [a] (type a)))
