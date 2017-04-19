(ns fantasy.multimethods)

(defmulti to-maybe (fn [a] (type a)))
