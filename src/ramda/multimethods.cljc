(ns ramda.multimethods)

(defmulti of identity)

(defmulti chain-rec identity)

(defmulti from (fn [a] (type a)))
