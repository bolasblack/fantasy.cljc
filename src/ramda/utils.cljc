(ns ramda.utils)

(defmacro pull-to-ns [ns-map]
  `(do ~@(for [[ns vlist] (into [] ns-map)
               v vlist]
           `(def ~v ~(symbol (str ns "/" v))))))
