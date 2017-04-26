(ns fantasy.funcs
  (:require [fantasy.utils :as u]))

(defmulti to-maybe (fn [a] (type a)))

(defn n-ary [n f]
  (fn [& args] (apply f (take n args))))
