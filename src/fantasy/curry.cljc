(ns fantasy.curry
  (:require [fantasy.utils :as u]))

(def __ "@@ramda/placeholder")

(defn- arglist-count [f]
  #?(:clj (-> f class .getDeclaredMethods count)
     :cljs (-> (.keys js/Object f)
               (.filter (fn [name] (re-find #"cljs\$core\$IFn\$_invoke\$arity\$" name)))
               count
               (#(if (= % 0) 1 %)))))

(defn- arity [f]
  #?(:clj (-> f class .getDeclaredMethods first .getParameterTypes alength)
     :cljs (.-length f)))

;; https://github.com/ramda/ramda/blob/v0.23.0/src/internal/_curry1.js
(defn- curry-1 [f]
  (fn curry-1-fn [& [a :as args]]
    (if (or (= (count args) 0)
            (= a __))
      curry-1-fn
      (f a))))

;; https://github.com/ramda/ramda/blob/v0.23.0/src/internal/_curry2.js
(defn- curry-2 [f]
  (fn curry-2-fn [& [a b :as args]]
    (condp = (count args)
      0 curry-2-fn
      1 (if (= a __)
          curry-2-fn
          (curry-1 (fn [_b] (f a _b))))
      (cond
        (= a b __) curry-2-fn
        (= a __) (curry-1 (fn [_a] (f _a b)))
        (= b __) (curry-1 (fn [_b] (f a _b)))
        :else (f a b)))))

;; https://github.com/ramda/ramda/blob/v0.23.0/src/internal/_curry3.js
(defn- curry-3 [f]
  (fn curry-3-fn [& [a b c :as args]]
    (condp = (count args)
      0 curry-3-fn
      1 (if (= a __)
          curry-3-fn
          (curry-2 (fn [_b _c] (f a _b _c))))
      2 (cond
          (= a b __) curry-3-fn
          (= a __) (curry-2 (fn [_a _c] (f _a b _c)))
          (= b __) (curry-2 (fn [_b _c] (f a _b _c)))
          :else (curry-1 (fn [_c] (f a b _c))))
      (cond
        (= a b c __) curry-3-fn
        (= a b __) (curry-2 (fn [_a _b] (f _a _b c)))
        (= a c __) (curry-2 (fn [_a _c] (f _a b _c)))
        (= b c __) (curry-2 (fn [_b _c] (f a _b _c)))
        (= a __) (curry-1 (fn [_a] (f _a b c)))
        (= b __) (curry-1 (fn [_b] (f a _b c)))
        (= c __) (curry-1 (fn [_c] (f a b _c)))
        :else (f a b c)))))

;; https://github.com/ramda/ramda/blob/v0.23.0/src/internal/_curryN.js
(defn- curry-n [arity received f]
  (fn [& args]
    (let [[left combined] (loop [combined []
                                 args-idx 0
                                 left arity
                                 combined-idx 0]
                            (if (or (< combined-idx (count received))
                                    (< args-idx (count args)))
                              (let [combin-from (if (and (< combined-idx (count received))
                                                         (or (not= (get received combined-idx) __)
                                                             (>= args-idx (count args))))
                                                  :received
                                                  :args)
                                    result (if (= combin-from :received)
                                             (get received combined-idx)
                                             (nth args args-idx))
                                    next-combined (assoc combined combined-idx result)
                                    next-args-idx (if (= combin-from :args) (+ args-idx 1) args-idx)
                                    next-left (if (not= result __) (- left 1) left)
                                    next-combined-idx (+ combined-idx 1)]
                                (recur next-combined
                                       next-args-idx
                                       next-left
                                       next-combined-idx))
                              [left combined]))]
      (if (<= left 0)
        (apply f combined)
        (curry-n arity combined f)))))

(defn curry
  ([f]
   (condp = (arglist-count f)
     1 (curry (arity f) f)
     (u/throw-error (str "curry called with multiple arglist function, use (curry arity f) instead"))))
  ([arity f]
   (condp = arity
     0 f
     1 (curry-1 f)
     2 (curry-2 f)
     3 (curry-3 f)
     (curry-n arity [] f))))

(defmacro defcurry [name & body]
  `(def ~name (curry (fn ~@body))))
