(ns fantasy.utils)

(defmacro pull-to-ns [ns-map]
  `(do ~@(for [[ns vlist] (into [] ns-map)
               v vlist]
           `(def ~v ~(symbol (str ns "/" v))))))

(defmacro extend-types [types & body]
  `(do ~@(for [type types]
           `(extend-type ~type ~@body))))

(defmacro defpr [types & body]
  `(do ~@(for [type types]
           #?(:cljs `(extend-type ~type
                       ~'Object
                       (~'toString [this#]
                         ((fn ~@body) this#))
                       IPrintWithWriter
                       (-pr-writer [this# writer# opts#]
                         (write-all writer# ((fn ~@body) this#))))
              :clj `(defmethod clojure.core/print-method ~type [this# writer#]
                      (.write writer# ((fn ~@body) this#)))))))

(defn throw-error [msg]
  #?(:cljs (throw (js/Error msg))
     :clj (throw (Error. msg))))

(defn equals [a b]
  (and
   (= (type a) (type b))
   (= (.-value a) (.-value b))))

(defn chain-rec-next [v]
  {:next? true, :value v})

(defn chain-rec-done [v]
  {:next? false, :value v})

(defn invoke? [a]
  (or (fn? a)
      (ifn? a)))
