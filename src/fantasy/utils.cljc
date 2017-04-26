(ns fantasy.utils)

(defmacro if-cljs
  "Return then if we are generating cljs code and else for Clojure code.
   https://github.com/plumatic/schema/blob/master/src/clj/schema/macros.clj#L10-L19
   https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
  [then else]
  (if (:ns &env) then else))

(defmacro pull-to-ns [ns-map]
  `(do ~@(for [[ns vlist] (into [] ns-map)
               v vlist]
           `(def ~v ~(symbol (str ns "/" v))))))

(defmacro extend-types [types & body]
  `(do ~@(for [type types]
           `(extend-type ~type ~@body))))

(defmacro defpr [types & body]
  `(if-cljs
    (do ~@(for [type types]
            `(extend-type ~type
               ~'Object
               (~'toString [this#]
                ((fn ~@body) this#))
               ~'IPrintWithWriter
               (~'-pr-writer [this# writer# opts#]
                (~'write-all writer# ((fn ~@body) this#))))))
    (do ~@(for [type types]
            `(defmethod clojure.core/print-method ~type [this# writer#]
               (.write writer# ((fn ~@body) this#)))))))

(defmacro throw-error [msg]
  `(if-cljs
    (throw (js/Error ~msg))
    (throw (RuntimeException. ~(with-meta msg `{:tag java.lang.String})))))

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
