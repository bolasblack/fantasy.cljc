(ns fantasy.compose
  (:require [fantasy.protocols :as p]
            [fantasy.standard-func :as sf]
            [fantasy.utils :as u :include-macros true]))

(deftype Compose [value]
  #?@(:cljs [IEquiv (-equiv [this that] (u/equals this that))]
      :clj [Object (equals [this that] (u/equals this that))])

  p/Applicative

  p/Apply
  (p/ap [this f]
    (let [new-value (p/ap (.-value this)
                          (p/map (.-value f)
                                 (fn [u] (fn [y] (p/ap y u)))))]
      (new Compose new-value)))

  p/Functor
  (p/map [this f]
    (let [new-value (p/map (.-value this)
                           (fn [y] (p/map y f)))]
      (new Compose new-value)))

  p/Printable
  (p/-repr [this]
    (str "(Compose. " (.-value this) ")")))

(u/make-printable Compose)

(defmethod sf/of Compose [a value]
  (new Compose (sf/of (:type-rep-f a) (sf/of (:type-rep-g a) value))))

(defn gen-compose-type [type-rep-f type-rep-g]
  {:fl-of? true :type Compose
   :type-rep-f type-rep-f, :type-rep-g type-rep-g})
