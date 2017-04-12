(ns ramda.compose
  (:require [ramda.protocols :as p]
            [ramda.standard-func :as sf]
            [ramda.utils :as u :include-macros true]))

(defmacro defcompose [F G]
  `(do
     (deftype compose# [~'value]
       IEquiv
       (-equiv [this# other#]
         (u/equals this# other#))

       p/Applicative
       p/Apply
       (p/fl-ap [this# f#]
         (new compose# (p/fl-ap (.-value this#)
                                (p/fl-map (.-value f#)
                                          (fn [u#] (fn [y#] (p/fl-ap y# u#)))))))
       p/Functor
       (p/fl-map [this# f#]
         (new compose# (p/fl-map (.-value this#)
                                 (fn [y#] (p/fl-map y# f#))))))

     (u/defpr [compose#] [this#]
       (str "((Compose. " ~(name F) " " ~(name G) ") " (.-value this#) ")"))

     (defmethod sf/of compose# [type# value#]
       (new compose# (sf/of ~F (sf/of ~G value#))))

     compose#))
