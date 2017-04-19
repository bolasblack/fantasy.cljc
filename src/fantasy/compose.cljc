(ns fantasy.compose
  (:require [fantasy.protocols :as p]
            [fantasy.standard-func :as sf]
            [fantasy.utils :as u :include-macros true]))

(defmacro defcompose [F G]
  `(do
     (deftype compose# [~'value]
       IEquiv
       (-equiv [this# other#]
         (u/equals this# other#))

       p/Applicative

       p/Apply
       (p/ap [this# f#]
         (new compose# (p/ap (.-value this#)
                             (p/map (.-value f#)
                                    (fn [u#] (fn [y#] (p/ap y# u#)))))))
       p/Functor
       (p/map [this# f#]
         (new compose# (p/map (.-value this#)
                              (fn [y#] (p/map y# f#))))))

     (u/defpr [compose#] [this#]
       (str "((Compose. " ~(name F) " " ~(name G) ") " (.-value this#) ")"))

     (defmethod sf/of compose# [type# value#]
       (new compose# (sf/of ~F (sf/of ~G value#))))

     compose#))
