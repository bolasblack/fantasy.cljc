(ns fantasy.core
  (:refer-clojure :exclude [map reduce concat empty extend])
  (:require [fantasy.curry :as curry-ns]
            [fantasy.either :as either-ns]
            [fantasy.maybe :as maybe-ns]
            [fantasy.identity :as identity-ns]
            [fantasy.utils :as u :include-macros true]
            [fantasy.standard-func :as standard-fn]
            [fantasy.funcs :as funcs]
            [fantasy.builtin-patch :as builtin-patch]))

(u/pull-to-ns {curry-ns [curry __]
               either-ns [Either #?@(:cljs [Left Right]) left right left? right?]
               maybe-ns [Maybe #?@(:cljs [Just Nothing]) just nothing just? nothing? from-maybe]
               identity-ns [#?(:cljs Identity)]
               standard-fn [of chain-rec empty zero
                            map ap reduce concat equals
                            alt bimap extend promap
                            traverse lte chain extract]
               funcs [to-maybe n-ary]})

(builtin-patch/patch-all)
