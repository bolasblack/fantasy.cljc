(ns fantasy.core
  (:refer-clojure :exclude [map reduce concat empty])
  (:require [fantasy.curry :as curry-ns :include-macros true]
            [fantasy.either :as either-ns]
            [fantasy.maybe :as maybe-ns]
            [fantasy.identity :as identity-ns]
            [fantasy.utils :as u :include-macros true]
            [fantasy.standard-func :as standard-fn]
            [fantasy.multimethods :as multimethods]
            [fantasy.builtin-patch :as builtin-patch]))

(u/pull-to-ns {curry-ns [defcurry curry __]
               either-ns [Either Left Right left right left? right?]
               maybe-ns [Maybe Just Nothing just nothing just? nothing? from-maybe]
               identity-ns [Identity]
               standard-fn [of chain-rec empty zero
                            map ap reduce concat equals
                            alt bimap extend promap
                            traverse lte chain extract]
               multimethods [to-maybe]
               builtin-patch [patch-collection]})

(builtin-patch/patch-all)
