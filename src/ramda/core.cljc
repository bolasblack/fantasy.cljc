(ns ramda.core
  (:refer-clojure :exclude [map reduce concat empty])
  (:require [ramda.curry :as curry-ns :include-macros true]
            [ramda.either :as either-ns]
            [ramda.maybe :as maybe-ns]
            [ramda.identity :as identity-ns]
            [ramda.utils :as u :include-macros true]
            [ramda.standard-func :as standard-fn]
            [ramda.multimethods :as multimethods]
            [ramda.builtin-patch :as builtin-patch]))

(u/pull-to-ns {curry-ns [defcurry curry __]
               either-ns [Either Left Right left right]
               maybe-ns [Maybe Just Nothing just nothing from-maybe]
               identity-ns [Identity]
               standard-fn [of chain-rec empty zero
                            map ap reduce concat equals
                            alt bimap extend promap
                            traverse lte chain extract]
               multimethods [to-maybe]
               builtin-patch [patch-collection]})

(builtin-patch/patch-all)
