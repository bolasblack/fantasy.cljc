(ns ramda.core
  (:refer-clojure :exclude [map reduce concat])
  (:require [ramda.curry :as curry-ns :include-macros true]
            [ramda.either :as either]
            [ramda.maybe :as maybe]
            [ramda.utils :as u :include-macros true]
            [ramda.standard-func :as standard-fn]
            [ramda.multimethods :as multimethods]
            [ramda.builtin-patch :as builtin-patch]))

(u/pull-to-ns {curry-ns [defcurry curry __]
               either [Either Left Right left right]
               maybe [Maybe Just Nothing just nothing]
               standard-fn [of chain-rec map ap reduce concat equals alt bimap extend promap traverse lte chain extract]
               multimethods [from]
               builtin-patch [patch-collection]})

(builtin-patch/patch-all)
