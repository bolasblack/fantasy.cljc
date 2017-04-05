(ns ramda.core
  (:require [ramda.curry :as curry-ns]
            [ramda.func :as func-ns]
            [ramda.utils :as utils :include-macros true]))

(utils/pull-to-ns {curry-ns [curry __]
                   func-ns [always]})
