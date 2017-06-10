(def +project+ 'c4605/fantasy)
(def +version+ "0.2.0")
(def +description+ "")

(set-env!
 :resource-paths #{"src"}
 :dependencies '[[org.clojure/clojurescript "1.9.521" :scope "test"]
                 [org.clojure/clojure "1.8.0" :scope "test"]
                 [adzerk/boot-cljs "1.7.228-2" :scope "test"]
                 [adzerk/bootlaces "0.1.13" :scope "test"]
                 [adzerk/boot-test "1.2.0" :scope "test"]])

(require '[clojure.java.io :as io]
         '[clojure.java.shell :as shell]
         '[cljs.build.api :as cljs]
         '[boot.core :as boot]
         '[adzerk.boot-cljs :refer [cljs] :rename {cljs boot-cljs}]
         '[adzerk.bootlaces :refer [push-release]]
         '[adzerk.boot-test :refer [test] :rename {test test-clj}])

(task-options!
 pom {:project +project+
      :version +version+
      :description +description+
      :license {"MIT" "http://opensource.org/licenses/MIT"}
      :scm {:url "https://github.com/bolasblack/fantasy.cljc"}})

(deftask build-cljs []
  (boot-cljs :compiler-options {:target :nodejs}))

(deftask build-jar []
  (comp (pom)
        (jar)
        (install)))

(deftask deploy []
  (set-env! :repositories #(conj % ["clojars" {:url "https://clojars.org/repo/"}]))
  (comp (build-jar)
        (push-release)))

(deftask testing []
  (set-env! :source-paths #(conj % "test")))

(deftask test-cljs []
  (let [tmp (boot/tmp-dir!)
        output-dir-path (.toPath tmp)
        output-dir (.toString output-dir-path)
        output-file (.toString (.resolve output-dir-path "test.js"))]
    (fn middleware [next-handler]
       (fn handler [fileset]
         (boot/empty-dir! tmp)
         (let [out-files (boot/output-files fileset)
               js-files (boot/by-ext [".js"] out-files)
               _ (cljs/build
                  "./test/fantasy/test.cljs"
                  {:main 'fantasy.test
                   :target :nodejs
                   :asset-path ""
                   :output-to output-file
                   :output-dir output-dir})
               result (shell/sh "node" output-file :dir output-dir)
               key (if (= 0 (:exit result)) :out :err)]
           (println (key result)))
         (next-handler fileset)))))

(deftask test []
  (comp (test-cljs)
        (test-clj :include #"fantasy\..*(\-|\.)test")))
