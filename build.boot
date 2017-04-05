(def +project+ 'org.clojars.c4605/ramda)
(def +version+ "0.1.0")
(def +description+ "")

(set-env!
 :resource-paths #{"src"}
 :source-paths #{"test"}
 :dependencies '[[org.clojure/clojurescript "1.9.473" :scope "test"]
                 [org.clojure/clojure "1.8.0" :scope "test"]
                 [adzerk/boot-cljs "1.7.228-2" :scope "test"]
                 [adzerk/bootlaces "0.1.13" :scope "test"]])

(require '[adzerk.boot-cljs :refer [cljs]]
         '[adzerk.bootlaces :refer [push-release]])

(task-options!
 pom {:project +project+
      :version +version+
      :description +description+
      :license {"MIT" "http://opensource.org/licenses/MIT"}
      :scm {:url "https://github.com/bolasblack/ramda.clj"}})

(deftask build-cljs []
  (cljs :compiler-options {:target :nodejs}))

(deftask build-jar []
  (comp
    (pom)
    (jar)
    (install)
    (target)))

(deftask deploy []
  (set-env!
    :repositories #(conj % ["clojars" {:url "https://clojars.org/repo/"}]))
  (comp
    (build-jar)
    (push-release)))
