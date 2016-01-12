(defproject sqlkorma "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
    [org.clojure/clojure "1.7.0"]
    [korma "0.4.0"]
    [org.postgresql/postgresql "9.4-1206-jdbc42"]
  ]
  :main ^:skip-aot sqlkorma.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
