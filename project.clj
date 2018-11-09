(defproject higher-angler "0.1.0-SNAPSHOT"
  :description "Higher Angler: a higher-order probabilistic programming language"
  :url "https://github.com/mrordinaire/higher-angler"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]]
  :main ^:skip-aot higher-angler.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
