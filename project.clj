(defproject higher-angler "0.1.0-SNAPSHOT"
  :description "Higher Angler: a higher-order probabilistic programming language"
  :url "https://github.com/mrordinaire/higher-angler"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/tools.cli "0.3.7"]
                 [net.mikera/core.matrix "0.62.0"]
                 [anglican "1.0.0"]]
  :main ^:skip-aot higher-angler.core
  :target-path "target/%s"
  :jvm-opts ["-Xss8m"]
  :profiles {:uberjar {:aot :all}})
