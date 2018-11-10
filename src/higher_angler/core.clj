(ns higher-angler.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [higher-angler.errors :refer [check-error checked->]]
            [higher-angler.passes.addressing :refer [addressing]]
            [higher-angler.passes.cps :refer [cps-of-program]]
            [higher-angler.passes.desugar :refer [desugar]]
            [higher-angler.passes.parse :refer [parse]])
  (:gen-class)
  (:import [java.io PushbackReader]))

(def cli-options
  [["-h" "--help" "Print this help message" :default false]
   [nil "--parse" "Run up to parsing only" :default false]
   [nil "--desugar" "Run up to desugaring only" :default false]
   [nil "--addressing" "Run up to addressing only" :default false]
   [nil "--cps" "Run up to CPS transformation only" :default false]])

(defn usage
  [option-summary]
  (->> ["Higher Angler - a higher-order probabilistic programming language"
        ""
        "Usage: program-name [options] <input-files|->..."
        ""
        "Options:"
        option-summary]
       (string/join \newline)))

(defn process-options
  [{:keys [options summary errors arguments] :as parsed-options}]
  (cond
    (:help options) (do (println (usage summary))
                        (System/exit 1))
    errors (do (println (str "Unable to parse command line arguments:\n\n"
                             (string/join \newline errors)))
               (System/exit 1))
    (nil? (seq arguments)) (assoc parsed-options :arguments ["-"])
    :else parsed-options))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [{:keys [options arguments]} (process-options (parse-opts args cli-options))]
    (doseq [f arguments]
      (let [parse-result (with-open [r (if (= "-" f)
                                         *in*
                                         (PushbackReader. (io/reader f)))]
                           (parse r))
            result (checked->
                     parse-result
                     #(and (not (:parse options)) (check-error %))
                     desugar
                     #(and (not (:desugar options)) (check-error %))
                     addressing
                     #(and (not (:addressing options)) (check-error %))
                     cps-of-program)]
        (if (:higher-angler.errors/error result)
          (do (println (:higher-angler.errors/message result))
              (System/exit 2))
          (pprint result))))))
