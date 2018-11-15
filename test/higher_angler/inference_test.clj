(ns higher-angler.inference-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [anglican.stat :refer [empirical-mean]]
            [higher-angler.inference :refer [query]]
            [higher-angler.passes.parse :refer [parse]]))

(def number-of-samples 100000)

(def burn-in 10000)

(defn higher-angler-query
  [algorithm program nsamples & options]
  (->> (apply query algorithm program options)
       (take nsamples)))

(deftest program-1
  (testing "program 1"
    (println "running program 1")
    (let [parse-result (with-open [r (->> "examples/e1.clj"
                                          io/resource
                                          io/reader
                                          java.io.PushbackReader.)]
                         (parse r))
          result (higher-angler-query
                   :likelihood-weighting parse-result number-of-samples
                   :burn-in burn-in)
          mean (empirical-mean result)]
      (println "program 1")
      (println "mean" mean))))

(deftest program-2
  (testing "program 2"
    (println "running program 2")
    (let [parse-result (with-open [r (->> "examples/e2.clj"
                                          io/resource
                                          io/reader
                                          java.io.PushbackReader.)]
                         (parse r))
          result (higher-angler-query
                   :likelihood-weighting parse-result number-of-samples
                   :burn-in burn-in)
          mean (empirical-mean result)]
      (println "program 2")
      (println (take 5 result))
      (println "mean" mean))))

(deftest program-3
  (testing "program 3"
    (println "running program 3")
    (let [parse-result (with-open [r (->> "examples/e3.clj"
                                          io/resource
                                          io/reader
                                          java.io.PushbackReader.)]
                         (parse r))
          result (higher-angler-query
                   :likelihood-weighting parse-result number-of-samples
                   :burn-in burn-in)
          mean (empirical-mean result)]
      (println "program 3")
      (println "mean" mean))))
