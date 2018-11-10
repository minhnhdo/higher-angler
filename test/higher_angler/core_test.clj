(ns higher-angler.core-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [higher-angler.errors :refer [check-error checked->]]
            [higher-angler.passes.addressing :refer [addressing]]
            [higher-angler.passes.cps :refer [cps-of-program]]
            [higher-angler.passes.desugar :refer [desugar]]
            [higher-angler.passes.parse :refer [parse]]))

(deftest can-desugar-examples-twice
  (doseq [i (range 1 4)]
    (let [filename (str "examples/e" i ".clj")]
      (testing (str "can desugar " filename " twice")
        (let [parse-result (with-open [r (java.io.PushbackReader. (io/reader (io/resource filename)))]
                (parse r))
              output (higher-angler.errors/checked->
                       parse-result
                       check-error desugar
                       check-error addressing
                       check-error cps-of-program)]
          (is (nil? (:higher-angler.errors/error output)))
          (let [desugared-twice (desugar output)]
            (is (nil? (:higher-angler.errors/error (desugar output))))
            (is (= output (desugar output)))))))))
