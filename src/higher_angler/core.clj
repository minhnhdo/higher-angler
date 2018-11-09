(ns higher-angler.core
  (:require [higher-angler.errors :refer [check-error checked->]]
            [higher-angler.passes.desugar :refer [desugar]]
            [higher-angler.passes.parse :refer [parse]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [result (checked->
                 (parse *in*)
                 check-error desugar)]
    (if (:higher-angler.errors/error result)
      (do (println (:higher-angler.errors/message result))
          (System/exit 2))
      ())))
