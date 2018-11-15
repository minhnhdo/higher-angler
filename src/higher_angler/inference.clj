(ns higher-angler.inference
  (:require [anglican.runtime :refer [observe* sample*]]
            [higher-angler.errors :refer [check-error checked-> infer-error]]
            [higher-angler.higher-order-primitives :refer [primitives]]
            [higher-angler.passes.desugar :refer [desugar]]
            higher-angler.primitives)
  (:import [clojure.lang IPersistentMap IPersistentVector Keyword]))

(declare interp)

(defn- interp-list
  [state ^IPersistentMap sub ^IPersistentVector e]
  (let [[op] e]
    (cond
      (= 'fn op) e
      (= 'sample op) (sample* (interp state sub (second e)))
      (= 'observe op) (let [[_ dist-e v-e] e
                            dist (interp state sub dist-e)
                            v (interp state sub v-e)]
                        (swap! state + (observe* dist v))
                        v)
      (= 'if op) (let [[_ cond-e then-e else-e] e
                       cond-v (interp state sub cond-e)]
                   (if cond-v
                     (interp state sub then-e)
                     (interp state sub else-e)))
      :else (loop [es (rest e)
                   vs []]
              (if (seq es)
                (let [v (interp state sub (first es))]
                  (recur (rest es) (conj vs v)))
                (let [op-v (interp state sub op)]
                  (if (list? op-v)
                    (let [[_ arguments body] op-v]
                      (interp state (into sub (map vector arguments vs)) body))
                    (apply op-v vs))))))))

(defn- interp
  [state ^IPersistentMap sub ^IPersistentVector e]
  (cond
    (list? e) (interp-list state sub e)
    (contains? sub e) (sub e)
    :else (eval e)))

(defn- likelihood-weighting
  ^IPersistentVector
  [^IPersistentMap sub e & _]
  (let [state (atom 0.0)]
    (repeatedly #(do (reset! state 0.0)
                     (binding [*ns* (the-ns 'higher-angler.primitives)]
                       (let [result (interp state sub e)]
                         [@state result]))))))

(def ^:private interp-algorithm
  {:likelihood-weighting likelihood-weighting})

(defn infer
  [^Keyword algorithm ^IPersistentVector program & options]
  (let [alg (interp-algorithm algorithm)
        {:keys [burn-in] :or {burn-in 10000}} options
        output (checked->
                 program
                 check-error desugar)]
    (cond
      (nil? alg) (infer-error "Unknown algorithm " algorithm
                              " for interpreting")
      (:higher-angler.errors/error output) output
      :else (let [combined-defns (into primitives (pop output))
                  sub (apply hash-map
                             (interleave (map second combined-defns)
                                         (map #(let [[_ fn-name arguments body] %]
                                                 (list 'fn arguments body))
                                              combined-defns)))]
              (drop burn-in (apply alg sub (peek output) options))))))
