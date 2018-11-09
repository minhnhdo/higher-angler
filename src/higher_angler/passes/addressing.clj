(ns higher-angler.passes.addressing
  (:require [higher-angler.higher-order-primitives :refer [primitives]]
            higher-angler.primitives)
  (:import [java.util.concurrent.atomic AtomicLong]))

(def ^:private current-c
  (AtomicLong.))

(declare addressing-expression)

(defn- addressing-list
  [current-address defined-names e]
  (let [[op] e]
    (cond
      (= 'fn op) (let [[_ arguments body] e
                       new-arguments (apply vector current-address arguments)]
                   (list 'fn new-arguments
                         (addressing-expression
                           current-address
                           (into defined-names new-arguments)
                           body)))
      (= 'if op) (let [[_ cond-e then-e else-e] e]
                   (list 'if
                         (addressing-expression
                           current-address defined-names cond-e)
                         (addressing-expression
                           current-address defined-names then-e)
                         (addressing-expression
                           current-address defined-names else-e)))
      :else (let [addressed (map #(addressing-expression
                                    current-address defined-names %)
                                 e)]
              (apply list
                     (first addressed)
                     (list 'push-address
                           current-address
                           (.getAndIncrement current-c))
                     (rest addressed))))))

(defn- addressing-expression
  [current-address defined-names e]
  (cond
    (list? e) (addressing-list current-address defined-names e)
    (vector? e) (apply vector (map #(addressing-expression
                                      current-address defined-names %)
                                   e))

    (and (symbol? e)
         (not (defined-names e))
         (ifn? (binding [*ns* (the-ns 'higher-angler.primitives)] (eval e))))
    (list 'fn [current-address '& 'args] (list 'apply e 'args))

    :else e))

(defn- addressing-defn
  [defined-names d]
  (let [[_ fn-name arguments body] d
        current-address (gensym "address-")
        new-arguments (apply vector current-address arguments)]
    (list 'defn fn-name new-arguments
          (addressing-expression
            current-address (into defined-names new-arguments) body))))

(defn addressing
  [program]
  (let [defns (into primitives (pop program))
        defined-names (into #{'sample 'observe} (map second defns))]
    (conj (mapv #(addressing-defn defined-names %) defns)
          (let [address (gensym "address-")]
            (list 'fn [address]
                  (addressing-expression address
                                         defined-names
                                         (peek program)))))))
