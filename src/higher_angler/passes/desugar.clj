(ns higher-angler.passes.desugar)

(defn- desugar-identifier
  [e]
  (if (= '_ e)
    (gensym)
    e))

(declare desugar-expression)

(defn- desugar-fn
  [e]
  (let [[_ params body] e]
    (list 'fn (mapv desugar-identifier params)
          (desugar-expression body))))

(defn- desugar-let
  [e]
  (let [[_ bindings & body] e
        reversed-desugared-bindings (rseq
                                      (mapv #(vector
                                               (desugar-identifier (nth % 0))
                                               (desugar-expression (nth % 1)))
                                            (partition 2 bindings)))
        reversed-desugared-exps (rseq (mapv desugar-expression body))
        desugared-body (reduce #(list (list 'fn [(gensym)] %1) %2)
                               reversed-desugared-exps)]
    (reduce #(list (list 'fn [(nth %2 0)] %1) (nth %2 1))
            desugared-body
            reversed-desugared-bindings)))

(defn- desugar-loop
  [expr]
  (let [[_ c e f & body] expr
        as (repeatedly (count body) #(gensym "a-"))
        g (gensym "g-")
        bound (gensym "bound-")
        initial-value (gensym "initial-value-")
        loop-helper (gensym "loop-helper-")
        loop-helper-expr '(fn [loop-helper i c v g]
                            (if (= i c)
                              v
                              (let [new-v (g i v)]
                                (loop-helper loop-helper (+ i 1) c new-v g))))]
    (desugar-let
      (list 'let (into (apply vector
                              bound c
                              initial-value e
                              (interleave as body))
                       [g (list 'fn '[i w] (apply list f 'i 'w as))
                        loop-helper loop-helper-expr])
            (list loop-helper loop-helper 0 bound initial-value g)))))

(defn- desugar-foreach
  [e]
  (let [[_ c bindings & body] e
        partitioned-bindings (partition 2 bindings)]
    (apply list 'vector
           (for [i (range c)]
             (desugar-let
               (apply list 'let (vec (mapcat #(let [v (nth % 0)
                                                    e (nth % 1)]
                                                [v (list 'nth e i)])
                                             partitioned-bindings))
                      body))))))

(defn- desugar-list
  [e]
  (let [[op & params] e]
    (cond
      (= 'fn op) (desugar-fn e)
      (= 'if op) (let [[cond-exp then-exp else-exp] (map desugar-expression params)]
                   ;; the above explicit binding is to handle if expressions
                   ;; with missing else and then branches
                   (list 'if cond-exp then-exp else-exp))
      (= 'let op) (desugar-let e)
      (= 'foreach op) (desugar-foreach e)
      (= 'loop op) (desugar-loop e)
      :else (apply list op (map desugar-expression params)))))

(defn- desugar-expression
  [e]
  (cond
    (and (list? e) (seq e)) (desugar-list e)
    (list? e) (list 'list)
    (vector? e) (apply list 'vector (map desugar-expression e))
    (set? e) (apply list 'hash-set (map desugar-expression e))
    (map? e) (apply list 'hash-map (map desugar-expression
                                        (mapcat identity (seq e))))
    :else e))

(defn- desugar-defn
  [d]
  (let [[_ fn-name arguments body] d]
    (list
      'defn (desugar-identifier fn-name)
      (mapv desugar-identifier arguments)
      (desugar-expression body))))

(defn desugar
  [program]
  (conj (mapv desugar-defn (pop program))
        (desugar-expression (peek program))))
