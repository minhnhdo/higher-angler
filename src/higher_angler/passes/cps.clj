(ns higher-angler.passes.cps
  (:require higher-angler.primitives))

(declare cps-of-expression)

(defn- cps-of-list
  [defined-names k s e]
  (let [[op] e]
    (cond
      (= 'fn op) (let [[_ arguments body] e
                       new-k (gensym "k-")
                       body-cps (cps-of-expression
                                  (into defined-names arguments)
                                  new-k
                                  s
                                  body)]
                   (list k s (list 'fn (apply vector new-k s arguments)
                                   body-cps)))
      (= 'if op) (let [[_ cond-e then-e else-e] e
                       then-cps (cps-of-expression defined-names k s then-e)
                       else-cps (cps-of-expression defined-names k s else-e)
                       v (gensym "v-")]
                   (cps-of-expression defined-names
                                      (list 'fn [s v] (if v then-cps else-cps))
                                      s
                                      cond-e))
      (= 'sample op) (let [v-addr (gensym "v-addr-")
                           v1 (gensym "v-")
                           [_ e-addr e1] e
                           e1-cps (cps-of-expression
                                    defined-names
                                    (list 'fn [s v1]
                                          (list 'my-sample k s v-addr v1))
                                    s
                                    e1)]
                       (cps-of-expression defined-names
                                          (list 'fn [s v-addr] e1-cps)
                                          s
                                          e-addr))
      (= 'observe op) (let [v-addr (gensym "v-addr-")
                            v1 (gensym "v-")
                            v2 (gensym "v-")
                            [_ e-addr e1 e2] e
                            e2-cps (cps-of-expression
                                     defined-names
                                     (list 'fn [s v2]
                                           (list 'my-observe k s v-addr v1 v2))
                                     s
                                     e2)
                            e1-cps (cps-of-expression defined-names
                                                      (list 'fn [s v1] e2-cps)
                                                      s
                                                      e1)]
                        (cps-of-expression defined-names
                                           (list 'fn [s v-addr] e1-cps)
                                           s
                                           e-addr))
      :else (let [[last-v :as vs] (repeatedly (count e) #(gensym "v-"))
                  [last-e & es] (reverse e)
                  last-cps (cps-of-expression defined-names
                                              (list 'fn [s last-v]
                                                    (list (last vs)
                                                          k
                                                          s
                                                          (rest (reverse vs))))
                                              s
                                              last-e)]
              (reduce (fn [acc [exp v]]
                        (cps-of-expression defined-names
                                           (list 'fn [s v] acc)
                                           s
                                           exp))
                      last-cps
                      (map vector es (reverse vs)))))))

(defn- cps-of-expression
  [defined-names k s e]
  (cond
    (list? e) (cps-of-list defined-names k s e)

    (and (symbol? e)
         (not (defined-names e))
         (ifn? (binding [*ns* (the-ns 'higher-angler.primitives)] (eval e))))
    (list k s (list 'fn [k s '& 'args] (list k s (list 'apply e 'args))))

    :else (list k s e)))

(defn- cps-of-defn
  [defined-names d]
  (let [[_ fn-name arguments body] d
        k (gensym "k-")
        s (gensym "s-")]
    (list 'defn fn-name (apply vector k s arguments)
          (cps-of-expression (into defined-names arguments) k s body))))

(defn cps-of-program
  [program]
  (let [defined-names (into #{'push-address 'sample 'observe}
                            (map second (pop program)))]
    (conj (mapv #(cps-of-defn defined-names %) (pop program))
          (let [v (gensym "v-")
                s (gensym "s-")
                new-s (gensym "s-")
                [_ arguments body] (peek program)
                body-cps (cps-of-expression (into defined-names arguments)
                                            (list 'fn [s v]
                                                  (list 'my-return s v))
                                            new-s
                                            body)]
            (list 'fn (apply vector s arguments) body-cps)))))
