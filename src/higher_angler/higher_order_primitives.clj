(ns higher-angler.higher-order-primitives)

(def primitives
  '[(defn loop-helper [i c v g]
      (if (= i c)
        v
        ((fn [new-v] (loop-helper (+ i 1) c new-v g)) (g i v))))
    (defn map
      [f col]
      (if (seq col)
        (cons (f (first col))
              (map f (rest col)))
        (list)))
    (defn reduce
      [f v col]
      (if (seq col)
        (reduce f (f v (first col)) (rest col))
        v))
    (defn repeatedly
      [n f]
      (if (<= n 0)
        (list)
        (cons (f) (repeatedly (- n 1) f))))])
