(defn marsaglia-normal [mean var]
  (let [d (uniform-continuous -1.0 1.0)
        x (sample d)
        y (sample d)
        s (+ (* x x ) (* y y ))]
    (if (< s 1)
      (+ mean (* (sqrt var)
                 (* x (sqrt (* -2 (/ ( log s) s))))))
      (marsaglia-normal mean var))))

(let [mu (marsaglia-normal 1 5)
      sigma (sqrt 2)
      lik (normal mu sigma)]
  (observe lik 8)
  (observe lik 9)
  mu)
