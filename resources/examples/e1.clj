(let [p 0.01
      dist (flip p)
      until-success (fn [until-success p n]
                      (if (sample dist)
                        n
                        (until-success until-success p (+ n 1))))]
  (until-success until-success p 0))
