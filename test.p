(def a 2)
(cons a (quote b))
(def y (lam (x) x))
(y (quote 1))

