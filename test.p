(cons (quote a) (quote b))
((lam (x y) (cons (car x) y)) (quote (a b)) (cdr (quote (c d))))