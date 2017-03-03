(use matchable)

'(call func args ...)
'(callt func args ...) ; tailcall
'(call0 func)
'(callt0 func) ; tailcall

'(mov a -> b)
'(not a -> b)
'(ret a)

'(* d a b) ; d = a * b
'(/ d a b) ; d = a / b
'(% d a b) ; d = a % b

'(and d a b) ; d = a & b
'(xor d a b) ; d = a ^ b
'(or  d a b) ; d = a | b
'(null? a)
'(= a b)
'(< a b)
'(> a b)
'(eq? a b)
'(1+ d a) ; d = a + 1
'(1- d a) ; d = a - 1

'(jmp a)

'(kstr d str) ; set d to str
'(kshr d short) ; set d to short
'(kshr d num) ; set d to bignum
'(ksym d sym) ; set d to symbol
'(knul d) ; set d to null
'(cnew d a b) ; create a new cons of a and b and put it in d
'(car d c) ; put car of c in d
'(cdr d c) ; put cdr of c in d
'(lanew d frees fmls) ; new list

'(vnew d n) ; create a new vector of size n
'(vset v i n) ; v[i] = d
'(vget d v i) ; d = v[i]

(define (compile-expr e)
  (match e
	 (('+ d a b) ; d = a + b
	  `((movl ,a ,d)
	    (addl ,d ,b)))
	 (('- d a b) ; d = a - b
	  `((movl ,a ,d)
	    (addl ,b ,d)))
	 ))

