(use posix utils fmt fmt-color srfi-13)

;; util

(define (eq-length? n l)
  (eq? n (length l)))

(define (print-success str)
  (print (fmt #f (fmt-green str))))

(define (print-fail str)
  (print (fmt #f (fmt-red str))))

(define (asm-tree-to-str tree)
  (string-join (apply append
		      (map (lambda (n)
			     (list (conc (->string (car n)) " " (string-join (map ->string (cdr n)) ", "))))
			   (compile-program tree)))
	       "\n"))

;; init tests

(load "po.scm")
(system "mkdir -p /tmp/po_tests")
(system "cp driver.c /tmp/po_tests") ; move driver to working dir

(define-syntax make-test ; TODO: don't do this
  (syntax-rules ()
		((make-test expr cmp expect-val)
		 (begin
		   (display (string-append
			      (string-pad-right (->string expr) 20)
			      (string-pad (string-upcase (symbol->string cmp)) 10) " "
			      (string-pad-right (->string expect-val) 5)))
		   (with-output-to-file
		     "/tmp/po_tests/scheme_entry.s"
		     (lambda ()
		       (print (asm-tree-to-str expr))))
		   (system "cd /tmp/po_tests && cc -std=c99 -g -malign-double -m32 -o scheme_test scheme_entry.s driver.c 2>/dev/null")
		   (let ((r (with-input-from-pipe "cd /tmp/po_tests && ./scheme_test" read)))
		     (if (eval `(,cmp ,expect-val ,r)) ; TODO: use unhygenic macros instead...
		       (print-success "SUCCESS")
		       (begin
			 (print-fail "FAILED")
			 (print r " " cmp " " expect-val)
			 (print (read-all "/tmp/po_tests/scheme_entry.s"))
			 (exit 1))
		       ))))))

(make-test 0			'eq? 0)
(make-test 3			'eq? 3)
(make-test -3			'eq? -3)
(make-test #\3			'eq? #\3)
(make-test #\a			'eq? #\a)
(make-test #\!			'eq? #\!)
(make-test #t			'eq? #t)
(make-test #f			'eq? #f)
(make-test '() 			'eq-length? 0)
(make-test `(zero? 0)		'eq? #t)
(make-test `(add1 0)		'eq? 1)
(make-test `(add1 -1)		'eq? 0)
(make-test `(add1 10)		'eq? 11)
(make-test `(sub1 10)		'eq? 9)
(make-test `(integer->char 65)	'eq? #\A)
(make-test `(char->integer #\A)	'eq? 65)
(make-test `(null? ,'())	'eq? #t)
(make-test `(null? 0)		'eq? #f)
(make-test `(null? #\a)		'eq? #f)
(make-test `(null? #t)		'eq? #f)
(make-test `(integer? #t)	'eq? #f)
(make-test `(integer? #\a)	'eq? #f)
(make-test `(integer? ,'())	'eq? #f)
(make-test `(integer? 45)	'eq? #t)
(make-test `(boolean? #t)	'eq? #t)
(make-test `(boolean? #f)	'eq? #t)
(make-test `(boolean? #\t)	'eq? #f)
(make-test `(boolean? ,'())	'eq? #f)
(make-test `(boolean? 31)	'eq? #f)
(make-test `(zero? 0)		'eq? #t)
(make-test `(zero? 4)		'eq? #f)
(make-test `(zero? -4)		'eq? #f)
(make-test `(+ 3 2)		'eq? 5)
(make-test `(+ 4 3 2)		'eq? 9)
(make-test `(+ 1 2 3 4 5 6)     'eq? 21)
(make-test `(+ (add1 1) 2)      'eq? 4)
(make-test `(+ (+ 1 2 3) 2)     'eq? 8)
(make-test `(- 3 2 1)		'eq? 0)
(make-test `(- 1 2 3)		'eq? -4)
(make-test `(let ((a 3))
	      a)		'eq? 3)
(make-test `(let ((a 3))
	      (add1 a))		'eq? 4)
(make-test `(let ((a 3) (b 1))
	      (+ a b))		'eq? 4)
(make-test `(if #t
	      2
	      3)		'eq? 2)
(make-test `(if #f
	      2
	      3)		'eq? 3)
(make-test `(if (zero? (+ 1 2 3 4))
	      #\<
	      #\>)		'eq? #\>)
(make-test `(if (zero? (+ 1 2 -1 -2))
	      #\<
	      #\>)		'eq? #\<)
(make-test `(if (eq? 1 2)
	      #\y
	      #\n)		'eq? #\n)
(make-test `(if (eq? (+ 1 1) 2)
	      #\y
	      #\n)		'eq? #\y)
(make-test `(car (cons (+ 1 3) 2))	'eq? 4)
(make-test `(cdr (cons (+ 1 3) 2))	'eq? 2)
(make-test `(let ((a (cons 10 20)))
	      (car a))			'eq? 10)
(make-test `(car (cons 1 (cons 1 '()))) 'eq? 1)
(make-test `(let ((a (cons 1 (cons 3 (cons 2 '())))))
	      (let ((b (cons 3 a)))
		(car (cdr b))))		'eq? 1)
(make-test `(let ((a (cons 1 (cons 3 (cons 2 '())))))
	      (let ((b (cons 3 a)))
		(car b)))		'eq? 3)
(make-test `(let ((a (cons 1 (cons 2 ,'()))))
	      (cdr (cdr a)))	'eq? '(quote ()))
(make-test `(vector-length (make-vector 2))		'eq? 2)
(make-test `(string-length (make-string 4))		'eq? 4)
(make-test `(string-set! (make-string 1) 0 #\B)	'equal? "B")
(make-test `(string-ref (string-set! (make-string 5) 4 #\A) 4)	'eq? #\A)
(make-test `(string-ref (string-set! (make-string 1) 0 #\B) 0)	'eq? #\B)
