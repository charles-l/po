(use posix utils fmt fmt-color srfi-13)
(load "po.scm")

;; util

(define (eq-length? n l)
  (eq? n (length l)))

;;

(define-syntax make-test
  (syntax-rules ()
		((make-test expr cmp expect-val)
		 (begin
		   (display (string-append
			      (string-pad-right (->string expr) 20)
			      (string-pad (string-upcase (symbol->string cmp)) 10) " "
			      (string-pad-right (->string expect-val) 5)))
		   (with-output-to-file "/tmp/scheme_entry.s"
					(lambda () (display (compile-program expr))))
		   (system "cp driver.c /tmp")
		   (system "cd /tmp && cc -c scheme_entry.s && cc -o scheme_test driver.c scheme_entry.o")
		   (let ((r (with-input-from-pipe "cd /tmp && ./scheme_test" read)))
		     (if (eval `(,cmp ,expect-val ,r))
		       (print (fmt #f (fmt-green "PASSED")))
		       (begin
			 (print r)
			 (print (read-all "/tmp/scheme_entry.s"))
			 (print (fmt #f (fmt-red "FAILED"))))))))))

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
