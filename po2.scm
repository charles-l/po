(use srfi-1 srfi-13 srfi-69 matchable)

(define word-size 8) ; x86_64

(define main-asm)
(define func-asm)

(define fixnum-mask  #b11)
(define fixnum-tag   #b00)
(define fixnum-shift 2)

(define null-mask #b111111)
(define null-tag  #b101111)

; debug
(define (p . args)
  (map (cut format (current-error-port) "~A " <>) args)
  (format (current-error-port) "~%"))

(define (emit . args)
  (set! main-asm (append main-asm (list args))))

(define-syntax emit*
  (lambda (exp rename compare)
    (cons (rename 'begin)
	  (map (cut cons 'emit <>) (cdr exp)))))

(define (imm-rep i)
  (cond
    ((integer? i)
     (arithmetic-shift i fixnum-shift))
    ((null? i)
     null-tag)))

(define (make-env)
  (let ((stack '()))
    (define (push name)
      (set! stack (append stack `(,name))))

    (define (pop)
      (set! stack (cdr stack)))

    (define (lookup name)
      (call/cc
	(lambda (ret)
	  (let l ((s stack))
	    (cond
	      ((null? s) (ret #f))
	      ((eq? (car s) name) 0)
	      (else (add1 (l (cdr s)))))))))

    (define (debug)
      (print stack))

    (define (dispatch . args)
      (cond
	((eq? (car args) 'push) (push (cadr args)))
	((eq? (car args) 'pop) (pop))
	((eq? (car args) 'lookup) (lookup (cadr args)))
	((eq? (car args) 'debug) (debug))))

    dispatch))

(define (deref reg off)
  (symbol-append '\[ reg '+ (string->symbol (->string off)) '\]))

(define (load-var si #!optional (reg 'rax))
  (emit 'mov (deref 'rbp (- (* word-size (+ 1 si)))) reg))

(define (imm? e)
  (or (integer? e) (null? e)))

; (define (desugar))
; * convert define to set
; * convert cond to if with begin statements
; * convert let to lambda

(define (emit-eval-exp e env)
  (match e
	 (('set! var e)
	  (env 'push var)
	  (emit 'push (imm-rep e)))
	 (('begin e ...)
	  (map (cut emit-eval-exp <> env) e))
	 (('if c t e)
	  (let ((el (gensym 'else)) (done (gensym 'done)))
	    (emit-eval-exp c env)
	    (emit 'cmp null-tag 'rax) ; if
	    (emit 'je el)

	    (emit-eval-exp t env) ; then
	    (emit 'jmp done)

	    (emit-label el) ; else
	    (emit-eval-exp e env)

	    (emit-label done)))
	 ((? symbol? e)
	  (cond
	    ((env 'lookup e) => load-var)
	    (else
	      (error "unknown binding " e))))
	 ((? imm? e)
	  (emit 'mov (imm-rep e) 'rax))))

(define (emit-label e)
  (emit (symbol-append e ':)))

(define (compile prog)
  (fluid-let
    ((main-asm '()) (func-asm '()))
    (emit 'global '_start)
    (emit-label '_start)
    (emit 'push 'rbp)
    (emit 'mov 'rsp 'rbp)
    (let ((env (make-env)))
      (emit-eval-exp prog env))

    (emit 'mov 'rax 'rbx)
    (emit 'mov 1 'rax)
    (emit 'int #x80)

    (emit 'pop 'rbp)
    main-asm))

(define (print-instr i)
  (cond
    ((> (length i) 1)
     (display "\t")
     (display (car i))
     (display " ")
     (print (string-join (reverse (map ->string (cdr i))) ",")))
    (else
      (display (car i)) (newline))))

(with-output-to-file "./out.s"
		     (lambda ()
		       (map print-instr
			    (compile '(begin
					(set! a 3)
					(set! b 5)
					a
					(if 3
					  1
					  2))))))

(system "yasm -f elf64 out.s && ld out.o && echo 'DONE'")
