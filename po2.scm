(use srfi-1 srfi-13 srfi-69 matchable)

(define word-size 8) ; x86_64

(define main-asm)
(define func-asm)

(define fixnum-mask  #b11)
(define fixnum-tag   #b00)
(define fixnum-shift 2)

(define null-mask #b111111)
(define null-tag  #b101111)

(define closure-mask #b111)
(define closure-tag  #b110)

(define reg-arg-order '(rdi rsi rdx rcx r8 r9))

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
  (let ((stack '(())))
    (define (push v)
      (set-car! stack (append (car stack) `(,v))))

    (define (push-args args)
      (map (lambda (a r) (push `(,a ,r))) args reg-arg-order))

    (define (pop)
      (set! stack (cdr stack)))

    (define (push-new-frame)
      (set! stack (cons '() stack)))

    (define (pop-stack-frame)
      (set! stack (cdr stack)))

    (define (lookup name)
      (call/cc
	(lambda (ret)
	  (cond
	    ((null? stack) (ret #f))
	    ((list-index
	       (lambda (x)
		 (cond
		   ((eq? x name) #t)
		   ((and (list? x) (eq? (car x) name))
		    (ret (cadr x)))
		   (else #f)))
	       (car stack)) => (lambda (x) x))
	    (else
	      (error "variable unbound" name))))))

    (define (debug)
      (p stack))

    (define (dispatch . args)
      (cond
	((eq? (car args) 'push) (push (cadr args)))
	((eq? (car args) 'push-args) (push-args (cadr args)))
	((eq? (car args) 'pop) (pop))
	((eq? (car args) 'lookup) (lookup (cadr args)))
	((eq? (car args) 'new-frame) (push-new-frame))
	((eq? (car args) 'pop-frame) (pop-stack-frame))
	((eq? (car args) 'debug) (debug))
	(else
	  (error "unknown command" args))))

    dispatch))

(define (deref reg off)
  (symbol-append '\[ reg '+ (string->symbol (->string off)) '\]))

(define (load-var si #!optional (reg 'rax))
  (if (number? si)
    (emit 'mov (deref 'rbp (- (* word-size (+ 1 si)))) reg)
    (emit 'mov si reg)))

(define (imm? e)
  (or (integer? e) (null? e)))

; (define (desugar))
; * convert define to set
; * convert cond to if with begin statements
; * convert let to lambda

(define (with-emit-to-list thunk)
  (fluid-let ((main-asm '()))
	     (thunk)
	     main-asm))

(define (emit-tag tag reg)
  (emit 'or tag reg))

(define (rem-tag tag reg)
  (emit 'sub tag reg))

(define (proc-call? e)
  (and (list? e) (> (length e) 0)))

; assumes func addr is in rax
(define (emit-apply-proc args)
  ; TODO: check tag
  (rem-tag closure-tag 'rax)
  (emit 'call 'rax))

(define (emit-debug-exp e)
  (emit #\; (string-take (->string e)
		(min (string-length (->string e)) 40))))

(define (emit-eval-exp e env)
  (emit-debug-exp e)
  (match e
	 (('set! var e)
	  (env 'push var)
	  (emit-eval-exp e env)
	  (emit 'push 'rax))
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
	 (('asm%% asm ...)
	  (apply emit asm))
	 (('lambda (fmls ...) body ...)
	  (let ((label (gensym 'lambda)))
	    (set! func-asm
	      (append func-asm
		      (with-emit-to-list
			(lambda ()
			  (emit-label label)
			  (emit 'push 'rbp)
			  (emit 'mov 'rsp 'rbp)
			  (env 'new-frame)
			  (env 'push-args fmls)
			  (map (cut emit-eval-exp <> env) body)
			  (emit 'pop 'rbp)
			  (emit 'ret)
			  (env 'pop-frame)))))
	    (emit 'mov label 'rax)
	    (emit-tag closure-tag 'rax)))
	 ((? proc-call? e)
	  (map (lambda (e r)
		 (emit-eval-exp e env)
		 (emit 'mov 'rax r))
	       (cdr e)
	       reg-arg-order)
	  (emit-eval-exp (car e) env)
	  (emit-apply-proc (cdr e)))
	 ((? symbol? e)
	  (cond
	    ((env 'lookup e) => load-var)
	    (else
	      (error "unknown binding " e))))
	 ((? imm? e)
	  (emit 'mov (imm-rep e) 'rax))
	 (=>
	   (error "failed to parse expression" e))))

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
    (append func-asm main-asm)))

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
					(set! putchar%
					  (lambda (c)
					    ; TODO: shift down c
					    (asm%% shr 2 rdi)
					    (asm%% push rdi)

					    (asm%% mov rbp rsi)
					    (asm%% mov 1 rax)
					    (asm%% mov 1 rdi)
					    (asm%% mov 1 rdx)
					    (asm%% syscall)

					    (asm%% pop rdi)))
					(putchar% 46))))))

(system "yasm -f elf64 out.s && ld out.o && echo 'DONE'")
