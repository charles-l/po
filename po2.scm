(use srfi-1 srfi-13 srfi-69 matchable)

(define word-size 8) ; x86_64

(define main-asm)
(define func-asm)

(define fixnum-mask  #b11)
(define fixnum-tag   #b00)
(define fixnum-shift 2)

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
    (else
      (error "Unknown type for " i))))

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

(define (var-val i)
  (if (number? i)
    (deref 'rbp (- (* word-size (+ 1 i))))
    i))

(define (imm? e)
  (integer? e))

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
(define (emit-apply-proc name args)
  ; TODO: check tag
  (emit 'call (asm-nice-name name)))

(define (emit-debug-exp e)
  (emit #\; (string-take (->string e)
		(min (string-length (->string e)) 40))))

; get the immediate rep value if it's available
(define (emit-or-imm e env)
  (cond
    ((imm? e)
     (imm-rep e))
    ((symbol? e)
     (var-val (env 'lookup e)))
    (else
      (emit-eval-exp e env)
      'rax)))

(define (emit-eval-exp e env)
  (emit-debug-exp e)
  (match e
	 (('def var* ...)
	  ; TODO: assert this is at the start of a function
	  ; TODO: hoist vars
	  (emit 'sub (* word-size (length var*)) 'rsp)
	  (map (cut env 'push <>) var*))
	 (('set! var val)
	  (let ((i (env 'lookup var)))
	    (if i
	      (emit 'mov (emit-or-imm val env) (conc "DWORD " (var-val i)))
	      (error "var not defined" var))))
	 (('begin e ...)
	  (map (cut emit-eval-exp <> env) e))
	 (('if c t e)
	  (let ((el (gensym 'else)) (done (gensym 'done)))
	    (emit-eval-exp c env)
	    (emit 'cmp 0 'rax) ; if
	    (emit 'je el)

	    (emit-eval-exp t env) ; then
	    (emit 'jmp done)

	    (emit-label el) ; else
	    (emit-eval-exp e env)

	    (emit-label done)))
	 (('asm% asm ...)
	  (apply emit (map (lambda (x)
			     (if (and (list? x) (eq? (car x) 'unquote))
			       (emit-or-imm (cadr x) env)
			       x)) asm)))
	 (('proc name (fmls ...) body ...)
	  (let ((l (asm-nice-name name)))
	    (set! func-asm
	      (append func-asm
		      (with-emit-to-list
			(lambda ()
			  (emit 'type 'function l)
			  (emit-label l)
			  (emit 'push 'rbp)
			  (emit 'mov 'rsp 'rbp)
			  (env 'new-frame)
			  (env 'push-args fmls)
			  (map (cut emit-eval-exp <> env) body)
			  (emit 'leave)
			  (emit 'ret)
			  (env 'pop-frame)))))
	    (emit 'mov l 'rax)
	    (emit-tag closure-tag 'rax)))
	 ((? proc-call? e)
	  (map (lambda (e r)
		 (emit 'mov (emit-or-imm e env) r))
	       (cdr e)
	       reg-arg-order)
	  (emit-apply-proc (car e) (cdr e)))
	 ((? symbol? e)
	  (cond
	    ((env 'lookup e) => (lambda (x) (emit 'mov (var-val x) 'rax)))
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

    (emit 'leave)
    (append func-asm main-asm)))

(define (asm-nice-name name)
  (string->symbol (conc "_po_" (string-translate* (->string name) '(("%" . "__PERCENT") ("-" . "__"))))))

(define (print-instr i)
  (cond
    ((> (length i) 1)
     (display "\t")
     (display (car i))
     (display " ")
     (print (string-join (reverse (map ->string (cdr i))) ",")))
    (else
      (display (car i)) (newline))))

(with-output-to-file "./boot.s"
		     (lambda ()
		       (map print-instr
			    (compile '(begin
					(def a b c)
					(proc eq? (a b)
					      (asm% xor rax rax)
					      (asm% mov 1 rbx) ; true
					      (asm% cmp ,a ,b)
					      (asm% cmove rbx rax)
					      (asm% shl 2 rax))

					(proc alloc-block (s)
					      (asm% mov 45 rax)
					      (asm% syscall))

					(proc putchar (c)
					      (asm% shr 2 ,c)
					      (asm% push ,c)

					      (asm% mov rsp rsi)
					      (asm% mov 1 rax)
					      (asm% mov 1 rdi)
					      (asm% mov 1 rdx)
					      (asm% syscall)

					      (asm% pop rdi))
					(if (eq? 1 2)
					  (putchar 65)
					  (putchar 66))
					(alloc-block 2))))))

(system "yasm -f elf64 boot.s && ld boot.o && echo 'DONE'")
