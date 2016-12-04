(use srfi-1 srfi-13)

(define word-size 4)
(define double-word (* 2 word-size))
(define stack-start (- word-size)) ; start of the stack (offset of %esp)

;;; data types

(define fixnum-mask 3)
(define fixnum-tag 0)
(define fixnum-shift 2)

(define char-mask 255)
(define char-tag 15)
(define char-shift 8)

(define boolean-mask 127)
(define boolean-tag 31)
(define boolean-shift 7)

(define null-mask 255)
(define null-tag 47)

;;; heap data types

(define heap-mask 7)
(define heap-shift 3)

(define pair-tag 1)
(define vector-tag 2)
(define string-tag 3)
(define symbol-tag 5)
(define closure-tag 6)

; compile time syntax checking
(define-syntax expect
  (syntax-rules ()
		((expect expr str)
		 (handle-exceptions exn
		   (error (string-append "expected " str))
		   expr))))

(define (expect-true expr con . msg)
  (if con
    con
    (error (string-append (->string expr) ": " (string-join (map ->string msg) " ")))))

;;; emit dsl

(define-syntax with-asm-to-list
  (syntax-rules ()
		((with-asm-to-list body ...)
		 (fluid-let ((main-asm '()))
			    body ...
			    main-asm))))

(define (uniq-label s)
    (gensym s))

(define ($ v) ; stick a "$" at the start of an immediate asm value
  (symbol-append '$ (string->symbol (->string v))))

(define (deref v)
  (symbol-append '\( v '\)))

(define (label e)
  (symbol-append e ':))

(define (emit . args)
  (set! main-asm (append main-asm `(,args))))

; (emit* ('a 'b 'c) ('d e f)) eq to (emit 'a 'b 'c) (emit 'd e f)
(define-syntax emit*
  (lambda (exp rename compare)
    (append `(begin)
	    (map (lambda (x) (cons 'emit x)) (cdr exp)))))

(define (emit-immediate e)
  (emit 'movl (immediate-rep e) '%eax))

(define (emit-cmp-eax val) ; cmp eax to val (TODO: make this more efficient)
  (emit* ('movl (immediate-rep #f) '%ecx) ; move #t and #f into registers
	 ('movl (immediate-rep #t) '%edx)
	 ('cmpl val '%eax)
	 ('cmovzl '%edx '%ecx)
	 ('movl '%ecx '%eax))) ; move the result to %eax

(define (emit-new-heap-val size tag initials)
  (mapi (lambda (e i)
	  (emit 'movl e (conc (* i word-size) "(%esi)")))
	initials)
  (emit* ('movl '%esi  '%eax)
	 ('orl  tag    '%eax)
	 ('addl size   '%esi) ; advance %esi
	 ('addl ($ 11) '%esi) ; and align it on 8 bytes
	 ('andl ($ -8) '%esi)))

(define (emit-push-all-to-stack l si env) ; push a list to the stack
  (if (not (null? l))
    (begin
      (emit-expr (car l) si env)
      (emit-push-to-stack si)
      (emit-push-all-to-stack (cdr l) (- si word-size) env))
    (+ si word-size))) ; return ending stack pos

(define (emit-apply-stack op si-start n) ; apply an operator to a stack
  (if (= n 1)
    (emit 'movl (stack-pos si-start) '%eax)
    (begin
      (emit-apply-stack op (+ si-start word-size) (sub1 n))
      (emit op (stack-pos si-start) '%eax))))

(define (emit-push-to-stack si)
  (emit 'movl '%eax (stack-pos si)))

(define (emit-mask-data mask) ; leave just the type behind for type checks
  (emit 'andl mask '%eax))

(define (emit-function-header name)
  (emit* ('.text)
	 ('.global name)
	 ('.type name '@function)
	 ('.align 8)
	 ((label name))))

(define (emit-code fmls frees body si env)
  (let ((code-id (uniq-label 'L)))
    (set! main-asm
      (append (with-asm-to-list
		(emit-function-header code-id)
		(let f ((fmls fmls) (si (- word-size)) (env env))
		  (cond
		    ((null? fmls)
		     (map (cut emit-expr <> si env) body))
		    (else
		      (f (cdr fmls)
			 (- si word-size)
			 (push-var (car fmls) si env)))))
		(emit 'ret))
	      main-asm))
    code-id))

; assumes e, si, and env are bound

(define (emit-primcall e si env)
  (case (car e)
    ((add1)
     (emit-expr (cadr e) si env)
     (emit 'addl (immediate-rep 1) '%eax))
    ((sub1)
     (emit-expr (cadr e) si env)
     (emit 'subl (immediate-rep 1) '%eax))
    ((integer->char)
     (emit-expr (cadr e) si env)
     (emit 'shl ($ (- char-shift fixnum-shift)) '%eax)
     (emit 'orl ($ char-tag) '%eax))
    ((char->integer)
     (emit-expr (cadr e) si env)
     (emit 'shr ($ (- char-shift fixnum-shift)) '%eax))
    ((null?)
     (emit-expr (cadr e) si env)
     (emit-cmp-eax ($ null-tag)))
    ((integer?)
     (emit-expr (cadr e) si env)
     (emit-mask-data ($ fixnum-mask))
     (emit-cmp-eax ($ fixnum-tag)))
    ((boolean?)
     (emit-expr (cadr e) si env)
     (emit-mask-data ($ boolean-mask))
     (emit-cmp-eax ($ boolean-tag)))
    ((procedure?)
     (emit-expr (cadr e) si env)
     (emit-mask-data ($ heap-mask))
     (emit-cmp-eax ($ closure-tag)))
    ((zero?)
     (emit-expr (cadr e) si env)
     (emit-cmp-eax ($ 0)))
    ((car) ; TODO: check that type is a pair
     (emit-expr (cadr e) si env)
     (emit 'movl "-1(%eax)" '%eax))
    ((cdr) ; TODO: check that type is a pair
     (emit-expr (cadr e) si env)
     (emit 'movl "3(%eax)" '%eax))
    ((eq?)
     (emit-expr (cadr e) si env)
     (emit 'mov '%eax '%ebx)
     (emit-expr (caddr e) si env)
     (emit-cmp-eax "%ebx"))
    ((+)
     (emit-apply-stack 'addl
		       (emit-push-all-to-stack (cdr e) si env)
		       (length (cdr e))))
    ((-)
     (emit-apply-stack 'subl
		       (emit-push-all-to-stack (cdr e) si env)
		       (length (cdr e))))
    ((if)
     (let ((L0 (uniq-label 'if)) (L1 (uniq-label 'else)))
       (emit-expr (cadr e) si env)
       (emit 'cmpl (immediate-rep #f) '%eax)
       (emit 'je L0)
       (emit-expr (caddr e) si env)
       (emit 'jmp L1)
       (emit (label L0))
       (emit-expr (cadddr e) si env)
       (emit (label L1))))
    ((cons)
     (emit-expr (cadr e) si env) ; compile sub exprs first
     (emit-push-to-stack si) ; and push scratch to stack
     (emit-expr (caddr e) (- si word-size) env)
     (emit* ('movl '%eax "4(%esi)") ; second word of esi
	    ('movl (stack-pos si) '%eax) ; move from stack to
	    ('movl '%eax "0(%esi)") ; first word of esi
	    ('movl '%esi '%eax)
	    ('orl  ($ pair-tag) '%eax) ; mark as pair
	    ('addl ($ double-word) '%esi))) ; bump esi forward
    ((make-vector) ; TODO: add vector-set and vector-ref and type check
     (emit-expr (cadr e) si env)
     (emit* ('shr ($ fixnum-shift) '%eax) ; no need for type data - we already know it's a uint
	    ('imul ($ word-size) '%eax) ; each element is sizeof(word)
	    ('movl '%eax '%ebx)) ; backup %eax
     (emit-new-heap-val '%ebx ($ vector-tag) '(%ebx)))
    ((vector-length)
     (emit-expr (cadr e) si env)
     (emit 'movl (conc (- vector-tag) "(%eax)") '%eax))
    ((make-string)
     (emit-expr (cadr e) si env)
     (emit 'shr ($ fixnum-shift) '%eax) ; no need for type data - we already know it's a uint
     (emit 'movl '%eax '%ebx) ; backup %eax
     (emit-new-heap-val '%ebx ($ string-tag) '(%ebx)))
    ((string-length)
     (emit-expr (cadr e) si env)
     (emit 'movl (conc (->string (- string-tag)) "(%eax)") '%eax)
     (emit 'shl  ($ fixnum-shift) '%eax)) ; shift back to typed fixnum
    ((string-set!)
     (emit-expr (cadr e) si env)
     (emit 'movl '%eax '%ecx) ; string
     (emit 'movl '%eax '%edx) ; save original string ptr for later
     (emit-expr (caddr e) si env)
     (emit 'movl '%eax '%ebx) ; index
     (emit 'shr  ($ fixnum-shift) '%ebx)
     (emit-expr (cadddr e) si env) ; char
     (emit* ('addl '%ebx '%ecx)
	    ('shr  ($ char-shift) '%eax)
	    ('movb '%al (conc (- (- string-tag 4)) "(%ecx)")) ; TODO: fix warning for this
	    ('movl '%edx '%eax)))
    ((string-ref)
     (emit-expr (cadr e) si env) ; string
     (emit 'movl '%eax '%ecx)
     (emit-expr (caddr e) si env) ; index
     (emit* ('shr  ($ fixnum-shift) '%eax)
	    ('addl '%eax '%ecx)
	    ('movb (conc (- (- string-tag 4)) "(%ecx)") '%ah)
	    ('orl  ($ char-tag) '%eax)))
    ((labels)
     (emit-expr (caddr e)
		si
		(let l ((bindings (cadr e)) (env (make-env '())))
		  (if (null? bindings)
		    env
		    (l (cdr bindings)
		       (push-var (caar bindings)
				 (emit-code
				   (cadr (cadar bindings))
				   (caddr (cadar bindings))
				   (cdddr (cadar bindings))
				   si env) ; TODO: fix the env here (i.e. remove other locals)
				 env))))))
    ((closure)
     (emit-closure (cadr e) (cddr e) env))
    (else #f)))

(define (emit-closure lab free-vars env)
  (emit-new-heap-val
    ($ (* word-size (+ 2 (length free-vars))))
    ($ closure-tag)
    (list ($ (length free-vars))
	  ($ (expect-true lab (lookup lab env) "function not found")))))

(define (emit-funcall e si env)
  (emit-push-all-to-stack (cdr e) (- si word-size) env) ; save the args
  (emit-expr (car e) si env)
  (emit* ('addl ($ (+ si word-size)) '%esp) ; neg size of current stack (and ret addr)
	 ('subl ($ closure-tag) '%eax)
	 ('addl ($ word-size) '%eax)
	 ('call '*\(%eax\))
	 ('subl ($ (+ si word-size)) '%esp))) ; add back size of current stack (and ret addr)

(define (emit-expr e si env)
  (cond ((immediate? e)
	 (emit-immediate e))
	((variable? e)
	 (emit 'movl (stack-pos (lookup! e env)) '%eax))
	((let? e)
	 (emit-let (cadr e) (caddr e) si env))
	((funcall? e)
	 (if (eq? 'funcall (car e))
	   (emit-funcall (cdr e) si env)
	   (emit-primcall e si env)))
	(else ; shouldn't be reached
	  (error "invalid expression " e))))

(define (emit-let bindings body si env)
  (let f ((b* bindings) (new-env env) (si si))
    (cond
      ((null? b*) (emit-expr body si new-env))
      (else
	(let ((b (car b*)))
	  (emit-expr (cadr b) si new-env)
	  (emit 'movl '%eax (stack-pos si))
	  (f (cdr b*)
	     (push-var (car b) si new-env)
	     (- si word-size)))))))

;;;

(define (mapi f l)
  (let r ((i 0) (l l)) (cond
     ((null? l) '())
     (else
       (cons (f (car l) i)
	     (r (+ 1 i) (cdr l)))))))

(define (make-env bindings) bindings)

(define (lookup var env) ; returns the stack offset of a variable in env
  (cadr (or (assoc var env) '(#f #f))))

(define (lookup! var env)
  (let ((e (lookup var env)))
    (if e
      e
      (error "undefined binding" var))))

(define (push-var var si env)
  `((,var ,si) . ,env))

(define (stack-pos si)
  (cond
    ((symbol? si) (symbol-append '$ si)) ; label
    ((zero? si) (error "0(%esp) can't be set"))
    (else
      (conc si "(%esp)"))))

(define (immediate? v) ; literal value that fits in one word (1, #\a, #t, '())
  (or (integer? v) (char? v) (boolean? v) (null? v)))

(define (variable? v)
  (symbol? v))

(define (funcall? e) ; a function call
  (>= (length e) 1))

(define (let? e) ; TODO: add more syntax check here
  (eq? 'let (car e)))

(define (immediate-rep p) ; convert a lisp value to an immediate
  ($ ; TODO: convert all these arithmetic shifts to logical shifts (find the proper function call)
    (cond
      ((integer? p) ; lower two bits are 00
       (arithmetic-shift p fixnum-shift))
      ((char? p)    ; lower eight bits are 00001111
       (bitwise-ior (arithmetic-shift (char->integer p) char-shift) char-tag))
      ((boolean? p) ; lower 7 bits are 0011111
       (bitwise-ior (arithmetic-shift (if p 1 0) boolean-shift) boolean-tag))
      ((null? p)    ; lower 8 bits are 00101111
       null-tag))))

(define main-asm) ; push all the assembly into here
(define (compile-program prog)
  (with-asm-to-list
    (emit-function-header 'scheme_entry)
    (emit 'movl "4(%esp)" '%esi) ; mov heap pointer to esi
    (emit-expr prog stack-start (make-env '()))
    (emit 'ret)))
