(use srfi-1 srfi-13)

(define word-size 4)
(define double-word (* 2 word-size))

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

(define %immed 	 '%eax) ; immediate register
(define %stack 	 '%esp) ; stack pointer
(define %heap 	 '%esi) ; heap pointer
(define %closure '%edi) ; closure pointer

(define saveable-regs (list %closure))

(define (->symbol a)
  (string->symbol (->string a)))

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

(define (reg-call s) ; oddity of x86 syntax
  (symbol-append '* s))

(define (deref v #!optional offset)
  (symbol-append (->symbol (if offset offset 0)) '\( v '\)))

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
  (emit 'movl (immediate-rep e) %immed))

(define (emit-cmp-eax val) ; cmp eax to val (TODO: make this more efficient)
  (emit* ('movl (immediate-rep #f) '%ecx) ; move #t and #f into registers
	 ('movl (immediate-rep #t) '%edx)
	 ('cmpl val %immed)
	 ('cmovzl '%edx '%ecx)
	 ('movl '%ecx %immed))) ; move the result to %eax

(define (emit-new-heap-val size tag initials)
  (mapi (lambda (e i)
	  (emit* ('movl e %immed)
		 ('movl %immed (deref %heap (* i word-size)))))
	initials)
  (emit* ('movl %heap  %immed)
	 ('orl  tag    %immed)
	 ('addl size   %heap) ; advance %esi
	 ('andl ($ -8) %heap))) ; alignment 8 bytes

; TODO: I don't like this function. Probably worth removing at some point.
(define (emit-push-all-to-stack l si env) ; push a list to the stack (with emit-expr for every elem)
  (if (not (null? l))
    (begin
      (emit-expr (car l) si env)
      (emit-push-to-stack si %immed)
      (emit-push-all-to-stack (cdr l) (- si word-size) env))
    (+ si word-size))) ; return ending stack pos

; apply an operator to a stack (IN REVERSE ORDER!!)
(define (emit-apply-stack op si-start n)
  (if (= n 1)
    (emit 'movl (deref %stack si-start) %immed)
    (begin
      (emit-apply-stack op (+ si-start word-size) (sub1 n))
      (emit op (deref %stack si-start) %immed))))

(define (emit-push-to-stack si . vars)
  (mapi (lambda (v i)
	  (emit 'movl v (deref %stack (- si (* word-size i)))))
	vars))

(define (emit-pop-from-stack si . vars)
  (mapi (lambda (v i)
	  (emit 'movl (deref %stack (- si (* word-size i))) v))
	vars))

(define (emit-mask-data mask) ; leave just the type behind for type checks
  (emit 'andl mask %immed))

(define (emit-function-header name)
  (emit* ('.text)
	 ('.global name)
	 ('.type name '@function)
	 ('.align 8)
	 ((label name))))

; TODO: refactor this
(define (emit-code fmls frees body si env)
  (let ((code-id (uniq-label 'L)) (n (length fmls)))
    (set! main-asm
      (append (with-asm-to-list
		(emit-function-header code-id)
		(let f ((fmls fmls)
			(si (- word-size))
			(env (append env (map
					   (lambda (f i)
					     (list f 'free (* word-size i)))
					   frees
					   (iota (length frees) 2)))))
		  (cond
		    ((null? fmls)
		     ; emit every sexp in the body
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
  (define (emit-arg-expr n)
    (emit-expr (list-ref e n) si env))
  (case (car e)
    ((add1)
     (emit-arg-expr 1)
     (emit 'addl (immediate-rep 1) %immed))
    ((sub1)
     (emit-arg-expr 1)
     (emit 'subl (immediate-rep 1) %immed))
    ((integer->char)
     (emit-arg-expr 1)
     (emit 'shl ($ (- char-shift fixnum-shift)) %immed)
     (emit 'orl ($ char-tag) %immed))
    ((char->integer)
     (emit-arg-expr 1)
     (emit 'shr ($ (- char-shift fixnum-shift)) %immed))
    ((null?)
     (emit-arg-expr 1)
     (emit-cmp-eax ($ null-tag)))
    ((integer?)
     (emit-arg-expr 1)
     (emit-mask-data ($ fixnum-mask))
     (emit-cmp-eax ($ fixnum-tag)))
    ((boolean?)
     (emit-arg-expr 1)
     (emit-mask-data ($ boolean-mask))
     (emit-cmp-eax ($ boolean-tag)))
    ((procedure?)
     (emit-arg-expr 1)
     (emit-mask-data ($ heap-mask))
     (emit-cmp-eax ($ closure-tag)))
    ((zero?)
     (emit-arg-expr 1)
     (emit-cmp-eax ($ 0)))
    ((car) ; TODO: check that type is a pair
     (emit-arg-expr 1)
     (emit 'movl (deref %immed -1) %immed))
    ((cdr) ; TODO: check that type is a pair
     (emit-arg-expr 1)
     (emit 'movl (deref %immed 3) %immed))
    ((eq?)
     (emit-arg-expr 1)
     (emit 'mov %immed '%ebx)
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
       (emit-arg-expr 1)
       (emit 'cmpl (immediate-rep #f) %immed)
       (emit 'je L0)
       (emit-arg-expr 2)
       (emit 'jmp L1)
       (emit (label L0))
       (emit-arg-expr 3)
       (emit (label L1))))
    ((cons)
     (emit-arg-expr 1) ; compile sub exprs first
     (emit-push-to-stack si %immed) ; and push scratch to stack
     (emit-expr (caddr e) (- si word-size) env)
     (emit* ('movl %immed (deref %heap 4)) ; second word of esi
	    ('movl (deref %stack si) %immed) ; move from stack to
	    ('movl %immed (deref %heap)) ; first word of esi
	    ('movl %heap %immed)
	    ('orl  ($ pair-tag) %immed) ; mark as pair
	    ('addl ($ double-word) %heap))) ; bump esi forward
    ((make-vector) ; TODO: add vector-set and vector-ref and type check
     (emit-arg-expr 1)
     (emit* ('shr ($ fixnum-shift) %immed) ; no need for type data - we already know it's a uint
	    ('imul ($ word-size) %immed) ; each element is sizeof(word)
	    ('movl %immed '%ebx)) ; backup %eax
     (emit-new-heap-val '%ebx ($ vector-tag) '(%ebx)))
    ((vector-length)
     (emit-arg-expr 1)
     (emit 'movl (deref %immed (- vector-tag)) %immed))
    ((make-string)
     (emit-arg-expr 1)
     (emit 'shr ($ fixnum-shift) %immed) ; no need for type data - we already know it's a uint
     (emit 'movl %immed '%ebx) ; backup %eax
     (emit-new-heap-val '%ebx ($ string-tag) '(%ebx)))
    ((string-length)
     (emit-arg-expr 1)
     (emit 'movl (deref %immed (- string-tag)) %immed)
     (emit 'shl  ($ fixnum-shift) %immed)) ; shift back to typed fixnum
    ((string-set!)
     (emit-arg-expr 1)
     (emit 'movl %immed '%ecx) ; string
     (emit 'movl %immed '%edx) ; save original string ptr for later
     (emit-arg-expr 2)
     (emit 'movl %immed '%ebx) ; index
     (emit 'shr  ($ fixnum-shift) '%ebx)
     (emit-arg-expr 3)
     (emit* ('addl '%ebx '%ecx)
	    ('shr  ($ char-shift) %immed)
	    ('movb '%al (deref '%ecx (- (- string-tag 4)))) ; TODO: fix warning for this
	    ('movl '%edx %immed)))
    ((string-ref)
     (emit-arg-expr 1) ; string
     (emit 'movl %immed '%ecx)
     (emit-arg-expr 2) ; string
     (emit* ('shr  ($ fixnum-shift) %immed)
	    ('addl %immed '%ecx)
	    ('movb (deref '%ecx (- (- string-tag 4))) '%ah)
	    ('orl  ($ char-tag) %immed)))
    ((labels)
     (emit-expr (caddr e)
		si
		(let l ((bindings (cadr e)) (env (make-env '())))
		  (if (null? bindings)
		    env
		    (l (cdr bindings)
		       (push-var (caar bindings)
				 (emit-code
				   (cadr  (cadar bindings))
				   (caddr (cadar bindings))
				   (cdddr (cadar bindings))
				   si env) ; TODO: fix the env here (i.e. remove other locals)
				 env))))))
    ((closure)
     (emit-closure (cadr e) (cddr e) env))
    (else #f)))

(define (make-frees lvars)
  (map (cut list <> 'free <>) lvars (iota (length lvars) 1)))

(define (emit-closure lab free-vars env)
  (emit-new-heap-val
    ($ (* word-size (+ 2 (length free-vars))))
    ($ closure-tag)
    `(,($ (length free-vars))
      ,($ (lookup! lab env))
      ,@(map (cut lookup! <> env) free-vars))))

(define (save-regs si)
  (apply emit-push-to-stack si saveable-regs)
  (emit 'addl ($ (- si (* word-size (length saveable-regs)))) %stack))

(define (restore-regs si)
  (emit 'subl ($ (- si (* word-size (length saveable-regs)))) %stack)
  (apply emit-pop-from-stack si saveable-regs))

(define (emit-funcall e si env)
  (save-regs si)
  (let ((si (- word-size))) ; leave space for return point
    (emit-push-all-to-stack (cdr e)
			    (- si word-size) ; need to hop over return slot
			    env) ; save the args
    (emit-expr (car e) si env)
    (emit* ('subl ($ closure-tag) %immed)
	   ('movl %immed %closure) ; set the current closure addr
	   ('addl ($ word-size) %immed)
	   ('call (reg-call (deref %immed)))))
  (restore-regs si))

(define (emit-expr e si env)
  (cond ((immediate? e)
	 (emit-immediate e))
	((variable? e)
	 (emit 'movl (lookup! e env) %immed))
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
	  (emit 'movl %immed (deref %stack si))
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

(define (lookup var env) ; returns the memory location of a binding
  (let ((var (assoc var env)))
    (cond
      ((not var) #f) ; failed lookup
      ((eq? (cadr var) 'free)
       (deref %closure (caddr var)))
      ((number? (cadr var))
       (deref %stack (cadr var)))
      (else
	(cadr var)))))

(define (lookup! var env)
  (let ((e (lookup var env)))
    (if e
      e
      (error "undefined binding" var))))

(define (push-var var si env)
  `((,var ,si) . ,env))

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
    (emit 'movl ($ 0) %closure) ; null out closure pointer (main isn't a closure)
    (emit 'movl (deref %stack 4) %heap) ; mov heap pointer to esi
    (emit-expr prog (- word-size) (make-env '()))
    (emit 'ret)))
