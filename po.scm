(use format srfi-1)

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

;;; emit dsl

(define (uniq-label s)
  (gensym s))

(define ($ v) ; stick a "$" at the start of an immediate asm value
  (symbol-append '$ (string->symbol (->string v))))

(define (label e)
  (symbol-append e ':))

(define (emit . args)
  (set! asm-list (append asm-list `(,args))))

(define (emit-immediate e)
  (emit 'movl (immediate-rep e) '%eax))

(define (emit-cmp-eax val) ; cmp eax to val (TODO: make this more efficient)
  (emit 'movl (immediate-rep #f) '%ecx) ; move #t and #f into registers
  (emit 'movl (immediate-rep #t) '%edx)
  (emit 'cmpl val '%eax)
  (emit 'cmovzl '%edx '%ecx)
  (emit 'movl '%ecx '%eax)) ; move the result to %eax

(define (emit-push-all-to-stack l si env) ; push a list to the stack
  (if (not (null? l))
    (begin
      (emit-expr (car l) si env)
      (emit-push-to-stack si)
      (emit-push-all-to-stack (cdr l) (- si word-size) env))))

(define (emit-apply-stack op si) ; apply an operator to a stack
  (if (>= si (- word-size))
    (emit 'movl (stack-pos si) '%eax)
    (begin
      (emit-apply-stack op (+ si word-size))
      (emit op (stack-pos si) '%eax))))

(define (emit-push-to-stack si)
  (emit 'movl '%eax (stack-pos si)))

(define (emit-mask-data mask) ; leave just the type behind for type checks
  (emit 'andl mask '%eax))

(define (emit-primative e si env)
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
    ((zero?)
     (emit-expr (cadr e) si env)
     (emit-cmp-eax ($ 0)))
    ((eq?)
     (emit-expr (cadr e) si env)
     (emit 'mov '%eax '%ebx)
     (emit-expr (caddr e) si env)
     (emit-cmp-eax "%ebx"))
    ((+)
     (emit-push-all-to-stack (cdr e) si env)
     (emit-apply-stack "addl" (- (* word-size (length (cdr e))))))
    ((-)
     (emit-push-all-to-stack (cdr e) si env)
     (emit-apply-stack "subl" (- (* word-size (length (cdr e))))))
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
     (emit 'movl '%eax "4(%esi)") ; second word of esi

     (emit 'movl (stack-pos si) '%eax) ; move from stack to
     (emit 'movl '%eax "0(%esi)") ; first word of esi

     (emit 'movl '%esi '%eax)
     (emit 'orl  ($ pair-tag) '%eax) ; mark as pair
     (emit 'addl ($ double-word) '%esi)) ; bump esi forward
    ((car) ; TODO: check that type is a pair
     (emit-expr (cadr e) si env)
     (emit 'movl "-1(%eax)" '%eax))
    ((cdr) ; TODO: check that type is a pair
     (emit-expr (cadr e) si env)
     (emit 'movl "3(%eax)" '%eax))
    ((make-vector) ; TODO: add vector-set and vector-ref
     (emit-expr (cadr e) si env)
     (emit 'movl '%eax "0(%esi)") ; maybe shift 2 right (no real need for immediate)?
     (emit 'movl '%eax '%ebx)
     (emit 'movl '%esi '%eax)
     (emit 'orl ($ vector-tag) '%eax)
     (emit 'addl ($ 11) '%ebx)
     (emit 'andl ($ -8) '%ebx) ; clear out the lower 3 bits
     (emit 'addl '%ebx '%esi))
    ((vector-length)
     (emit-expr (cadr e) si env)
     (emit 'movl (string-append (->string (- vector-tag)) "(%eax)") '%eax))
    ((make-string)
     (emit-expr (cadr e) si env)
     (emit 'shr ($ fixnum-shift) '%eax) ; no need for type data - we already know it's a uint
     (emit 'movl '%eax "0(%esi)") ; and yes - you can hold up to a 4GB string
     (emit 'movl '%eax '%ebx)
     (emit 'movl '%esi '%eax)
     (emit 'orl ($ string-tag) '%eax)
     (emit 'addl ($ 11) '%ebx)
     (emit 'andl ($ -8) '%ebx) ; clear out the lower 3 bits
     (emit 'addl '%ebx '%esi))
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
     (emit 'addl '%ebx '%ecx)
     (emit 'shr  ($ char-shift) '%eax)
     (emit 'movb '%eax (string-append (->string (- (- string-tag 4))) "(%ecx)"))
     (emit 'movl '%edx '%eax))
    ((string-ref)
     (emit-expr (cadr e) si env) ; string
     (emit 'movl '%eax '%ecx)
     (emit-expr (caddr e) si env) ; index
     (emit 'shr  ($ fixnum-shift) '%eax)
     (emit 'addl '%eax '%ecx)
     (emit 'movb (string-append (->string (- (- string-tag 4))) "(%ecx)") '%ah)
     (emit 'orl  ($ char-tag) '%eax))))

(define (emit-expr e si env)
  (cond ((immediate? e)
	 (emit-immediate e))
	((variable? e)
	 (let ((v (lookup e env)))
	   (if v
	     (emit 'movl (stack-pos v) '%eax)
	     (error "undefined binding" e))))
	((let? e)
	 (emit-let (cadr e) (caddr e) si env))
	((primcall? e)
	 (emit-primative e si env))
	(else
	  (error "can't generate for expression " e))))

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

(define (make-env) '())

(define (lookup var env) ; returns the stack offset of a variable in env
  (cadr (or (assoc var env) '(#f #f))))

(define (push-var var si env)
  `((,var ,si) . ,env))

(define (stack-pos si)
  (if (zero? si) (error "0(%esp) can't be set"))
  (string-append (->string si) "(%esp)"))

(define (immediate? v) ; literal value that fits in one word (1, #\a, #t, '())
  (or (integer? v) (char? v) (boolean? v) (null? v)))

(define (variable? v)
  (symbol? v))

(define (primcall? e) ; a built in function call
  (> (length e) 1))

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

(define asm-list)
(define (compile-program p)
  (fluid-let ((asm-list '()))
	     (emit '.globl 'scheme_entry) ; boilerplate
	     (emit '.code32) ; currently only supporting x86 asm
	     (emit '.type 'scheme_entry '@function)
	     (emit (label 'scheme_entry))
	     (emit 'movl "4(%esp)" '%esi) ; mov heap pointer to esi
	     (emit-expr p stack-start (make-env))
	     (emit 'ret)
	     asm-list))
