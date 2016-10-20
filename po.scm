(use format srfi-1)

(define word-size 4)
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

;;; emit dsl

(define (uniq-label s)
  (->string (gensym s)))

(define (asm-literal v) ; stick a "$" at the start of an immediate asm value
  (string-append "$" (->string v)))

(define (emit-label e)
  (display (string-append (->string e) ":\n")))

(define (evaluate-arg e) ; emits either the literal symbol or the unquoted value
  (if (list? e)
    (if (eq? 'unquote (car e))
      (cadr e))
    (->string e)))

(define-syntax emit ; magic emit macro - uses `,` for unquote, otherwise places the literal value
  (er-macro-transformer
    (lambda (e r c)
      (let ((op (cadr e)) (rest (cddr e)))
	`(display
	   (string-append "\t" ,(evaluate-arg op) " "
			  ,(if rest
			     `(string-join ,(append '(list) (map evaluate-arg rest)) ", "))
			  "\n"))))))

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
  (asm-literal ; TODO: convert all these arithmetic shifts to logical shifts (find the proper function call)
    (cond
      ((integer? p) ; lower two bits are 00
       (arithmetic-shift p fixnum-shift))
      ((char? p)    ; lower eight bits are 00001111
       (bitwise-ior (arithmetic-shift (char->integer p) char-shift) char-tag))
      ((boolean? p) ; lower 7 bits are 0011111
       (bitwise-ior (arithmetic-shift (if p 1 0) boolean-shift) boolean-tag))
      ((null? p)    ; lower 8 bits are 00101111
       null-tag))))

(define (emit-immediate e)
  (emit movl ,(immediate-rep e) %eax))

(define (emit-cmp-eax val) ; cmp eax to val (TODO: make this more efficient)
  (emit mov ,(immediate-rep #f) %ecx) ; move #t and #f into registers
  (emit mov ,(immediate-rep #t) %edx)
  (emit cmpl ,val %eax)
  (emit cmovzl %edx %ecx)
  (emit mov %ecx %eax)) ; move the result to %eax

(define (emit-push-to-stack l si env) ; push a list to the stack
  (if (not (null? l))
    (begin
      (emit-expr (car l) si env)
      (emit movl %eax ,(stack-pos si)) ; push value onto stack
      (emit-push-to-stack (cdr l) (- si word-size) env))))

(define (emit-apply-stack op si) ; apply an operator to a stack
  (if (>= si (- word-size))
    (emit movl ,(stack-pos si) %eax)
    (begin
      (emit-apply-stack op (+ si word-size))
      (emit ,op ,(stack-pos si) %eax))))

(define (emit-mask-data mask) ; leave just the type behind for type checks
  (emit andl ,mask %eax))

(define (emit-primative e si env)
  (case (car e)
    ((add1)
     (emit-expr (cadr e) si env)
     (emit addl ,(immediate-rep 1) %eax))
    ((sub1)
     (emit-expr (cadr e) si env)
     (emit subl ,(immediate-rep 1) %eax))
    ((integer->char)
     (emit-expr (cadr e) si env)
     (emit shl ,(asm-literal (- char-shift fixnum-shift)) %eax)
     (emit orl ,(asm-literal char-tag) %eax))
    ((char->integer)
     (emit-expr (cadr e) si env)
     (emit shr ,(asm-literal (- char-shift fixnum-shift)) %eax))
    ((null?)
     (emit-expr (cadr e) si env)
     (emit-cmp-eax (asm-literal null-tag)))
    ((integer?)
     (emit-expr (cadr e) si env)
     (emit-mask-data (asm-literal fixnum-mask))
     (emit-cmp-eax (asm-literal fixnum-tag)))
    ((boolean?)
     (emit-expr (cadr e) si env)
     (emit-mask-data (asm-literal boolean-mask))
     (emit-cmp-eax (asm-literal boolean-tag)))
    ((zero?)
     (emit-expr (cadr e) si env)
     (emit-cmp-eax (asm-literal 0)))
    ((eq?)
     (emit-expr (cadr e) si env)
     (emit mov %eax %ebx)
     (emit-expr (caddr e) si env)
     (emit-cmp-eax "%ebx"))
    ((+)
     (emit-push-to-stack (cdr e) si env)
     (emit-apply-stack "addl" (- (* word-size (length (cdr e))))))
    ((-)
     (emit-push-to-stack (cdr e) si env)
     (emit-apply-stack "subl" (- (* word-size (length (cdr e))))))
    ((if)
     (emit-if (cadr e) (caddr e) (cadddr e) si env))))

(define (emit-expr e si env)
  (cond ((immediate? e)
	 (emit-immediate e))
	((variable? e)
	 (let ((v (lookup e env)))
	   (if v
	     (emit movl ,(stack-pos v) %eax)
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
	  (emit movl %eax ,(stack-pos si))
	  (f (cdr b*)
	     (push-var (car b) si new-env)
	     (- si word-size)))))))

(define (emit-if test then-expr else-expr si env)
  (let ((L0 (uniq-label 'if)) (L1 (uniq-label 'else)))
    (emit-expr test si env)
    (emit cmpl ,(immediate-rep #f) %eax)
    (emit je ,L0)
    (emit-expr then-expr si env)
    (emit jmp ,L1)
    (emit-label L0)
    (emit-expr else-expr si env)
    (emit-label L1)))

(define (compile-program p)
  (with-output-to-string
    (lambda ()
      (emit .globl scheme_entry) ; boilerplate
      (emit .code32) ; currently only supporting x86 asm
      (emit .type scheme_entry @function)
      (emit-label 'scheme_entry)
      (emit movl "8(%esp)" %esi) ; mov heap pointer to esi
      (emit-expr p stack-start (make-env))
      (emit ret))))
