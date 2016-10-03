(use format srfi-1)

(define word-size 4)
(define stack-start (- word-size)) ; start of the stack (offset of %esp)

;; data types
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

(define (stack-pos si)
  (if (zero? si) (error "0(%esp) can't be set"))
  (string-append (->string si) "(%esp)"))

(define (immediate? v) ; literal value that fits in one word (1, #\a, #t, '())
  (or (integer? v) (char? v) (boolean? v) (null? v)))

(define (primcall? e) ; a built in function call
  (> (length e) 1))

(define-syntax emit
  (er-macro-transformer
    (lambda (e r c)
      (let ((op (cadr e) (a (caddr e)) (b (cadddr e))))
	`(string-append ,(if (any (cut eq? <> (string-ref (->string op) 1))
				  '(#\: #\.))
			   ""
			   "\t")
			,(string-append (->string op)
					(->string a) ", "
					(->string b) "\n"))))))

(define (immediate-rep p) ; convert a lisp value to an immediate
  (cond
    ((integer? p) ; lower two bits are 00
     (arithmetic-shift p fixnum-shift))
    ((char? p)    ; lower eight bits are 00001111
     (bitwise-ior (arithmetic-shift (char->integer p) char-shift) char-tag))
    ((boolean? p) ; lower 7 bits are 0011111
     (bitwise-ior (arithmetic-shift (if p 1 0) boolean-shift) boolean-tag))
    ((null? p)    ; lower 8 bits are 00101111
     null-tag)))

(define (emit-immediate e)
  (emit "movl $~a, %eax"
	(immediate-rep e)))

(define (emit-cmp-eax val) ; cmp eax to val (TODO: make this more efficient)
  (string-append
    (emit "mov $~a, %ecx" (immediate-rep #f)) ; move #t and #f into registers
    (emit "mov $~a, %edx" (immediate-rep #t))
    (emit "cmp $~a, %eax" val)
    (emit "cmovzl %edx, %ecx")
    (emit "mov %ecx, %eax"))) ; move the result to %eax

(define (emit-push-to-stack l si) ; push a list to the stack
  (if (null? l)
    ""
    (string-append
      (emit-expr (car l) si)
      (emit "movl %eax, ~a" ; push that variable onto the stack
	    (stack-pos si))
      (emit-push-to-stack (cdr l) (- si word-size)))))

(define (emit-apply-stack op si) ; apply an operator to a stack
  (if (>= si (- word-size))
    (emit "movl ~a, %eax" (stack-pos si))
    (string-append
      (emit-apply-stack op (+ si word-size))
      (emit "~a ~a, %eax"
	    op
	    (stack-pos si)))))

(define (emit-mask-data mask) ; leave just the type behind for type checks
  (emit "andl $~a, %eax" mask))

(define (emit-primative e si)
  (case (car e)
    ((add1)
     (string-append
       (emit-expr (cadr e) si)
       (emit "addl $~a, %eax" (immediate-rep 1))))
    ((sub1)
     (string-append
       (emit-expr (cadr e) si)
       (emit "subl $~a, %eax" (immediate-rep 1))))
    ((integer->char)
     (string-append
       (emit-expr (cadr e) si)
       (emit "shl $~a, %eax" (- char-shift fixnum-shift))
       (emit "orl $~a, %eax" char-tag)))
    ((char->integer)
     (string-append
       (emit-expr (cadr e) si)
       (emit "shr $~a, %eax" (- char-shift fixnum-shift))))
    ((null?)
     (string-append
       (emit-expr (cadr e) si)
       (emit-cmp-eax null-tag)))
    ((integer?)
     (string-append
       (emit-expr (cadr e) si)
       (emit-mask-data fixnum-mask)
       (emit-cmp-eax fixnum-tag)))
    ((boolean?)
     (string-append
       (emit-expr (cadr e) si)
       (emit-mask-data boolean-mask)
       (emit-cmp-eax boolean-tag)))
    ((zero?)
     (string-append
       (emit-expr (cadr e) si)
       (emit-cmp-eax 0)))
    ((+)
     (string-append
       (emit-push-to-stack (cdr e) si)
       (emit-apply-stack "addl" (- (* word-size (length (cdr e)))))))
    ((-)
     (string-append
       (emit-push-to-stack (cdr e) si)
       (emit-apply-stack "subl" (- (* word-size (length (cdr e)))))))))

(define (emit-expr e si)
  (cond ((immediate? e)
	 (emit-immediate e))
	((primcall? e)
	 (emit-primative e si))
	(else
	  (error "can't generate for expression " e))))

(define (compile-program p)
  (string-append
    (emit ".globl scheme_entry") ; boilerplate
    (emit ".code32") ; currently only supporting x86 asm
    (emit ".type scheme_entry, @function")
    (emit "scheme_entry:")
    (emit-expr p stack-start)
    (emit "ret")))
