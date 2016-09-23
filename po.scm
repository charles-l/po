(use format srfi-1)

(define fixnum-shift 2)
(define char-shift 8)
(define char-tag 15)
(define boolean-shift 7)
(define boolean-tag 31)
(define null-tag 47)

(define (immediate? v)
  (or (integer? v) (char? v) (boolean? v) (null? v)))

(define (primcall? e)
  (= (length e) 2))

(define (emit fmt . sp) ; emit without tab
  (define label (eq? (string-ref fmt (- (string-length fmt) 1)) #\:))
  (define directive (eq? (string-ref fmt 0) #\.))
  (string-append
    (if (or label directive)
      ""
      "\t")
    (if (null? sp)
      fmt
      (apply (cut format #f fmt <>) sp))
    "\n"))

(define (immediate-rep p)
    (cond
      ((integer? p) ; lower two bits are 00
       (arithmetic-shift p fixnum-shift))
      ((char? p)    ; lower eight bits are 00001111
       (bitwise-ior (arithmetic-shift (char->integer p) char-shift) char-tag))
      ((boolean? p) ; lower 7 bits are 0011111
       (bitwise-ior (arithmetic-shift (if p 1 0) boolean-shift) boolean-tag))
      ((null? p)    ; lower 8 bits are 00101111
       null-tag)))

(define (emit-expr e)
  (cond ((immediate? e)
         (emit "movl $~a, %eax" (immediate-rep e)))
        ((primcall? e) ; call with only one argument that can fit in a single register
         (case (car e)
           ((add1)
            (string-append
              (emit-expr (cadr e))
              (emit "addl $~a, %eax" (immediate-rep 1))))
           ((sub1)
            (string-append
              (emit-expr (cadr e))
              (emit "subl $~a, %eax" (immediate-rep 1))))
           ((integer->char)
            (string-append
              (emit-expr (cadr e))
              (emit "shl $~a, %eax" 6)
              (emit "orl $~a, %eax" char-tag)))
           ((char->integer)
            (string-append
              (emit-expr (cadr e))
              (emit "shr $~a, %eax" 6)))
           ((null?)
            (string-append
              (emit-expr (cadr e))
              (emit "mov $~a, %ecx" (immediate-rep #f))
              (emit "mov $~a, %edx" (immediate-rep #t))
              (emit "cmp $~a, %eax" null-tag)
              (emit "cmovzl %edx, %ecx")
              (emit "mov %ecx, %eax")))))
        (else
          "")))

(define (compile-program p)
  (string-append
    (emit ".globl scheme_entry")
    (emit ".type scheme_entry, @function")
    (emit "scheme_entry:")
    (emit-expr p)
    (emit "ret")))

(print (compile-program `(null? ,'())))
