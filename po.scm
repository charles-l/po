(use format)

(define fixnum-shift 2)
(define char-shift 8)
(define char-tag 15)
(define boolean-shift 7)
(define boolean-tag 31)
(define null-tag 47)

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

(define (compile-program p)
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

  (string-append
    (emit ".globl scheme_entry")
    (emit ".type scheme_entry, @function")
    (emit "scheme_entry:")
    (emit "movl $~a, %eax" (immediate-rep p))
    (emit "ret")))

(print (compile-program #f))
