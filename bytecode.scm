(use srfi-13)
(define *val*  'eax)
(define *env*  'ebx)
(define *fun*  'ecx)
; TODO: write internal heap manager
(define *t1*   'edx)
(define word-size 4)

(define env.init '(()))

(define num-tag  #b000)
(define nil-tag  #b001)
(define true-tag #b010)

(define cons-tag #b011)
(define str-tag  #b100)
(define sym-tag  #b101)
(define fun-tag  #b110)
(define vec-tag  #b111)

(define (tag v tag)
  (bitwise-ior (arithmetic-shift v 3) tag))

(define (untag v)
  (arithmetic-shift v -3))

(define (tagged-val v)
  (cond
    ((number? v) (tag v num-tag))
    ((null? v) (tag v nil-tag))
    ((eq? #t v) (tag v true-tag))))

;;;

(define (env-push env k)
  (cons (cons k (car env)) (cdr env)))

(define (env-new-frame env)
  (cons '() env))

(define (env-pop-frame env)
  (cdr env))

(define (env-lookup env k)
  (list-index (cut eq? <> k) (car env)))

(define (CONSTANT v)
  `(mov ,*val* ,(tagged-val v)))

; TODO:
;(define (GLOBAL-SET! i m)
;  (delay (m)
;         (global-update! i *val*)))

(define (SEQUENCE m m+)
  (append (m)
          (m+)))

(define (FIX-LET m* m+)
  (append (m*)
          ; TODO: figure out how to extend env
          ;(set! *env* (sr-extend* *env* *val*))
          (m+)
          ; TODO: bump env to next frame
          (set! *env* (stack-frame-next *env*))
          ))

(define (STORE-ARGUMENT m m* rank)
  (append (m)
          `(push ,*val*)
          (m*)
          `(pop ,*val*)))

(define (FIX-CLOSURE m+)
  (append (m+)
          ; TODO: make closure and move it to *val*
          ))

(define (REGULAR-CALL m m*)
  (append m (PUSH-VAL)
          m* (POP-FUNCTION) (PRESERVE-ENV)
          (FUNCTION-INVOKE) (RESTORE-ENV)))

(define (PUSH-VAL)
  `(push ,*val*))

(define (POP-VAL)
  `(pop ,*val*))

(define (POP-FUNCTION)
  `(pop ,*fun*))

(define (PRESERVE-ENV)
  `(push ,*env*))

(define (FUNCTION-INVOKE)
  `(call ,*fun*))

(define (RESTORE-ENV)
  `(pop ,*env*))

(define (JMP l)
  `(jmp ,l))

(define (MALLOC n)
  `((push ,n)
    (call malloc)))

(define (CONS a b)
  `(,@(MALLOC (* 2 word-size))
    (mov ,*t1* ,*val*)
    ,a
    (mov (,*t1*) ,*val*)
    ;`(mov ,(+ addr word-size) ,b) ; TODO: pack b in lower portion
    ))

(define (CMP-NIL r)
  ; TODO: cmp nil tag against reg r
  '())

(define (JMP-FALSE l)
  (CMP-NIL *val*)
  `(je ,l))

(define (ALTERNATIVE con t e)
  (let ((le (gensym 'else)) (ld (gensym 'd)))
    (append
      con
      (JMP-FALSE le)
      t (JMP ld)
      le e
      ld)))

(define (print-instr i)
  (cond
    ((string? i) (print i ":"))
    ((display "\t")
     (display (car i))
     (display " ")
     (print (string-join (map ->string (cdr i)) ",")))))

(print-instr '("global _start"))
(print-instr '("extern malloc"))
(print-instr '("extern exit"))
(print-instr "_start")
(map print-instr (CONS (CONSTANT 1) (CONSTANT 2)))

(print-instr `(push 0))
(print-instr '(call exit))
