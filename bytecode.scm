(use srfi-13 srfi-69)
(define *val*  'eax)
(define *env*  'ebx)
(define *fun*  'ecx)
(define *t1*   'edx)
(define word-size 4)

(define num-tag  #b000)
(define nil-tag  #b001)
(define cons-tag #b010)
(define str-tag  #b011)
(define sym-tag  #b100)
(define vec-tag  #b101)
(define fun-tag  #b110)

(define (tag v tag)
  (bitwise-ior (arithmetic-shift v 3) tag))

(define (deref . e)
  (symbol-append '\[ (string->symbol (string-join (map ->string e))) '\]))

(define (untag v)
  (arithmetic-shift v -3))

(define (tagged-val v)
  (cond
    ((number? v) (tag v num-tag))
    ((null? v) (tag v nil-tag))
    ;((eq? #t v) (tag v true-tag))
    ))

;;;

(define env.init '(()))

(define (env-push env k)
  (cons (cons k (car env)) (cdr env)))

(define (env-new-frame env)
  (cons '() env))

(define (env-pop-frame env)
  (cdr env))

(define (env-lookup env k)
  (list-index (cut eq? <> k) (car env)))

(define global-vars (make-hash-table))
(define (global-ref i)
  (if (hash-table-exists? global-vars i)
    (hash-table-ref global-vars i)
    (let ((s (gensym i)))
      (hash-table-set! global-vars i s)
      s)))

(define (CONSTANT v)
  `(mov ,*val* ,(tagged-val v)))

(define (GLOBAL-SET! i m)
  (let ((sym (global-ref i)))
    `(,m
       (mov ,(deref sym) ,*val*))))

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
  `(,m
     (push ,*val*)
     ,m*
     (pop ,*fun*)
     (push ,*env*)
     (call ,*fun*)
     (pop ,*env*)))

(define (JMP l)
  `(jmp ,l))

(define (MALLOC n)
  `((push ,n)
    (call malloc)))

(define (CONS a b)
  `(,@(MALLOC (* 2 word-size))
    (mov ,*t1* ,*val*)
    ,a
    (mov ,(deref *t1*) ,*val*)
    ,b
    (mov ,(deref *t1* '+ word-size) ,*val*)
    (mov ,*val* ,*t1*)
    (or  ,*val* ,cons-tag)))

(define (CMP-NIL r)
  ; TODO: cmp nil tag against reg r
  '())

(define (JMP-FALSE l)
  `(,@(CMP-NIL *val*)
     (je ,l)))

(define (CAR v)
  ; TODO: assert type
  `((mov ,v ,(deref v '- cons-tag))))

(define (CDR v)
  ; TODO: assert type
  `((mov ,v ,(deref v '+ (- word-size cons-tag)))))

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

(print "section .text")
(print "global _start")
(print "extern malloc")
(print "extern exit")
(print-instr "_start")
(map print-instr
     (append
       (GLOBAL-SET! 'a (CONSTANT 3))
       (CONS (CONSTANT 1) (CONSTANT 2))
       (CAR *val*)))

(print-instr '(push 0))
(print-instr '(call exit))

(print "section .data")
(for-each (lambda (l)
            (print (conc (hash-table-ref global-vars l) " " 'dw " " 0)))
          (hash-table-keys global-vars))
