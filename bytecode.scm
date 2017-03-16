(use srfi-1 srfi-13 srfi-69)
(define *val*  'eax)
(define *env*  'ebx)
(define *fun*  'ecx)
(define *t1*   'edx)
(define word-size  4)
(define short-size 2)

(define num-tag  #b000)
(define nil-tag  #b001)
(define cons-tag #b010)
(define str-tag  #b011)
(define sym-tag  #b100)
(define vec-tag  #b101)
(define fun-tag  #b110)

(define (with-index f)
  (let ((i 0))
    (lambda (e)
      (set! i (add1 i))
      (f e i))))

(define (tag v tag)
  (bitwise-ior (arithmetic-shift v 3) tag))

(define (tsize t e)
  (symbol-append t e))

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

(define (CONST v)
  `(mov ,*val* ,(tagged-val v)))

(define (GLOBAL-SET! i m)
  (let ((sym (global-ref i)))
    `(,m
       (mov ,(deref sym) ,*val*))))

(define (GLOBAL-REF i)
  `((mov ,*val* ,(deref sym))))

(define (SEQUENCE m m+)
  (append (m)
          (m+)))

(define (PUSH v)
  `((push ,v)))

(define (POP v)
  `((pop ,v)))

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

(define (CCALL f . args)
  `(,@(append-map PUSH args)
    (extern ,f)
    (call ,f)
    ,@(concatenate (make-list (length args) (POP *t1*)))
    ))

(define (ALTERNATIVE con t e)
  (let ((le (gensym 'else)) (ld (gensym 'd)))
    (append
      con
      (JMP-FALSE le)
      t (JMP ld)
      le e
      ld)))

(define (STRING s)
  `(,@(MALLOC (+ short-size (string-length s)))
     (mov ,*t1* ,*val*)
     (mov ,(tsize 'word (deref *t1*)) ,(string-length s))
     ,@(map (with-index
              (lambda (e i)
                `(mov ,(tsize 'byte (deref *t1* '+ (+ short-size i))) ,(char->integer e))))
            (string->list s))))

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
(print-instr "_start")
(map print-instr
     (append
       (GLOBAL-SET! 'a (CONST 45))
       (CONS (CONST 45) (CONST 2))
       (CAR *val*)
       (CCALL 'putchar *val*)
       (STRING "blahblahblah")
       `((inc ,*val*))
       `((inc ,*val*))
       `((inc ,*val*))
       (CCALL 'puts *val*)))

(map print-instr (CCALL 'exit 0))

(print "section .data")
(for-each (lambda (l)
            (print (conc (hash-table-ref global-vars l) " " 'dw " " 0)))
          (hash-table-keys global-vars))
