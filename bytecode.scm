(use srfi-1 srfi-13 srfi-69)
(define *val*  'eax)
(define *env*  'ebx)
(define *fun*  'ecx)
(define *t1*   'edx)
(define *t2*   'edi)
(define *sp*   'esp)
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
      (let ((r (f e i)))
        (set! i (add1 i))
        r))))

(define (logn x n)
  (inexact->exact (/ (log x) (log n))))

(define (tag v tag)
  (bitwise-ior (arithmetic-shift v 3) tag))

(define (tsize t e)
  (symbol-append t e))

(define (deref . e)
  (symbol-append '\[ (string->symbol (string-join (map ->string e))) '\]))

(define (untag v)
  (arithmetic-shift v -3))

(define (register? r)
  (any (cut eq? r <>) (list *val* *env* *fun* *t1* *sp*)))

(define (reg-or-val v)
  (if (register? v)
    v
    (tagged-val v)))

(define (tagged-val v)
  (cond
    ((number? v) (tag v num-tag))
    ((null? v) (tag 0 nil-tag))
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

(define (_CONST v)
  `((mov ,*val* ,v)))

(define (CONST v)
  (_CONST (reg-or-val v)))

(define (SYMBOL s)
  `(,@(STRING (symbol->string s))
     (sub ,*val* 3) ; untag *val* so C has a real pointer
     ,@(CCALL 'st_get_or_set *val*)))

(define (GLOBAL-SET! i m)
  (let ((sym (global-ref i)))
    `(,m
       (mov ,(deref sym) ,*val*))))

(define (GLOBAL-REF i)
  `((mov ,*val* ,(deref i))))

(define (PUSH v)
  (_PUSH (reg-or-val v)))

(define (_PUSH v)
  `((push ,v)))

(define (POP v)
  `((pop ,v)))

(define (STORE-ARGUMENT m m* rank)
  (append (m)
          `(PUSH ,*val*)
          (m*)
          `(POP ,*val*)))

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
  (CCALL 'malloc n))

(define (CONS) ; cons top two stack values
  `(,@(MALLOC (* 2 word-size))
     ,@(POP *t1*) ; car
     ,@(POP *t2*) ; cdr
     (mov ,(deref *val*) ,*t1*)
     (mov ,(deref *val* '+ word-size) ,*t2*)
     (or  ,*val* ,cons-tag)))

(define (CMP-NIL r)
  ; TODO: cmp nil tag against reg r
  '())

(define (JMP-FALSE l)
  `(,@(CMP-NIL *val*)
     (je ,l)))

(define (CAR)
  ; TODO: assert type
  `((mov ,*val* ,(deref *val* '- cons-tag))))

(define (CDR)
  ; TODO: assert type
  `((mov ,*val* ,(deref *val* '+ (- word-size cons-tag)))))

(define (CCALL f . args)
  `(,@(append-map (lambda (i)
                    (cond
                      ((list? i)
                       (append i (_PUSH *val*)))
                      (else (_PUSH i))))
                  (reverse args))
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
     (mov ,(tsize 'word (deref *val*)) ,(string-length s))
     ,@(map (with-index
              (lambda (e i)
                `(mov ,(tsize 'byte (deref *val* '+ (+ short-size i)))
                      ,(char->integer e))))
            (string->list s))
     (or ,*val* ,str-tag)))

(define (VEC n)
  ; [n (h) | v1 (w) | v2 (w) ...]
  `(,@(MALLOC (+ short-size (* word-size n)))
     (mov ,(tsize 'word (deref *val*)) ,n)
     (or ,*val* ,vec-tag)))

(define (VEC-REF) ; *val* = *STACK[*val*]
  ; TODO: assert that vec ref is within arr bounds
  `((shl ,*val* ,(logn word-size 2)) ; (* *val* word-size) using bitshifts :D
    (mov ,*val* ,(deref *sp* '+ short-size '+ *val*))))

(define (VEC-SET!) ; *STACK[POP(STACK)] = *val*
  ; TODO: assert vec ref is within arr bounds
  `("vset"
    ,@(POP *t1*)
     (shl ,*t1* ,(logn word-size 2))
     (mov ,*t2* ,(deref *sp*))
     (mov ,(deref *t2* '- 5 '+ *t1*) ,*val*)))

(define (FUN-PROLOGUE)
  `((push ebp)
    (mov  ebp esp)))

(define (FUN-EPILOGUE)
  `((pop ebp)
    (ret)))

(define (print-instr i)
  (cond
    ((string? i) (print i ":"))
    ((display "\t")
     (display (car i))
     (display " ")
     (print (string-join (map ->string (cdr i)) ",")))))

(print "section .text")
(print "global po_entry")
(print "extern malloc")
(print "extern symtab")
(print-instr "po_entry")
(map print-instr
     (append
       (FUN-PROLOGUE)

       ; DIS IS ALL BORKED
       (VEC 3)
       (PUSH *val*)
       (_PUSH 2)
       (_CONST 2)
       (VEC-SET!)


       (_CONST 2)
       (VEC-REF)

       (POP *t1*)

       ;`((mov ,*t1* ,*val*))
       (CCALL 'printf (STRING " hi 0x%x\n") *val*)

       ;(SYMBOL 'blah)
       ;`((mov ,*t1* ,*val*))
       ;(CCALL 'printf (STRING " %x\n") *t1*)

       (FUN-EPILOGUE)
       ))

(map print-instr (CCALL 'exit 0))

(print "section .data")
(for-each (lambda (l)
            (print (conc (hash-table-ref global-vars l) " " 'dw " " 0)))
          (hash-table-keys global-vars))
