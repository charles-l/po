(use srfi-1)

(define env.init '())
(define env.global env.init)

(define-syntax definitial
  (syntax-rules ()
                ((definitial name)
                 (begin (set! env.global (cons (cons 'name 'void) env.global))
                        'name))
                ((definitial name value)
                 (begin (set! env.global (cons (cons 'name value) env.global))
                        'name))))

(define-syntax defprimitive
  (syntax-rules ()
                ((defprimitive name value arity)
                 (definitial name
                             (lambda (vals env)
                               (if (= arity (length vals))
                                 (apply value vals)
                                 (error "incorrect arity" (list 'name vals))))))))

(define (evlis exps env)
  (cond
    ((null? exps) '())
    (else (cons (c:eval (car exps) env)
                (evlis (cdr exps) env)))))

(define (eprogn exps env)
  (cond ((null? exps) '())
        ((pair? (cdr exps))
         (begin (c:eval (car exps) env)
                (eprogn (cdr exps) env)))
        (else
          (c:eval (car exps) env))))

(define (c:apply fn args env)
  (if (procedure? fn)
    (fn args env)
    (error "not a function" fn)))

(define (make-lambda vars body env)
  (lambda (vals current.env)
    (eprogn body (extend env vars vals))))

(define (make-closure fun env)
  (lambda (vals current.env)
    (fun vals env)))

(define (lookup id env)
  (cond
    ((assoc id env) => cdr)
    (else (error "no such binding" id))))

(define (extend env vars vals)
  (append
    (map (lambda (v d)
           (cons v d))
         vars vals)
    env))

(define (update! id env val)
  (cond
    ((assoc id env) => (lambda (v)
                         (set-cdr! v val)))
    (else
      (error "no such binding" id))))

(define (c:eval e env)
  (if (atom? e)
    (cond
      ((symbol? e) (lookup e env))
      ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
       e)
      (else (error "cannot eval" e)))
    (case (car e)
      ((quote) (cadr e))
      ((if) (if (c:eval (cadr e) env)
              (c:eval (caddr e) env)
              (c:eval (cadddr e) env)))
      ((begin) (eprogn (cdr e) env))
      ((set!) (update! (cadr e) env (c:eval (caddr e) env)))
      ((closure)
       (let* ((f   (cadr e))
              (fun (make-lambda (cadr f) (cddr f) env)))
         (make-closure fun env)))
      ((lambda) (make-lambda (cadr e) (cddr e) env))
      ((letrec)
       (let ((new-env (extend env
                              (map car (cadr e))
                              (map (lambda (binding) 'void)
                                   (cadr e)))))
         (map (lambda (binding)
                (update! (car binding)
                         new-env
                         (c:eval (cadr binding) new-env)))
              (cadr e))
         (eprogn (cddr e) new-env)))
      (else (c:apply (c:eval (car e) env)
                     (evlis (cdr e) env)
                     env)))))

(define (test e expr)
  (let ((t (c:eval expr env.global)))
    (if (eq? e t)
      (display ".")
      (begin (newline)
             (print "failed on (eq? " e " " t ")")))))


(definitial foo)
(definitial bar)
(definitial a)

(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive eq? eq? 2)
(defprimitive < < 2)

(newline)
(test 1 1)
(test #\a #\a)
(test 'a ''a)
(test #t #t)
(test 'b '(if #f 'a 'b))
(test 'a '(if #t 'a 'b))
(test 3 '(begin (set! a 3) a))
(test 'a '((lambda (a) a) 'a))
(test 1 '((closure (lambda (a) a) x y z) 1))
(test 9 '(letrec ((x 9) (y 2) (z x))
           x))
(test 8 '(letrec ((x 8) (y 2) (z x))
           x y z))
(test 2 '(letrec ((x (lambda (y)
                       (+ y 1))))
           (x 1)))
(test 9 '((letrec ((x 9))
            (closure (lambda () x) x))))
(newline)
