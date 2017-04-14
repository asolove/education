#lang eopl

(require (only-in racket/base list-ref))
(require (only-in racket/list index-of))

(require "./explicit-refs-syntax.scm")

;; Explicit refs language
; plus exercises
; 4.1: Reference created in each call, so no shared state between calls
; 4.8, 4.10: begin
; 4.13: multiple-arg procs

; Environment data type with support for letrec
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val expval?)
   (env environment?))
  (extend-env*
   (vars (list-of symbol?))
   (vals (list-of expval?))
   (env environment?))
  (extend-env-rec
   (p-names (list-of symbol?))
   (b-vars (list-of symbol?))
   (p-bodies (list-of expression?))
   (env environment?)))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
                 (eopl:error `(Could not find ,search-var in environment)))
      (extend-env (var val rest-env)
                  (if (eqv? var search-var)
                      val
                      (apply-env rest-env search-var)))
      (extend-env* (vars vals rest-env)
                   (let ((i (index-of vars search-var)))
                     (if i
                         (list-ref vals i)
                         (apply-env rest-env search-var))))
      (extend-env-rec (p-names b-vars p-bodies saved-env)
                      (let ((p-index (index-of p-names search-var)))
                        (if p-index
                            (proc-val (procedure (list-ref b-vars p-index)
                                                 (list-ref p-bodies p-index)
                                                 env))
                            (apply-env saved-env search-var)))))))
           

; ExpVal data type
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (proc-val
   (proc proc?))
  (ref-val
   (r reference?)))

(define (expval->num val)
  (cases expval val
    (num-val (n) n)
    (else (eopl:error `(trying to get number from incompatible value ,val)))))

(define (expval->bool val)
  (cases expval val
    (bool-val (b) b)
    (else (eopl:error `(trying to get boolean from incompatible value, val)))))

(define (expval->proc val)
  (cases expval val
    (proc-val (p) p)
    (else (eopl:error '(trying to get proc from incompatible value ,val)))))

(define (expval->ref val)
  (cases expval val
    (ref-val (r) r)
    (else (eopl:error `(trying to get ref from incompatible value, val)))))

; Proc data type
(define (proc? value)
  (procedure? value))

(define (procedure vars body env)
  (lambda (vals)
    (value-of body (extend-env* vars vals env))))

(define (apply-procedure proc vals)
  (proc vals))

; Store data type
(define (empty-store) '())

(define (get-store) the-store)

(define the-store '())

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v) (integer? v))

(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val)))
    next-ref))

(define (deref ref)
  (list-ref the-store ref))

(define (setref! ref val)
  (set! the-store
        (letrec ((setref-inner
                  (lambda (store1 ref1)
                    (cond
                      ((null? store1)
                       (eopl:error `(Invalid reference ,ref1 in ,the-store)))
                      ((zero? ref1)
                       (cons val (cdr store1)))
                      (else
                       (cond (car store1)
                             (setref-inner  (cdr store1) (- ref1 1))))))))
          (setref-inner the-store ref))))




; Environment data type
(define (init-env)
  (extend-env
   'i (num-val 1)
   (extend-env
    'v (num-val 5)
    (extend-env
     'x (num-val 10)
     (empty-env)))))

; Interpreter
(define (run string)
  (value-of-program (scan&parse string)))

(define (value-of-program prog)
  (initialize-store!)
  (cases program prog
    (a-program (exp)
               (value-of exp (init-env)))))

(define (value-of exp env)
  (cases expression exp
    (const-exp (n) (num-val n))
    (diff-exp (e1 e2)
              (num-val (- (expval->num (value-of e1 env))
                          (expval->num (value-of e2 env)))))
    (zero?-exp (exp)
               (bool-val (= 0 (expval->num (value-of exp env)))))

    (if-exp (cond if-true if-false)
            (if (expval->bool (value-of cond env))
                (value-of if-true env)
                (value-of if-false env)))
    (var-exp (var)
             (apply-env env var))
    (let-exp (var expr body)
             (let ((new-env (extend-env var (value-of expr env) env)))
               (value-of body new-env)))
    
    (letrec-exp (p-names b-vars p-bodies letrec-body)
                (let ((new-env (extend-env-rec p-names b-vars p-bodies env)))
                  (value-of letrec-body new-env)))
    
    (proc-exp (vars body)
              (proc-val (procedure vars body env)))
                
                
    (call-exp (fn-exp arg-exps)
              (let ((proc (expval->proc (value-of fn-exp env)))
                    (args (map (lambda (exp)
                                 (value-of exp env))
                               arg-exps)))
                (apply-procedure proc args)))

    (begin-exp (first-exp rest-exps)
               (car (reverse (map (lambda (expr)
                                    (value-of expr env))
                                  (cons first-exp rest-exps)))))

    (newref-exp (val)
                (ref-val (newref (value-of val env))))
    
    (deref-exp (ref)
               (deref (expval->ref (value-of ref env))))
    (setref-exp (ref val)
                (setref!
                 (expval->ref (value-of ref env))
                 (value-of val env)))))


(define ref-program
  "let x = newref(0)
     in begin
        setref(x, 2);
        deref(x)
     end")

(define multi-arg-ref-program
  "let x = newref(20)
     in let decrRef = proc (ref amount) setref(ref, -(deref(ref), amount))
        in begin (decrRef x 10); deref(x) end")
