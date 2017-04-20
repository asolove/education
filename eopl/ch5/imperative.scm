#lang eopl

(require (only-in racket/base list-ref))
(require (only-in racket/list index-of))

(require "./imperative-syntax.scm")

;; Imperative interpreter

(define identifier? symbol?)

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
   (proc proc?)))

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

; Interpreter registers
(define exp '())
(define env '())
(define cont '())
(define val '())
(define proc1 '())

; Proc data type
(define (proc? value)
  (procedure? value))

(define (procedure var body saved-env)
  (lambda ()
    (set! exp body)
    (set! env (extend-env var val env))
    (value-of/k)))

(define (apply-procedure/k)
  (proc1))

; Environment data type
(define (init-env)
  (extend-env
   'i (num-val 1)
   (extend-env
    'v (num-val 5)
    (extend-env
     'x (num-val 10)
     (empty-env)))))

; Continuation data type
(define-datatype continuation continuation?
  (end-cont)
  (diff-cont1
   (e2 expression?)
   (env environment?)
   (k continuation?))
  (diff-cont2
   (v1 expval?)
   (k continuation?))
  (zero?-cont
   (k continuation?))
  (if-cont
   (if-true expression?)
   (if-false expression?)
   (env environment?)
   (k continuation?))
  (let-cont
   (var identifier?)
   (body expression?)
   (env environment?)
   (k continuation?))
  (call-cont
   (arg-exp expression?)
   (env environment?)
   (k continuation?))
  (call-cont2
   (val1 expval?)
   (k continuation?)))
         
  

(define (apply-cont)
  (cases continuation cont
    (end-cont ()
              (display "End of computation")
              val)
    (diff-cont1 (e2 saved-env k)
                (set! exp e2)
                (set! env saved-env)
                (set! cont (diff-cont2 val k))
                (value-of/k))
                
    (diff-cont2 (v1 saved-cont)
                (set! val (num-val (- (expval->num v1) (expval->num val))))
                (set! cont saved-cont)
                (apply-cont))
    (zero?-cont (k)
                (set! val (bool-val (= 0 (expval->num val))))
                (set! cont k)
                (apply-cont))
    (if-cont (if-true if-false saved-env k)
             (set! exp (if (expval->bool val) if-true if-false))
             (set! env saved-env)
             (set! cont k)
             (value-of/k))
    (let-cont (var body saved-env k)
              (set! exp body)
              (set! env (extend-env var val saved-env))
              (set! cont k)
              (value-of/k))
    (call-cont (arg-exp saved-env k)
                 (set! exp arg-exp)
                 (set! env saved-env)
                 (set! cont (call-cont2 val k))
                 (value-of/k))
    (call-cont2 (proc k)
                (set! proc1 (expval->proc proc))
                (set! cont k)
                (apply-procedure/k))))

; Interpreter
(define (run string)
  (value-of-program (scan&parse string)))

(define (value-of-program prog)
  (cases program prog
    (a-program (an-exp)
               (set! exp an-exp)
               (set! env (init-env))
               (set! cont (end-cont))
               (value-of/k))))

(define (trampoline bounce)
  (if (expval? bounce)
      bounce
      (trampoline (bounce))))

(define (value-of/k)
  (cases expression exp
    (const-exp (n)
               (set! val (num-val n))
               (apply-cont))
    (diff-exp (e1 e2)
              (set! exp e1)
              (set! cont (diff-cont1 e2 env cont))
              (value-of/k))
    (zero?-exp (exp1)
               (set! exp exp1)
               (set! cont (zero?-cont cont))
               (value-of/k))
    (if-exp (cond if-true if-false)
            (set! exp cond)
            (set! cont (if-cont if-true if-false env cont))
            (value-of/k))
    (var-exp (var)
             (set! val (apply-env env var))
             (apply-cont))
    (let-exp (var exp1 body)
             (set! exp exp1)
             (set! cont (let-cont var body env cont))
             (value-of/k))
    
    (letrec-exp (p-names b-vars p-bodies letrec-body)
                (let ((new-env (extend-env-rec p-names b-vars p-bodies env)))
                  (set! exp letrec-body)
                  (set! env new-env)
                  (value-of/k)))
    
    (proc-exp (var body)
              (set! val (proc-val (procedure var body env)))
              (apply-cont))
                
    (call-exp (fn-exp arg-exp)
              (set! exp fn-exp)
              (set! cont (call-cont arg-exp env cont))
              (value-of/k))))
              


(define mutual-letrec-program
  "letrec
      even(x) = if zero?(x) then 1 else (odd -(x,1))
      odd(x) = if zero?(x) then 0 else (even -(x,1))
   in (odd 13)")