#lang eopl

(require (only-in racket/base list-ref))
(require (only-in racket/list index-of))

(require "./letrec-syntax.scm")

;; Letrec language
; Plus ex 3.32: mutually-recursive functions

; Environment data type with support for letrec
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val expval?)
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

; Proc data type
(define (proc? value)
  (procedure? value))

(define (procedure var body env)
  (lambda (val)
    (value-of body (extend-env var val env))))

(define (apply-procedure proc val)
  (proc val))

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
  (cases program prog
    (a-program (exp) (value-of exp (init-env)))))

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
    
    (proc-exp (var body)
              (proc-val (procedure var body env)))
                
                
    (call-exp (fn-exp arg-exp)
              (let ((proc (expval->proc (value-of fn-exp env)))
                    (arg (value-of arg-exp env)))
                (apply-procedure proc arg)))))


(define mutual-letrec-program
  "letrec
      even(x) = if zero?(x) then 1 else (odd -(x,1))
      odd(x) = if zero?(x) then 0 else (even -(x,1))
   in (odd 13)")