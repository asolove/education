#lang eopl

(require (only-in racket/base
                  identifier?))
(require "./env.scm")
(require "./let-syntax.scm")

;; Let Language
; Plus exercises...
;   3.6: minus
;   3.9: cons, car, cdr, null?, empty-cons
;   3.16: let with multiple variables

; ExpVal data type
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (cons-val
   (cons (lambda (v) (or (pair? v) (null? v))))))

(define (expval->num val)
  (cases expval val
    (num-val (n) n)
    (else (eopl:error `(trying to get number from incompatible value ,val)))))

(define (expval->bool val)
  (cases expval val
    (bool-val (b) b)
    (else (eopl:error `(trying to get boolean from incompatible value, val)))))

(define (expval->cons val)
  (cases expval val
    (cons-val (c) c)
    (else (eopl:error `(trying to get cons from incompatible value ,val)))))

; Syntax data type


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
    (minus-exp (exp) (num-val (- (expval->num (value-of exp env)))))
    (diff-exp (e1 e2)
              (num-val (- (expval->num (value-of e1 env))
                          (expval->num (value-of e2 env)))))
    (zero?-exp (exp)
               (bool-val (= 0 (expval->num (value-of exp env)))))

    (cons-exp (aexp dexp)
              (cons-val
               (cons (value-of aexp env)
                     (value-of dexp env))))
    (car-exp (cons-exp)
             (car (expval->cons (value-of cons-exp env))))
    (cdr-exp (cons-exp)
             (cdr (expval->cons (value-of cons-exp env))))
    (null?-exp (cons-exp)
               (bool-val (null? (expval->cons (value-of cons-exp env)))))
    (emptylist-exp ()
                   (cons-val '()))
    
    (if-exp (cond if-true if-false)
            (if (expval->bool (value-of cond env))
                (value-of if-true env)
                (value-of if-false env)))
    (var-exp (var)
             (apply-env env var))
    (let-exp (vars exprs body)
             (let ((new-env (extend-env*
                             vars
                             (map (lambda (expr)
                                    (value-of expr env))
                                  exprs)
                             env)))
               (value-of body new-env)))))