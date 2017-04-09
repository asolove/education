#lang eopl

(require racket/match)

(provide (all-defined-out))

(define (empty-env) '())
(define (empty-env? env) (null? env))

(define (extend-env* vars vals env)
  (cons (cons vars vals) env))

(define (extend-env var val env)
  (extend-env* (list var) (list val) env))

(define (apply-env env var)
  (if (empty-env? env)
      (eopl:error `(couldn't find ,var in environment))
      (match env
        [(cons (cons vars vals) next-env)
         (apply-env-inner var vars vals next-env)])))
          
(define (apply-env-inner var vars vals next-env)
  (cond ((null? vars)
         (apply-env next-env var))
        ((eqv? var (car vars))
         (car vals))
        (else
         (apply-env-inner var (cdr vars) (cdr vals) next-env))))