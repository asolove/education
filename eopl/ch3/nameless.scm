#lang eopl

(require (only-in racket/base list-ref))
(require (only-in racket/list index-of))

(require "./nameless-syntax.scm")

;; Nameless transfomration of letrec language

; Static environment
(define (empty-senv) '())
(define (extend-senv var senv)
  (cons var senv))
(define (apply-senv senv var)
  (cond ((null? senv) (eopl:error `(Unable to find mvar in environment)))
        ((eqv? var (car senv)) 0)
        (else (+ 1 (apply-senv (cdr senv) var)))))

(define (init-senv)
  (extend-senv 'i (extend-senv 'v (extend-senv 'x (empty-senv)))))


; Indexed environment
(define (extend-env val env)
  (cons val env))
(define (apply-env n env)
  (if (zero? n)
      (car env)
      (apply-env (- n 1) (cdr env))))
(define (init-env)
  (list 2 5 10))

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

; Translation
(define (translation-of-program prog)
  (cases program prog
    (a-program (exp)
               (a-program (translation-of exp (init-senv))))))

(define (translation-of exp senv)
  (cases expression exp
    (const-exp (n) exp)
    (diff-exp (e1 e2)
              (diff-exp (translation-of e1 senv)
                        (translation-of e2 senv)))
    (zero?-exp (exp)
               (zero?-exp (translation-of exp senv)))
    (if-exp (cond if-true if-false)
            (if-exp (translation-of cond senv)
                    (translation-of if-true senv)
                    (translation-of if-false senv)))
    (var-exp (var)
             (nameless-var-exp (apply-senv senv var)))
    (let-exp (var expr body)
             (nameless-let-exp (translation-of expr senv)
                               (translation-of body (extend-senv var senv))))
    (proc-exp (var body)
              (nameless-proc-exp
               (translation-of body (extend-senv var senv))))
    (call-exp (fn-exp arg-exp)
              (call-exp
               (translation-of fn-exp senv)
               (translation-of arg-exp senv)))
    (else (eopl:error '(Invalid expression ,exp found in translation-of)))))

; Interpreter
(define (run string)
  (value-of-program (translation-of-program (scan&parse string))))

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
    (nameless-var-exp (n)
                      (apply-env n env))
    (nameless-let-exp (val body)
                      (value-of
                       body
                       (extend-env (value-of val env) env)))
    (nameless-proc-exp (body)
                       (value-of body env))
    (call-exp (fn-exp arg-exp)
              (let ((proc (expval->proc (value-of fn-exp env)))
                    (arg (value-of arg-exp env)))
                (apply-procedure proc arg)))
    (else (eopl:error `(Invalid expr ,exp in value-of)))))


(define mutual-letrec-program
  "letrec
      even(x) = if zero?(x) then 1 else (odd -(x,1))
      odd(x) = if zero?(x) then 0 else (even -(x,1))
   in (odd 13)")