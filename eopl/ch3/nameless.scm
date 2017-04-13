#lang eopl

(require (only-in racket/base list-ref))
(require (only-in racket/list index-of))

(require "./nameless-syntax.scm")

;; Nameless transformation of letrec language
; + Ex 3.40: transform letrec

; Static environment for translation
(define (empty-senv) '())

(define (extend-senv var letrec? senv)
  (cons (cons var letrec?) senv))

; Tries to find var in the list of vars in senv
; if found, returns its index and whether it was bound by letrec
; senv x var -> (n, letrec?) | error
(define (apply-senv senv var)
  (apply-senv-n senv var 0))

(define (apply-senv-n senv var n)
  (cond ((null? senv) (eopl:error `(Unable to find mvar in environment)))
        ((eqv? var (caar senv)) (cons n (cdar senv)))
        (else (apply-senv-n (cdr senv) var (+ n 1)))))

(define (init-senv)
  (extend-senv 'i #f (extend-senv 'v #f (extend-senv 'x #f (empty-senv)))))


; Indexed environment
(define (env? e) (pair? e))
(define (extend-env val env)
  (cons val env))
(define (apply-env n env)
  (if (zero? n)
      (car env)
      (apply-env (- n 1) (cdr env))))
(define (init-env)
  (list (num-val 2) (num-val 5) (num-val 10)))

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
    (else (eopl:error `(trying to get proc from incompatible value ,val)))))

; Proc data type
(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-env env?)))

(define (apply-procedure a-proc val)
  (cases proc a-proc
    (procedure (body saved-env)
               (value-of body (extend-env val saved-env)))))

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
             (let* ((binding (apply-senv senv var))
                    (n (car binding))
                    (letrec? (cdr binding)))
               (if letrec?
                   (nameless-letrec-var-exp n)
                   (nameless-var-exp n))))
    
    (let-exp (var expr body)
             (nameless-let-exp (translation-of expr senv)
                               (translation-of body (extend-senv var #f senv))))
    (letrec-exp (var fn-arg p-body letrec-body)
                (nameless-letrec-exp
                 (translation-of p-body (extend-senv fn-arg #f
                                                     (extend-senv var #t senv)))
                 (translation-of letrec-body (extend-senv var #t senv))))

    (proc-exp (var body)
              (nameless-proc-exp
               (translation-of body (extend-senv var #f senv))))
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
    (nameless-letrec-var-exp (n)
                             (cases proc (expval->proc (apply-env n env))
                               (procedure (p-body saved-env)
                                          (proc-val (procedure
                                                     p-body
                                                     (extend-env (apply-env n env)
                                                      saved-env))))))
    
    (nameless-let-exp (val body)
                      (value-of
                       body
                       (extend-env (value-of val env) env)))
    (nameless-letrec-exp (p-body letrec-body)
                         (value-of
                          letrec-body
                          (extend-env (proc-val (procedure p-body env)) env)))
                                   
    (nameless-proc-exp (body)
                       (proc-val (procedure body env)))
    (call-exp (fn-exp arg-exp)
              (let ((proc (expval->proc (value-of fn-exp env)))
                    (arg (value-of arg-exp env)))
                (apply-procedure proc arg)))
    (else (eopl:error `(Invalid expr ,exp in value-of)))))


(define letrec-program
  "letrec plus (x) = proc (y)
                       if zero?(x) then y else ((plus -(x,1)) -(y,-(0,1)))
     in ((plus 2) 2)")