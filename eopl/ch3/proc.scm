#lang eopl

(require (only-in racket/base
                  identifier?))
(require "./env.scm")
(require "./proc-syntax.scm")

;; Proc Language
; Plus exercises...
;   3.21 multiple arguments
;   3.23 Y combinator for recusion

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

(define (procedure vars body env)
  (lambda (vals)
    (value-of body (extend-env* vars vals env))))

(define (apply-procedure proc vals)
  (proc vals))

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
    (let-exp (vars exprs body)
             (let ((new-env (extend-env*
                             vars
                             (map (lambda (expr)
                                    (value-of expr env))
                                  exprs)
                             env)))
               (value-of body new-env)))
    (proc-exp (vars body)
              (proc-val (procedure vars body env)))
    (call-exp (fn-exp arg-exps)
              (let ((proc (expval->proc (value-of fn-exp env)))
                    (args (map (lambda (exp)
                                 (value-of exp env))
                               arg-exps)))
                (apply-procedure proc args)))))

(define times-program
  "let maketimes = proc (maker)
                     proc (x y)
                       if zero?(x)
                        then 0
                        else -(((maker maker) -(x, 1) y), -(0, y))
     in let times = proc (x y) ((maketimes maketimes) x y)
       in (times 2 3)")

(define fact-program
  "let maketimes = proc (maker)
                     proc (x y)
                       if zero?(x)
                        then 0
                        else -(((maker maker) -(x, 1) y), -(0, y))
     in let times = proc (x y) ((maketimes maketimes) x y)
     in let makefact = proc (maker)
                proc(x)
                  if zero?(x) then 1 else (times ((maker maker) -(x,1)) x)
     in let fact = proc(x) ((makefact makefact) x)
     in (fact 10)")