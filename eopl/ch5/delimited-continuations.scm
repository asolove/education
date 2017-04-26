#lang eopl

(require (only-in racket/base list-ref))
(require (only-in racket/list index-of))

(require "./delimited-continuations-syntax.scm")

;; Continuation-passing interpreter
;; with language access to delimited continuations

(define debug? #t)

(define (debug form)
  (when debug?
    (begin
      (newline)
      (display form)
      (newline))))

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
                            (proc-val (procedure (list (list-ref b-vars p-index))
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

(define (procedure vars body env)
  (lambda (vals k)
    (value-of/k body (extend-env* vars vals env) k)))

(define (apply-procedure/k proc val k)
  (proc val k))

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
(define-datatype cont cont?
  (id-cont)
  (end-cont)
  (diff-cont1
   (e2 expression?)
   (env environment?)
   (k cont?))
  (diff-cont2
   (v1 expval?)
   (k cont?))
  (zero?-cont
   (k cont?))
  (if-cont
   (if-true expression?)
   (if-false expression?)
   (env environment?)
   (k cont?))
  (let-cont
   (var identifier?)
   (body expression?)
   (env environment?)
   (k cont?))
  (call-cont
   (arg-exp (list-of expression?))
   (env environment?)
   (k cont?))
  (eval-arg-cont
   (proc proc?)
   (env environment?)
   (past-args (list-of expval?))
   (rest-arg-exps (list-of expression?))
   (k cont?))
  (reset-cont
   (k cont?)))
         
  

(define (apply-cont k val)
  (debug `( () . (Apply cont ,k to ,val)))
  (cases cont k
    (id-cont ()
             val)
    (end-cont ()
              (debug "End of computation")
              val)
    (diff-cont1 (e2 env k)
                (value-of/k e2 env (diff-cont2 val k)))
    (diff-cont2 (v1 k)
                (apply-cont k (num-val (- (expval->num v1)
                                          (expval->num val)))))
    (zero?-cont (k)
                (apply-cont k (bool-val (= 0 (expval->num val)))))
    (if-cont (if-true if-false env k)
             (value-of/k
              (if (expval->bool val) if-true if-false)
              env
              k))
    (let-cont (var body env k)
              (value-of/k body (extend-env var val env) k))
    (call-cont (arg-exps env k)
               (let ((args (reverse arg-exps)))
                 (value-of/k
                  (car args)
                  env
                  (eval-arg-cont (expval->proc val) env '() (cdr args) k))))
    (eval-arg-cont (proc env past-args rest-arg-exps k)
                    (let ((args (cons val past-args)))
                      (if (null? rest-arg-exps)
                          (apply-procedure/k proc args k)
                          (value-of/k (car rest-arg-exps) env
                                      (eval-arg-cont
                                       proc args (cdr rest-arg-exps) k)))))
    (reset-cont (k)
                (apply-cont k val))))

(define (replace-nested-k k fn)
  (cases cont k
    (id-cont () k)
    (end-cont () k)
    (diff-cont1 (e2 env k)
                (diff-cont1 e2 env (fn k)))
    (diff-cont2 (v1 k)
                (diff-cont2 v1 (fn k)))
    (zero?-cont (k)
                (zero?-cont (fn k)))
    (if-cont (a b c k)
             (if-cont a b c (fn k)))
    (let-cont (a b c k)
              (let-cont a b c (fn k)))
    (call-cont (a b k)
               (call-cont a b (fn k)))
    (eval-arg-cont (a b c d k)
                   (eval-arg-cont a b c d (fn k)))
    (reset-cont (k)
                (reset-cont (fn k)))))

(define (until-last-reset k)
  (cases cont k
    (reset-cont (k)
                (id-cont))
    (else (replace-nested-k k until-last-reset))))

(define (before-last-reset k)
  (cases cont k
    (reset-cont (k) k)
    (id-cont () k)
    (end-cont () k)
    (diff-cont1 (a b k) (before-last-reset k))
    (diff-cont2 (a k) (before-last-reset k))
    (zero?-cont (k) (before-last-reset k))
    (if-cont (a b c k) (before-last-reset k))
    (let-cont (a b c k) (before-last-reset k))
    (call-cont (a b k) (before-last-reset k))
    (eval-arg-cont (a b c d k) (before-last-reset k))))
                
    

; Interpreter
(define (run string)
  (value-of-program (scan&parse string)))

(define (value-of-program prog)
  (cases program prog
    (a-program (exp) (value-of/k exp (init-env) (end-cont)))))

(define (value-of/k exp env k)
  (debug `( Getting value of ,exp))
  (cases expression exp
    (const-exp (n) (apply-cont k (num-val n)))
    (diff-exp (e1 e2)
              (value-of/k e1 env
                          (diff-cont1 e2 env k)))
    (zero?-exp (exp)
               (value-of/k exp env
                           (zero?-cont k)))
    (if-exp (cond if-true if-false)
            (value-of/k cond env
                        (if-cont if-true if-false env k)))
    (var-exp (var)
             (apply-cont k (apply-env env var)))
    (let-exp (var expr body)
             (value-of/k expr env
                         (let-cont var body env k)))
    
    (letrec-exp (p-names b-vars p-bodies letrec-body)
                (let ((new-env (extend-env-rec p-names b-vars p-bodies env)))
                  (value-of/k letrec-body new-env k)))
    
    (proc-exp (vars body)
              (apply-cont k (proc-val (procedure vars body env))))
                
    (call-exp (fn-exp arg-exps)
              (value-of/k fn-exp env (call-cont arg-exps env k)))
    (shift-exp (var body)
               (let* ((dc (until-last-reset k))
                      (dc-proc (proc-val
                                (lambda (vals k)
                                  (apply-cont k (apply-cont dc (car vals))))))
                      (k2 (before-last-reset k)))
                 (value-of/k
                  body
                  (extend-env var dc-proc env)
                  k2)))
 
    (reset-exp (body)
               (value-of/k body env (reset-cont k)))))
              


(define mutual-letrec-program
  "letrec
      even(x) = if zero?(x) then 1 else (odd -(x,1))
      odd(x) = if zero?(x) then 0 else (even -(x,1))
   in (odd 13)")

(define simple-dc-program
  "-(0, reset( -(4, shift(k => (k 1)))))")

(define dc-program
  " -(0, reset( -(4, shift(k => (k (k 1))))))")
; k x = 4 - x
; k 1 = 3
; k (k 1) = 1
; whole expression: -1