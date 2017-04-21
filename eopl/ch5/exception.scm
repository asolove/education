#lang eopl

(require (only-in racket/base list-ref))
(require (only-in racket/list index-of))

(require "./exception-syntax.scm")

;; Exceptions

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
  (raise-cont
   (k cont?))
  (try-cont
   (var identifier?)
   (exp expression?)
   (env environment?)
   (k cont?)))
        
(define (apply-cont k val)
  (cases cont k
    (end-cont ()
              (display "End of computation")
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
    (try-cont (var exp env k)
              (apply-cont k val))
    (raise-cont (k)
                (exec-catch-cont k val))))

(define (exec-catch-cont k val)
  (cases cont k
    (end-cont ()
              (eopl:error "Exception reached the top level: program dead."))
    (diff-cont1 (_ _2 k)
                (exec-catch-cont k val))
    (diff-cont2 (_ k)
                (exec-catch-cont k val))
    (zero?-cont (k)
                (exec-catch-cont k val))
    (if-cont (_ _2 _3 k)
             (exec-catch-cont k val))
    (let-cont (_ _2 _3 k)
              (exec-catch-cont k val))
    (call-cont (_ _2 k)
               (exec-catch-cont k val))
    (eval-arg-cont (_ _2 _3 _4 k)
                   (exec-catch-cont k val))
    (raise-cont (k)
                (exec-catch-cont k val))
    (try-cont (var exp env k)
              (value-of/k exp (extend-env var val env) k))))
    

; Interpreter
(define (run string)
  (value-of-program (scan&parse string)))

(define (value-of-program prog)
  (cases program prog
    (a-program (exp) (value-of/k exp (init-env) (end-cont)))))

(define (value-of/k exp env k)
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
    (raise-exp (exp)
               (value-of/k exp env (raise-cont k)))
    (try-exp (body-exp var catch-exp)
             (value-of/k body-exp env
                         (try-cont var catch-exp env k)))))
              


(define try-raise-program
  "let raiseX = proc (x) raise(x)
      in let y = 1
      in try (raiseX 2) catch (v) -(v,y)")