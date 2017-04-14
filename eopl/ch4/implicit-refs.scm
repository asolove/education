#lang eopl

(require (only-in racket/base list-ref))
(require (only-in racket/list index-of))

(require "./implicit-refs-syntax.scm")

;; Implicit refs language
; and exercises:
; 4.15 those are indexes into the store, not expressed values
; 4.17 multiarg procs, let
; 4.18 multiarg letrec

; Environment data type with support for letrec
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val reference?)
   (env environment?))
  (extend-env*
   (vars (list-of symbol?))
   (vals (list-of reference?))
   (env environment?))
  (extend-env-rec
   (p-names (list-of symbol?))
   (b-varss (list-of (list-of symbol?)))
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
      (extend-env-rec (p-names b-varss p-bodies saved-env)
                      (let ((p-index (index-of p-names search-var)))
                        (if p-index
                            (newref
                             (proc-val (procedure (list-ref b-varss p-index)
                                                  (list-ref p-bodies p-index)
                                                  env)))
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
  (lambda (vals)
    (value-of body (extend-env* vars (map newref vals) env))))

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
  (empty-env))

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
             (deref (apply-env env var)))
    (let-exp (vars exprs body)
             (let* ((refs (map (lambda (expr)
                                 (newref (value-of expr env))) exprs))
                    (new-env (extend-env* vars refs env)))
               (value-of body new-env)))
    
    (letrec-exp (p-names b-varss p-bodies letrec-body)
                (let ((new-env (extend-env-rec p-names b-varss p-bodies env)))
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

    (assign-exp (var val)
                (begin
                  (setref! (apply-env env var)
                           (value-of val env))
                  (num-val 42)))
    
    ))


(define implicit-refs-program
  "       let x = 0
             in letrec even()
                        = if zero?(x)
                          then 1
                          else begin
                                set x = -(x,1);
                                (odd) end
                       odd()
                        = if zero?(x)
                          then 0
                          else begin
                                set x = -(x,1);
                                (even)
                               end
                 in begin set x = 13; (odd) end")

(define implicit-refs-program2
  "          let g = let count = 0
                     in proc (dummy)
                         begin
                          set count = -(count,-1);
                          count
                         end
             in let a = (g 11)
                in let b = (g 11)
                   in -(a, b)")

