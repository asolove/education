#lang r5rs

(define q 'quote)

(define (evaluate exp env)
  (if (atom? exp)
      (cond ((symbol? exp) (lookup exp env))
            ((or (number? exp)
                 (string? exp)
                 (char? exp)
                 (boolean? exp)
                 (vector? exp))
             exp)
            (else (wrong "Cannot evaluate" exp)))
      (case (car exp)
        ((quote) (cadr exp))
        ((if) (if (evaluate (cadr exp) env)
                  (evaluate (caddr exp) env)
                  (evaluate (cadddr exp) env)))
        ((begin) (eprogn (cdr env) env))
        ((set!) (update! (cadr exp) env (evaluate (caddr exp) env)))
        ((lambda) (make-function (cadr exp) (cddr exp) env))
        ((-) (- (evaluate (cadr exp) env)
                (evaluate (caddr exp) env)))
        (else (invoke (evaluate (car exp) env)
                       (evlis (cdr exp) env))))))

(define (update! var env val)
  (if (pair? env)
      (if (eq? (caar env) var)
          (begin (set-cdr! (car env) val)
                 val)
          (update! var (cdr env) val))
      (wrong "Variable binding doesn't exist" var)))

(define (extend env variables values)
  (cond ((pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (wrong "Too few values" variables)))
         ((null? variables)
          (if (null? values)
              env
              (wrong "Too many values" values)))
         ((symbol? variables) (cons (cons variables values) env))))
         

(define (lookup var env)
  (cond ((null? env) (wrong "Could not find in environment" var))
        ((eq? (caar env) var) (cdar env))
        (else (lookup var (cdr env)))))

(define (atom? exp)
  (not (pair? exp)))

(define (wrong message exp)
  (scheme-report-environment -1))

(define (eprogn exps env)
  (cond ((null? exps) (wrong "Empty begin"))
        ((null? (cdr exps)) (evaluate (car exps) env))
        (else (begin
                (evaluate (car exps) env)
                (eprogn (cdr exps) env)))))

(define (make-function args code env)
  (lambda (params)
    (eprogn code (extend env args params))))

(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (wrong "Not a function" fn)))

(define (evlis ls env)
  (if (null? ls)
      '()
      (cons (evaluate (car ls) env)
            (evlis (cdr ls) env))))

(display
 (evaluate '(((lambda (x z) (lambda (y) (if x y z))) #t 2) 4) '()))