#lang scheme

; 1.11
; subst always recurs on smaller pieces,
; subst-in-s-exp recurs on the same-size piece.
; So each pass through the mutual recursion results in a smaller problem.

; 1.12
(define (subst new old slist)
  (cond ((null? slist) '())
        ((cons? slist)
         (cons (subst new old (car slist))
               (subst new old (cdr slist))))
        ((eq? slist old) new)
        (else slist)))

; 1.13
(define (subst-with-map new old slist)
  (map (lambda (sexp) (subst-sexp-with-map new old sexp)) slist))

(define (subst-sexp-with-map new old sexp)
  (cond ((list? sexp) (subst-with-map new old sexp))
        ((eqv? old sexp) new)
        (else old)))

;; Rule: Follow the Grammar
;; When defining a procedure that operates on inductively defined data,
;; the structure of the program should be patterned after the structure
;; of the data.

;;; 1.3 Auxiliary procedures & context arguments

;; No Mysterious Auxiliaries!
;; When defining an auxiliary procedure, always specify what it does
;; on all arguments, not just the initial values.

; 1.16 (invert lst)
; invert : Listof(Tuple(A, B)) -> Listof(Tuple(B, A))
; usage (invert '((a 1) (b 2))) = '((1 a) (2 b))
(define (invert lst)
  (if (null? lst)
      lst
      (cons (reverse (car lst))
            (invert (cdr lst)))))

; 1.19
; list-set : Listof(Any) x Nat x Any -> Listof(Any)
; usage (list-set '(a b c d) 2 'cookie) = '(a b cookie d)
(define (list-set lst n new)
  (if (zero? n)
      (cons new (cdr lst))
      (cons (car lst)
            (list-set (cdr lst) (- n 1) new))))

; 1.21
; product : Listof(A) x Listof(b) -> ListOf(Tuple(A, B))
; usage (product '(1 2) '(a b)) = '((1 a) (1 b) (2 a) (2 b))
(define (product xs ys)
  (if (null? xs)
      '()
      (append (product-zip (car xs) ys)
              (product (cdr xs) ys))))

(define (product-zip x ys)
  (if (null? ys)
      '()
      (cons (list x (car ys))
            (product-zip x (cdr ys)))))