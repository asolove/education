#lang eopl

(require racket/match)
(require (only-in racket/base
                  identifier?))

;;;; 2: Data Abstraction
;;; 2.1: Specifying Data via Interfaces

; 2.3
; Diff-tree ::= (one) | (diff Diff-tree Diff-tree)
; 1. Prove infinite many representations
; Proof by contradiction
;   Assume that dt is the longest representation for number N
;   Then construct (diff-tree dt (diff-tree (one) (one)))
;   This has the value N-0=N, therefore it is a longer representation.
;   This contradicts the promise, and N has no longest representation.
; 2. 
(define (one) '(one))
(define (one? dt) (equal? dt (one)))
(define (diff-tree dt1 dt2) `(diff ,dt1 ,dt2))
(define (diff-tree-left dt) (cadr dt))
(define (diff-tree-right dt) (caddr dt))

(define (diff-tree-value dt)
  (if (one? dt) 1
      (- (diff-tree-value (diff-tree-left dt))
         (diff-tree-value (diff-tree-right dt)))))

(define (zero) (diff-tree (one) (one)))
(define (is-zero? dt)
  (equal? (diff-tree-value dt) 0))
(define (negate dt)
  (diff-tree (zero) dt))
(define (negative-one)
  (negate (one)))
(define (successor dt)
  (diff-tree dt (negative-one)))
(define (predecessor dt)
  (diff-tree dt 1))

; 3
(define (diff-tree-plus dt1 dt2)
  (diff-tree dt1 (negate dt2)))

;;; 2.2 Representation Strategies for Data Types

; The Interpreter Recipe
; 1. Look at a piece of data.
; 2. Decide what kind of data it represents.
; 3. Extract the components of the datum and do the right thing with them.

; 2.11 Ribcage environment representation
; empty-env: -> env
; extend-env: var x val x env -> env
; apply-env: env x var -> val
; extend-env* listof(var) x listof(val) x env -> env
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


;; 2.2.3 Procedural representation

;;; 2.3 Interfaces for Recursive Data Types

; 2.19 Binary tree
; Bintree::=()|(Int Bintree Bintree)
; BintreeCursor::=(Bintree Listof(Bintree))

(define (btree-leaf) '())
(define (at-leaf? btree) (null? btree))

(define (btree-node val lson rson)
  (list val lson rson))

(define (number->btree n)
  (btree-node n (btree-leaf) (btree-leaf)))

(define (current-element btree)
  (match btree
    [(list val lson rson) val]
    ('() (eopl:error `(empty btree has no element)))))

(define (insert-to-right n btree)
  (match btree
    [(list val lson rson)
     (btree-node val lson
                 (btree-node n rson (btree-leaf)))]))

(define (insert-to-left n btree)
  (match btree
    [(list val lson rson)
     (btree-node val
                 (btree-node n lson (btree-leaf))
                 rson)]))

(define (move-to-left btree)
  (match btree
    [(list val lson rson) lson]
    ['() (eopl:error `(Can't move to left in empty tree))]))

(define (move-to-right btree)
  (match btree
    [(list val lson rson) rson]
    ['() (eopl:error `(Can't move to left in empty tree))]))

; 2.20 Binary tree cursor
; btc := (Bintree, Listof(Bintree))
(define (btc-leaf parents)
  (list (btree-leaf) parents))
  
(define (btc-node val lson rson parents)
  (list (btree-node val lson rson) parents))

(define (at-btc-leaf? btc) (null? (car btc)))
 
(define (number->btc n)
  (btc-node n (btree-leaf) (btree-leaf) '()))

(define (btc-current-element btc)
  (current-element (car btc)))

(define (btc-insert-to-left n btc)
  (match btc
    [(list btree parents) (list (insert-to-left n btree) parents)]))

(define (btc-insert-to-right n btc)
  (match btc
    [(list btree parents) (list (insert-to-right n btree) parents)]))

(define (btc-move-to-left btc)
  (match btc
    [(list btree parents)
     (list (move-to-left btree) (cons btree parents))]))

(define (btc-move-to-right btc)
  (match btc
    [(list btree parents)
     (list (move-to-right btree) (cons btree parents))]))

(define (btc-move-up btc)
  (match btc
    [(list btree (cons parent ancestors))
     (list parent ancestors)]))

(define (btc-at-root? btc)
  (match btc
    [(list btree parents) (null? parents)]))

;;; 2.4 A Tool for Defining Recursive Data Types

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define-datatype s-list s-list?
  (an-s-list
   (sexps (list-of s-exp?))))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))


(define-datatype s-exp s-exp?
  (symbol-s-exp
   (sym symbol?))
  (s-list-s-exp
   (slst s-list?)))
