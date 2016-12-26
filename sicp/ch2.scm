#lang racket

; Tree data structure
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)

; Set operations
(define (element-of-set? set x)
  (cond ((null? set) #f)
        ((= (entry set) x) #t)
        ((> (entry set) x) (element-of-set? (left-branch set) x))
        (else (element-of-set? (right-branch set) x))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= (entry set) x) set)
        ((> (entry set) x) (make-tree
                            (entry set)
                            (adjoin-set x (left-branch set))
                            (right-branch set)))
        (else (make-tree (entry set)
                         (left-branch set)
                         (adjoin-set x (right-branch set))))))
