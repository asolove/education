;; Ex 2.29
(define (make-mobile left right)
  (list left right))

(define mobile-left car)
(define mobile-right cadr)

(define (make-branch length structure)
  (list length structure))
(define branch-length car)
(define branch-structure cadr)

(define (total-weight s)
  (if (number? s)
      s
      (+ (total-weight (branch-structure (mobile-left s)))
         (total-weight (branch-structure (mobile-right s))))))

;; Is mobile balanced? Returns #f if not, or total weight if it balances
(define (balanced? s)
  (if (number? s)
      s
      (let ((lw (balanced? (branch-structure (mobile-left s))))
            (rw (balanced? (branch-structure (mobile-right s)))))
        (and lw
             rw
             (eq? (* (branch-length (mobile-left s)) lw)
                  (* (branch-length (mobile-right s)) rw))
             (+ lw rw)))))

(define mobile-example
  (make-mobile
   (make-branch 10 1)
   (make-branch 2
                (make-mobile (make-branch 2 3)
                             (make-branch 3 2)))))


;; Ex 2.31
(define (tree-map f tree)
  (cond ((null? tree) '())
        ((not (pair? (car tree))) (cons (f (car tree)) (tree-map f (cdr tree))))
        (else (cons (tree-map f (car tree)) (tree-map f (cdr tree))))))

;;Tree data structure
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)

;; Set operations
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
