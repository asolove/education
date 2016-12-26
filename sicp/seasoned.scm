
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define sub1
  (lambda (x)
    (- x 1)))

(define add1
  (lambda (x)
    (+ x 1)))

;;; Chapter 11
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (or (is-first? (car lat) (cdr lat))
                (two-in-a-row? (cdr lat)))))))

(define is-first?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (eq? (car lat) a)))))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? preceding (car lat))
                (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond ((null? lat) #f)
          (else (two-in-a-row-b? (car lat) (cdr lat))))))

(define sum-of-prefixes-helper
  (lambda (tup sum)
    (cond ((null? tup) '())
          (else (let ((sum (+ sum (car tup))))
                  (cons sum (sum-of-prefixes-helper (cdr tup) sum)))))))
                  
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-helper tup 0)))

(define (nth n l)
  (if (or (> n (length l)) (< n 0))
    (error "Index out of bounds.")
    (if (eq? n 0)
      (car l)
      (nth (- n 1) (cdr l)))))

(define scramble-helper
  (lambda (tup prev-tup)
    (cond ((null? tup) '())
          (else (let ((prev-tup (cons (car tup) prev-tup)))
                  (cons (nth (sub1 (car tup)) prev-tup)
                        (scramble-helper (cdr tup)
                                         prev-tup)))))))

(define scramble
  (lambda (tup)
    (scramble-helper tup '())))

;;; Chapter 12
(define Y
  (lambda (fn)
    ((lambda (f) (f f))
     (lambda (f)
       (fn (lambda (x) ((f f) x)))))))

(define my-length
  (Y (lambda (length)
       (lambda (lat)
         (if (null? lat)
               0
               (+ 1 (length (cdr lat))))))))

;;; Chapter 13
(define intersect
  (lambda (set1 set2)
    (cond ((null? set2) '())
          ((member? (car set2) set1)
           (cons (car set2) (intersect set1 (cdr set2))))
          (else (intersect set1 (cdr set2))))))
