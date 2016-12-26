(define (square x) (* x x))

(define (my-abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

;;; 1.1.7 Square roots by Newton's method

(define (sqrt-iter guess x prev-guess)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter (improve guess x)
                 x
                 guess)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess prev-guess)
  (< (/ (abs (- guess prev-guess)) guess) 0.001))

(define (my-sqrt x)
  (sqrt-iter 1.0 x 0))

;;; Ex 1.6: infinite loop by always evaluating recursive case

;;; Ex 1.7: (my-sqrt (square 0.001)) => 0.03
;;;         (my-sqrt (square (square 10000000)) => nope
;;; Changes made in function defs above.

;; Chapter 1.2: procedures and the processes they generate

;;; Ex 1.9 a is recursive, b is iterative
;;; Ex 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;;; f x = x * 2
(define (f n) (A 0 n))

;;; g x = 2 ^ x
(define (g n) (A 1 n))

;;; h x = 2 ^ h(x-1)
(define (h n) (A 2 n))

;; 1.2.2 Tree recursion

;;; counting coins

(define (ways amount coins)
  (let ((calls 0))
    (define (helper amount coins)
      (set! calls (+ calls 1))
      (cond ((< amount 0) 0)
            ((= amount 0) 1)
            ((null? coins) 0)
            (else (+ (helper amount (cdr coins))
                     (helper (- amount (car coins)) coins)))))
    (let ((answer (helper amount coins)))
      (display calls)
      (display "\n")
      answer)))

(define coins '(50 25 10 5 1))

;;; Ex 1.11

(define (f-r n)
  (if (< n 3)
      n
      (+ (f-r (- n 1))
         (* 2 (f-r (- n 2)))
         (* 3 (f-r (- n 3))))))

(define (f-i n)
  (define (helper step a b c)
    (if (= step n)
        a
        (helper (+ step 1)
                b c (+ c (* 2 b) (* 3 a)))))
  (helper 0 0 1 2))

;;; Ex 1.12 Pascal's triangle
(define (pascal row col)
  (cond ((= row 0) 1)
        ((= col 0) 1)
        ((= col row) 1)
        (else (+ (pascal (- row 1) (- col 1))
                 (pascal (- row 1) col)))))

;;; 1.2.3 Orders of growth
;;; Ex 1.14  (ways x coins) calls: 10: 41, 20: 151, 30: 391, 40: 837 exponential!
;;;           Memory usage is linear to x

;;; Ex 1.15

(define (my-sin x)
  (if (< (abs x) 0.1)
      x
      (let ((y (my-sin (/ x 3))))
        (- (* 3 y)
           (* 4 (expt y 3))))))

;; growth in space: logarithmic. Number of steps: logarithmic

;; Ex 1.16: iterative fast exponent using invariant & accumulator

(define (my-expt b n)
  (define (helper b n a)
    (cond ((= n 0) a)
          ((odd? n) (helper b (- n 1) (* a b)))
          (else (helper (square b) (/ n 2) a))))
  (helper b n 1))

