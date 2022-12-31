#lang br/quicklang

(define-macro (bf-module-begin PARSE-TREE)
  #'(#%module-begin
     PARSE-TREE))
(provide (rename-out [bf-module-begin #%module-begin]))

(define-macro (bf-program OP-OR-LOOP-ARG ...)
  #'(void OP-OR-LOOP-ARG ...))
(provide bf-program)

(define-macro (bf-loop "[" OP-OR-LOOP-ARG ... "]")
  #'(until (zero? (current-byte))
           OP-OR-LOOP-ARG ...))
(provide bf-loop)

(define-macro-cases bf-op
  [(bf-op ">") #'(inc-ptr!)]
  [(bf-op "<") #'(dec-ptr!)]
  [(bf-op "+") #'(inc-val!)]
  [(bf-op "-") #'(dec-val!)]
  [(bf-op ".") #'(put-char!)]
  [(bf-op ",") #'(get-char!)])
(provide bf-op)

(define ptr 0)
(define memory (make-vector 30000 0))

(define (current-byte) (vector-ref memory ptr))
(define (set-current-byte! val) (vector-set! memory ptr val))

(define (inc-ptr!)
  (set! ptr (+ ptr 1)))

(define (dec-ptr!)
  (set! ptr (- ptr 1)))

(define (inc-val!)
  (set-current-byte! (+ (current-byte) 1)))
  

(define (dec-val!)
  (set-current-byte! (- (current-byte) 1)))

(define (put-char!)
  (write-byte (current-byte)))

(define (get-char!)
  (set-current-byte! (read-byte)))

(provide get-char!)