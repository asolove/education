#lang eopl

(provide (all-defined-out))


;; initial state copied from https://github.com/mwand/eopl3

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("proc" "(" (arbno identifier) ")" expression)
     proc-exp)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression) "in" expression)
     letrec-exp)

    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)

    (expression
     ("set" identifier "=" expression)
     assign-exp)

    (expression
     ("spawn" "(" expression ")")
     spawn-exp)

    (expression
     ("mutex()")
     mutex-exp)

    (expression
     ("wait" "(" expression ")")
     wait-exp)

    (expression
     ("signal" "(" expression ")")
     signal-exp)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
