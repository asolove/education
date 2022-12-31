#lang br/quicklang
(require "parser.rkt")
(require brag/support)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (define module-datum `(module bf-mod "expander.rkt"
                          ,parse-tree))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define (make-tokenizer port)
  (define (next-token)
    (define bf-lexer
      (lexer
       [(char-set "<>-+,.[]") lexeme]
       [any-char (next-token)]))
    (bf-lexer port))
  next-token)