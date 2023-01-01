#lang br
(require jsonic/parser jsonic/tokenizer brag/support)


(parse-to-datum (apply-tokenizer-maker make-tokenizer #<<HERE
"hi"
// comment
[{stuff: 2}]
HERE
                                       ))