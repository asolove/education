#lang brag

bf-program : (bf-op | bf-loop)*
bf-op      : ">" | "<" | "+" | "-" | "." | ","
bf-loop    : "[" (bf-op | bf-loop)* "]"


; > increase pointer position by one
; < decrease pointer position by one
; + increase value of current byte
; - decrease value of current byte
; . write current byte to stdout
; , read a byte from stdin to current byte
; [ ] loop code inside until current byte is zero
