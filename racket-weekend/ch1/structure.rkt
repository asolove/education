#lang racket

(define (welcome num-days)
  (displayln (format "This is a ~a-day workshop" num-days)))

(module+ main
         (welcome 2))
