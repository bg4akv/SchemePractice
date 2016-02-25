#lang racket

(define id (lambda (x) (+ 1 x)))



(call/cc (lambda (c) (c 1)))
(call/cc (lambda (c) (+ (c 1) 1)))
(define cont (call/cc (lambda (c) (c 9))))


(define cc #f)
(+ 1 (call/cc (lambda (k) (set! cc k) 100)))

(cc 2)
