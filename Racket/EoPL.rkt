#lang racket

(define zero
  (lambda () 0))
(define is-zero?
  (lambda (n) (zero? n)))
(define successor
  (lambda (n) (+ n 1)))
(define predecessor
  (lambda (n) (- n 1)))



(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

(define apply-env
  (lambda (env search-var)
    (cond
      [(eqv? (car env) 'empty-env)
       (report-no-binding-found search-var)]
      [(eqv? (car env) 'extend-env)
       (let ([saved-var (cadr env)]
             [saved-val (caddr env)]
             [saved-env (cadddr env)])
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var)))]
      [else
       (report-invalid-env env)])))

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (error 'apply-env "Bad environment: ~s" env)))
