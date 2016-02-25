#lang racket/base

; id := λx.x
(define id (lambda (x) x))
(id 0)

; apply := λf.λx(f x)
(define apply (lambda (f) (lambda (x) (f x))))
((apply id) 1)

; self := λf.(f f)
(define self (lambda (f) (f f)))
(self id)

; T := λx.λy.x
; F := λx.λy.y
(define T (lambda (x) (lambda (y) x)))
(define F (lambda (x) (lambda (y) y)))
((T 0) 1)
((F 0) 1)

; NOT := λx.((x F) T)
(define NOT (lambda (x) ((x F) T)))
(((NOT T) 2) 3)
(((NOT F) 2) 3)

; AND := λx.λy.((x y) F)
(define AND (lambda (x) (lambda (y) ((x y) F))))
((((AND T) T) 4) 5)
((((AND F) T) 4) 5)
((((AND T) F) 4) 5)
((((AND F) F) 4) 5)

; OR := λx.λy.((x T) y)
(define OR (lambda (x) (lambda (y) ((x T) y))))
((((OR T) T) 6) 7)
((((OR F) T) 6) 7)
((((OR T) F) 6) 7)
((((OR F) F) 6) 7)


(define COND (lambda (x) (lambda (y) (lambda (z) ((x y) z)))))
(((COND T) 8) 9)
(((COND F) 8) 9)




(define succ (lambda (n) (lambda (f) (lambda (x) (f ((n f) x))))))
(define zero (lambda (f) (lambda (x) x)))

(define one   (succ zero))
(define two   (succ one))
(define three (succ two))
(define four  (succ three))
(define five  (succ four))
(define six   (succ five))
(define seven (succ six))
(define eight (succ seven))
(define nine  (succ eight))


(define iszero (lambda (x) (x T)))


;(Y f)
;= (f (Y f))
;= (f (f (Y f)))
;= ...
;(define Y (lambda (f) (f (Y f))))


;Y := λf.(λx.(f (x x)) λx.(f (x x)))
;(Y g)
;= (λf.(λx.(f (x x)) λx.(f (x x))) g)
;= (λx.(g (x x)) λx.(g (x x)))
;= (g (λx.(g (x x))  λx.(g (x x)))) 
;= (g (Y g))

(define Y (lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x))))))





