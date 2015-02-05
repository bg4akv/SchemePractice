#lang racket

(define atom?
  (lambda (x)
    (and (not (null? x)) (not (pair? x)))))

(define lat?
  (lambda (l)
    (cond 
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))


(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? o1 (car lat)) (eq? o2 (car lat)))
        (cons new (cdr lat)))
      (else
        (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
        (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else
        (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
        (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else
        (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
        (cons new (multisubst new old (cdr lat))))
      (else
        (cons (car lat) (multisubst new old (cdr lat)))))))

(define add1
  (lambda (x) (+ x 1)))

(define sub1
  (lambda (x) (- x 1)))

(define plus
  (lambda (x y)
    (cond 
      ((zero? y) x)
      (else (add1 (plus x (sub1 y)))))))

(define minus
  (lambda (x y)
    (cond 
      ((zero? y) x)
      (else (sub1 (minus x (sub1 y)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (plus (car tup) (addtup (cdr tup)))))))

(define times
  (lambda (x y)
    (cond
      ((zero? y) 0)
      (else (plus x (times x (sub1 y)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (plus (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define greater? 
  (lambda (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (else (greater? (sub1 x) (sub1 y))))))

(define less? 
  (lambda (x y)
    (cond
      ((zero? y) #f)
      ((zero? x) #t)
      (else (less? (sub1 x) (sub1 y))))))

(define equal? 
  (lambda (x y)
    (cond
      ((greater? x y) #f)
      ((less? x y) #f)
      (else #t))))

(define exp
  (lambda (x y)
    (cond
      ((zero? y) 1)
      (else (times x (exp x (sub1 y)))))))

(define division
  (lambda (x y)
    (cond
      ((less? x y) 0)
      (else (add1 (division (minus x y) y))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(1 3 ))