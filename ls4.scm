#lang racket
; The Little Schemer ch. 4

(require rackunit)
(provide up addtup tup+ pick rempick occur)

(define plus
  (lambda (x y)
    (cond
      ((zero? y)
        x)
      (else
        (plus (add1 x) (sub1 y))))))

(define minus
  (lambda (x y)
    (cond
      ((zero? y)
        x)
      (else
        (minus (sub1 x) (sub1 y))))))

(define mult
  (lambda (x y)
    (cond
      ((zero? y) 0)
      (else (plus x (mult x (sub1 y))))
    )))

(define lt
  (lambda (x y)
    (cond
      ((zero? y) #f)
      ((zero? x) #t)
      (else (lt (sub1 x) (sub1 y)))
    )
))

(define gt
  (lambda (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (else (gt (sub1 x) (sub1 y)))
    )
))

(define equ
  (lambda (x y)
    (not (or (gt x y) (lt x y)))))

(define up
  (lambda (x y)
    (cond
      ((zero? y) 1)
      (else (mult x (up x (sub1 y))))
    )
  )
)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
        (plus (car tup) (addtup (cdr tup)))
      )
    )))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
        (cons
          (plus (car tup1) (car tup2))
          (tup+ (cdr tup1) (cdr tup2)))
      )
    )))

(define len
  (lambda (lat)
    (if (null? lat)
      0
      (add1 (len (cdr lat))))))

(define pick
  (lambda (n lat)
    (if (= n 1)
      (car lat)
      (pick (sub1 n) (cdr lat))
    )
))

(define rempick
  (lambda (n lat)
    (if (= n 1)
      (cdr lat)
      (cons
        (car lat)
        (rempick (sub1 n) (cdr lat))
      )
    )
))

(define occur
  (lambda (x lat)
    (cond
      ((null? lat) 0)
      ((equal? (car lat) x)
       (add1 (occur x (cdr lat))))
      (else (occur x (cdr lat)))
    )
))

; unit tests

(check-equal? (occur 1 '(0 0 2 2 2 0 799)) 0)
(check-equal? (occur 1 '(1 0 2 2 2 0 799)) 1)
(check-equal? (occur 1 '(1 0 2 1 2 1 799)) 3)
(check-equal? (occur 1 '(0 0 2 2 2 0 1)) 1)
(check-equal? (occur 1 '()) 0)


(check-equal? (pick 1 '(10 20 30)) 10)
(check-equal? (pick 2 '(10 20 30)) 20)
(check-equal? (pick 3 '(10 20 30)) 30)

(check-equal? (rempick 1 '(10 20 30)) '(20 30))
(check-equal? (rempick 2 '(10 20 30)) '(10 30))
(check-equal? (rempick 3 '(10 20 30)) '(10 20))
(check-equal? (rempick 1 '(10)) '())

(check-equal? (len '()) 0)
(check-equal? (len '(2 4 8)) 3)
(check-equal? (len '(2)) 1)

(check-equal? (up 10 3) 1000)
(check-equal? (up 200 1) 200)
(check-equal? (up 7 0) 1)
(check-equal? (up 0 100) 0)

(check-true (equ 0 0))
(check-true (equ 10 10))
(check-false (equ 17 3))
(check-false (equ 0 10))
(check-false (equ 10 0))

(check-true (gt 3 2))
(check-false (gt 2 3))
(check-true (gt 3 0))
(check-false (gt 0 2))

(check-true (lt 2 3))
(check-false (lt 3 2))
(check-true (lt 0 3))
(check-false (lt 2 0))

(check-equal? (tup+ '(1 2 3 4) '(1 2 3 4)) '(2 4 6 8))
(check-equal? (tup+ '(1 2) '(1 10 100 1000)) '(2 12 100 1000))
(check-equal? (tup+ '(1 10 100 1000) '(1 2)) '(2 12 100 1000))
(check-equal? (tup+ '() '()) '())

(check-equal? (addtup '(1 2 3 4 5)) 15)
(check-equal? (addtup '()) 0)

(check-equal? (mult 4 7) 28)
(check-equal? (mult 134 20) 2680)
(check-equal? (mult 10 0) 0)
(check-equal? (mult 0 10) 0)
(check-equal? (mult 10 1) 10)
(check-equal? (mult 1 10) 10)
(check-equal? (mult 0 0) 0)

(check-equal? (plus 4 7) 11)
(check-equal? (plus 10 0) 10)
(check-equal? (plus 0 10) 10)
(check-equal? (plus 0 0) 0)

(check-equal? (minus 7 4) 3)
(check-equal? (minus 10 0) 10)
(check-equal? (minus 10 10) 0)
(check-equal? (minus 0 0) 0)
