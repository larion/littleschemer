#lang racket
; The Little Schemer ch. 4

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

(eq? (occur 1 '(0 0 2 2 2 0 799)) 0)
(eq? (occur 1 '(1 0 2 2 2 0 799)) 1)
(eq? (occur 1 '(1 0 2 1 2 1 799)) 3)
(eq? (occur 1 '(0 0 2 2 2 0 1)) 1)
(eq? (occur 1 '()) 0)


(eq? (pick 1 '(10 20 30)) 10)
(eq? (pick 2 '(10 20 30)) 20)
(eq? (pick 3 '(10 20 30)) 30)

(equal? (rempick 1 '(10 20 30)) '(20 30))
(equal? (rempick 2 '(10 20 30)) '(10 30))
(equal? (rempick 3 '(10 20 30)) '(10 20))
(equal? (rempick 1 '(10)) '())

(eq? (len '()) 0)
(eq? (len '(2 4 8)) 3)
(eq? (len '(2)) 1)

(eq? (up 10 3) 1000)
(eq? (up 200 1) 200)
(eq? (up 7 0) 1)
(eq? (up 0 100) 0)

(equ 0 0)
(equ 10 10)
(not (equ 17 3))
(not (equ 0 10))
(not (equ 10 0))

(gt 3 2)
(not (gt 2 3))
(gt 3 0)
(not (gt 0 2))

(lt 2 3)
(not (lt 3 2))
(lt 0 3)
(not (lt 2 0))

(equal? (tup+ '(1 2 3 4) '(1 2 3 4)) '(2 4 6 8))
(equal? (tup+ '(1 2) '(1 10 100 1000)) '(2 12 100 1000))
(equal? (tup+ '(1 10 100 1000) '(1 2)) '(2 12 100 1000))
(equal? (tup+ '() '()) '())

(eq? (addtup '(1 2 3 4 5)) 15)
(eq? (addtup '()) 0)

(eq? (mult 4 7) 28)
(eq? (mult 134 20) 2680)
(eq? (mult 10 0) 0)
(eq? (mult 0 10) 0)
(eq? (mult 10 1) 10)
(eq? (mult 1 10) 10)
(eq? (mult 0 0) 0)

(eq? (plus 4 7) 11)
(eq? (plus 10 0) 10)
(eq? (plus 0 10) 10)
(eq? (plus 0 0) 0)

(eq? (minus 7 4) 3)
(eq? (minus 10 0) 10)
(eq? (minus 10 10) 0)
(eq? (minus 0 0) 0)
