#lang racket
; The Little Schemer ch. 1 2 3

(require "ls_base.scm" rackunit)

(provide lat? member? rember multirember firsts insertR multiinsertR insertL multiinsertL subst)

;TODO multisubst

(define lat? (lambda (lst)
    (if (null? lst)
      #t
      (and
        (atom? (car lst))
        (lat? (cdr lst))))))

(define member? (lambda (x lst)
  (cond
    ((null? lst) #f)
    (else (or (eq? (car lst) x) (member? x (cdr lst)))))))

(define rember
  (lambda (x lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) x) (cdr lst))
      (else (cons
        (car lst)
        (rember x (cdr lst)))))))

(define multirember
  (lambda (x lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) x) (multirember x (cdr lst)))
      (else
        (cons
          (car lst)
          (multirember x (cdr lst)))
      ))))

(define firsts
  (lambda (lst)
    (cond
      ((null? lst) '())
      (else (cons
        (car (car lst))
        (firsts (cdr lst)))))))

(define insertR
  (lambda (new old lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) old)
       (cons
        (car lst)
        (cons
          new
          (cdr lst))
      ))
      (else
        (cons
          (car lst)
          (insertR new old (cdr lst)))
      ))))

(define multiinsertR
  (lambda (new old lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) old)
       (cons
        (car lst)
        (cons
          new
          (multiinsertR new old (cdr lst)))
      ))
      (else
        (cons
          (car lst)
          (multiinsertR new old (cdr lst)))
      ))))

(define insertL
  (lambda (new old lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) old)
        (cons new lst))
      (else
        (cons
          (car lst)
          (insertL new old (cdr lst)))
      ))))

(define multiinsertL
  (lambda (new old lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) old)
        (cons new
           (cons old
              (multiinsertL new old (cdr lst)))))
      (else
        (cons
          (car lst)
          (multiinsertL new old (cdr lst)))
      ))))

(define subst
  (lambda (new old lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) old)
        (cons new (cdr lst)))
      (else
        (cons
          (car lst)
          (subst new old (cdr lst)))
      ))))

(define subst2
  (lambda (new old1 old2 lst)
    (cond
      ((null? lst) '())
      ((or (eq? (car lst) old1) (eq? (car lst) old2))
        (cons new (cdr lst)))
      (else
        (cons
          (car lst)
          (subst2 new old1 old2 (cdr lst)))
      ))))

; unit tests

; UT multirember
(check-equal? (multirember 1 '())
        '())
(check-equal? (multirember 1 '(2 1 3))
        '(2 3))
(check-equal? (multirember 1 '(2 1 1 1 3))
        '(2 3))
(check-equal? (multirember 1 '(2 2 1 1 1))
        '(2 2))
(check-equal? (multirember 1 '(1 1 1 2))
        '(2))
(check-equal? (multirember 100 '(100 1 100 2 100 3 100 4))
        '(1 2 3 4))
(check-equal? (multirember 100 '(1 100 2 100 3 100 4 100))
        '(1 2 3 4))

; UT subst2
(check-equal?
  (subst2 2 1 100 '(1 3 4))
  '(2 3 4))
(check-equal?
  (subst2 5 3 100 '(1 3 4))
  '(1 5 4))
(check-equal?
  (subst2 5 4 100 '(1 3 4))
  '(1 3 5))
(check-equal?
  (subst2 5 6 100 '(1 3 4))
  '(1 3 4))

(check-equal?
  (subst2 2 100 1 '(1 3 4))
  '(2 3 4))
(check-equal?
  (subst2 5 100 3 '(1 3 4))
  '(1 5 4))
(check-equal?
  (subst2 5 100 4 '(1 3 4))
  '(1 3 5))
(check-equal?
  (subst2 5 100 6 '(1 3 4))
  '(1 3 4))

(check-equal?
  (subst2 2 3 1 '(1 3 4))
  '(2 3 4))
(check-equal?
  (subst2 5 4 3 '(1 3 4))
  '(1 5 4))
(check-equal?
  (subst2 5 4 4 '(1 3 4))
  '(1 3 5))

; UT subst
(check-equal?
  (subst 2 1 '(1 3 4))
  '(2 3 4))
(check-equal?
  (subst 5 3 '(1 3 4))
  '(1 5 4))
(check-equal?
  (subst 5 4 '(1 3 4))
  '(1 3 5))
(check-equal?
  (subst 5 6 '(1 3 4))
  '(1 3 4))

; UT insertL
(check-equal?
  (insertL 2 1 '(1 3 4))
  '(2 1 3 4))
(check-equal?
  (insertL 5 3 '(1 3 4))
  '(1 5 3 4))
(check-equal?
  (insertL 5 4 '(1 3 4))
  '(1 3 5 4))
(check-equal?
  (insertL 5 6 '(1 3 4))
  '(1 3 4))

; UT multiinsertL
(check-equal?
  (multiinsertL 2 1 '(1 3 4))
  '(2 1 3 4))
(check-equal?
  (multiinsertL 5 3 '(1 3 4))
  '(1 5 3 4))
(check-equal?
  (multiinsertL 5 4 '(1 3 4))
  '(1 3 5 4))
(check-equal?
  (multiinsertL 5 6 '(1 3 4))
  '(1 3 4))

(check-equal?
  (multiinsertL 2 1 '(1 3 4 1))
  '(2 1 3 4 2 1))
(check-equal?
  (multiinsertL 5 3 '(3 1 3 4))
  '(5 3 1 5 3 4))
(check-equal?
  (multiinsertL 5 3 '(3 3 3 3))
  '(5 3 5 3 5 3 5 3))
(check-equal?
  (multiinsertL 3 3 '(3 3 3))
  '(3 3 3 3 3 3))

; UT insertR
(check-equal?
  (insertR 2 1 '(1 3 4))
  '(1 2 3 4))
(check-equal?
  (insertR 5 3 '(1 3 4))
  '(1 3 5 4))
(check-equal?
  (insertR 5 4 '(1 3 4))
  '(1 3 4 5))
(check-equal?
  (insertR 5 6 '(1 3 4))
  '(1 3 4))

; UT multiinsertR
(check-equal?
  (multiinsertR 2 1 '(1 3 4))
  '(1 2 3 4))
(check-equal?
  (multiinsertR 5 3 '(1 3 4))
  '(1 3 5 4))
(check-equal?
  (multiinsertR 5 4 '(1 3 4))
  '(1 3 4 5))
(check-equal?
  (multiinsertR 5 6 '(1 3 4))
  '(1 3 4))

(check-equal?
  (multiinsertR 2 1 '(1 3 4 1))
  '(1 2 3 4 1 2))
(check-equal?
  (multiinsertR 5 3 '(3 1 3 4))
  '(3 5 1 3 5 4))
(check-equal?
  (multiinsertR 5 3 '(3 3 3 3))
  '(3 5 3 5 3 5 3 5))
(check-equal?
  (multiinsertR 3 3 '(3 3 3))
  '(3 3 3 3 3 3))

; UT firsts
(check-equal?
  (firsts '())
  '())
(check-equal?
  (firsts '((1 10 20) (2 40 50 60) (3 70 80) (4)))
  '(1 2 3 4))

; UT lat?
(check-true (lat? `(1 2 3 4)))
(check-true (lat? `(5)))
(check-true (lat? `()))
(check-false(lat? `(1 (2 3) 4)))
(check-false(lat? `((1 2 3 4))))
(check-false(lat? `((1 2) 3 4)))
(check-false(lat? `(1 2 (3 4))))

; UT member?
(check-true (member? 2 '(1 2 3 4)))
(check-true (member? 1 '(1 2 3 4)))
(check-true (member? 4 '(1 2 3 4)))
(check-false (member? 5 '(1 2 3 4)))
(check-false (member? 2 '()))

; UT rember
(check-equal?
  (rember 1 '(1 2 3))
  '(2 3))
(check-equal?
  (rember 2 '(1 2 3))
  '(1 3))
(check-equal?
  (rember 3 '(1 2 3))
  '(1 2))
(check-equal?
  (rember 1 '(1 2 3 1 2 3))
  '(2 3 1 2 3))
(check-equal?
  (rember 2 '(1 2 3 1 2 3))
  '(1 3 1 2 3))
(check-equal?
  (rember 3 '(1 2 3 1 2 3))
  '(1 2 1 2 3))
(check-equal?
  (rember 4 '(1 2 3))
  '(1 2 3))
