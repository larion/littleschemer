#lang racket

(require "ls_base.scm" "ls4.scm" "ls7.scm" rackunit)

(provide align Ackermann Y)

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (target last lat)
    (if (number? last)
      (keep-looking target (pick last lat) lat)
      (eq? target last))))

(define shift
  (lambda (p)
    (build
      (first (first p))
      (build
        (second (first p))
        (second p)))))

(define align
  (lambda (pora)
    (cond
      [(atom? pora) pora]
      [(atom? (first pora))
       (build
         (first pora)
         (align (second pora)))]
      [else
        (align (shift pora))])))

(define Ackermann
  (lambda (n m)
    (cond
      [(eq? n 0) (add1 m)]
      [(eq? m 0) (Ackermann (sub1 n) 1)]
      [else (Ackermann (sub1 n) (Ackermann n (sub1 m)))])))

(define Y
  (lambda (mk-func)
    ((lambda (g) (g g))
      (lambda (f)
        (mk-func (lambda (x) ((f f) x)))))))

(check-true
  (looking 'caviar '(6 2 4 grits caviar 5 7 3)))

(check-false
  (looking 'caviar '(6 2 grits caviar 5 7 3)))

(check-equal?
  (shift '((a b) c))
  '(a (b c)))
(check-equal?
  (shift '((a b) (c d)))
  '(a (b (c d))))

(check-equal?
  (align '(a ((b (c ((d e) f))) g)))
  '(a (b (c (d (e (f g)))))))

(check-equal? (Ackermann 0 0) 1)
(check-equal? (Ackermann 0 1) 2)
(check-equal? (Ackermann 0 2) 3)
(check-equal? (Ackermann 1 0) 2)
(check-equal? (Ackermann 1 1) 3)
(check-equal? (Ackermann 1 2) 4)
(check-equal? (Ackermann 2 0) 3)
(check-equal? (Ackermann 2 1) 5)
(check-equal? (Ackermann 2 2) 7)
(check-equal? (Ackermann 3 0) 5)
(check-equal? (Ackermann 3 1) 13)
(check-equal? (Ackermann 3 2) 29)
(check-equal? (Ackermann 3 3) 61)
(check-equal? (Ackermann 4 0) 13)
(check-equal? (Ackermann 4 1) 65533)

(define mk-fact
  (lambda (fact)
    (lambda (n)
      (cond
        ((eq? n 1) 1)
        (else
          (* n (fact (sub1 n))))))))

(define fact (Y mk-fact))

(check-equal? (fact 1) 1)
(check-equal? (fact 2) 2)
(check-equal? (fact 3) 6)
(check-equal? (fact 4) 24)
(check-equal? (fact 5) 120)
