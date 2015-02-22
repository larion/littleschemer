#lang racket

(require "ls_base.scm" "ls4.scm")

(provide value value2 numbered?)

(define numbered?
  (lambda (expr)
    (cond
      ((atom? expr) (number? expr))
      (else (and
        (numbered? (car expr))
        (numbered? (car (cdr (cdr expr)))))))))


(define value
  (lambda (nexpr)
    (cond
      ((atom? nexpr) nexpr)
      ((eq? (car (cdr nexpr)) '+)
        (+ (value (car nexpr)) (value (car (cdr (cdr nexpr))))))
      ((eq? (car (cdr nexpr)) '-)
        (- (value (car nexpr)) (value (car (cdr (cdr nexpr))))))
      ((eq? (car (cdr nexpr)) '*)
        (* (value (car nexpr)) (value (car (cdr (cdr nexpr))))))
      ((eq? (car (cdr nexpr)) '^)
        (up (value (car nexpr)) (value (car (cdr (cdr nexpr))))))
      )
))

(define value2
  (lambda (nexpr)
    (cond
      ((atom? nexpr) nexpr)
      ((eq? (car nexpr) '+)
        (+ (value2 (car (cdr nexpr))) (value2 (car (cdr (cdr nexpr))))))
      ((eq? (car nexpr) '-)
        (- (value2 (car (cdr nexpr))) (value2 (car (cdr (cdr nexpr))))))
      ((eq? (car nexpr) '*)
        (* (value2 (car (cdr nexpr))) (value2 (car (cdr (cdr nexpr))))))
      ((eq? (car nexpr) '^)
        (up (value2 (car (cdr nexpr))) (value2 (car (cdr (cdr nexpr))))))
      )
))

;; unit tests

(numbered? '((1 + (2 + 3)) + (7 + 1000)))
(numbered? '(1 + (27 + ((3 + 7) + 1000))))
(numbered? '(1 + ((2 + 3) + (7 + 1000))))
(numbered? '((1 + 2) + (3 + (7 + 1000))))

(not (numbered? '((1 + (2 + cat)) + (7 + 1000))))
(not (numbered? '(1 + (27 + ((3 + pudding) + 1000)))))
(not (numbered? '(1 + ((2 + 3) + (7 + x)))))
(not (numbered? '((1 + 2) + (foobar + (7 + 1000)))))
(not (numbered? '((foo + bar) + (foobar + (bar + foo)))))

(eq?
(value '(1 + ((7 * 6) - ((5 ^ (5 - 3)) + (4 + (2 * 4))))))
6)

(eq?
(value2 '(+ 1 (- (* 7 6) (+ (^ 5 (- 5 3)) (+ 4 (* 2 4))))))
6)
