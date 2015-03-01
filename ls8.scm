#lang racket

(require "ls_base.scm" "ls4.scm" "ls7.scm" rackunit)

(provide multiinsertLR multiinsertLRco multiremberT value insert-fg)

; TODO? subst

(define evens-only*
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((atom? (car lst))
       (if (is-even? (car lst))
         (cons
           (car lst)
           (evens-only* (cdr lst)))
         (evens-only* (cdr lst))))
      (else
        (cons
          (evens-only* (car lst))
          (evens-only* (cdr lst)))))))

; TODO do this w/o the helper funcs (the way it was done in the book)
(define evens-only*co
  (lambda (lst col)
    (let
      ([even-lst (lambda (x _ __) x)]
      [even-prod (lambda (_ x __) x)]
      [odd-sum   (lambda (_ __ x) x)])
    (cond
      ((null? lst)
        (col '() 1 0))
      ((atom? (car lst))
       (if (is-even? (car lst))
         (evens-only*co (cdr lst)
           (lambda (evenlst evenprod oddsum)
             (col (cons (car lst) evenlst) (* (car lst) evenprod) oddsum)))
         (evens-only*co (cdr lst)
           (lambda (evenlst evenprod oddsum)
             (col evenlst evenprod (+ (car lst) oddsum))))))
      (else
        (evens-only*co (cdr lst)
          (lambda (evenlst evenprod oddsum)
            (col
              (cons (evens-only*co (car lst) even-lst) evenlst)
              (* (evens-only*co (car lst) even-prod) evenprod)
              (+ (evens-only*co (car lst) odd-sum)   oddsum)))))))))

(define is-even? (lambda (x) (= (* (quotient x 2) 2) x)))

(define multiinsertLR
  (lambda (new oldL oldR lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) oldL)
        (cons new
           (cons oldL
             (multiinsertLR new oldL oldR (cdr lst)))))
      ((eq? (car lst) oldR)
        (cons oldR
          (cons new
            (multiinsertLR new oldL oldR (cdr lst)))))
      (else
        (cons
          (car lst)
            (multiinsertLR new oldL oldR (cdr lst))))
      )))

(define multiinsertLRco
  (lambda (new oldL oldR lst col)
    (cond
      ((null? lst) (col '() 0 0))
      ((eq? (car lst) oldL)
       (multiinsertLRco new oldL oldR (cdr lst)
          (lambda (newlst lefti righti)
            (col
              (cons new (cons oldL newlst))
              (add1 lefti)
              righti))))
      ((eq? (car lst) oldR)
       (multiinsertLRco new oldL oldR (cdr lst)
          (lambda (newlst lefti righti)
            (col
              (cons oldR (cons new newlst))
              lefti
              (add1 righti)))))
      (else
       (multiinsertLRco new oldL oldR (cdr lst)
          (lambda (newlst lefti righti)
            (col
              (cons (car lst) newlst)
              lefti
              righti)))))))

; like multirember but takes an S -> S -> Bool function instead of just using
; eq?
(define multirember-f
  (lambda (f?)
      (lambda (x lst)
        (cond
          ((null? lst) '())
          ((f? (car lst) x) ((multirember-f f?) x (cdr lst)))
          (else
            (cons
              (car lst)
              ((multirember-f f?) x (cdr lst)))
          )))))

; like multirember but takes an S -> Bool function instead of using (lambda (x)
; ( lambda (y) (eq? x y))) old)
(define multiremberT
  (lambda (f?)
      (lambda (lst)
        (cond
          ((null? lst) '())
          ((f? (car lst)) ((multiremberT f?) (cdr lst)))
          (else
            (cons
              (car lst)
              ((multiremberT f?) (cdr lst)))
          )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value
  (lambda (nexpr)
    (if (atom? nexpr)
      nexpr
      ((atom-to-function (operator nexpr))
       (value (first-sub-exp nexpr))
       (value (second-sub-exp nexpr))))))

    (define atom-to-function
      (lambda (x)
        (cond
          ((eq? x '+) +)
          ((eq? x '-) -)
          ((eq? x '*) *)
          ((eq? x '^) up))))

;;; helper functions

(define first-sub-exp
  (lambda (nexpr)
    (car nexpr)))

(define second-sub-exp
  (lambda (nexpr)
    (car (cdr (cdr nexpr)))))

(define operator
  (lambda (nexpr)
    (car (cdr nexpr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Traverse lst until f? (the test function)
; is true of (car lst) and old, then apply the function
; g to 1) the matching element in the list 2) the new element
; and 3) the rest of the list. Then join the return value
; of this function to the traversed part of the list.
(define insert-fg
  (lambda (f? g)
    (lambda (new old lst)
      (cond
        ((null? lst) '())
        ((f? (car lst) old)
         (g new old (cdr lst)))
        (else
          (cons
            (car lst)
            ((insert-fg f? g) new old (cdr lst))))))))

(define leftSeq
  (lambda (new old lst)
    (cons new (cons old lst))))

(define rightSeq
  (lambda (new old lst)
    (cons old (cons new lst))))

(define delSeq (lambda (new old lst) lst))

(define insertL-f (lambda (f?) (insert-fg f? leftSeq)))
(define insertR-f (lambda (f?) (insert-fg f? rightSeq)))
(define rember-f
  (lambda (f?)
    (lambda (x lst)
      ((insert-fg f? delSeq) #f x lst))))

; unit tests

(check-equal?
  ((insertR-f =) 2 1 '(1 3 4))
  '(1 2 3 4))
(check-equal?
  ((insertR-f =) 5 3 '(1 3 4))
  '(1 3 5 4))
(check-equal?
  ((insertR-f =) 5 4 '(1 3 4))
  '(1 3 4 5))
(check-equal?
  ((insertR-f =) 5 6 '(1 3 4))
  '(1 3 4))

(check-equal?
  ((insertL-f =) 2 1 '(1 3 4))
  '(2 1 3 4))
(check-equal?
  ((insertL-f =) 5 3 '(1 3 4))
  '(1 5 3 4))
(check-equal?
  ((insertL-f =) 5 4 '(1 3 4))
  '(1 3 5 4))
(check-equal?
  ((insertL-f =) 5 6 '(1 3 4))
  '(1 3 4))

(check-equal?
  ((insert-fg = rightSeq) 2 1 '(1 3 4))
  '(1 2 3 4))
(check-equal?
  ((insert-fg = rightSeq) 5 3 '(1 3 4))
  '(1 3 5 4))
(check-equal?
  ((insert-fg = rightSeq) 5 4 '(1 3 4))
  '(1 3 4 5))
(check-equal?
  ((insert-fg = rightSeq) 5 6 '(1 3 4))
  '(1 3 4))

(check-equal?
  ((insert-fg = leftSeq) 2 1 '(1 3 4))
  '(2 1 3 4))
(check-equal?
  ((insert-fg = leftSeq) 5 3 '(1 3 4))
  '(1 5 3 4))
(check-equal?
  ((insert-fg = leftSeq) 5 4 '(1 3 4))
  '(1 3 5 4))
(check-equal?
  ((insert-fg = leftSeq) 5 6 '(1 3 4))
  '(1 3 4))


(check-equal?
  ((rember-f =) 1 '(1 2 3))
  '(2 3))
(check-equal?
  ((rember-f =) 2 '(1 2 3))
  '(1 3))
(check-equal?
  ((rember-f =) 3 '(1 2 3))
  '(1 2))
(check-equal?
  ((rember-f =) 1 '(1 2 3 1 2 3))
  '(2 3 1 2 3))
(check-equal?
  ((rember-f =) 2 '(1 2 3 1 2 3))
  '(1 3 1 2 3))
(check-equal?
  ((rember-f =) 3 '(1 2 3 1 2 3))
  '(1 2 1 2 3))
(check-equal?
  ((rember-f =) 4 '(1 2 3))
  '(1 2 3))

(check-equal?
  ((rember-f eq?) 'cat '(dog cat dinosaur))
  '(dog dinosaur))
(check-equal?
  ((rember-f equal?) '(dog cat) '((dog cat) dinosaur))
  '(dinosaur))
(check-equal?
  ((rember-f eqset?) '(dog cat) '((cat dog) (dinosaur)))
  '((dinosaur)))

(check-eq?
  (value '(1 + ((7 * 6) - ((5 ^ (5 - 3)) + (4 + (2 * 4))))))
  6)

(check-equal? ((multirember-f =) 1 '())
        '())
(check-equal? ((multirember-f =) 1 '(2 1 3))
        '(2 3))
(check-equal? ((multirember-f =) 1 '(2 1 1 1 3))
        '(2 3))
(check-equal? ((multirember-f =) 1 '(2 2 1 1 1))
        '(2 2))
(check-equal? ((multirember-f =) 1 '(1 1 1 2))
        '(2))
(check-equal? ((multirember-f =) 100 '(100 1 100 2 100 3 100 4))
        '(1 2 3 4))
(check-equal? ((multirember-f =) 100 '(1 100 2 100 3 100 4 100))
        '(1 2 3 4))

(define eq?-cur
  (lambda (x)
    (lambda (y)
      (= x y))))

(check-equal? ((multiremberT (eq?-cur 1)) '())
        '())
(check-equal? ((multiremberT (eq?-cur 1)) '(2 1 3))
        '(2 3))
(check-equal? ((multiremberT (eq?-cur 1)) '(2 1 1 1 3))
        '(2 3))
(check-equal? ((multiremberT (eq?-cur 1)) '(2 2 1 1 1))
        '(2 2))
(check-equal? ((multiremberT (eq?-cur 1)) '(1 1 1 2))
        '(2))
(check-equal? ((multiremberT (eq?-cur 100)) '(100 1 100 2 100 3 100 4))
        '(1 2 3 4))
(check-equal? ((multiremberT (eq?-cur 100)) '(1 100 2 100 3 100 4 100))
        '(1 2 3 4))

(check-equal? (multiinsertLR 'salty 'fish 'chips '(chips with fish and chips with chips)) '(chips salty with salty fish and chips salty with chips salty))

(check-equal? (multiinsertLRco 'salty 'fish 'chips '(chips with fish and chips with chips) (lambda (x y z) x) ) '(chips salty with salty fish and chips salty with chips salty))

(check-equal? (multiinsertLRco 'salty 'fish 'chips '(chips with fish and chips with chips) (lambda (x y z) y) ) 1)
(check-equal? (multiinsertLRco 'salty 'fish 'chips '(chips with fish and chips with chips) (lambda (x y z) z) ) 3)

(check-equal? (evens-only* '(1 2 (1 2 3 4) 5 7 9 12 (2 4 (6 7)))) '(2 (2 4) 12 (2 4 (6))))

(check-equal? (evens-only*co '(1 2 (1 2 3 4) 5 7 9 12 (2 4 (6 7))) (lambda (x y z) x)) '(2 (2 4) 12 (2 4 (6))))
(check-equal? (evens-only*co '(1 2 (1 2 3 4) 5 7 9 12 (2 4 (6 7))) (lambda (x y z) y)) 9216)
(check-equal? (evens-only*co '(1 2 (1 2 3 4) 5 7 9 12 (2 4 (6 7))) (lambda (x y z) z)) 33)
