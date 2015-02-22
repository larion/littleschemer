#lang racket

(require "ls_base.scm" rackunit)

(provide rember* insertR* insertL* member* leftmost occur* subst*)

(define rember*
  (lambda (x lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) x)
       (rember* x (cdr lst)))
      ((atom? (car lst))
        (cons
          (car lst)
          (rember* x (cdr lst))))
      (else
        (cons
          (rember* x (car lst))
          (rember* x (cdr lst))
        ))
)))

(define insertR*
  (lambda (new old lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) old)
       (cons
        (car lst)
        (cons
          new
          (insertR* new old (cdr lst))
      )))
      ((atom? (car lst))
        (cons
          (car lst)
          (insertR* new old (cdr lst))))
      (else
        (cons
          (insertR* new old (car lst))
          (insertR* new old (cdr lst))
        )))))

(define insertL*
  (lambda (new old lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) old)
         (cons
          new
          (cons
            (car lst)
            (insertL* new old (cdr lst))
          )))
      ((atom? (car lst))
          (cons
            (car lst)
            (insertL* new old (cdr lst))))
      (else
          (cons
            (insertL* new old (car lst))
            (insertL* new old (cdr lst))
          )))))

(define occur*
  (lambda (x lst)
    (cond
      ((null? lst) 0)
      ((equal? (car lst) x)
       (add1 (occur* x (cdr lst))))
      ((atom? (car lst))
       (occur* x (cdr lst)))
      (else (+
        (occur* x (cdr lst))
        (occur* x (car lst))))
    )
))

(define subst*
  (lambda (new old lst)
    (cond
      ((null? lst) '())
      ((eq? (car lst) old)
        (cons
          new
          (subst* new old (cdr lst))))
      ((atom? (car lst))
       (cons
         (car lst)
         (subst* new old (cdr lst))))
      (else
        (cons
          (subst* new old (car lst))
          (subst* new old (cdr lst)))
      ))))

(define member* (lambda (x lst)
  (cond
    ((null? lst) #f)
    ((eq? (car lst) x) #t)
    ((atom? (car lst))
        (member* x (cdr lst)))
    (else (or
        (member* x (car lst)) (member* x (cdr lst)))))))

(define leftmost (lambda (lst)
    (if (atom? (car lst)) (car lst) (leftmost (car lst)))))

(define eqlist (lambda (x y)
    (or
        (and
            (null? x)
            (null? y)
        )
        (and
            (not (null? x))
            (not (null? y))
            (eqlist (cdr x) (cdr y))
            (or
                (and
                    (atom? (car x))
                    (atom? (car y))
                    (eq?  (car x) (car y))
                )
                (and
                    (not (atom? (car x)))
                    (not (atom? (car y)))
                    (eqlist (car x) (car y))
                ))))))

;equal skipped b/c its pretty trivial

; unit test

(check-equal?
(rember* 'sauce
'(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
'(((tomato)) ((bean)) (and ((flying)))))

(check-equal? (insertR* 'roast 'chuck
'((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
'((how much (wood)) could ((a (wood) chuck roast)) (((chuck roast))) (if (a) ((wood chuck roast))) could chuck roast wood )
)

(check-equal? (occur* 'banana '((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy))) 5)

(check-equal? (subst* 'orange 'banana
'((banana) (split ((((banana ice))) (cream (banana)) sherbet)) (banana) (bread) (banana brandy)))
'((orange) (split ((((orange ice))) (cream (orange)) sherbet)) (orange) (bread) (orange brandy)))

(check-equal? (insertL* 'pecker 'chuck
'((how much (wood)) could ((a (wood) chuck)) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
'((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood))

(check-true (member* 'chips '((potato) (chips ((with) fish) (chips)))))

(check-equal?
    (leftmost '((potato) (chips ((with) fish) (chips))))
    'potato)

(check-false (eqlist
'(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce))
'(((tomato)) ((bean)) (and ((flying))))))

(check-true (eqlist
'((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood)
'((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood)))

(check-false (eqlist
'((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chuck))) (if (a) ((wood pecker chuck))) could pecker chuck wood)
'((how much (wood)) could ((a (wood) pecker chuck)) (((pecker chalk))) (if (a) ((wood pecker chuck))) could pecker chuck wood)))
