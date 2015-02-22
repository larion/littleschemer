#lang racket

(require "ls123.scm" "ls_base.scm" rackunit)

(provide set? makeset eqset? subset? intersect? disjunct? intersect intersectall union fun? a-pair? first second build revrel)

(define set?
  (lambda (lat)
    (if (null? lat)
      #t
      (and
        (not (member? (car lat) (cdr lat)))
        (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) lat)
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else
        (cons
          (car lat)
          (makeset (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (if (null? lat)
      '()
      (cons
        (car lat)
        (multirember (car lat) (makeset2 (cdr lat)))))))

(define subset?
  (lambda (lat1 lat2)
    (if (null? lat1) #t (and
      (member? (car lat1) lat2)
      (subset? (cdr lat1) lat2)))))

(define eqset?
  (lambda (lat1 lat2)
    (and
      (subset? lat1 lat2)
      (subset? lat2 lat1))))

(define intersect?
  (lambda (lat1 lat2)
    (if (null? lat1) #f
      (or
        (member? (car lat1) lat2)
        (intersect? (cdr lat1) lat2)))))

(define disjunct? (lambda (lat1 lat2) (not (intersect? lat1 lat2))))

(define intersect
  (lambda (lat1 lat2)
    (cond
      ((null? lat1) '())
      ((member? (car lat1) lat2)
        (cons
          (car lat1)
          (intersect (cdr lat1) lat2)))
      (else (intersect (cdr lat1) lat2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else
        (cons
          (car set1)
          (union (cdr set1) set2))))))

; TODO simplify
(define intersectall
  (lambda (l-set)
      (if (null? (cdr l-set))
       (car l-set)
       (intersectall
         (cons
           (intersect
             (car l-set)
             (car (cdr l-set)))
           (cdr (cdr l-set)))))))

(define a-pair?
  (lambda (l)
    (and
        (not (atom? l))
        (not (null? l))
        (not (null? (cdr l)))
        (null? (cdr (cdr l))))))

(define first (lambda (l) (car l)))

(define second (lambda (l) (car (cdr l))))

(define build (lambda (x1 x2) (cons x1 (cons x2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (if (null? rel)
      '()
      (cons
        (build
          (second (car rel))
          (first (car rel)))
        (revrel (cdr rel))))))

;; unit tests

(check-false (a-pair? 'bla))
(check-false (a-pair? '()))
(check-false (a-pair? '3))
(check-false (a-pair? '(1)))
(check-false (a-pair? '(bla)))
(check-false (a-pair? '(bla bla bla)))
(check-false (a-pair? '(1 2 3)))
(check-true (a-pair? '(1 2)))
(check-true (a-pair? '(bla bla)))

(check-equal?
  (first '(1 2))
  1)
(check-equal?
  (first '(foo bar))
  'foo)
(check-equal?
  (first '((((foo))) (bar)))
  '(((foo))) )

(check-equal?
  (second '(1 2))
  2)
(check-equal?
  (second '(foo bar))
  'bar)
(check-equal?
  (second '((((foo))) (bar)))
  '(bar) )
(check-equal?
  (build 'foo 'bar)
  '(foo bar))
(check-equal?
  (build '((foo)) '(bar))
  '(((foo)) (bar)) )

(check-true (fun? '((1 2) (2 2) (3 2))))
(check-true (fun? '()))
(check-false (fun? '((1 2) (1 3))))

(check-equal?
  (revrel '((1 2) (3 4)))
  '((2 1) (4 3)))
(check-equal?
  (revrel '((1 2) (1 3) (1 5)))
  '((2 1) (3 1) (5 1)))
(check-equal?
  (revrel '())
  '())

(check-true (set? '()))
(check-true (set? '(pears)))
(check-true (set? '(apple pears strawberry)))
(check-true (set? '(1)))
(check-true (set? '(1 2 3 4 5 6 7 apple)))
(check-false (set? '(apple pears strawberry apple)))
(check-false (set? '(apple pears strawberry pears)))
(check-false (set? '(apple pears strawberry strawberry)))
(check-false (set? '(apple apple strawberry)))
(check-false (set? '(1 2 3 4 5 6 7 8 9 0 1 2 3)))

(check-equal?
  (makeset '(apple pear peach peach peach apple apple banana strawberry plum apple plum orange))
  '(pear peach banana strawberry apple plum orange))

(check-equal?
  (makeset2 '(apple pear peach peach peach apple apple banana strawberry plum apple plum orange))
  '(apple pear peach banana strawberry plum orange))

(check-true (subset? '(1 3 5) '(1 2 3 4 5 6 7 8 9 10)))
(check-true (subset? '() '(1 2 3 4 5 6 7 8 9 10)))
(check-true (subset? '(1 2 3 4 5 6 7 8 9 10) '(1 2 3 4 5 6 7 8 9 10)))
(check-false (subset? '(0) '(1 2 3 4 5 6 7 8 9 10)))
(check-false (subset? '(0 1 2) '(1 2 3 4 5 6 7 8 9 10)))

(check-true (eqset? '(6 large chickens with wings) '(6 chickens with large wings)))
(check-true (eqset? '() '()))
(check-false (eqset? '(6 large chickens with wings) '(6 dogs with large wings)))
(check-false (eqset? '(large chickens with wings) '(6 chickens with wings)))
(check-false (eqset? '(6 small chickens with wings) '(6 large chickens with wings)))

(check-true (intersect? '(1 2 3) '(3 4 5)))
(check-true (intersect? '(1 2 3) '(1 5 6 7)))
(check-true (intersect? '(3 3 3) '(0 3 0)))
(check-false (intersect? '(1 2 3) '(4 5 6)))
(check-false (intersect? '() '(4 5 6)))

(check-false (disjunct? '(1 2 3) '(3 4 5)))
(check-false (disjunct? '(1 2 3) '(1 5 6 7)))
(check-false (disjunct? '(3 3 3) '(0 3 0)))
(check-true (disjunct? '(1 2 3) '(4 5 6)))
(check-true (disjunct? '() '(4 5 6)))

(check-equal?
  (intersect '(1 2 3) '(3 4 5))
  '(3))
(check-equal?
  (intersect '(1 2 3) '(1 5 6 7))
  '(1))
(check-equal?
  (intersect '(0 1 2 3 4 5 6 7 8 9 10) '(15 14 13 12 11 10 9 8 7 6 5))
  '(5 6 7 8 9 10))
(check-equal?
  (intersect '(1 2 3) '(4 5 6))
  '())
(check-equal?
  (intersect '() '(4 5 6))
  '())

(check-equal?
  (union '(1 2 3) '(3 4 5))
  '(1 2 3 4 5))
(check-equal?
  (union '(1 2 3) '(1 5 6 7))
  '(2 3 1 5 6 7))
(check-equal?
  (union '(0 1 2 3 4 5 6 7 8 9 10) '(15 14 13 12 11 10 9 8 7 6 5))
  '(0 1 2 3 4 15 14 13 12 11 10 9 8 7 6 5))
(check-equal?
  (union '(1 2 3) '(4 5 6))
  '(1 2 3 4 5 6))
(check-equal?
  (union '() '(4 5 6))
  '(4 5 6))

(check-equal?
  (intersectall '((a b c) (c a d e) (e f g h a b)))
  '(a))
(check-equal?
  (intersectall '((a b c) (c a d e b) (e f g h a b)))
  '(a b))
(check-equal?
  (intersectall '((c) (c a d e b) (e f g h a b)))
  '())
