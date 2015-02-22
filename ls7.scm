#lang racket

(require "ls123.scm" "ls_base.scm")

(provide set? makeset subset? intersect? disjunct? intersect intersectall union fun? a-pair? first second build revrel)

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

(not (a-pair? 'bla))
(not (a-pair? '()))
(not (a-pair? '3))
(not (a-pair? '(1)))
(not (a-pair? '(bla)))
(not (a-pair? '(bla bla bla)))
(not (a-pair? '(1 2 3)))
(a-pair? '(1 2))
(a-pair? '(bla bla))

(equal?
  (first '(1 2))
  1)
(equal?
  (first '(foo bar))
  'foo)
(equal?
  (first '((((foo))) (bar)))
  '(((foo))) )

(equal?
  (second '(1 2))
  2)
(equal?
  (second '(foo bar))
  'bar)
(equal?
  (second '((((foo))) (bar)))
  '(bar) )
(equal?
  (build 'foo 'bar)
  '(foo bar))
(equal?
  (build '((foo)) '(bar))
  '(((foo)) (bar)) )

(fun? '((1 2) (2 2) (3 2)))
(fun? '())
(not (fun? '((1 2) (1 3))))

(equal?
  (revrel '((1 2) (3 4)))
  '((2 1) (4 3)))
(equal?
  (revrel '((1 2) (1 3) (1 5)))
  '((2 1) (3 1) (5 1)))
(equal?
  (revrel '())
  '())

(set? '())
(set? '(pears))
(set? '(apple pears strawberry))
(set? '(1))
(set? '(1 2 3 4 5 6 7 apple))
(not (set? '(apple pears strawberry apple)))
(not (set? '(apple pears strawberry pears)))
(not (set? '(apple pears strawberry strawberry)))
(not (set? '(apple apple strawberry)))
(not (set? '(1 2 3 4 5 6 7 8 9 0 1 2 3)))

(equal?
  (makeset '(apple pear peach peach peach apple apple banana strawberry plum apple plum orange))
  '(pear peach banana strawberry apple plum orange))

(equal?
  (makeset2 '(apple pear peach peach peach apple apple banana strawberry plum apple plum orange))
  '(apple pear peach banana strawberry plum orange))

(subset? '(1 3 5) '(1 2 3 4 5 6 7 8 9 10))
(subset? '() '(1 2 3 4 5 6 7 8 9 10))
(subset? '(1 2 3 4 5 6 7 8 9 10) '(1 2 3 4 5 6 7 8 9 10))
(not (subset? '(0) '(1 2 3 4 5 6 7 8 9 10)))
(not (subset? '(0 1 2) '(1 2 3 4 5 6 7 8 9 10)))

(eqset? '(6 large chickens with wings) '(6 chickens with large wings))
(eqset? '() '())
(not (eqset? '(6 large chickens with wings) '(6 dogs with large wings)))
(not (eqset? '(large chickens with wings) '(6 chickens with wings)))
(not (eqset? '(6 small chickens with wings) '(6 large chickens with wings)))

(intersect? '(1 2 3) '(3 4 5))
(intersect? '(1 2 3) '(1 5 6 7))
(intersect? '(3 3 3) '(0 3 0))
(not (intersect? '(1 2 3) '(4 5 6)))
(not (intersect? '() '(4 5 6)))

(not (disjunct? '(1 2 3) '(3 4 5)))
(not (disjunct? '(1 2 3) '(1 5 6 7)))
(not (disjunct? '(3 3 3) '(0 3 0)))
(disjunct? '(1 2 3) '(4 5 6))
(disjunct? '() '(4 5 6))

(equal?
  (intersect '(1 2 3) '(3 4 5))
  '(3))
(equal?
  (intersect '(1 2 3) '(1 5 6 7))
  '(1))
(equal?
  (intersect '(0 1 2 3 4 5 6 7 8 9 10) '(15 14 13 12 11 10 9 8 7 6 5))
  '(5 6 7 8 9 10))
(equal?
  (intersect '(1 2 3) '(4 5 6))
  '())
(equal?
  (intersect '() '(4 5 6))
  '())

(equal?
  (union '(1 2 3) '(3 4 5))
  '(1 2 3 4 5))
(equal?
  (union '(1 2 3) '(1 5 6 7))
  '(2 3 1 5 6 7))
(equal?
  (union '(0 1 2 3 4 5 6 7 8 9 10) '(15 14 13 12 11 10 9 8 7 6 5))
  '(0 1 2 3 4 15 14 13 12 11 10 9 8 7 6 5))
(equal?
  (union '(1 2 3) '(4 5 6))
  '(1 2 3 4 5 6))
(equal?
  (union '() '(4 5 6))
  '(4 5 6))

(equal?
  (intersectall '((a b c) (c a d e) (e f g h a b)))
  '(a))
(equal?
  (intersectall '((a b c) (c a d e b) (e f g h a b)))
  '(a b))
(equal?
  (intersectall '((c) (c a d e b) (e f g h a b)))
  '())
