#lang racket

(require "ls_base.scm" "ls7.scm" rackunit)

(provide value)

;;; high level functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define meanings
  (lambda (l table)
    (cond
      [(null? l) '()]
      [else (cons (meaning (car l) table) (meanings (cdr l) table))])))

(define expression-to-action
  (lambda (expr)
    (cond
      [(atom? expr) (atom-to-action expr)]
      [else (list-to-action expr)])))

(define atom-to-action
  (lambda (e)
    (cond
      [(eq? e 'cons) *const]
      [(eq? e 'car) *const]
      [(eq? e 'cdr) *const]
      [(eq? e 'null?) *const]
      [(eq? e 'eq?) *const]
      [(number? e) *const]
      [(eq? e 'number?) *const]
      [(eq? e 'null?) *const]
      [(eq? e 'sub1) *const]
      [(eq? e 'add1) *const]
      [(eq? e 'atom?) *const]
      [(eq? e 'zero?) *const]
      [(eq? e 'quote) *const]
      [else *identifier])))

(define list-to-action
  (lambda (l)
    (cond
      [(eq? (car l) 'quote) *quote]
      [(eq? (car l) 'lambda) *lambda]
      [(eq? (car l) 'cond) *cond]
      [else *application])))

;;; *const ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *const
  (lambda (e table)
    (cond
      [(number? e) e]
      [(eq? e #t) #t]
      [(eq? e #f) #f]
      [else (build 'primitive e)])))

;;; *quote ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

;;; *identifier ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *identifier
  (lambda (e table)
    (lookup-in-table e table (lambda (e) (displayln "invalid identifier") (car '())))))

;;; *lambda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons
             table (cdr e)))))

(define table-of (lambda (l) (car l)))

(define formals-of (lambda (l) (car (cdr l))))

(define body-of (lambda (l) (car (cdr (cdr l)))))

;;; *cond ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *cond
  (lambda (e table)
    (evcon (cdr e) table)))

(define evcon
  (lambda (lines table)
    (cond
      [(else? (question-of (car lines))) (meaning (answer-of (car lines)) table)]
      [(meaning (question-of (car lines)) table)
        (meaning (answer-of (car lines)) table)]
      [else (evcon (cdr lines) table)])))

(define question-of first)
(define answer-of second)
(define else? (lambda (x) (and (atom? x) (eq? x 'else))))

;;; *application ;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *application
  (lambda (e table)
    (apply-func
      (meaning (func e) table)
      (meanings (args e) table))))

(define apply-func
  (lambda (func-record args)
    (cond
      [(primitive? func-record) (apply-primitive (second func-record) args)] ;TODO
      [(non-primitive? func-record) (apply-closure (second func-record) args)])))

(define apply-closure
  (lambda (c-record args)
    (meaning
      (body-of c-record)
      (extend-table
        (build
          (formals-of c-record)
          args)
        (table-of c-record)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      [(eq? name 'car)
        (cons (first vals) (second vals))]
      [(eq? name 'car)
        (car (first vals))]
      [(eq? name 'cdr)
        (cdr (first vals))]
      [(eq? name 'null?)
        (null? (first vals))]
      [(eq? name 'eq?)
        (eq? (first vals) (second vals))]
      [(eq? name 'atom?)
        (atom? (first vals))]
      [(eq? name 'zero?)
        (zero? (first vals))]
      [(eq? name 'add1)
        (add1 (first vals))]
      [(eq? name 'sub1)
        (sub1 (first vals))]
      [(eq? name 'number?)
        (number? (first vals))])))

(define func car)

(define args cdr)

(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

;;; table manipulation functions ;;;;;;;;;;;;;;;;;;;;;;

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
        (lookup-in-entry
          name
          (car table)
          (lambda (name)
            (lookup-in-table name (cdr table) table-f)))))))

(define extend-table cons)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
      name
      (first entry)
      (second entry)
      entry-f)))

(define lookup-in-entry-help
  (lambda (name names vals entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? name (car names)) (car vals))
      (else (lookup-in-entry-help
              name
              (cdr names)
              (cdr vals)
              entry-f)))))

; Unit tests

(check-equal?
    (lookup-in-entry 'bar (build '(foo bar) '(42 16)) (lambda x #f))
    16)
(check-equal?
    (lookup-in-entry 'foo (build '(foo bar) '(42 16)) (lambda x #f))
    42)
(check-false (lookup-in-entry 'foobar (build '(foo bar) '(42 16)) (lambda x #f)))

; lookup-in-table
; with one entry the same as lookup-in-entry
(check-equal?
    (lookup-in-table 'foo `(,(build '(foo bar) '(42 16))) (lambda x #f))
    42)
(check-equal?
    (lookup-in-table 'bar `(,(build '(foo bar) '(42 16))) (lambda x #f))
    16)
(check-equal?
    (lookup-in-table 'foobar `(,(build '(foo bar) '(42 16))) (lambda x #f))
    #f)

; earlier entries dominate later ones
(check-equal?
    (lookup-in-table 'bar `(,(build '(foo bar) '(42 16)) ,(build '(foo bar) '(16 42))) (lambda x #f))
    16)
(check-equal?
    (lookup-in-table 'foo `(,(build '(foo bar) '(42 16)) ,(build '(foo bar) '(16 42))) (lambda x #f))
    42)

; will fall back on later ones if neccessary
(check-equal?
    (lookup-in-table 'foobar `(,(build '(foo bar) '(42 16)) ,(build '(foo bar foobar) '(16 42 4))) (lambda x #f))
    4)
(check-equal?
    (lookup-in-table 'foobar `(,(build '(foo bar) '(42 16)) ,(build '(foo bar) '(16 42))) (lambda x #f))
    #f)

; TODO unit tests for a lot of functions

; simple integration tests

(check-equal?
  (value '(add1 1))
  2)

(check-equal?
  (value '((lambda (x) (add1 x)) 3))
  4)

; the ultimate test case :)
(check-equal?
  (value ; interpreter
  '((
   (lambda (mk-func) ; Y Combinator
    ((lambda (g) (g g))
      (lambda (f)
        (mk-func (lambda (x y) ((f f) x y))))))
    (lambda (f) ; open-recursive form of the Ackermann function
      (lambda (n m)
        (cond
          [(eq? n 0) (add1 m)]
          [(eq? m 0) (f (sub1 n) 1)]
          [else (f (sub1 n) (f n (sub1 m)))]))))
     3 3))
  61)
