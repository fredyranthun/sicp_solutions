#lang racket

;; Exercise 2.54
(define (equal-list? l1 l2)
  (cond
    [(and (null? l1) (null? l2)) true]
    [(or (null? l1) (null? l2)) false]
    [(eq? (car l1) (car l2)) (equal-list? (cdr l1) (cdr l2))]
    [else false]))

(equal-list? '(a list of words) '(a list of words))
(equal-list? '(a list of words) '(a (list of) words))
(equal-list? '(a list of words) '(a list of))

;; Differentiation

(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp) (if (same-variable? exp var) 1 0)]
    [(sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var))]
    [(product? exp)
     (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
               (make-product (deriv (multiplier exp) var) (multiplicand exp)))]
    [(exponentiation? exp)
     (make-product (make-product (exponent exp)
                                 (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
                   (deriv (base exp) var))]
    [else (error "unknown expression type: DERIV" exp)]))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; (define (make-sum a1 a2)
;   (cond
;     [(=number? a1 0) a2]
;     [(=number? a2 0) a1]
;     [(and (number? a1) (number? a2)) (+ a1 a2)]
;     [else (list '+ a1 a2)]))

(define (=number? exp n)
  (and (number? exp) (= exp n)))

(define (make-product m1 m2)
  (cond
    [(or (=number? m1 0) (=number? m2 0)) 0]
    [(=number? m1 1) m2]
    [(=number? m2 1) m1]
    [(and (number? m1) (number? m2)) (* m1 m2)]
    [else (list '* m1 m2)]))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (apply make-sum (cddr s)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (caddr p))

;; Exercise 2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (make-exponentiation base exponent)
  (cond
    [(=number? exponent 0) 1]
    [(=number? exponent 1) base]
    [(and (number? base) (number? exponent)) (expt base exponent)]
    [else (list '** base exponent)]))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

;; 2.57

(define (make-sum add . aug)
  (cond
    [(null? aug) add]
    [(=number? add 0) (apply make-sum aug)]
    [(=number? (car aug) 0) (apply make-sum add (cdr aug))]
    [(and (number? add) (number? (car aug))) (apply make-sum (+ add (car aug)) (cdr aug))]
    [else (append (list '+ add) aug)]))
