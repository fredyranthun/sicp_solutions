#lang racket

;; (a/b) + (c/d) = (ad + bc) / bd
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))

;; (a/b) - (c/d) = (ad - bc) / bd
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))

;; (a/b) * (c/d) = (ac) / (bd)
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))

;; (a/b) / (c/d) = (a/b) * (d/c) = (ad) / (bc)
(define (div-rat x y)
  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))

;; (a/b) = (c/d) if ad = bc
(define (equal-rat? x y)
  (= (* (numer x) (denom y)) (* (numer y) (denom x))))

;; Version 1
; (define (make-rat n d) (cons n d))

;; Version 2
; (define (make-rat n d)
;   (let ([g (gcd n d)]) (cons (/ n g) (/ d g))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Exercise 2.1

(define (make-rat n d)
  (let ([g (gcd n d)]
        [sign (if (>= (* n d) 0) 1 -1)])
    (cons (* sign (/ (abs n) g)) (/ (abs d) g))))

;; Exercise 2.2

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (let ([x (average (x-point (start-segment s)) (x-point (end-segment s)))]
        [y (average (y-point (start-segment s)) (y-point (end-segment s)))])
    (make-point x y)))

(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 1.0 1.0))))
