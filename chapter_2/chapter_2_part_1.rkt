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
  (display ")")
  (newline))

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment s)
  (let ([x (average (x-point (start-segment s)) (x-point (end-segment s)))]
        [y (average (y-point (start-segment s)) (y-point (end-segment s)))])
    (make-point x y)))

(print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 1.0 1.0))))

;; Exercise 2.3
(define (make-rectangle bottom-left top-right)
  (cons bottom-left top-right))

(define (bottom-left-rect r)
  (car r))

(define (top-right-rect r)
  (cdr r))

(define (rectangle-area rect)
  (let* ([bottom-left (bottom-left-rect rect)]
         [top-right (top-right-rect rect)]
         [width (- (x-point top-right) (x-point bottom-left))]
         [height (- (y-point top-right) (y-point bottom-left))])
    (* width height)))

(define (rectangle-perimeter rect)
  (let* ([bottom-left (bottom-left-rect rect)]
         [top-right (top-right-rect rect)]
         [width (- (x-point top-right) (x-point bottom-left))]
         [height (- (y-point top-right) (y-point bottom-left))])
    (+ (* 2 width) (* 2 height))))

;; Exercise 2.4
(define (cons-proc x y)
  (lambda (m) (m x y)))

(cons-proc 1 2)
(lambda (m) (m 1 2))

(define (car-proc z)
  (z (lambda (p _q) p)))

(car-proc (cons-proc 1 2))
((cons-proc 1 2) (lambda (p _q) p))
((lambda (m) (m 1 2)) (lambda (p _q) p))
((lambda (p _q) p) 1 2)
1

;; The cdr is analogous
(define (cdr-proc z)
  (z (lambda (_p q) q)))

(car-proc (cons-proc 1 2))
(cdr-proc (cons-proc 1 2))

;; Exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

;; zero is a procedure. It has as argument a procedure and returns another procedure. (f' -> f'')
;; The procedure returned is basically the identity, only return the unmodified argument.

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; add one returns a similar procedure than zero, applying the argument one time more than its
;; initial argument.

(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x))) ;; one

(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x)))) ;; two

(define two (lambda (f) (lambda (x) (f (f x)))))

;; will apply f n + m times in the parameter x
(define (sum n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;; Lists

;; Exercise 2.17

(define (my-last-pair items)
  (if (null? (cdr items))
      items
      (my-last-pair (cdr items))))

;; Exercise 2.18

(define (my-reverse items)
  (if (null? items)
      null
      (append (my-reverse (cdr items)) (cons (car items) null))))

;; Exercise 2.20

(define (same-parity a . items)
  (let ([pred (if (even? a) even? odd?)])
    (define (f items)
      (cond
        [(null? items) null]
        [(pred (car items)) (cons (car items) (f (cdr items)))]
        [else (f (cdr items))]))
    (cons a (f items))))

;; Exercise 2.21

(define (square-list items)
  (if (null? items)
      null
      (cons (sqr (car items)) (square-list (cdr items)))))

(define (square-list-2 items)
  (map sqr items))

;; Exercise 2.23

(define (my-for-each f items)
  (cond
    [(null? items) true]
    [else
     (f (car items))
     (my-for-each f (cdr items))]))

(define (count-leaves x)
  (cond
    [(null? x) 0]
    [(not (pair? x)) 1]
    [else (+ (count-leaves (car x)) (count-leaves (cdr x)))]))

;; Exercise 2.24

(list 1 (list 2 (list 3 4)))

;; -> v | -> V | null
;;    1      -> V | -> V | null
;;              2      -> V | -> 4 | null
;;                        3

;; (1 (2 (3 4)))
;; 1     (2 (3 4))
;;        2   (3 4)
;;             3  4

;; Exercise 2.25

(define a '(1 3 (5 7) 9))
(car (cdaddr a))
(define b '((7)))
(caar b)
(define c '(1 (2 (3 (4 (5 (6 7)))))))
(cadadr (cadadr (cadadr c)))

;; Exercise 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(list 1 2 3 4 5 6)

(cons x y)
(list (list 1 2 3) 4 5 6)

(list x y)
(list (list 1 2 3) (list 4 5 6))

;; Exercise 2.27

(define (deep-reverse items)
  (cond
    [(null? items) null]
    [(list? (car items)) (append (deep-reverse (cdr items)) (cons (deep-reverse (car items)) null))]
    [else (append (deep-reverse (cdr items)) (cons (car items) null))]))

;; Exercise 2.28

(define (fringe tree)
  (cond
    [(null? tree) null]
    [(not (pair? tree)) (list tree)]
    [else (append (fringe (car tree)) (fringe (cdr tree)))]))

;; Exercise 2.29

;; a.
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

;; b
(define (weight-mobile mobile)
  (if (null? mobile)
      0
      (+ (weight-branch (left-branch mobile)) (weight-branch (right-branch mobile)))))

(define (weight-branch branch)
  (let ([struct (branch-structure branch)])
    (if (number? struct)
        struct
        (weight-mobile struct))))

(define (total-weight mobile)
  (weight-mobile mobile))

(total-weight (make-mobile (make-branch 1 2) (make-branch 3 4)))

;; c.

(define (balanced? mobile)
  (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
       (balanced-branch? (left-branch mobile))
       (balanced-branch? (right-branch mobile))))

(define (torque branch)
  (* (weight-branch branch) (branch-length branch)))

(define (balanced-branch? branch)
  (let ([struct (branch-structure branch)])
    (if (number? struct)
        true
        (balanced? struct))))

;; d.
;; would need to change the selectors, instead of cadr, would use cdr.

;; Exercise 2.30

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (sqr sub-tree)))
       tree))

(define (square-tree-2 tree)
  (cond
    [(null? tree) null]
    [(pair? (car tree)) (cons (square-tree-2 (car tree)) (square-tree-2 (cdr tree)))]
    [else (cons (sqr (car tree)) (square-tree-2 (cdr tree)))]))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; Exercise 2.31

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define (square-tree-3 tree)
  (tree-map sqr tree))
(square-tree-3 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list null)
      (let ([rest (subsets (cdr s))]) (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))
;; (1 2 3)
;; rest -> subsets (2 3)
;; rest -> subsets (3)
;; rest -> subsets '() -> list '()

;; It works because it takes all the values without the car and append to them
;; those list of values with the car.
;; Starting with the list with a empty set, we add the last element (3) to it and append.
;; -> '() (3)
;; Then we take the previous element and add to all of the sets:
;; -> '() (3) (2) (2 3)
;; And so on:
;; -> '() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)
