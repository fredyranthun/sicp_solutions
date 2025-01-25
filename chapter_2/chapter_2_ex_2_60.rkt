#lang racket

(define (element-of-set? x set)
  (cond
    [(null? set) false]
    [(equal? x (car set)) true]
    [else (element-of-set? x (cdr set))]))

(define (adjoin-set x set)
  (cons x set))

(define (intersection-set set1 set2)
  (filter (lambda (x) (element-of-set? x set2)) set1))

(define (union-set set1 set2)
  (append set1 set2))

;; The sets will grow faster, despite the operations of union and adjoin will be faster
;; O(n) and O(1), respectively.
;; If we have to accomplish many union and adjoin operations, maybe is a good estrategy, considering
;; the possibility of removing the duplicates in a certain frequency.
