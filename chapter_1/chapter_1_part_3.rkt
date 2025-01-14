#lang racket

;; Exercise 1.16

(define (fast-expt-iter a b n)
  (cond
    [(= n 0) a]
    [(not (even? n)) (fast-expt-iter (* a b) b (- n 1))]
    [else (fast-expt-iter a (sqr b) (/ n 2))]))

(define (fast-expt b n)
  (fast-expt-iter 1 b n))

(fast-expt 2 10)
(fast-expt 3 4)

;; Exercise 1.17
(define (double n)
  (* n 2))
(define (halve n)
  (/ n 2))

(define (fast-mult a b)
  (cond
    [(= b 0) 0]
    [(even? b) (fast-mult (double a) (halve b))]
    [else (+ a (fast-mult a (- b 1)))]))

(fast-mult 15 10)
(fast-mult 12 25)
(fast-mult 1 25)

;; Exercise 1.18
(define (fast-mult-iter a b acc)
  (cond
    [(= b 0) acc]
    [(even? b) (fast-mult-iter (double a) (halve b) acc)]
    [else (fast-mult-iter a (- b 1) (+ a acc))]))

(define (fast-mult-2 a b)
  (fast-mult-iter a b 0))

(fast-mult-2 15 10)
(fast-mult-2 12 25)

;; Exercise 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))

;; p' -> (+ (* p p) (* q q))
;; q' -> (+ (* q q) (* 2 p q))

(define (fib-iter a b p q count)
  (cond
    [(= count 0) b]
    [(even? count) (fib-iter a b (+ (* p p) (* q q)) (+ (* q q) (* 2 p q)) (/ count 2))]
    [else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1))]))

(fib 10)
