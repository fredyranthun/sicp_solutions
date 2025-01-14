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

;; Exercise 1.20

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Applicative Order Evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40)) ; 1
(gcd 40 6)
(gcd 6 (remainder 40 6)) ; 2
(gcd 6 4)
(gcd 4 (remainder 6 4)) ; 3
(gcd 4 2)
(gcd 2 (remainder 4 2)) ; 4
(gcd 2 0)
2

;; Normal Order Evaluation
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; and continues until b equals 0.

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond
    [(> (sqr test-divisor) n) n]
    [(divides? test-divisor n) test-divisor]
    [else (find-divisor n (next test-divisor))]))
(define (divides? a b)
  (= (remainder b a) 0))
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 11)
(prime? 21)
(prime? 1023)

(define (expmod base exp m)
  (cond
    [(= exp 0) 1]
    [(even? exp) (remainder (sqr (expmod base (/ exp 2) m)) m)]
    [else (remainder (* base (expmod base (- exp 1) m)) m)]))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond
    [(= times 0) true]
    [(fermat-test n) (fast-prime? n (- times 1))]
    [else false]))

;; Exercise 1.21

(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999) ; 7

;; 1.22
(define (current-time-seconds)
  (/ (current-inexact-milliseconds) 1000))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time-seconds)))
(define (start-prime-test n start-time)
  (cond
    [(prime? n) (report-prime (- (current-time-seconds) start-time))]
    [else false]))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  true)

(define (search-for-primes s n)
  (cond
    [(= n 0)
     (newline)
     (display "Finish.")]
    [(even? s) (search-for-primes (+ s 1) n)]
    [(timed-prime-test s) (search-for-primes (+ s 2) (- n 1))]
    [else (search-for-primes (+ s 2) n)]))

(search-for-primes 1000 3)
(search-for-primes 10000 3)
(search-for-primes 1000000 3)
