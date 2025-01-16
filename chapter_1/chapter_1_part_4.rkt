#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (cube x)
  (* x x x))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

;; Exercise 1.29

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (next x)
    (+ x (* 2 h)))
  (define (term-1 x)
    (* 4 (f x)))
  (define (term-2 x)
    (* 2 (f x)))
  (* (/ h 3.0)
     (+ (f a) (f b) (sum term-1 (+ a h) next (- b h)) (sum term-2 (+ a (* 2 h)) next (- b h)))))

(integral-simpson cube 0 1 100)
(integral-simpson cube 0 1 1000)

;; Exercise 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (pi-sum-iter a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum-iter pi-term a pi-next b))

(* 8 (pi-sum-iter 1 1000))

;; Exercise 1.31

(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial n)
  (define (identity x)
    x)
  (define (inc x)
    (+ x 1))
  (product identity 1 inc n))

(factorial 7)

(define (pi-prod n)
  (define (next x)
    (+ x 2))
  (define (identity x)
    x)
  (/ (* (product identity 2 next n) (product identity 4 next n))
     (* (product identity 3 next (- n 1)) (product identity 3 next (+ n 1)))))

(* 4.0 (pi-prod 10000))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (pi-prod-iter n)
  (define (next x)
    (+ x 2))
  (define (identity x)
    x)
  (/ (* (product-iter identity 2 next n) (product-iter identity 4 next n))
     (* (product-iter identity 3 next (- n 1)) (product-iter identity 3 next (+ n 1)))))

(* 4.0 (pi-prod-iter 10000))

;; Exercise 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

(define (pi-sum-acc a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum-acc pi-term a pi-next b))

(* 8 (pi-sum-acc 1 1000))

(define (pi-prod-acc n)
  (define (next x)
    (+ x 2))
  (define (identity x)
    x)
  (/ (* (product-acc identity 2 next n) (product-acc identity 4 next n))
     (* (product-acc identity 3 next (- n 1)) (product-acc identity 3 next (+ n 1)))))

(* 4.0 (pi-prod-acc 10000))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (inc x)
  (+ x 1))
(define (identity x)
  x)

(accumulate-iter * 1 identity 1 inc 7)

;; Exercise 1.33
(define (filtered-accumulate combiner null-value pred term a next b)
  (cond
    [(> a b) null-value]
    [(pred a) (combiner (term a) (filtered-accumulate combiner null-value pred term (next a) next b))]
    [else (filtered-accumulate combiner null-value pred term (next a) next b)]))

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

(define (sum-of-primes a b)
  (filtered-accumulate + 0 prime? identity a inc b))

(sum-of-primes 2 10) ;; 17

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relative-primes? a b)
  (= (gcd a b) 1))

(define (prod-relative-primes n)
  (define (pred x)
    (relative-primes? n x))
  (filtered-accumulate * 1 pred identity 1 inc (- n 1)))

(prod-relative-primes 10) ; 189
