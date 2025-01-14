#lang racket

;; Exercise 1.9

(define (inc a)
  (+ a 1))
(define (dec a)
  (- a 1))

(define (sum a b)
  (if (= a 0)
      b
      (inc (sum (dec a) b))))

(define (sum-2 a b)
  (if (= a 0)
      b
      (sum-2 (dec a) (inc b))))

;; first sum procedure
(sum 4 5)
(inc (sum (dec 4) 5))
(inc (sum 3 5))
(inc (inc (sum (dec 3) 5)))
(inc (inc (sum 2 5)))
(inc (inc (inc (sum (dec 2) 5))))
(inc (inc (inc (sum 1 5))))
(inc (inc (inc (inc (sum (dec 1) 5)))))
(inc (inc (inc (inc (sum 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

;; second sum procedure

(sum-2 4 5)
(sum-2 (dec 4) (inc 5))
(sum-2 3 6)
(sum-2 (dec 3) (inc 6))
(sum-2 2 7)
(sum-2 (dec 2) (inc 7))
(sum-2 1 8)
(sum-2 (dec 1) (inc 8))
(sum-2 0 9)
9

;; The first process is recursive, the second one iterative

;; Exercise 1.10
(define (A x y)
  (cond
    [(= y 0) 0]
    [(= x 0) (* 2 y)]
    [(= y 1) 2]
    [else (A (- x 1) (A x (- y 1)))]))

;; (A 1 10)
(A 1 10)
(A 0 (A 1 9))
(* 2 (A 1 9))
(* 2 (A 0 (A 1 8)))
(* 2 (* 2 (A 1 8)))
(* 2 (* 2 (A 0 (A 1 7))))
(* 2 (* 2 (* 2 (A 1 7))))
(* 2 (* 2 (* 2 (* 2 (A 1 6)))))
(* 2 (* 2 (* 2 (* 2 (* 2 (A 1 5))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 4)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 3))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (A 1 1))))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 2)))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 4))))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 (* 2 8)))))))
(* 2 (* 2 (* 2 (* 2 (* 2 (* 2 16))))))
(* 2 (* 2 (* 2 (* 2 (* 2 32)))))
(* 2 (* 2 (* 2 (* 2 64))))
(* 2 (* 2 (* 2 128)))
(* 2 (* 2 256))
(* 2 512)
1024

;; (A 2 4)
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 (* 2 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (* 2 (A 1 3)))
(A 1 (* 2 (A 0 (A 1 2))))
(A 1 (* 2 (* 2 (A 1 2))))
(A 1 (* 2 (* 2 (A 0 (A 1 1)))))
(A 1 (* 2 (* 2 (A 0 2))))
(A 1 (* 2 (* 2 (* 2 2))))
(A 1 (* 2 (* 2 4)))
(A 1 (* 2 8))
(A 1 16)
;; continues.
;; turns out to be 2 ^ 16, as (A 1 10) is 2 ^ 10.
65536

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (A 0 (A 1 1)))
(A 2 (A 0 2))
(A 2 (* 2 2))
(A 2 4)
;; which is 2 ^ 16
65536

; (define (f n)
; (A 0 n))
;; (f n) computes n * 2

; (define (g n)
; (A 1 n))
;; (g n) computes 2 ^ n

; (define (h n)
; (A 2 n))
;; (h n) computes 2 ^ (2 ^ n)

; (define (k n)
; (* 5 n n))
;; (k n) computes 5 * n ^ 2

;; Exercise 1.11

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
(f 0) ;; 0
(f 1) ;; 1
(f 2) ;; 2
(f 3)
(+ (f 2) (* 2 (f 1)) (* 3 (f 0)))
(+ 2 (* 2 1) (* 3 0))
(+ 2 2 0)
4

(define (f-iter a b c count)
  (if (= count 0)
      c
      (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

(define (f-linear n)
  (f-iter 2 1 0 n))

(f 3)
(f-linear 3)
(f 4)
(f-linear 4)

;; Exercise 1.12

(define (pascal row col)
  (if (or (= row col) (= col 1))
      1
      (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col))))

;; row 1
(pascal 1 1)
;; row 2
(pascal 2 1)
(pascal 2 2)
;; row 3
(pascal 3 1)
(pascal 3 2)
(pascal 3 3)
;; row 4
(pascal 4 1)
(pascal 4 2)
(pascal 4 3)
(pascal 4 4)
;; row 5
(pascal 5 1)
(pascal 5 2)
(pascal 5 3)
(pascal 5 4)
(pascal 5 5)

;; Exercise 1.15
(define (cube x)
  (* x x x))
(define (p x)
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)
(p (sine (/ 12.15 3)))
(p (sine 4.05))
(p (p (sine (/ 4.05 3))))
(p (p (sine 1.35)))
(p (p (p (sine (/ 1.35 3)))))
(p (p (p (sine 0.45))))
(p (p (p (p (sine (/ 0.45 3))))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine (/ 0.15 3)))))))
(p (p (p (p (p (sine 0.05))))))
(p (p (p (p (p 0.05)))))

;; Space and number of steps grow by log3.
