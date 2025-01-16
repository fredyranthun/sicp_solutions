#lang racket

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ([next (f guess)])
      ; (display guess)
      ; (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point cos 1)

(define (average a b)
  (/ (+ a b) 2))
(define (sqrt-fp x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(sqrt-fp 4)

;; Exercise 1.35
;; x² = x + 1
;; x = (x + 1) / x
;; x = 1 + 1/x
(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1.0))

golden-ratio

;; Exercise 1.36

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;; 35 steps without average damping

(define (average-damp f)
  (lambda (x) (average x (f x))))

(fixed-point (average-damp (lambda (x) (/ (log 1000) (log x)))) 2.0)
;; 10 steps with average damping

;; Exercise 1.37

(define (cont-frac n d k)
  (define (iter i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(/ 1.0 (cont-frac (lambda (_i) 1.0) (lambda (_i) 1.0) 10))
(/ 1.0 (cont-frac (lambda (_i) 1.0) (lambda (_i) 1.0) 15)) ;; 5 digit precision
(/ 1.0 (cont-frac (lambda (_i) 1.0) (lambda (_i) 1.0) 30)) ;; already has more than 10 digit precision
(/ 1.0 (cont-frac (lambda (_i) 1.0) (lambda (_i) 1.0) 40)) ;; full precision (same as 1000)
(/ 1.0 (cont-frac (lambda (_i) 1.0) (lambda (_i) 1.0) 50))
(/ 1.0 (cont-frac (lambda (_i) 1.0) (lambda (_i) 1.0) 100))
; (/ 1.0 (cont-frac (lambda (_i) 1.0) (lambda (_i) 1.0) 1000))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(/ 1.0 (cont-frac-iter (lambda (_i) 1.0) (lambda (_i) 1.0) 10))
(/ 1.0 (cont-frac-iter (lambda (_i) 1.0) (lambda (_i) 1.0) 15))
(/ 1.0 (cont-frac-iter (lambda (_i) 1.0) (lambda (_i) 1.0) 30))
(/ 1.0 (cont-frac-iter (lambda (_i) 1.0) (lambda (_i) 1.0) 40))
(/ 1.0 (cont-frac-iter (lambda (_i) 1.0) (lambda (_i) 1.0) 50))
(/ 1.0 (cont-frac-iter (lambda (_i) 1.0) (lambda (_i) 1.0) 100))

;; 1.38
(define (den i)
  (if (= (remainder (- i 2) 3) 0)
      (* (/ (+ i 1) 3) 2)
      1))

(+ 2 (cont-frac (lambda (_i) 1.0) den 10)) ;; 6 decimal digits precision
(+ 2 (cont-frac (lambda (_i) 1.0) den 20))

;; Exercise 1.39

;; (i - 1) * 2 + 1
;; 2*i - 1

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1)
                   x
                   (- (sqr x))))
             (lambda (i) (- (* 2.0 i) 1))
             k))

(tan-cf 1 10)
(tan 1)
(tan-cf 2 10)
(tan 2)

;; Exercise 1.40
(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (expt x 3) (* a (expt x 2)) (* b x) c)))

(newtons-method (cubic 1 1 1) 1)

;; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))
((double inc) 1)

(double double)
(lambda (p) (double (double p)))
;; calling (double p) -> (p2)
; (lambda (p) (double p2))
; (lambda (p) (lambda (x) (p2 (p2 x))))
(((double double) inc) 1) ; 5 -> procedure is applied 4 times.
;; when I call (double (double double)), this previous procedure is applied 4 times,
;; or 16 the initial procedure.
;; So inc will be applied 16 times in the parameter 5, resulting in 21.

(((double (double double)) inc) 5)

;; Exercise 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

((compose sqr inc) 6)

;; Exercise 1.43

(define (repeat f n)
  (if (= n 1)
      f
      (compose f (repeat f (- n 1)))))

((repeat sqr 2) 5)

;; Exercise 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
  ((repeat smooth n) f))

;; Exercise 1.45

(define (nth-root x n)
  (fixed-point ((repeat average-damp 2) (lambda (y) (/ x (expt y (- n 1))))) 2.0))

(nth-root 262144 6)

;; Exercise 1.46

(define (iterative-improve good-enough? improve)
  (define (aux x)
    (if (good-enough? x)
        (improve x)
        (aux (improve x))))
  (lambda (x) (aux x)))

(define (fixed-point-2 f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve good-enough? f) first-guess))

(fixed-point cos 1)
(fixed-point-2 cos 1)

(define (sqrt-ii x)
  (define (good-enough? guess)
    (< (abs (- x (sqr guess))) tolerance))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt-ii 4)
