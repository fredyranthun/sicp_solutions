#lang racket
(require rackunit)
;; Applicative order evaluation
;; calculates first the operator and then the operands

;; Normal order evaluation
;; calculates the operands first and then applies the operator

;; Exercise 1.1

10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(define a 3) ;; no output
(define b (+ a 1)) ;; no outpu
(+ a b (* a b)) ;; 19
(= a b) ;; #f
(if (and (> b a) (< b (* a b))) b a) ;; 4
(cond
  [(= a 4) 6]
  [(= b 4) (+ 6 7 a)]
  [else 25]) ;; 16
(+ 2 (if (> b a) b a)) ;; 6
(* (cond
     [(> a b) a]
     [(< a b) b]
     [else -1])
   (+ a 1)) ;; 16

;; Exercise 1.2
(define num (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))))
(define den (* 3 (- 6 2) (- 2 7)))
(/ num den)

;; Exercise 1.3

(define (square-sum a b)
  (+ (* a a) (* b b)))

(define (p a b c)
  (if (> a b)
      (square-sum a (max b c))
      (square-sum b (max a c))))

(check-equal? (p 1 2 3) 13)
(check-equal? (p 1 3 2) 13)
(check-equal? (p 3 2 1) 13)
(check-equal? (p 3 1 2) 13)
(check-equal? (p 2 1 3) 13)
(check-equal? (p 2 3 1) 13)

;; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 10 -11) ;; 21

;; in this case, the operator will be + in the case b is bigger than 0, and - otherwise.
;; it is an interesting use case for compound operators, allowing us to sum a with the absolute value of b.

;; Exercise 1.5

;;(define (p)
;;  (p))
;; (define (test x y)
;;   (if (= x 0) x y))
;; (test 0 (p-2))

;; in the case of normal order evalution, the value returned or printed is 0, otherwise, if it uses the
;; applicative order evalution, then it will be caught in an infinite loop, successively calling p-2.
;; This occurs because the operands will be evaluate before the the body of the funtion 'test' being
;; evaluated in the applicative order.

;; Exercise 1.6

;; The new-if procedure will not work in the same way as the conventional if construct,
;; since it will evaluate both operands before deciding the branch to follow.
;; In the case of the sqrt-iter procedure, the recursion will never end, since it will always
;; evaluate the second branch in the new-if.

;; Exercise 1.7

(define (good-enough? guess x)
  (< (abs (- (sqr guess) x)) 0.001))

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))
;; (my-sqrt 2)
;; (sqrt 2)

;; (my-sqrt 4)
;; (sqrt 4)

(my-sqrt 0.001)
(sqrt 0.001)
;; For small numbers, the precision is not enough for doing the correct number of
;; iterations.

;; (my-sqrt 1000000000000123123)
;; (sqrt 1000000000000123123)

;; For greater numbers, the approximation will take too long to converge to the correct result,
;; and sometimes won't finish.

(define (new-good-enough? guess new-guess)
  (< (abs (/ (- guess new-guess) guess)) 0.001))

(define (new-sqrt-iter guess x)
  (let ([improved-guess (improve guess x)])
    (if (new-good-enough? guess improved-guess)
        guess
        (new-sqrt-iter improved-guess x))))

(define (my-new-sqrt x)
  (new-sqrt-iter 1.0 x))

(my-new-sqrt 2)
(sqrt 2)

(my-new-sqrt 0.001)
(sqrt 0.001)
;; the result was closer to the correct square root.

(my-new-sqrt 1000000000000123123)
(sqrt 1000000000000123123)

;; the calculation finished, the precision was limited to the size of number.

(define (improve-cube guess x)
  (/ (+ (/ x (sqr guess)) (* 2 guess)) 3.0))

(define (good-enough-cube? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (cube-root-iter guess x)
  (if (good-enough-cube? guess x)
      guess
      (cube-root-iter (improve-cube guess x) x)))

(define (cube-root x)
  (cube-root-iter 1.0 x))

(cube-root 64)

(define (sqrt-block x)
  (define (good-enough? guess)
    (< (abs (- (sqr guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt-block 4)
