#lang racket
(require math/number-theory)

(define (enumerate-interval start end)
  (range start (+ end 1)))

(define (enumerate-tree tree)
  (cond
    [(null? tree) null]
    [(not (pair? tree)) (list tree)]
    [else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree)))]))

(define (sum-odd-squares tree)
  (foldl + 0 (map sqr (filter odd? (enumerate-tree tree)))))

;; Exercise 2.33

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (map-2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append-2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-2 sequence)
  (accumulate (lambda (_ acc) (+ acc 1)) 0 sequence))

;; Exercise 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;; Exercise 2.35

(define (count-leaves t)
  (accumulate + 0 (map (lambda (_x) 1) (enumerate-tree t))))

(count-leaves (list 1 (list 2 3 (list 4 5 6) (list 7 8))))

;; Exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs)) (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;; Exercise 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ([cols (transpose n)]) (map (lambda (r) (matrix-*-vector cols r)) m)))

;; Exercise 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(fold-right / 1 (list 1 2 3))
;; (/ 3 1) -> 3 ;; (/ 2 3) -> 2/3 ;; (/ 1 2/3) -> 3/2
(fold-left / 1 (list 1 2 3))
;; (/ 1 1) -> 1 ;; (/ 1 2) -> 1/2 ;; (/ 1/2 3) -> 1/6
(fold-right list null (list 1 2 3))
;; (list 3 null) -> (list 2 (list 3 null)) -> (list 1 (list 2 (list 3 null)))
(fold-left list null (list 1 2 3))
;;(list null 1) -> (list (list null 1) 2) -> (list (list (list null 1) 2) 3)

;; For operations that are both commutative and associative, fold-left and fold-right will
;; produce the same result.

;; Exercise 2.39
(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) null sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

;;

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(prime-sum-pairs 10)

(define (permutations s)
  (if (null? s) ;; empty set
      (list null) ;; sequence containing empty set
      (flatmap (lambda (x) (map (lambda (p) (cons x p)) (permutations (remove x s)))) s)))

;; Exercise 2.40

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs-2 n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

;; Exercise 2.41

(define (unique-triples n)
  (flatmap (lambda (p)
             (let ([i (car p)]
                   [j (cadr p)])
               (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1)))))
           (unique-pairs n)))

(define (sum-triple-equals-s? triple s)
  (= s (+ (car triple) (cadr triple) (caddr triple))))

(define (triples-sum-to-s n s)
  (filter (lambda (t) (sum-triple-equals-s? t s)) (unique-triples n)))

;; Exercise 2.42

;; a queen is a pair like: (list column row)
;; empty board is a list with null
;; on example of positions: (list )
(define empty-board null)

(define (adjoin-position r k rest-of-queens)
  (cons (list k r) rest-of-queens))

; (define (safe? k positions)
;   (let ([k-queen (car (filter (lambda (p) (= k (car p))) positions))])
;     (empty? (filter (lambda (p)
;                       (or (= (cadr p) (cadr k-queen))
;                           (and (= (abs (- (car p) (car k-queen)))
;                                   (abs (- (cadr p) (cadr k-queen)))))))
;                     (remove k-queen positions)))))

(define (safe? k positions)
  (let ([k-queen (findf (lambda (p) (= k (car p))) positions)])
    (not (findf (lambda (p)
                  (or (= (cadr p) (cadr k-queen))
                      (and (= (abs (- (car p) (car k-queen))) (abs (- (cadr p) (cadr k-queen)))))))
                (remove k-queen positions)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)
;; Two solutions

;; | |x| | |
;; | | | |x|
;; |x| | | |
;; | | |x| |

;; | | |x| |
;; |x| | | |
;; | | | |x|
;; | |x| | |

(queens 5)
