#lang racket

(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ([left-size (quotient (- n 1) 2)])
        (let ([left-result (partial-tree elts left-size)])
          (let ([left-tree (car left-result)]
                [non-left-elts (cdr left-result)]
                [right-size (- n (+ left-size 1))])
            (let ([this-entry (car non-left-elts)]
                  [right-result (partial-tree (cdr non-left-elts) right-size)])
              (let ([right-tree (car right-result)]
                    [remaining-elts (cdr right-result)])
                (cons (make-tree this-entry left-tree right-tree) remaining-elts))))))))

;; a.
;; We start by define the left-tree, recursively calling partial-tree with the value of left-size,
;; being (n - 1)/ 2.
;; The procedure will return the left tree + the remaining elements.
;; the first element is the entry of the tree.
;; the remaining will be used to form the right-tree. The size of right tree is
;; n - left-size - 1.
;; We then return the new tree together with the remaining elements, if any.
;; When we call list->tree with (1 3 5 7 9 11), we call
;; (partial-tree (1 3 5 7 9 11) 6)
;; So the left-size = 2 -> (quotient 5 2)
;; elts = (1 3 5 7 9 11)
;; left-result is (partial-tree elts 2)
;; Its left-size will be 0 -> (quotient 1 2)
;; so it will generate an empty branch.
;; entry -> 1
;; right-tree -> 3
;; the left branch of the new tree is (make-tree 1 null (make-tree 3 null null))
;; Our main entry is 5, and the size of right branch will be 3.
;; right-result is (partial-tree (7 9 11) 3)
;; size of its left branch will be one
;; so left branch is (make-tree 7 null null)
;; entry is 9
;; right branch is (make-tree 11 null null)
;; final result is (make-tree 5 (1 null (3 null null)) (9 (7 null null) (11 null null)))
;; Value returned by the REPL: '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;; b.
;; O(n)
