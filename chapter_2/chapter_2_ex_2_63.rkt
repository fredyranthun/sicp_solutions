#lang racket

(define (entry tree)
  (car tree))
(define (left-branch tree)
  (cadr tree))
(define (right-branch tree)
  (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define tree-1
  (make-tree 7
             (make-tree 3 (make-tree 1 null null) (make-tree 5 null null))
             (make-tree 9 null (make-tree 11 null null))))

(define tree-2
  (make-tree 3
             (make-tree 1 null null)
             (make-tree 7 (make-tree 5 null null) (make-tree 9 null (make-tree 11 null null)))))

(define tree-3
  (make-tree 5
             (make-tree 3 (make-tree 1 null null) null)
             (make-tree 9 (make-tree 7 null null) (make-tree 11 null null))))

(define (element-of-set? x set)
  (cond
    [(null? set) false]
    [(= x (entry set)) true]
    [(< x (entry set)) (element-of-set? x (left-branch set))]
    [(> x (entry set)) (element-of-set? x (right-branch set))]))

(define (adjoin-set x set)
  (cond
    [(null? set) (make-tree x '() '())]
    [(= x (entry set)) set]
    [(< x (entry set)) (make-tree (entry set) (adjoin-set x (left-branch set)) (right-branch set))]
    [(> x (entry set)) (make-tree (entry set) (left-branch set) (adjoin-set x (right-branch set)))]))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree) (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

;; a.
;; both procedures produce the same ordered list for the trees 1, 2 and 3:
;; '(1 3 5 7 9 11)

;; b.
;; first procedure uses a append method -> complexity O(n log n)
;; (append (tree->list-1 (left-branch tree)) (cons 7 (tree->list-1 (right-branch tree))))
;; (append (append (1 null null) (cons 3 (5 null null))) (cons 7 (append () (cons 9 (11 null null))))
;; (append (append (1) (cons 3 (5))) (cons 7 (cons 9 (11))))
;; (append (append (1) (3 5)) (cons 7 (9 11)))
;; (append (1 3 5) (7 9 11))
;; (1 3 5 7 9 11)

;; The second one has complexity O(n)
