#lang racket
(require "driver.rkt" math suffixtree)

; assumption - cur is to be joined on the minor axis and that rotation is in centers
(define (combine phrases centers cur)
  (define combine-candidates (hash-ref centers (ortho-center cur)))
  (define selected-candidates (filter (λ (b) (phrase-filter phrases cur b)) (set->list combine-candidates)))
  (map (λ (b) (combine-winners cur b)) selected-candidates))

(define (phrase-filter phrases cur b)
  (define lhs-phrases (get-phrases cur))
  (define rhs-words (get-words b))
  (define desired-phrases (map (λ (l r) (append l (list r))) lhs-phrases rhs-words))
  (andmap (λ (p) (tree-contains? phrases (vector->label (list->vector p)))) desired-phrases))

(define (combine-winners cur other)
  (define input-shape (array-shape (ortho-data cur)))
  (define target-shape (list-update array-shape (sub1 (length array-shape)) add1))
  (define data (add-to-end (ortho-data cur) (ortho-data other) target-shape))
  (define center (calculate-local-center data))
  (define diagonal (append (list (car (ortho-diagonals cur))) (map set-union (cdr (ortho-diagonals cur)) (drop-right (ortho-diagonals other) 1)) (list (last (ortho-diagonals other)))))
  (ortho data center diagonal))

(define (get-phrases b)
  (define arr (ortho-data b))
  (define dims (array-shape arr))
  (define volume (apply * dims))
  (define phrase-length (last dims))
  (define remaining-volume (/ volume phrase-length))
  (array->list* (array-reshape arr (list->vector (list remaining-volume phrase-length)))))

(define (get-words b)
  (define phrases (get-phrases (ortho-data b)))
  (map last phrases))

(define (add-to-end d1 d2 target-shape) ; TODO use vector transforms instead of lists
  (define phrases (ortho-data d1))
  (define words (ortho-data d2))
  (array-reshape (list*->array (map (λ (l r) (append l (list r))) phrases words)) target-shape))

(define (calculate-local-center data)
  (define dims (array-shape data))
  (array-slice-ref data (calculate-center-dims dims)))

(define (calculate-center-dims dims)
  (define almost (map (λ (x) (range x)) dims))
  (list-update almost (sub1 (length almost)) (λ (l) (drop-right l 1))))