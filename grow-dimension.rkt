#lang racket
(require "driver.rkt" math suffixtree)

; assumption - cur is to be joined on the minor axis and that rotation is in centers and up to date with latest known. That is, the rotation expressed in centers
; includes all representatives of that permutation from boxes.
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
  (define target-shape (list-update (vector->list input-shape) (sub1 (length (vector->list input-shape))) add1))
  (define data (add-to-end (ortho-data cur) (ortho-data other) target-shape))
  (define center (calculate-local-center data))
  (define diagonal (append (list (car (ortho-diagonals cur))) (map set-union (cdr (ortho-diagonals cur)) (drop-right (ortho-diagonals other) 1)) (list (last (ortho-diagonals other)))))
  (ortho data center diagonal))

(module+ test
  (require rackunit)
  (check-equal?
   (combine-winners (ortho
                     (array #[#["a" "b"] #["c" "d"]])
                     (array #[#["a"] #["c"]])
                     (list (set "a") (set "b" "c") (set "d")))
                    (ortho
                     (array #[#["b" "e"] #["d" "f"]])
                     (array #[#["b"] #["d"]])
                     (list (set "b") (set "e" "d") (set "f"))))
   (ortho
    (array #[#["a" "b" "e"] #["c" "d" "f"]])
    (array #[#["a" "b"] #["c" "d"]])
    (list (set "a") (set "b" "c") (set "d" "e") (set "f")))))

; a b e
; c d f

(define (get-phrases arr)
  (define dims (vector->list (array-shape arr)))
  (define volume (apply * dims))
  (define phrase-length (last dims))
  (define remaining-volume (/ volume phrase-length))
  (array->list* (array-reshape arr (list->vector (list remaining-volume phrase-length)))))

(define (get-words b)
  (define phrases (get-phrases b))
  (map last phrases))

; TODO calculate target shape more locally or bundle data together
; TODO eliminate casts
(define (add-to-end cur b target-shape)
  (define phrases (get-phrases cur))
  (define words (get-words b))
  (define desired-phrases (list*->array (map (λ (l r) (append l (list r))) phrases words) string?))
  (array-reshape desired-phrases (list->vector target-shape)))

(module+ test
  (require rackunit)
  (check-equal?
   (add-to-end
    (array #[#["a" "b"] #["c" "d"]])
    (array #[#["b" "e"] #["d" "f"]])
    '(2 3))
   (array #[#["a" "b" "e"] #["c" "d" "f"]])))

(define (calculate-local-center data)
  (define dims (array-shape data))
  (array-slice-ref data (calculate-center-dims dims)))

(module+ test
  (require rackunit)
  (check-equal?
   (calculate-local-center (array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]]))
   (array #[#[#["a"] #["c"]] #[#["e"] #["g"]]])))      

(define (calculate-center-dims dims)
  (define almost (map (λ (x) (range x)) (vector->list dims)))
  (list-update almost (sub1 (length almost)) (λ (l) (drop-right l 1))))