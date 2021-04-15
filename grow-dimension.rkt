#lang racket
(require "driver.rkt" math suffixtree racket/trace)

(define (drive-in s cur)
  (define dims (vector->list (array-shape (ortho-data cur))))
  (define increment (make-increment s cur))
  (define centers (for/fold ([centers (state-centers s)])
                            ([box increment])
                    (hash-update centers (calculate-foreign-lhs-center box) (λ (s) (set-add s box)) (set))))
  (define new-dims (list-update dims (sub1 (length dims)) add1))
  (define boxes (make-boxes increment s))
  (state centers (state-next s) (state-prev s) boxes (state-phrases s) (state-raw s) (list->set increment)))
(provide drive-in)

(define (make-increment s cur)
  (apply append (map rotations (combine (state-phrases s) (state-centers s) cur))))

(define (make-boxes increment s)
  (for/fold ([boxes (state-boxes s)])
            ([box increment])
    (hash-update boxes (vector->list (array-shape (ortho-data box))) (λ (s) (set-union s (set box))) (set))))

(define (rotations o)
  (define arr-to-ortho ((curry r2o) o))
  (define arrs (rots (ortho-data o)))
  (cons o (map arr-to-ortho arrs)))

(define (calculate-center-local-dims dims)
  (define almost (map (λ (x) (range x)) (vector->list dims)))
  (list-update almost (sub1 (length almost)) cdr))

(define (r2o o arr)
  (ortho arr (calculate-rhs-local-center arr) (ortho-diagonals o)))
  
(define (rots arr)
  (define dims (array-dims arr))
  (map (λ (ax)
         (array-axis-swap arr (sub1 dims) ax))
       (range (sub1 dims))))

(module+ test
  (require rackunit)
  (define phrases-three (make-tree))
  (tree-add! phrases-three (vector->label/with-sentinel (list->vector (list "a" "b" "e"))))
  (tree-add! phrases-three (vector->label/with-sentinel (list->vector (list "c" "d" "f"))))
  (check-equal? (drive-in (state
                           (hash (array #[#["b"] #["d"]])
                                 (set
                                  (ortho
                                   (array #[#["b" "e"] #["d" "f"]])
                                   (array #[#["e"] #["f"]])
                                   (list (set "b") (set "e" "d") (set "f")))))
                           null
                           null
                           #hash()
                           phrases-three
                           null
                           null)
                          (ortho
                           (array #[#["a" "b"] #["c" "d"]])
                           (array #[#["b"] #["d"]])
                           (list (set "a") (set "b" "c") (set "d"))))
                (state
   (hash
    (array #[#["a"] #["b"] #["e"]])
    (set
     (ortho
      (array #[#["a" "c"] #["b" "d"] #["e" "f"]])
      (array #[#["c"] #["d"] #["f"]])
      (list (set "a") (set "b" "c") (set "d" "e") (set "f"))))
    (array #[#["a" "b"] #["c" "d"]])
    (set
     (ortho
      (mutable-array #[#["a" "b" "e"] #["c" "d" "f"]])
      (array #[#["b" "e"] #["d" "f"]])
      (list (set "a") (set "b" "c") (set "d" "e") (set "f"))))
    (array #[#["b"] #["d"]])
    (set
     (ortho
      (array #[#["b" "e"] #["d" "f"]])
      (array #[#["e"] #["f"]])
      (list (set "b") (set "d" "e") (set "f")))))
   '()
   '()
   (hash
    '(3 2)
    (set
     (ortho
      (array #[#["a" "c"] #["b" "d"] #["e" "f"]])
      (array #[#["c"] #["d"] #["f"]])
      (list (set "a") (set "b" "c") (set "d" "e") (set "f"))))
    '(2 3)
    (set
     (ortho
      (mutable-array #[#["a" "b" "e"] #["c" "d" "f"]])
      (array #[#["b" "e"] #["d" "f"]])
      (list (set "a") (set "b" "c") (set "d" "e") (set "f")))))
   phrases-three
   '()
   (set
    (ortho
     (array #[#["a" "c"] #["b" "d"] #["e" "f"]])
     (array #[#["c"] #["d"] #["f"]])
     (list (set "a") (set "b" "c") (set "d" "e") (set "f")))
    (ortho
     (mutable-array #[#["a" "b" "e"] #["c" "d" "f"]])
     (array #[#["b" "e"] #["d" "f"]])
     (list (set "a") (set "b" "c") (set "d" "e") (set "f")))))))

(define (calculate-center-dims-foreign dims)
  (define almost (map (λ (x) (range x)) (vector->list dims)))
  (list-update almost (sub1 (length almost)) (λ (l) (drop-right l 1))))

(define (calculate-foreign-lhs-center box)
  (define dims (array-shape (ortho-data box)))
  (array-slice-ref (ortho-data box) (calculate-center-dims-foreign dims)))

; assumption - cur is to be joined on the minor axis and that rotation is in centers and up to date with latest known.
(define (combine phrases centers cur)
  (define combine-candidates (hash-ref centers (ortho-center cur) (set)))
  (define selected-candidates (filter (λ (b) (phrase-filter phrases (ortho-data cur) (ortho-data b))) (set->list combine-candidates)))
  (map (λ (b) (combine-winners cur b)) selected-candidates))

(module+ test
  (require rackunit)
  (define phrases (make-tree))
  (tree-add! phrases (vector->label/with-sentinel (list->vector (list "a" "b" "e"))))
  (tree-add! phrases (vector->label/with-sentinel (list->vector (list "c" "d" "f"))))
  (check-equal?
   (combine phrases (hash (array #[#["b"] #["d"]])
                          (set
                           (ortho
                            (array #[#["b" "e"] #["d" "f"]])
                            (array #[#["e"] #["f"]])
                            (list (set "b") (set "e" "d") (set "f")))))
            (ortho
             (array #[#["a" "b"] #["c" "d"]])
             (array #[#["b"] #["d"]])
             (list (set "a") (set "b" "c") (set "d"))))
   (list
    (ortho
     (array #[#["a" "b" "e"] #["c" "d" "f"]])
     (array #[#["b" "e"] #["d" "f"]])
     (list (set "a") (set "b" "c") (set "d" "e") (set "f"))))))

(define (phrase-filter phrases cur b)
  (define lhs-phrases (get-phrases cur))
  (define rhs-words (get-words b))
  (define desired-phrases (map (λ (l r) (append l (list r))) lhs-phrases rhs-words))
  (andmap (λ (p) (tree-contains? phrases (vector->label (list->vector p)))) desired-phrases))

(module+ test
  (require rackunit)
  (define phrases-two (make-tree))
  (tree-add! phrases-two (vector->label/with-sentinel (list->vector (list "a" "b" "e"))))
  (tree-add! phrases-two (vector->label/with-sentinel (list->vector (list "c" "d" "f"))))
  (check-true
   (phrase-filter phrases-two
                  (array #[#["a" "b"] #["c" "d"]])
                  (array #[#["b" "e"] #["d" "f"]])))
  (check-false
   (phrase-filter phrases-two
                  (array #[#["a" "b"] #["c" "d"]])
                  (array #[#["b" "x"] #["d" "f"]]))))

(define (combine-winners cur other)
  (define input-shape (array-shape (ortho-data cur)))
  (define target-shape (list-update (vector->list input-shape) (sub1 (length (vector->list input-shape))) add1))
  (define data (add-to-end (ortho-data cur) (ortho-data other) target-shape))
  (define center (calculate-rhs-local-center data))
  (define diagonal (append (list (car (ortho-diagonals cur))) (map set-union (cdr (ortho-diagonals cur)) (drop-right (ortho-diagonals other) 1)) (list (last (ortho-diagonals other)))))
  (ortho data center diagonal))

(module+ test
  (require rackunit)
  (check-equal?
   (combine-winners (ortho
                     (array #[#["a" "b"] #["c" "d"]])
                     (array #[#["b"] #["d"]])
                     (list (set "a") (set "b" "c") (set "d")))
                    (ortho
                     (array #[#["b" "e"] #["d" "f"]])
                     (array #[#["e"] #["f"]])
                     (list (set "b") (set "e" "d") (set "f"))))
   (ortho
    (array #[#["a" "b" "e"] #["c" "d" "f"]])
    (array #[#["b" "e"] #["d" "f"]])
    (list (set "a") (set "b" "c") (set "d" "e") (set "f")))))

; a b  b e
; c d  d f

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

(define (calculate-rhs-local-center data)
  (define dims (array-shape data))
  (array-slice-ref data (calculate-center-dims dims)))

(module+ test
  (require rackunit)
  (check-equal?
   (calculate-rhs-local-center (array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]]))
   (array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])))      

(define (calculate-center-dims dims)
  (define almost (map (λ (x) (range x)) (vector->list dims)))
  (list-update almost (sub1 (length almost)) cdr))