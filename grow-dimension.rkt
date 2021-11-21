#lang racket
(require "driver.rkt" math)

(define (drive-in s cur)
  (define dims (vector->list (array-shape (ortho-data cur))))

  (define known-boxes (hash-ref (state-boxes s) dims (set)))
  (define made-boxes (make-increment s cur))
  (define increment (list->set (filter-not (λ (x) (set-member? known-boxes x)) (set->list made-boxes))))
  
  (define lhs-center-to-ortho (for/fold ([centers (state-lhs-center-to-ortho s)])
                                        ([box increment])
                                (hash-update centers (ortho-lhs-center box) (λ (s) (set-add s box)) (set))))
  (define rhs-center-to-ortho (for/fold ([centers (state-rhs-center-to-ortho s)])
                                        ([box increment])
                                (hash-update centers (ortho-rhs-center box) (λ (s) (set-add s box)) (set))))
  (define boxes (make-boxes increment s))
  (state lhs-center-to-ortho rhs-center-to-ortho (state-next s) (state-prev s) boxes (state-phrases s) (list->set increment)))
(provide drive-in)

(define (make-increment s cur)
  (apply append (map rotations (combine (state-phrases s) (state-lhs-center-to-ortho s) (state-rhs-center-to-ortho s) cur))))

(define (make-boxes increment s)
  (for/fold ([boxes (state-boxes s)])
            ([box increment])
    (hash-update boxes (vector->list (array-shape (ortho-data box))) (λ (s) (set-union s (set box))) (set))))

(define (rotations o)
  (define arr-to-ortho ((curry r2o) o))
  (define arrs (rots (ortho-data o)))
  (cons o (map arr-to-ortho arrs)))

(define (r2o o arr)
  (ortho arr (calculate-lhs-center arr) (calculate-rhs-center arr) (ortho-diagonals o)))
  
(define (rots arr)
  (define dims (array-dims arr))
  (map (λ (ax)
         (array-axis-swap arr (sub1 dims) ax))
       (range (sub1 dims))))

(define (make-phrases raw)
  (for/fold ([phrases (set)])
            ([i (range 1 (add1 (length raw)))])
    (set-union phrases (tails (take raw i)))))

(define (tails raw)
  (if (= 1 (length raw))
      (set raw)
      (set-union (set raw) (tails (cdr raw)))))

(module+ test
  (require rackunit)
  (define phrases-three (set-union (make-phrases (list "a" "b" "e")) (make-phrases (list "c" "d" "f"))))
  (check-equal? (drive-in (state
                           (hash (array #[#["b"] #["d"]])
                                 (set
                                  (ortho
                                   (array #[#["b" "e"] #["d" "f"]])
                                   (array #[#["b"] #["d"]])
                                   (array #[#["e"] #["f"]])
                                   (list (set "b") (set "e" "d") (set "f")))))
                           (hash (array #[#["e"] #["f"]])
                                 (set
                                  (ortho
                                   (array #[#["b" "e"] #["d" "f"]])
                                   (array #[#["b"] #["d"]])
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
                           (array #[#["a"] #["c"]])
                           (array #[#["b"] #["d"]])
                           (list (set "a") (set "b" "c") (set "d"))))
                (state
                 (hash
                  (array #[#["a"] #["b"] #["e"]])
                  (set
                   (ortho
                    (array #[#["a" "c"] #["b" "d"] #["e" "f"]])
                    (array #[#["a"] #["b"] #["e"]])
                    (array #[#["c"] #["d"] #["f"]])
                    (list (set "a") (set "b" "c") (set "d" "e") (set "f"))))
                  (array #[#["a" "b"] #["c" "d"]])
                  (set
                   (ortho
                    (mutable-array #[#["a" "b" "e"] #["c" "d" "f"]])
                    (array #[#["a" "b"] #["c" "d"]])
                    (array #[#["b" "e"] #["d" "f"]])
                    (list (set "a") (set "b" "c") (set "d" "e") (set "f"))))
                  (array #[#["b"] #["d"]])
                  (set
                   (ortho
                    (array #[#["b" "e"] #["d" "f"]])
                    (array #[#["b"] #["d"]])
                    (array #[#["e"] #["f"]])
                    (list (set "b") (set "d" "e") (set "f")))))
                 (hash
                  (array #[#["c"] #["d"] #["f"]])
                  (set
                   (ortho
                    (array #[#["a" "c"] #["b" "d"] #["e" "f"]])
                    (array #[#["a"] #["b"] #["e"]])
                    (array #[#["c"] #["d"] #["f"]])
                    (list (set "a") (set "b" "c") (set "d" "e") (set "f"))))
                  (array #[#["b" "e"] #["d" "f"]])
                  (set
                   (ortho
                    (mutable-array #[#["a" "b" "e"] #["c" "d" "f"]])
                    (array #[#["a" "b"] #["c" "d"]])
                    (array #[#["b" "e"] #["d" "f"]])
                    (list (set "a") (set "b" "c") (set "d" "e") (set "f"))))
                  (array #[#["e"] #["f"]])
                  (set
                   (ortho
                    (array #[#["b" "e"] #["d" "f"]])
                    (array #[#["b"] #["d"]])
                    (array #[#["e"] #["f"]])
                    (list (set "b") (set "d" "e") (set "f")))))
                 '()
                 '()
                 (hash
                  '(3 2)
                  (set
                   (ortho
                    (array #[#["a" "c"] #["b" "d"] #["e" "f"]])
                    (array #[#["a"] #["b"] #["e"]])
                    (array #[#["c"] #["d"] #["f"]])
                    (list (set "a") (set "b" "c") (set "d" "e") (set "f"))))
                  '(2 3)
                  (set
                   (ortho
                    (mutable-array #[#["a" "b" "e"] #["c" "d" "f"]])
                    (array #[#["a" "b"] #["c" "d"]])
                    (array #[#["b" "e"] #["d" "f"]])
                    (list (set "a") (set "b" "c") (set "d" "e") (set "f")))))
                 phrases-three
                 '()
                 (set
                  (ortho
                   (array #[#["a" "c"] #["b" "d"] #["e" "f"]])
                   (array #[#["a"] #["b"] #["e"]])
                   (array #[#["c"] #["d"] #["f"]])
                   (list (set "a") (set "b" "c") (set "d" "e") (set "f")))
                  (ortho
                   (mutable-array #[#["a" "b" "e"] #["c" "d" "f"]])
                   (array #[#["a" "b"] #["c" "d"]])
                   (array #[#["b" "e"] #["d" "f"]])
                   (list (set "a") (set "b" "c") (set "d" "e") (set "f")))))))

(define (calculate-center-dims-foreign dims)
  (define almost (map (λ (x) (range x)) (vector->list dims)))
  (list-update almost (sub1 (length almost)) (λ (l) (drop-right l 1))))

(define (calculate-lhs-center arr)
  (define dims (array-shape arr))
  (array-slice-ref arr (calculate-center-dims-foreign dims)))

; assumption - cur is to be joined on the minor axis and that rotation is in centers and up to date with latest known.
(define (combine phrases lhs-center-to-ortho rhs-center-to-ortho cur)
  (define left-combine-candidates (hash-ref lhs-center-to-ortho (ortho-rhs-center cur) (set)))
  (define left-selected-candidates (filter (λ (b) (phrase-filter phrases (ortho-data cur) (ortho-data b))) (set->list left-combine-candidates)))
  (define left-winners (map (λ (b) (combine-winners cur b)) left-selected-candidates))
  (define right-combine-candidates (hash-ref rhs-center-to-ortho (ortho-lhs-center cur) (set)))
  (define right-selected-candidates (filter (λ (b) (phrase-filter phrases (ortho-data b) (ortho-data cur))) (set->list right-combine-candidates)))
  (define right-winners (map (λ (b) (combine-winners b cur)) right-selected-candidates))
  (append left-winners right-winners))

(module+ test
  (require rackunit)
  (check-equal?
   (combine
    (set-union (make-phrases (list "a" "b" "e")) (make-phrases (list "c" "d" "f")))
    (hash (array #[#["b"] #["d"]])
          (set
           (ortho
            (array #[#["b" "e"] #["d" "f"]])
            (array #[#["b"] #["d"]])
            (array #[#["e"] #["f"]])
            (list (set "b") (set "e" "d") (set "f")))))
    (hash (array #[#["e"] #["f"]])
          (set
           (ortho
            (array #[#["b" "e"] #["d" "f"]])
            (array #[#["b"] #["d"]])
            (array #[#["e"] #["f"]])
            (list (set "b") (set "e" "d") (set "f")))))
    (ortho
     (array #[#["a" "b"] #["c" "d"]])
     (array #[#["a"] #["c"]])
     (array #[#["b"] #["d"]])
     (list (set "a") (set "b" "c") (set "d"))))
   (list
    (ortho
     (array #[#["a" "b" "e"] #["c" "d" "f"]])
     (array #[#["a" "b"] #["c" "d"]])
     (array #[#["b" "e"] #["d" "f"]])
     (list (set "a") (set "b" "c") (set "d" "e") (set "f"))))))

(define (phrase-filter phrases cur b)
  (define lhs-phrases (get-phrases cur))
  (define rhs-words (get-words b))
  (define desired-phrases (map (λ (l r) (append l (list r))) lhs-phrases rhs-words))
  (andmap (λ (p) (set-member? phrases p)) desired-phrases))

(module+ test
  (require rackunit)
  (define phrases-two (set-union (make-phrases (list "a" "b" "e")) (make-phrases (list "c" "d" "f"))))
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
  (define rhs-center (calculate-rhs-center data))
  (define lhs-center (calculate-lhs-center data))
  (define diagonal (append (list (car (ortho-diagonals cur))) (map set-union (cdr (ortho-diagonals cur)) (drop-right (ortho-diagonals other) 1)) (list (last (ortho-diagonals other)))))
  (ortho data lhs-center rhs-center diagonal))

(module+ test
  (require rackunit)
  (check-equal?
   (combine-winners (ortho
                     (array #[#["a" "b"] #["c" "d"]])
                     (array #[#["a"] #["c"]])
                     (array #[#["b"] #["d"]])
                     (list (set "a") (set "b" "c") (set "d")))
                    (ortho
                     (array #[#["b" "e"] #["d" "f"]])
                     (array #[#["b"] #["d"]])
                     (array #[#["e"] #["f"]])
                     (list (set "b") (set "e" "d") (set "f"))))
   (ortho
    (array #[#["a" "b" "e"] #["c" "d" "f"]])
    (array #[#["a" "b"] #["c" "d"]])
    (array #[#["b" "e"] #["d" "f"]])
    (list (set "a") (set "b" "c") (set "d" "e") (set "f")))))

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

(define (calculate-rhs-center data)
  (define dims (array-shape data))
  (array-slice-ref data (calculate-rhs-center-dims dims)))

(module+ test
  (require rackunit)
  (check-equal?
   (calculate-rhs-center (array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]]))
   (array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])))      

(define (calculate-rhs-center-dims dims)
  (define almost (map (λ (x) (range x)) (vector->list dims)))
  (list-update almost (sub1 (length almost)) cdr))