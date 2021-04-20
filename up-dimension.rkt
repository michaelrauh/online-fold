#lang racket
(require "driver.rkt" math)

; assumption - input is at lowest volume for dimensionality (all 2s)
(define (drive-up s cur)
  (define dims (vector->list (array-shape (ortho-data cur))))
  (define new-dims (cons 2 dims))
  (define increment (list->set (apply append (map rotations (combine (state-next s) (state-prev s) (state-boxes s) cur dims)))))
  (define boxes (hash-update (state-boxes s) new-dims (λ (s) (set-union s increment)) (set)))
  (define lhs-center-to-ortho (for/fold ([centers (state-lhs-center-to-ortho s)])
                            ([box increment])
                    (hash-update centers (ortho-lhs-center box) (λ (s) (set-add s box)) (set))))
  (define rhs-center-to-ortho (for/fold ([centers (state-rhs-center-to-ortho s)])
                            ([box increment])
                    (hash-update centers (ortho-rhs-center box) (λ (s) (set-add s box)) (set))))
  
  (state lhs-center-to-ortho rhs-center-to-ortho (state-next s) (state-prev s) boxes (state-phrases s) (state-raw s) (list->set increment)))
(provide drive-up)

(define (calculate-lhs-center arr)
  (define dims (vector->list (array-shape arr)))
  (array-slice-ref arr (calculate-center-dims dims)))

(define (calculate-center-dims dims)
  (define almost (map (λ (x) (range x)) dims))
  (list-update almost (sub1 (length almost)) (λ (l) (drop-right l 1))))

(define (rotations o)
  (define arr-to-ortho ((curry r2o) o))
  (define arrs (rots (ortho-data o)))
  (cons o (map arr-to-ortho arrs)))

(define (calculate-rhs-center data)
  (define dims (array-shape data))
  (array-slice-ref data (calculate-rhs-center-dims dims)))

(define (calculate-rhs-center-dims dims)
  (define almost (map (λ (x) (range x)) (vector->list dims)))
  (list-update almost (sub1 (length almost)) cdr))

(define (r2o o arr)
  (ortho arr (calculate-lhs-center arr) (calculate-rhs-center arr) (ortho-diagonals o)))
  
(define (rots arr)
  (define dims (array-dims arr))
  (map (λ (ax)
         (array-axis-swap arr (sub1 dims) ax))
       (range (sub1 dims))))

(module+ test
  (require rackunit)
  (check-equal? (drive-up (state
                           #hash()
                           #hash()
                           #hash(("a" . (set "b" "c" "e")) ("b" . (set "d" "f")) ("c" . (set "d" "g")) ("d" . (set "h")) ("e" . (set "f" "g")) ("f" . (set "h")) ("g" . (set "h")))
                           #hash()
                           (hash '(2 2)
                                 (set
                                  (ortho
                                   (array #[#["e" "f"] #["g" "h"]])
                                   (array #[#["e"] #["g"]])
                                   (array #[#["f"] #["h"]])
                                   (list (set "e") (set "f" "g") (set "h")))
                                  (ortho
                                   (array #[#["a" "b"] #["c" "d"]])
                                   (array #[#["a"] #["c"]])
                                   (array #[#["b"] #["d"]])
                                   (list (set "a") (set "b" "c") (set "d")))))
                           null
                           null
                           null) (ortho
                                  (array #[#["a" "b"] #["c" "d"]])
                                  (array #[#["a"] #["c"]])
                                  (array #[#["b"] #["d"]])
                                  (list (set "a") (set "b" "c") (set "d"))))
                (state
                 (hash
                  (array #[#[#["a"] #["c"]] #[#["e"] #["g"]]])
                  (set
                   (ortho
                    (mutable-array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                    (mutable-array #[#[#["a"] #["c"]] #[#["e"] #["g"]]])
                    (mutable-array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h"))))
                  (array #[#[#["a"] #["c"]] #[#["b"] #["d"]]])
                  (set
                   (ortho
                    (array #[#[#["a" "e"] #["c" "g"]] #[#["b" "f"] #["d" "h"]]])
                    (mutable-array #[#[#["a"] #["c"]] #[#["b"] #["d"]]])
                    (array #[#[#["e"] #["g"]] #[#["f"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h"))))
                  (array #[#[#["a"] #["b"]] #[#["e"] #["f"]]])
                  (set
                   (ortho
                    (array #[#[#["a" "c"] #["b" "d"]] #[#["e" "g"] #["f" "h"]]])
                    (array #[#[#["a"] #["b"]] #[#["e"] #["f"]]])
                    (array #[#[#["c"] #["d"]] #[#["g"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))))
                 (hash
                  (array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                  (set
                   (ortho
                    (mutable-array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                    (mutable-array #[#[#["a"] #["c"]] #[#["e"] #["g"]]])
                    (mutable-array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h"))))
                  (array #[#[#["e"] #["g"]] #[#["f"] #["h"]]])
                  (set
                   (ortho
                    (array #[#[#["a" "e"] #["c" "g"]] #[#["b" "f"] #["d" "h"]]])
                    (mutable-array #[#[#["a"] #["c"]] #[#["b"] #["d"]]])
                    (array #[#[#["e"] #["g"]] #[#["f"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h"))))
                  (array #[#[#["c"] #["d"]] #[#["g"] #["h"]]])
                  (set
                   (ortho
                    (array #[#[#["a" "c"] #["b" "d"]] #[#["e" "g"] #["f" "h"]]])
                    (array #[#[#["a"] #["b"]] #[#["e"] #["f"]]])
                    (array #[#[#["c"] #["d"]] #[#["g"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))))
                 '#hash(("a" . (set "b" "c" "e"))
                        ("b" . (set "d" "f"))
                        ("c" . (set "d" "g"))
                        ("d" . (set "h"))
                        ("e" . (set "f" "g"))
                        ("f" . (set "h"))
                        ("g" . (set "h")))
                 '#hash()
                 (hash
                  '(2 2)
                  (set
                   (ortho
                    (array #[#["e" "f"] #["g" "h"]])
                    (array #[#["e"] #["g"]])
                    (array #[#["f"] #["h"]])
                    (list (set "e") (set "f" "g") (set "h")))
                   (ortho
                    (array #[#["a" "b"] #["c" "d"]])
                    (array #[#["a"] #["c"]])
                    (array #[#["b"] #["d"]])
                    (list (set "a") (set "b" "c") (set "d"))))
                  '(2 2 2)
                  (set
                   (ortho
                    (mutable-array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                    (mutable-array #[#[#["a"] #["c"]] #[#["e"] #["g"]]])
                    (mutable-array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))
                   (ortho
                    (array #[#[#["a" "e"] #["c" "g"]] #[#["b" "f"] #["d" "h"]]])
                    (array #[#[#["a"] #["c"]] #[#["b"] #["d"]]])
                    (array #[#[#["e"] #["g"]] #[#["f"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))
                   (ortho
                    (array #[#[#["a" "c"] #["b" "d"]] #[#["e" "g"] #["f" "h"]]])
                    (array #[#[#["a"] #["b"]] #[#["e"] #["f"]]])
                    (array #[#[#["c"] #["d"]] #[#["g"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))))
                 '()
                 '()
                 (set
                  (ortho
                   (mutable-array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                   (mutable-array #[#[#["a"] #["c"]] #[#["e"] #["g"]]])
                   (mutable-array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                   (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))
                  (ortho
                   (array #[#[#["a" "e"] #["c" "g"]] #[#["b" "f"] #["d" "h"]]])
                   (array #[#[#["a"] #["c"]] #[#["b"] #["d"]]])
                   (array #[#[#["e"] #["g"]] #[#["f"] #["h"]]])
                   (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))
                  (ortho
                   (array #[#[#["a" "c"] #["b" "d"]] #[#["e" "g"] #["f" "h"]]])
                   (array #[#[#["a"] #["b"]] #[#["e"] #["f"]]])
                   (array #[#[#["c"] #["d"]] #[#["g"] #["h"]]])
                   (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))))))
                       
                       
; TODO stop passing dims around when it can be calculated
(define (combine next prev boxes cur dims)
  (define combine-candidates (hash-ref boxes dims))
  (define next-candidates (filter (λ (b) (next-filter next cur b)) (set->list combine-candidates)))
  (define prev-candidates (filter (λ (b) (next-filter prev cur b)) (set->list combine-candidates)))
  (define selected-next-candidates (filter (λ (b) (diagonal-filter cur b)) next-candidates))
  (define selected-prev-candidates (filter (λ (b) (diagonal-filter cur b)) prev-candidates))
  (define next-winners (map (λ (b) (combine-winners cur b)) selected-next-candidates))
  (define prev-winners (map (λ (b) (combine-winners b cur)) selected-prev-candidates))
  (append next-winners prev-winners))

(module+ test
  (require rackunit)
  
  (check-equal? (combine
                 #hash(("a" . (set "b" "c" "e")) ("b" . (set "d" "f")) ("c" . (set "d" "g")) ("d" . (set "h")) ("e" . (set "f" "g")) ("f" . (set "h")) ("g" . (set "h")))
                 #hash(("b" . (set "a")) ("c" . (set "a")) ("d" . (set "b" "c")) ("e" . (set "a")) ("f" . (set "e")) ("g" . (set "e")) ("h" . (set "f")))
                 (hash '(2 2)
                       (set
                        (ortho
                         (array #[#["e" "f"] #["g" "h"]])
                         (array #[#["e"] #["g"]])
                         (array #[#["f"] #["h"]])
                         (list (set "e") (set "f" "g") (set "h")))
                        (ortho
                         (array #[#["a" "b"] #["c" "d"]])
                         (array #[#["a"] #["c"]])
                         (array #[#["b"] #["d"]])
                         (list (set "a") (set "b" "c") (set "d")))))
                 (ortho
                  (array #[#["a" "b"] #["c" "d"]])
                  (array #[#["a"] #["c"]])
                  (array #[#["b"] #["d"]])
                  (list (set "a") (set "b" "c") (set "d")))
                 '(2 2))
                (list (ortho
                       (array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                       (array #[#[#["a"] #["c"]] #[#["e"] #["g"]]])
                       (array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                       (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h"))))))

(define (next-filter next cur candidate)
  (for/and ([from-word (array->list (array-flatten (ortho-data cur)))]
            [target-word (array->list (array-flatten (ortho-data candidate)))])
    (set-member? (hash-ref next from-word (set)) target-word)))

(module+ test
  (require rackunit)
  (check-true (next-filter
               #hash(("a" . (set "b" "c" "e")) ("b" . (set "d" "f")) ("c" . (set "d" "g")) ("d" . (set "h")) ("e" . (set "f" "g")) ("f" . (set "h")) ("g" . (set "h")))
               (ortho
                (array #[#["a" "b"] #["c" "d"]])
                (array #[#["a"] #["c"]])
                (array #[#["b"] #["d"]])
                (list (set "a") (set "b" "c") (set "d")))
               (ortho
                (array #[#["e" "f"] #["g" "h"]])
                (array #[#["e"] #["g"]])
                (array #[#["f"] #["h"]])
                (list (set "e") (set "f" "g") (set "h")))))
  (check-false (next-filter
                #hash(("a" . (set "b" "c")) ("b" . (set "d" "f")) ("c" . (set "d" "g")) ("d" . (set "h")) ("e" . (set "f" "g")) ("f" . (set "h")) ("g" . (set "h")))
                (ortho
                 (array #[#["a" "b"] #["c" "d"]])
                 (array #[#["a"] #["c"]])
                 (array #[#["b"] #["d"]])
                 (list (set "a") (set "b" "c") (set "d")))
                (ortho
                 (array #[#["e" "f"] #["g" "h"]])
                 (array #[#["e"] #["g"]])
                 (array #[#["f"] #["h"]])
                 (list (set "e") (set "f" "g") (set "h"))))))

; assumption - diagonals are a list of set indexed by distance from top left corner
(define (diagonal-filter cur candidate)
  (for/and ([l (cdr (ortho-diagonals cur))]
            [r (drop-right (ortho-diagonals candidate) 1)])
    (set-empty? (set-intersect l r))))

(module+ test
  (require rackunit)
  (check-true (diagonal-filter (ortho
                                (array #[#["a" "b"] #["c" "d"]])
                                (array #[#["a"] #["c"]])
                                (array #[#["b"] #["d"]])
                                (list (set "a") (set "b" "c") (set "d")))
                               (ortho
                                (array #[#["e" "f"] #["g" "h"]])
                                (array #[#["e"] #["g"]])
                                (array #[#["f"] #["h"]])
                                (list (set "e") (set "f" "g") (set "h")))))
  (check-false (diagonal-filter (ortho
                                 (array #[#["a" "b"] #["c" "d"]])
                                 (array #[#["a"] #["c"]])
                                 (array #[#["b"] #["d"]])
                                 (list (set "a") (set "b" "c") (set "d")))
                                (ortho
                                 (array #[#["e" "f"] #["g" "h"]])
                                 (array #[#["e"] #["g"]])
                                 (array #[#["f"] #["h"]])
                                 (list (set "b") (set "f" "g") (set "h"))))))

; assumption - those to be combined are eligible
(define (combine-winners cur other)
  (define data (list*->array (list (array->list* (ortho-data cur)) (array->list* (ortho-data other))) string?))
  (define rhs-center (list*->array (list (array->list* (ortho-rhs-center cur)) (array->list* (ortho-rhs-center other))) string?))
  (define lhs-center (list*->array (list (array->list* (ortho-lhs-center cur)) (array->list* (ortho-lhs-center other))) string?))
  
  (define diagonal-center (for/list ([left-set (cdr (ortho-diagonals cur))]
                                     [right-set (drop-right (ortho-diagonals other) 1)])
                            (set-union left-set right-set)))
  (define diagonal (append (list (car (ortho-diagonals cur))) diagonal-center (list (last (ortho-diagonals other)))))
  (ortho data lhs-center rhs-center diagonal))


(module+ test
  (require rackunit)
  (check-equal? (combine-winners (ortho
                                  (array #[#["a" "b"] #["c" "d"]])
                                  (array #[#["a"] #["c"]])
                                  (array #[#["b"] #["d"]])
                                  (list (set "a") (set "b" "c") (set "d")))
                                 (ortho
                                  (array #[#["e" "f"] #["g" "h"]])
                                  (array #[#["e"] #["g"]])
                                  (array #[#["f"] #["h"]])
                                  (list (set "e") (set "f" "g") (set "h"))))
                (ortho
                 (array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                 (array #[#[#["a"] #["c"]] #[#["e"] #["g"]]])
                 (array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                 (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))))


; a b   e f
; c d   g h

; 0 1  1 2
; 1 2  2 3