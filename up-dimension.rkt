#lang racket
(require "driver.rkt" math)

; assumption - input is at lowest volume for dimensionality (all 2s)
(define (drive-up s cur)
  (define dims (vector->list (array-shape (ortho-data cur))))
  (define new-dims (cons 2 dims))
  (define increment (list->set (apply append (map rotations (combine (state-next s) (state-boxes s) cur dims)))))
  (define boxes (hash-update (state-boxes s) new-dims (λ (s) (set-union s increment)) (set)))
  (define centers (for/fold ([centers (state-centers s)])
                            ([box increment])
                    (hash-update centers (calculate-foreign-lhs-center box) (λ (s) (set-add s box)) (set))))
  
  (state centers (state-next s) (state-prev s) boxes (state-phrases s) (state-raw s) (list->set increment)))
(provide drive-up)

(define (calculate-foreign-lhs-center box)
  (define dims (vector->list (array-shape (ortho-data box))))
  (array-slice-ref (ortho-data box) (calculate-center-dims dims)))

(define (calculate-center-dims dims)
  (define almost (map (λ (x) (range x)) dims))
  (list-update almost (sub1 (length almost)) (λ (l) (drop-right l 1))))

(define (rotations o)
  (define arr-to-ortho ((curry r2o) o))
  (define arrs (rots (ortho-data o)))
  (cons o (map arr-to-ortho arrs)))

(define (calculate-rhs-local-center data)
  (define dims (array-shape data))
  (array-slice-ref data (calculate-center-local-dims dims)))

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
  (check-equal? (drive-up (state
                           #hash()
                           #hash(("a" . (set "b" "c" "e")) ("b" . (set "d" "f")) ("c" . (set "d" "g")) ("d" . (set "h")) ("e" . (set "f" "g")) ("f" . (set "h")) ("g" . (set "h")))
                           #hash()
                           (hash '(2 2)
                                 (set
                                  (ortho
                                   (array #[#["e" "f"] #["g" "h"]])
                                   (array #[#["f"] #["h"]])
                                   (list (set "e") (set "f" "g") (set "h")))
                                  (ortho
                                   (array #[#["a" "b"] #["c" "d"]])
                                   (array #[#["b"] #["d"]])
                                   (list (set "a") (set "b" "c") (set "d")))))
                           null
                           null
                           null) (ortho
                                  (array #[#["a" "b"] #["c" "d"]])
                                  (array #[#["b"] #["d"]])
                                  (list (set "a") (set "b" "c") (set "d"))))
                (state
                 (hash
                  (array #[#[#["a"] #["c"]] #[#["e"] #["g"]]])
                  (set
                   (ortho
                    (mutable-array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                    (mutable-array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h"))))
                  (array #[#[#["a"] #["c"]] #[#["b"] #["d"]]])
                  (set
                   (ortho
                    (array #[#[#["a" "e"] #["c" "g"]] #[#["b" "f"] #["d" "h"]]])
                    (array #[#[#["e"] #["g"]] #[#["f"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h"))))
                  (array #[#[#["a"] #["b"]] #[#["e"] #["f"]]])
                  (set
                   (ortho
                    (array #[#[#["a" "c"] #["b" "d"]] #[#["e" "g"] #["f" "h"]]])
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
                    (array #[#["f"] #["h"]])
                    (list (set "e") (set "f" "g") (set "h")))
                   (ortho
                    (array #[#["a" "b"] #["c" "d"]])
                    (array #[#["b"] #["d"]])
                    (list (set "a") (set "b" "c") (set "d"))))
                  '(2 2 2)
                  (set
                   (ortho
                    (mutable-array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                    (mutable-array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))
                   (ortho
                    (array #[#[#["a" "e"] #["c" "g"]] #[#["b" "f"] #["d" "h"]]])
                    (array #[#[#["e"] #["g"]] #[#["f"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))
                   (ortho
                    (array #[#[#["a" "c"] #["b" "d"]] #[#["e" "g"] #["f" "h"]]])
                    (array #[#[#["c"] #["d"]] #[#["g"] #["h"]]])
                    (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))))
                 '()
                 '()
                 (set
                  (ortho
                   (mutable-array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                   (mutable-array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                   (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))
                  (ortho
                   (array #[#[#["a" "e"] #["c" "g"]] #[#["b" "f"] #["d" "h"]]])
                   (array #[#[#["e"] #["g"]] #[#["f"] #["h"]]])
                   (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))
                  (ortho
                   (array #[#[#["a" "c"] #["b" "d"]] #[#["e" "g"] #["f" "h"]]])
                   (array #[#[#["c"] #["d"]] #[#["g"] #["h"]]])
                   (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))))))
                       
                       
; TODO stop passing dims around when it can be calculated
(define (combine next boxes cur dims)
  (define combine-candidates (hash-ref boxes dims))
  (define refined-candidates (filter (λ (b) (next-filter next cur b)) (set->list combine-candidates)))
  (define selected-candidates (filter (λ (b) (diagonal-filter cur b)) refined-candidates))
  (map (λ (b) (combine-winners cur b)) selected-candidates))

(module+ test
  (require rackunit)
  
  (check-equal? (combine
                 #hash(("a" . (set "b" "c" "e")) ("b" . (set "d" "f")) ("c" . (set "d" "g")) ("d" . (set "h")) ("e" . (set "f" "g")) ("f" . (set "h")) ("g" . (set "h")))
                 (hash '(2 2)
                       (set
                        (ortho
                         (array #[#["e" "f"] #["g" "h"]])
                         (array #[#["f"] #["h"]])
                         (list (set "e") (set "f" "g") (set "h")))
                        (ortho
                         (array #[#["a" "b"] #["c" "d"]])
                         (array #[#["a"] #["c"]])
                         (list (set "a") (set "b" "c") (set "d")))))
                 (ortho
                  (array #[#["a" "b"] #["c" "d"]])
                  (array #[#["b"] #["d"]])
                  (list (set "a") (set "b" "c") (set "d")))
                 '(2 2))
                (list (ortho
                       (array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                       (array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                       (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h"))))))

(define (next-filter next cur candidate)
  (for/and ([from-word (array->list (array-flatten (ortho-data cur)))]
            [target-word (array->list (array-flatten (ortho-data candidate)))])
    (set-member? (hash-ref next from-word) target-word)))

(module+ test
  (require rackunit)
  (check-true (next-filter
               #hash(("a" . (set "b" "c" "e")) ("b" . (set "d" "f")) ("c" . (set "d" "g")) ("d" . (set "h")) ("e" . (set "f" "g")) ("f" . (set "h")) ("g" . (set "h")))
               (ortho
                (array #[#["a" "b"] #["c" "d"]])
                (array #[#["b"] #["d"]])
                (list (set "a") (set "b" "c") (set "d")))
               (ortho
                (array #[#["e" "f"] #["g" "h"]])
                (array #[#["f"] #["h"]])
                (list (set "e") (set "f" "g") (set "h")))))
  (check-false (next-filter
                #hash(("a" . (set "b" "c")) ("b" . (set "d" "f")) ("c" . (set "d" "g")) ("d" . (set "h")) ("e" . (set "f" "g")) ("f" . (set "h")) ("g" . (set "h")))
                (ortho
                 (array #[#["a" "b"] #["c" "d"]])
                 (array #[#["b"] #["d"]])
                 (list (set "a") (set "b" "c") (set "d")))
                (ortho
                 (array #[#["e" "f"] #["g" "h"]])
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
                                (array #[#["b"] #["d"]])
                                (list (set "a") (set "b" "c") (set "d")))
                               (ortho
                                (array #[#["e" "f"] #["g" "h"]])
                                (array #[#["f"] #["h"]])
                                (list (set "e") (set "f" "g") (set "h")))))
  (check-false (diagonal-filter (ortho
                                 (array #[#["a" "b"] #["c" "d"]])
                                 (array #[#["b"] #["d"]])
                                 (list (set "a") (set "b" "c") (set "d")))
                                (ortho
                                 (array #[#["e" "f"] #["g" "h"]])
                                 (array #[#["f"] #["h"]])
                                 (list (set "b") (set "f" "g") (set "h"))))))

; assumption - those to be combined are eligible
(define (combine-winners cur other)
  (define data (list*->array (list (array->list* (ortho-data cur)) (array->list* (ortho-data other))) string?))
  (define center (list*->array (list (array->list* (ortho-center cur)) (array->list* (ortho-center other))) string?))
  
  (define diagonal-center (for/list ([left-set (cdr (ortho-diagonals cur))]
                                     [right-set (drop-right (ortho-diagonals other) 1)])
                            (set-union left-set right-set)))
  (define diagonal (append (list (car (ortho-diagonals cur))) diagonal-center (list (last (ortho-diagonals other)))))
  (ortho data center diagonal))


(module+ test
  (require rackunit)
  (check-equal? (combine-winners (ortho
                                  (array #[#["a" "b"] #["c" "d"]])
                                  (array #[#["b"] #["d"]])
                                  (list (set "a") (set "b" "c") (set "d")))
                                 (ortho
                                  (array #[#["e" "f"] #["g" "h"]])
                                  (array #[#["f"] #["h"]])
                                  (list (set "e") (set "f" "g") (set "h"))))
                (ortho
                 (array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                 (array #[#[#["b"] #["d"]] #[#["f"] #["h"]]])
                 (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))))


; a b   e f
; c d   g h

; 0 1  1 2
; 1 2  2 3