#lang racket
(require "driver.rkt" math)

(define (combine next boxes cur)
  (define combine-candidates (hash-ref boxes (vector->list (array-shape (ortho-data cur)))))
  (define refined-candidates (filter (λ (b) (next-filter next cur b)) (set->list combine-candidates)))
  (define selected-candidates (filter (λ (b) (diagonal-filter cur b)) refined-candidates))
  (map (λ (b) (combine-winners cur b)) selected-candidates))

(module+ test
  (require rackunit)
  
  (check-equal? (combine
                 #hash(("a" . (set "b" "c" "e")) ("b" . (set "d" "f")) ("c" . (set "d" "g")) ("d" . (set "h")) ("e" . (set "f" "g")) ("f" . (set "h")) ("g" . (set "h")))
                 (hash '(2 2)
                       (set
                        (ortho (array #[#["e" "f"] #["g" "h"]]) (array #[#["e"] #["g"]]) (list (set "e") (set "f" "g") (set "h")))
                        (ortho (array #[#["a" "b"] #["c" "d"]]) (array #[#["a"] #["c"]]) (list (set "a") (set "b" "c") (set "d")))))
                 (ortho
                  (array #[#["a" "b"] #["c" "d"]])
                  (array #[#["a"] #["c"]])
                  (list (set "a") (set "b" "c") (set "d"))))
                (list (ortho
                       (array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                       (array #[#[#["a"] #["c"]] #[#["e"] #["g"]]])
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
                (array #[#["a"] #["c"]])
                (list (set "a") (set "b" "c") (set "d")))
               (ortho
                (array #[#["e" "f"] #["g" "h"]])
                (array #[#["e"] #["g"]])
                (list (set "e") (set "f" "g") (set "h")))))
  (check-false (next-filter
                #hash(("a" . (set "b" "c")) ("b" . (set "d" "f")) ("c" . (set "d" "g")) ("d" . (set "h")) ("e" . (set "f" "g")) ("f" . (set "h")) ("g" . (set "h")))
                (ortho
                 (array #[#["a" "b"] #["c" "d"]])
                 (array #[#["a"] #["c"]])
                 (list (set "a") (set "b" "c") (set "d")))
                (ortho
                 (array #[#["e" "f"] #["g" "h"]])
                 (array #[#["e"] #["g"]])
                 (list (set "e") (set "f" "g") (set "h"))))))

(define (diagonal-filter cur candidate)
  (for/and ([l (cdr (ortho-diagonals cur))]
            [r (drop-right (ortho-diagonals candidate) 1)])
    (set-empty? (set-intersect l r))))

(module+ test
  (require rackunit)
  (check-true (diagonal-filter (ortho
                                (array #[#["a" "b"] #["c" "d"]])
                                (array #[#["a"] #["c"]])
                                (list (set "a") (set "b" "c") (set "d")))
                               (ortho
                                (array #[#["e" "f"] #["g" "h"]])
                                (array #[#["e"] #["g"]])
                                (list (set "e") (set "f" "g") (set "h")))))
  (check-false (diagonal-filter (ortho
                                 (array #[#["a" "b"] #["c" "d"]])
                                 (array #[#["a"] #["c"]])
                                 (list (set "a") (set "b" "c") (set "d")))
                                (ortho
                                 (array #[#["e" "f"] #["g" "h"]])
                                 (array #[#["e"] #["g"]])
                                 (list (set "b") (set "f" "g") (set "h"))))))

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
                                  (array #[#["a"] #["c"]])
                                  (list (set "a") (set "b" "c") (set "d")))
                                 (ortho
                                  (array #[#["e" "f"] #["g" "h"]])
                                  (array #[#["e"] #["g"]])
                                  (list (set "e") (set "f" "g") (set "h"))))
                (ortho
                 (array #[#[#["a" "b"] #["c" "d"]] #[#["e" "f"] #["g" "h"]]])
                 (array #[#[#["a"] #["c"]] #[#["e"] #["g"]]])
                 (list (set "a") (set "b" "c" "e") (set "d" "f" "g") (set "h")))))


; a b   e f
; c d   g h

; 0 1  1 2
; 1 2  2 3