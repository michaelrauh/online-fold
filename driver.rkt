#lang racket

(require "atom-smasher.rkt" math)
(struct state (lhs-center-to-ortho rhs-center-to-ortho next prev boxes phrases raw increment) #:transparent)
(provide (struct-out state) (struct-out ortho) drive)

; assumption - raw is nonempty. Only 2x2 are desired.
(define (drive s cur)
  (define prev (last (state-raw s)))
  (define new-raw (append (state-raw s) (list cur)))
  (define new-phrases (set-union (state-phrases s) (tails new-raw)))
  (define new-next (hash-update (state-next s) prev (λ (s) (set-add s cur)) (set)))
  (define new-prev (hash-update (state-prev s) cur (λ (s) (set-add s prev)) (set)))
  (define increment (make-boxes cur new-next new-prev))
  (define lhs-center-to-ortho (for/fold ([centers (state-lhs-center-to-ortho s)])
                                        ([box increment])
                                (hash-update centers (ortho-lhs-center box) (λ (s) (set-add s box)) (set))))
  (define rhs-center-to-ortho (for/fold ([centers (state-rhs-center-to-ortho s)])
                                        ([box increment])
                                (hash-update centers (ortho-rhs-center box) (λ (s) (set-add s box)) (set))))
  (define new-boxes (hash-update (state-boxes s) '(2 2) (λ (s) (set-union s increment)) (set)))
  (state lhs-center-to-ortho rhs-center-to-ortho new-next new-prev new-boxes new-phrases new-raw increment))

(define (tails raw)
  (if (= 1 (length raw))
      (set raw)
      (set-union (set raw) (tails (cdr raw)))))

(define (make-phrases raw)
  (for/fold ([phrases (set)])
            ([i (range 1 (add1 (length raw)))])
    (set-union phrases (tails (take raw i)))))

(module+ test
  (require rackunit)
  (define next #hash(("a" . (set "b" "c")) ("b" . (set "c")) ("c" . (set "d" "b")) ("d" . (set "a"))))
  (define prev #hash(("a" . (set "d")) ("b" . (set "a" "c")) ("c" . (set "a" "b")) ("d" . (set "c"))))
  (define tree (make-phrases '("a" "b" "c" "d" "a" "c" "b")))
  (define res (drive (state #hash() #hash() next prev #hash() tree
                            '("a" "b" "c" "d" "a" "c" "b") (set)) "d"))

  (check-equal? (state-lhs-center-to-ortho res)
                (hash
                 (array #[#["a"] #["b"]])
                 (set
                  (ortho
                   (array #[#["a" "c"] #["b" "d"]])
                   (array #[#["a"] #["b"]])
                   (array #[#["c"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d"))))
                 (array #[#["a"] #["c"]])
                 (set
                  (ortho
                   (array #[#["a" "b"] #["c" "d"]])
                   (array #[#["a"] #["c"]])
                   (array #[#["b"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d"))))))
  (check-equal? (state-rhs-center-to-ortho res)
                (hash
                 (array #[#["c"] #["d"]])
                 (set
                  (ortho
                   (array #[#["a" "c"] #["b" "d"]])
                   (array #[#["a"] #["b"]])
                   (array #[#["c"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d"))))
                 (array #[#["b"] #["d"]])
                 (set
                  (ortho
                   (array #[#["a" "b"] #["c" "d"]])
                   (array #[#["a"] #["c"]])
                   (array #[#["b"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d"))))))
  (check-equal? (state-next res)
                #hash(("a" . (set "b" "c"))
                      ("b" . ("d" set "c"))
                      ("c" . (set "d" "b"))
                      ("d" . (set "a"))))
  (check-equal? (state-prev res)
                #hash(("a" . (set "d"))
                      ("b" . (set "a" "c"))
                      ("c" . (set "a" "b"))
                      ("d" . ("b" set "c"))))
  (check-equal? (state-boxes res)
                (hash
                 '(2 2)
                 (set
                  (ortho
                   (array #[#["a" "b"] #["c" "d"]])
                   (array #[#["a"] #["c"]])
                   (array #[#["b"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d")))
                  (ortho
                   (array #[#["a" "c"] #["b" "d"]])
                   (array #[#["a"] #["b"]])
                   (array #[#["c"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d"))))))
  (check-true (set-member? (state-phrases res) (list "a" "b" "c" "d" "a" "c" "b" "d")))
  (check-equal? (state-raw res) '("a" "b" "c" "d" "a" "c" "b" "d"))
  (check-equal? (state-increment res)
                (set
                 (ortho
                  (array #[#["a" "b"] #["c" "d"]])
                  (array #[#["a"] #["c"]])
                  (array #[#["b"] #["d"]])
                  (list (set "a") (set "b" "c") (set "d")))
                 (ortho
                  (array #[#["a" "c"] #["b" "d"]])
                  (array #[#["a"] #["b"]])
                  (array #[#["c"] #["d"]])
                  (list (set "a") (set "b" "c") (set "d"))))))
