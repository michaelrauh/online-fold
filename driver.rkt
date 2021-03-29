#lang racket

(require "atom-smasher.rkt" suffixtree math)
(struct state (centers next prev boxes phrases raw increment) #:transparent)
(provide (struct-out state) (struct-out ortho) drive)

(define (drive s cur)
  (define prev (last (state-raw s)))
  (define new-raw (append (state-raw s) (list cur)))
  (define new-phrases (state-phrases s))
  (tree-add! new-phrases (vector->label/with-sentinel (list->vector new-raw)))
  (define new-next (hash-update (state-next s) prev (λ (s) (set-add s cur)) (set)))
  (define new-prev (hash-update (state-prev s) cur (λ (s) (set-add s prev)) (set)))
  (define increment (make-boxes cur new-next new-prev))
  (define new-centers (for/fold ([centers (state-centers s)])
                                ([box increment])
                        (hash-update centers (calculate-center box) (λ (s) (set-add s box)) (set))))
  (define new-boxes (hash-update (state-boxes s) '(2 2) (λ (s) (set-union s increment)) (set)))
  (state new-centers new-next new-prev new-boxes new-phrases new-raw increment))

; TODO stop mutating phrases
; TODO remove double conversion before adding to phrases
; TODO make raw a deque
; TODO change to typed racket and use the optimization coach

(module+ test
  (require rackunit)
  (define next #hash(("a" . (set "b" "c")) ("b" . (set "c")) ("c" . (set "d" "b")) ("d" . (set "a"))))
  (define prev #hash(("a" . (set "d")) ("b" . (set "a" "c")) ("c" . (set "a" "b")) ("d" . (set "c"))))
  (define tree (make-tree))
  (tree-add! tree (vector->label/with-sentinel (list->vector '("a" "b" "c" "d" "a" "c" "b"))))
  (define res (drive (state #hash() next prev #hash() tree
                            '("a" "b" "c" "d" "a" "c" "b") (set)) "d"))

  (check-equal? (state-centers res) (hash
                                     (array #[#["a"] #["b"]])
                                     (set
                                      (ortho
                                       (array #[#["a" "c"] #["b" "d"]])
                                       (array #[#["c"] #["d"]])
                                       (list (set "a") (set "b" "c") (set "d"))))
                                     (array #[#["a"] #["c"]])
                                     (set
                                      (ortho
                                       (array #[#["a" "b"] #["c" "d"]])
                                       (array #[#["b"] #["d"]])
                                       (list (set "a") (set "b" "c") (set "d"))))))
  (check-equal? (state-next res) #hash(("a" . (set "b" "c"))
                                       ("b" . ("d" set "c"))
                                       ("c" . (set "d" "b"))
                                       ("d" . (set "a"))))
  (check-equal? (state-prev res) #hash(("a" . (set "d"))
                                       ("b" . (set "a" "c"))
                                       ("c" . (set "a" "b"))
                                       ("d" . ("b" set "c"))))
  (check-equal? (state-boxes res) (hash
                                   '(2 2)
                                   (set
                                    (ortho
                                     (array #[#["a" "b"] #["c" "d"]])
                                     (array #[#["b"] #["d"]])
                                     (list (set "a") (set "b" "c") (set "d")))
                                    (ortho
                                     (array #[#["a" "c"] #["b" "d"]])
                                     (array #[#["c"] #["d"]])
                                     (list (set "a") (set "b" "c") (set "d"))))))
  (check-true (tree-contains? (state-phrases res) (vector->label (list->vector '("a" "b" "c" "d" "a" "c" "b" "d")))))
  (check-equal? (state-raw res) '("a" "b" "c" "d" "a" "c" "b" "d"))
  (check-equal? (state-increment res) (set
                                       (ortho
                                        (array #[#["a" "b"] #["c" "d"]])
                                        (array #[#["b"] #["d"]])
                                        (list (set "a") (set "b" "c") (set "d")))
                                       (ortho
                                        (array #[#["a" "c"] #["b" "d"]])
                                        (array #[#["c"] #["d"]])
                                        (list (set "a") (set "b" "c") (set "d"))))))
