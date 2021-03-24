#lang racket

(require "atom-smasher.rkt")
(struct state (centers next prev boxes) #:transparent)

(define (drive s prev cur)
  (define new-next (hash-update (state-next s) prev (λ (s) (set-add s cur)) (set)))
  (define new-prev (hash-update (state-prev s) cur (λ (s) (set-add s prev)) (set)))
  (define boxes (set-union (state-boxes s) (smash-and-grab cur new-next new-prev)))
  (define new-centers (for/fold ([centers (state-centers s)])
                                ([box boxes])
                        (hash-update centers (calculate-center box) (λ (s) (set-add s box)) (set))))
  (define new-boxes (set-union (state-boxes s) boxes))
  (state new-centers new-next new-prev new-boxes))

(module+ test
  (require rackunit)
  (check-equal? (drive (state #hash() #hash() #hash() (set)) "b" "d")
                (state #hash() (hash "b" (set "d")) (hash "d" (set "b")) (set)))

  (define next #hash(("a" . (set "b" "c")) ("b" . (set "c")) ("c" . (set "d" "b")) ("d" . (set "a"))))
  (define prev #hash(("a" . (set "d")) ("b" . (set "a" "c")) ("c" . (set "a" "b")) ("d" . (set "c"))))
  (check-equal? (drive (state #hash() next prev (set)) "b" "d")
                (state
                 (hash
                  '("a" "b")
                  (set
                   (box
                    '(("a" "c") ("b" "d"))
                    '("c" "d")
                    (list (set "a") (set "b" "c") (set "d"))))
                  '("a" "c")
                  (set
                   (box
                    '(("a" "b") ("c" "d"))
                    '("b" "d")
                    (list (set "a") (set "b" "c") (set "d")))))
                 '#hash(("a" . (set "b" "c"))
                        ("b" . ("d" set "c"))
                        ("c" . (set "d" "b"))
                        ("d" . (set "a")))
                 '#hash(("a" . (set "d"))
                        ("b" . (set "a" "c"))
                        ("c" . (set "a" "b"))
                        ("d" . ("b" set "c")))
                 (set
                  (box
                   '(("a" "b") ("c" "d"))
                   '("b" "d")
                   (list (set "a") (set "b" "c") (set "d")))
                  (box
                   '(("a" "c") ("b" "d"))
                   '("c" "d")
                   (list (set "a") (set "b" "c") (set "d")))))))

; TODO add tracking of raw and trie
; TODO consider removing redundant data by getting rid of set of boxes in favor of using values from centers