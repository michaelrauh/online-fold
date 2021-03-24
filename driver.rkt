#lang racket

(require "atom-smasher.rkt" trie)
(struct state (centers next prev boxes phrases) #:transparent)

(define (drive s prev cur)
  (define new-next (hash-update (state-next s) prev (λ (s) (set-add s cur)) (set)))
  (define new-prev (hash-update (state-prev s) cur (λ (s) (set-add s prev)) (set)))
  (define boxes (set-union (state-boxes s) (make-boxes cur new-next new-prev)))
  (define new-centers (for/fold ([centers (state-centers s)])
                                ([box boxes])
                        (hash-update centers (calculate-center box) (λ (s) (set-add s box)) (set))))
  (define new-boxes (set-union (state-boxes s) boxes))
  (define new-phrases (trie-add-item! (state-phrases s) (list prev cur)))
  (state new-centers new-next new-prev new-boxes new-phrases))

(module+ test
  (require rackunit)
  (check-equal? (drive (state #hash() #hash() #hash() (set) (make-trie-root)) "b" "d")
                (state #hash() (hash "b" (set "d")) (hash "d" (set "b")) (set) (trie-add-item! (make-trie-root) '("b" "d"))))

  (define next #hash(("a" . (set "b" "c")) ("b" . (set "c")) ("c" . (set "d" "b")) ("d" . (set "a"))))
  (define prev #hash(("a" . (set "d")) ("b" . (set "a" "c")) ("c" . (set "a" "b")) ("d" . (set "c"))))
  (check-equal? (drive (state #hash() next prev (set) (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (make-trie-root) '("a" "b")) '("b" "c")) '("c" "d")) '("d" "a")) '("a" "c")) '("c" "b"))) "b" "d")
                (state
                 (hash
                  '("a" "b") ; a b c d a c b d
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
                   (list (set "a") (set "b" "c") (set "d")))) (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (make-trie-root) '("a" "b")) '("b" "c")) '("c" "d")) '("d" "a")) '("a" "c")) '("c" "b")) '("b" "d")))))

; TODO add tracking of raw and trie
; TODO consider removing redundant data by getting rid of set of boxes in favor of using values from centers
; TODO check on performance of returning trie instead of accepting that it is mutating
; TODO find some way to make it clear that phrases gets mutated in drive
; TODO add depth tracking so that phrases can go as deep as necessary
; TODO change to passing only the new word, not previous, and dig previous out of raw
; TODO add text break handling