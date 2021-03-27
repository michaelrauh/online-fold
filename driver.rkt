#lang racket

(require "atom-smasher.rkt" trie)
(struct state (centers next prev boxes phrases raw increment) #:transparent)

(define (drive s cur phrase-length)
  (define prev (first (state-raw s)))
  (define new-raw (cons cur (state-raw s)))
  (define new-next (hash-update (state-next s) prev (λ (s) (set-add s cur)) (set)))
  (define new-prev (hash-update (state-prev s) cur (λ (s) (set-add s prev)) (set)))
  (define boxes (set-union (state-boxes s) (make-boxes cur new-next new-prev)))
  (define new-centers (for/fold ([centers (state-centers s)])
                                ([box boxes])
                        (hash-update centers (calculate-center box) (λ (s) (set-add s box)) (set))))
  (define new-boxes (set-union (state-boxes s) boxes))
  (define new-phrases (for/fold ([phrases (state-phrases s)])
                                ([phrase (map reverse (map (λ (pos) (take new-raw pos)) (range 2 (add1 phrase-length))))])
                        (trie-add-item! phrases phrase)))
  (state new-centers new-next new-prev new-boxes new-phrases new-raw boxes))

(module+ test
  (require rackunit)
  (check-equal? (drive (state #hash() #hash() #hash() (set) (make-trie-root) (list "b") (set)) "d" 2)
                (state #hash() (hash "b" (set "d")) (hash "d" (set "b")) (set) (trie-add-item! (make-trie-root) '("b" "d")) '("d" "b") (set)))

  (define next #hash(("a" . (set "b" "c")) ("b" . (set "c")) ("c" . (set "d" "b")) ("d" . (set "a"))))
  (define prev #hash(("a" . (set "d")) ("b" . (set "a" "c")) ("c" . (set "a" "b")) ("d" . (set "c"))))
  (check-equal? (drive (state #hash() next prev (set)
                              (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (make-trie-root) '("a" "b")) '("b" "c")) '("c" "d")) '("d" "a")) '("a" "c")) '("c" "b"))
                              '("b" "c" "a" "d" "c" "b" "a") (set))
                       "d" 2)
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
                   (list (set "a") (set "b" "c") (set "d"))))
                 (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (trie-add-item! (make-trie-root) '("a" "b")) '("b" "c")) '("c" "d")) '("d" "a")) '("a" "c")) '("c" "b")) '("b" "d"))
                 '("d" "b" "c" "a" "d" "c" "b" "a")
                 (set
                  (box
                   '(("a" "b") ("c" "d"))
                   '("b" "d")
                   (list (set "a") (set "b" "c") (set "d")))
                  (box
                   '(("a" "c") ("b" "d"))
                   '("c" "d")
                   (list (set "a") (set "b" "c") (set "d")))))))

; TODO find some way to make it clear that phrases gets mutated in drive
; TODO Replace trie with suffix tree
; TODO Replace raw with a queue
; TODO make a caller to drive that handles clearing raw and seeding with new data when there is a text break
; TODO stop passing phrase length
