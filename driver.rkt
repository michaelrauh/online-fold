#lang racket

(require "atom-smasher.rkt" pfds/deque/bankers suffixtree)
(struct state (centers next prev boxes phrases raw increment) #:transparent)
(provide state drive state? state-centers state-next state-prev state-boxes state-phrases state-raw state-increment)

(define (drive s cur)
  (displayln "one")
  (define prev (last (state-raw s)))
  (displayln "two")
  (define new-raw (enqueue cur (state-raw s)))
  (displayln "three")
  (define new-phrases (state-phrases s))
  (displayln "four")
  (tree-add! new-phrases (vector->label/with-sentinel (list->vector (deque->list new-raw)))) ; this line is slow
  (displayln "five")
  (define new-next (hash-update (state-next s) prev (λ (s) (set-add s cur)) (set)))
  (displayln "six")
  (define new-prev (hash-update (state-prev s) cur (λ (s) (set-add s prev)) (set)))
  (displayln "seven")
  (define boxes (set-union (state-boxes s) (make-boxes cur new-next new-prev)))
  (displayln "eight")
  (define new-centers (for/fold ([centers (state-centers s)])
                                ([box boxes])
                        (hash-update centers (calculate-center box) (λ (s) (set-add s box)) (set))))
  (displayln "nine")
  (define new-boxes (set-union (state-boxes s) boxes))
  (displayln "ten")
  (state new-centers new-next new-prev new-boxes new-phrases new-raw boxes))

; TODO stop mutating phrases
; TODO make a caller to drive that handles clearing raw and seeding with new data when there is a text break
; TODO remove double conversion before adding to phrases

(module+ test
  (require rackunit)
  (define next #hash(("a" . (set "b" "c")) ("b" . (set "c")) ("c" . (set "d" "b")) ("d" . (set "a"))))
  (define prev #hash(("a" . (set "d")) ("b" . (set "a" "c")) ("c" . (set "a" "b")) ("d" . (set "c"))))
  (define tree (make-tree))
  (tree-add! tree (vector->label/with-sentinel (list->vector '("a" "b" "c" "d" "a" "c" "b"))))
  (define res (drive (state #hash() next prev (set) tree
                              (enqueue "b" (enqueue "c" (enqueue "a" (enqueue "d" (enqueue "c" (enqueue "b" (enqueue "a" (deque)))))))) (set)) "d"))

  (check-equal? (state-centers res) (hash
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
     (list (set "a") (set "b" "c") (set "d"))))))
  (check-equal? (state-next res) #hash(("a" . (set "b" "c"))
         ("b" . ("d" set "c"))
         ("c" . (set "d" "b"))
         ("d" . (set "a"))))
  (check-equal? (state-prev res) #hash(("a" . (set "d"))
         ("b" . (set "a" "c"))
         ("c" . (set "a" "b"))
         ("d" . ("b" set "c"))))
  (check-equal? (state-boxes res) (set
   (box
    '(("a" "b") ("c" "d"))
    '("b" "d")
    (list (set "a") (set "b" "c") (set "d")))
   (box
    '(("a" "c") ("b" "d"))
    '("c" "d")
    (list (set "a") (set "b" "c") (set "d")))))
  (check-true (tree-contains? (state-phrases res) (vector->label (list->vector '("a" "b" "c" "d" "a" "c" "b" "d")))))
  (check-equal? (deque->list (state-raw res)) '("a" "b" "c" "d" "a" "c" "b" "d"))
  (check-equal? (state-increment res) (set
   (box
    '(("a" "b") ("c" "d"))
    '("b" "d")
    (list (set "a") (set "b" "c") (set "d")))
   (box
    '(("a" "c") ("b" "d"))
    '("c" "d")
    (list (set "a") (set "b" "c") (set "d"))))))

;(require racket/trace)
;(trace drive)