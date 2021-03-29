#lang racket

(require "atom-smasher.rkt" suffixtree)
(struct state (centers next prev boxes phrases raw increment) #:transparent)
(provide state drive state? state-centers state-next state-prev state-boxes state-phrases state-raw state-increment)

(define (drive s cur)
  (define prev (last (state-raw s)))
  (define new-raw (append (state-raw s) (list cur)))
  (define new-phrases (state-phrases s))
  (define deq new-raw)
  (define vect (list->vector deq))
  (define lab (vector->label/with-sentinel vect))
  (tree-add! new-phrases lab)
  (define new-next (hash-update (state-next s) prev (λ (s) (set-add s cur)) (set)))
  (define new-prev (hash-update (state-prev s) cur (λ (s) (set-add s prev)) (set)))
  (define boxes (set-union (state-boxes s) (make-boxes cur new-next new-prev)))
  (define new-centers (for/fold ([centers (state-centers s)])
                                ([box boxes])
                        (hash-update centers (calculate-center box) (λ (s) (set-add s box)) (set))))
  (define new-boxes (set-union (state-boxes s) boxes))
  (state new-centers new-next new-prev new-boxes new-phrases new-raw boxes))

; TODO stop mutating phrases
; TODO make a caller to drive that handles clearing raw and seeding with new data when there is a text break
; TODO remove double conversion before adding to phrases
; TODO convert box data to and centers from lists to vectors

(module+ test
  (require rackunit)
  (define next #hash(("a" . (set "b" "c")) ("b" . (set "c")) ("c" . (set "d" "b")) ("d" . (set "a"))))
  (define prev #hash(("a" . (set "d")) ("b" . (set "a" "c")) ("c" . (set "a" "b")) ("d" . (set "c"))))
  (define tree (make-tree))
  (tree-add! tree (vector->label/with-sentinel (list->vector '("a" "b" "c" "d" "a" "c" "b"))))
  (define res (drive (state #hash() next prev (set) tree
                              '("a" "b" "c" "d" "a" "c" "b") (set)) "d"))

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
  (check-equal? (state-raw res) '("a" "b" "c" "d" "a" "c" "b" "d"))
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