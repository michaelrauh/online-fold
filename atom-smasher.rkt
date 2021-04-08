#lang racket

(require data/monad 
         data/applicative
         math)

(struct res (a b c d) #:transparent)
(struct ortho (data center diagonals) #:transparent)
(provide make-boxes calculate-lhs-foreign-center (struct-out ortho))

; Assumption: This will only ever be called on an ortho with a 2x2 in it
(define (calculate-lhs-foreign-center b)
  (array-slice-ref (ortho-data b) '((0 1) (0))))

(module+ test
  (require rackunit)
  (check-equal? (calculate-lhs-foreign-center (ortho
                                   (array #[#["a" "b"] #["c" "d"]])
                                   (array #[#["a"] #["c"]])
                                   (list (set "a") (set "b" "c") (set "d"))))
                (array #[#["a"] #["c"]])))

; assumption - the latest word passed in is the furthest along in the stream. The stream is being fed in order.
(define (smash word next prev)
  ; d <- c <- a -> b -> d'
  (define d word)
  (list->set
   (sequence->list
    (do [c <- (hash-ref prev d (set))]
      [a <- (hash-ref prev c (set))]
      [b <- (hash-ref next a (set))]
      [d-prime <- (hash-ref next b (set))]
      (if (and
           (equal? d d-prime)
           (not (equal? b c))) (pure (res a b c d))
                               '())))))

(module+ test
  (require rackunit)
  (check-equal? (smash "b"
                       #hash()
                       #hash())
                (set))

  (check-equal? (smash "d"
                       #hash(("a" . (set "b" "c")) ("b" . (set "c" "d")) ("c" . (set "d" "b")) ("d" . (set "a")))
                       #hash(("a" . (set "d")) ("b" . (set "a" "c")) ("c" . (set "a" "b")) ("d" . (set "b" "c"))))
                (set (res "a" "c" "b" "d") (res "a" "b" "c" "d"))))

; assumption - res is well formed
(define (grab x)
  (define a (res-a x))
  (define b (res-b x))
  (define c (res-c x))
  (define d (res-d x))
  (ortho (array #[#[a b] #[c d]]) (array #[#[b] #[d]]) (list (set a) (set b c) (set d))))

(module+ test
  (require rackunit)
  (check-equal? (grab (res "a" "b" "c" "d"))
                (ortho
                 (array #[#["a" "b"] #["c" "d"]])
                 (array #[#["b"] #["d"]])
                 (list (set "a") (set "b" "c") (set "d")))))

(module+ test
  (require rackunit)
  (check-equal?
   (make-boxes "d"
               #hash(("a" . (set "b" "c")) ("b" . (set "c" "d")) ("c" . (set "d" "b")) ("d" . (set "a")))
               #hash(("a" . (set "d")) ("b" . (set "a" "c")) ("c" . (set "a" "b")) ("d" . (set "b" "c"))))
   (set
    (ortho
     (array #[#["a" "c"] #["b" "d"]])
     (array #[#["c"] #["d"]])
     (list (set "a") (set "b" "c") (set "d")))
    (ortho
     (array #[#["a" "b"] #["c" "d"]])
     (array #[#["b"] #["d"]])
     (list (set "a") (set "b" "c") (set "d"))))))

(define (make-boxes word next prev)
  (list->set (set-map (smash word next prev) grab)))

; a b
; c d

; a b c d a c b d