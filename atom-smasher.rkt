#lang racket

(require data/monad 
         data/applicative
         math)

(struct res (a b c d) #:transparent)
(struct ortho (data center diagonals) #:transparent)
(provide make-boxes calculate-center (struct-out ortho))

(define (calculate-center b)
  (array-slice-ref (ortho-data b) (list '(0 1) '(1))))

(module+ test
  (require rackunit)
  (check-equal? (calculate-center (ortho
                                   (array #[#["a" "b"] #["c" "d"]])
                                   (array #[#["a"] #["c"]])
                                   (list (set "a") (set "b" "c") (set "d"))))
                (array #[#["b"] #["d"]])))

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

(define (grab x)
  (define a (res-a x))
  (define b (res-b x))
  (define c (res-c x))
  (define d (res-d x))
  (ortho (array #[#[a b] #[c d]]) (array #[#[a] #[c]]) (list (set a) (set b c) (set d))))

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
     (array #[#["a"] #["b"]])
     (list (set "a") (set "b" "c") (set "d")))
    (ortho
     (array #[#["a" "b"] #["c" "d"]])
     (array #[#["a"] #["c"]])
     (list (set "a") (set "b" "c") (set "d"))))))

(define (make-boxes word next prev)
  (list->set (set-map (smash word next prev) grab)))

; a b
; c d

; a b c d a c b d