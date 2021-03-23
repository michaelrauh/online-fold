#lang racket

(require data/monad 
         data/applicative)

(struct res (a b c d) #:transparent)
(struct box (data center diagonals) #:transparent)

(define (smash word next prev)
  ; d <- c <- a -> b -> d'
  (define d word)
  (sequence->list
   (do [c <- (hash-ref prev d (list))]
     [a <- (hash-ref prev c (list))]
     [b <- (hash-ref next a (list))]
     [d-prime <- (hash-ref next b (list))]
     (if (and
          (equal? d d-prime)
          (not (equal? b c))) (pure (res a b c d))
                              '()))))
  

(module+ test
  (require rackunit)
  (check-equal? (smash "b"
                       #hash()
                       #hash())
                '())

  (check-equal? (smash "d"
                       #hash(("a" . (list "b" "c")) ("b" . (list "c" "d")) ("c" . (list "d" "b")) ("d" . (list "a")))
                       #hash(("a" . (list "d")) ("b" . (list "a" "c")) ("c" . (list "a" "b")) ("d" . (list "b" "c"))))
                (list (res "a" "c" "b" "d") (res "a" "b" "c" "d"))))

(define (grab x)
  (define a (res-a x))
  (define b (res-b x))
  (define c (res-c x))
  (define d (res-d x))
  (box (list (list a b) (list c d)) (list b d) (list (set a) (set b c) (set d))))

(module+ test
  (require rackunit)
  (check-equal? (grab (res "a" "b" "c" "d")) (box (list (list "a" "b") (list "c" "d")) (list "b" "d") (list (set "a") (set "b" "c") (set "d")))))


; a b
; c d

; a b c d a c b d