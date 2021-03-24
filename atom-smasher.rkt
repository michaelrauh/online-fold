#lang racket

(require data/monad 
         data/applicative)

(struct res (a b c d) #:transparent)
(struct box (data center diagonals) #:transparent)
(provide smash-and-grab calculate-center box)

(define (calculate-center b)
  (map car (box-data b)))

(module+ test
  (require rackunit)
  (check-equal? (calculate-center (box
                                   '(("a" "b") ("c" "d"))
                                   '("b" "d")
                                   (list (set "a") (set "b" "c") (set "d"))))
                '("a" "c")))

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
  (box (list (list a b) (list c d)) (list b d) (list (set a) (set b c) (set d))))

(module+ test
  (require rackunit)
  (check-equal? (grab (res "a" "b" "c" "d")) (box (list (list "a" "b") (list "c" "d")) (list "b" "d") (list (set "a") (set "b" "c") (set "d")))))

(module+ test
  (require rackunit)
  (check-equal?
   (smash-and-grab "d"
                   #hash(("a" . (set "b" "c")) ("b" . (set "c" "d")) ("c" . (set "d" "b")) ("d" . (set "a")))
                   #hash(("a" . (set "d")) ("b" . (set "a" "c")) ("c" . (set "a" "b")) ("d" . (set "b" "c"))))
   (set
    (box
     '(("a" "c") ("b" "d"))
     '("c" "d")
     (list (set "a") (set "b" "c") (set "d")))
    (box
     '(("a" "b") ("c" "d"))
     '("b" "d")
     (list (set "a") (set "b" "c") (set "d"))))))

(define (smash-and-grab word next prev)
  (list->set (set-map (smash word next prev) grab)))

; a b
; c d

; a b c d a c b d