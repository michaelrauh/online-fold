#lang racket

(require math)

(struct res (a b c d) #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     ; compare a and b
     (and (equal?-recur (res-a a) (res-a b))
          (equal?-recur (res-b a) (res-b b))
          (equal?-recur (res-c a) (res-c b))
          (equal?-recur (res-d a) (res-d b))))
   (define (hash-proc a hash-recur)
     ; compute primary hash code of a
     (+ (hash-recur (res-a a))
        (* 3 (hash-recur (res-b a)))
        (* 11 (hash-recur (res-c a)))
        (* 13 (hash-recur (res-d a)))))
   (define (hash2-proc a hash2-recur)
     ; compute secondary hash code of a
     (+ (hash2-recur (res-a a))
        (hash2-recur (res-b a))
        (hash2-recur (res-c a))
        (hash2-recur (res-d a))))])
(struct ortho (data lhs-center rhs-center diagonals) #:transparent ; todo opaque
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     ; compare a and b
     (and (equal?-recur (ortho-data a) (ortho-data b))
          (equal?-recur (ortho-lhs-center a) (ortho-lhs-center b))
          (equal?-recur (ortho-rhs-center a) (ortho-rhs-center b))
          (equal?-recur (ortho-diagonals a) (ortho-diagonals b))))
   (define (hash-proc a hash-recur)
     ; compute primary hash code of a
     (+ (hash-recur (ortho-data a))
        (* 3 (hash-recur (ortho-lhs-center a)))
        (* 11 (hash-recur (ortho-rhs-center a)))
        (* 13 (hash-recur (ortho-diagonals a)))))
   (define (hash2-proc a hash2-recur)
     ; compute secondary hash code of a
     (+ (hash2-recur (ortho-data a))
        (hash2-recur (ortho-lhs-center a))
        (hash2-recur (ortho-rhs-center a))
        (hash2-recur (ortho-diagonals a))))])
(provide make-boxes (struct-out ortho))
; assumption - the latest word passed in is the furthest along in the stream. The stream is being fed in order.
(define (smash word next prev)
   (define d word)
   (for*/set (
               [c (hash-ref prev d (set))]
               [a (hash-ref prev c (set))]
               [b (hash-ref next a (set))]
               [d-prime (hash-ref next b (set))]
               #:when (and
                       (equal? d d-prime)
                       (not (equal? b c))))
     (res a b c d)))

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
  (ortho (array #[#[a b] #[c d]]) (array #[#[a] #[c]]) (array #[#[b] #[d]]) (list (set a) (set b c) (set d))))

(module+ test
  (require rackunit)
  (check-equal? (grab (res "a" "b" "c" "d"))
                (ortho
                 (array #[#["a" "b"] #["c" "d"]])
                 (array #[#["a"] #["c"]])
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
     (array #[#["c"] #["d"]])
     (list (set "a") (set "b" "c") (set "d")))
    (ortho
     (array #[#["a" "b"] #["c" "d"]])
     (array #[#["a"] #["c"]])
     (array #[#["b"] #["d"]])
     (list (set "a") (set "b" "c") (set "d"))))))

(define (make-boxes word next prev)
  (list->set (set-map (smash word next prev) grab)))