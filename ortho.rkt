#lang racket

(require rebellion/collection/multiset threading)
(provide make-ortho ortho-size ortho-origin ortho-hops)

(struct node (name location)
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (and (equal?-recur (node-name a) (node-name b))
          (equal?-recur (node-location a) (node-location b))))
   (define (hash-proc a hash-recur)
     (+ (hash-recur (node-name a))
        (* 3 (hash-recur (node-location a)))))
   (define (hash2-proc a hash2-recur)
     (+ (hash2-recur (node-name a))
        (hash2-recur (node-location a))))])

(define (ortho-origin o)
  (~>
   o
   (car)
   (set-first)
   (node-name)))

(define (make-ortho a b c d)
  (list
   (set (node a (multiset)))
   (set (node b (multiset b)) (node c (multiset c)))
   (set (node d (multiset b c)))))

(define (ortho-size o)
  (define corner (set-first (last o)))
  (define numbers (hash-values (multiset-frequencies (node-location corner))))
  (apply multiset numbers))

(define (ortho-hops o)
  (apply set (set-map (cadr o) node-name)))

(module+ test
  (require rackunit)
  (define ortho (make-ortho "a" "b" "c" "d"))
  (define ortho2 (make-ortho "a" "c" "b" "d"))
  (check-equal? ortho ortho2)
  (check-equal? (ortho-origin ortho) "a")
  (check-equal? (ortho-size ortho) (multiset 1 1))
  (check-equal? (ortho-hops ortho) (set "b" "c")))