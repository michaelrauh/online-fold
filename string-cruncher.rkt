#lang racket
(require "cleaner.rkt" racket/hash)

(define (make-sliding-tuple s)
  (apply append (map zip (clean-sentences s))))

(define (zip x)
  (for/list ([i x] [j (cdr x)])
    (cons i j)))

(define (nexts s)
  (for/fold ([acc (make-immutable-hash)])
            ([tup (make-sliding-tuple s)])
    (hash-union
     acc
     (hash (car tup) (set (cdr tup)))
     #:combine set-union)))

(define (prevs s)
  (for/fold ([acc (make-immutable-hash)])
            ([tup (make-sliding-tuple s)])
    (hash-union
     acc
     (hash (cdr tup) (set (car tup)))
     #:combine set-union)))

(module+ test
  (require rackunit)
  (check-equal?
   (nexts "a b c a c. d e")
   (hash "a" (set "b" "c") "b" (set "c") "c" (set "a") "d" (set "e")))
  (check-equal?
   (prevs "a b c a c. d e")
   (hash "a" (set "c") "b" (set "a") "c" (set "b" "a") "e" (set "d"))))