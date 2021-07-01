#lang racket

(require "atom-smasher.rkt" math)
(struct state (lhs-center-to-ortho rhs-center-to-ortho next prev boxes phrases raw increment) ; todo move structs, remove unused methods
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     ; compare a and b
     (and (equal?-recur (state-lhs-center-to-ortho a) (state-lhs-center-to-ortho b))
          (equal?-recur (state-rhs-center-to-ortho a) (state-rhs-center-to-ortho b))
          (equal?-recur (state-next a) (state-next b))
          (equal?-recur (state-prev a) (state-prev b))
          (equal?-recur (state-boxes a) (state-boxes b))
          (equal?-recur (state-phrases a) (state-phrases b))
          (equal?-recur (state-raw a) (state-raw b))
          (equal?-recur (state-increment a) (state-increment b))))
   (define (hash-proc a hash-recur)
     ; compute primary hash code of a
     (+ (hash-recur (state-lhs-center-to-ortho a))
        (* 3 (hash-recur (state-rhs-center-to-ortho a)))
        (* 5 (hash-recur (state-next a)))
        (* 7 (hash-recur (state-prev a)))
        (* 11 (hash-recur (state-boxes a)))
        (* 13 (hash-recur (state-phrases a)))
        (* 17 (hash-recur (state-raw a)))
        (* 19 (hash-recur (state-increment a)))))
   (define (hash2-proc a hash2-recur)
     ; compute secondary hash code of a
     (+ (hash2-recur (state-lhs-center-to-ortho a))
        (hash2-recur (state-rhs-center-to-ortho a))
        (hash2-recur (state-next a))
        (hash2-recur (state-prev a))
        (hash2-recur (state-boxes a))
        (hash2-recur (state-phrases a))
        (hash2-recur (state-raw a))
        (hash2-recur (state-increment a))))])
(provide (struct-out state) (struct-out ortho) drive)

; assumption - raw is nonempty. Only 2x2 are desired.
(define (drive s cur)
  (define made-boxes (make-boxes cur (state-next s) (state-prev s)))
  (define known-boxes (hash-ref (state-boxes s) '(2 2) (set)))
  (define increment (list->set (filter-not (λ (x) (set-member? known-boxes x)) (set->list made-boxes))))
  (define lhs-center-to-ortho (for/fold ([centers (state-lhs-center-to-ortho s)])
                                        ([box increment])
                                (hash-update centers (ortho-lhs-center box) (λ (s) (set-add s box)) (set))))
  (define rhs-center-to-ortho (for/fold ([centers (state-rhs-center-to-ortho s)])
                                        ([box increment])
                                (hash-update centers (ortho-rhs-center box) (λ (s) (set-add s box)) (set))))
  (define new-boxes (hash-update (state-boxes s) '(2 2) (λ (s) (set-union s increment)) (set)))
  (state lhs-center-to-ortho rhs-center-to-ortho (state-next s) (state-prev s) new-boxes (state-phrases s) (state-raw s) increment)) ; todo stop passing constant parts of state back out

