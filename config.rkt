#lang racket
(require "cleaner.rkt" racket/hash)
(provide make-config project-forward project-backward)

(struct config (next prev phrase vocab))

(define (project-forward c o) 
  (hash-ref (config-next c) o (set)))

(define (project-backward c o)
  (hash-ref (config-prev c) o (set)))

(define (make-config s)
  (config
   (nexts s)
   (prevs s)
   (phrases s)
   (vocab s)))

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

(define (dict-add d l)
  (if (empty? l)
      d
      (hash-union d
                  (hash (car l) (dict-add (hash-ref d (car l) (hash)) (cdr l)))
                  #:combine (λ (a b) (rec-union a b)))))

(define (rec-union l r)
  (hash-union l r
              #:combine (λ (a b) (rec-union a b))))

(define (phrases s)
  (for/fold ([d (hash)])
            ([l (map reverse (clean-sentences s))])
    (dict-add d l)))

(define (vocab s)
  (apply set-union (map list->set (clean-sentences s))))

(module+ test
  (require rackunit)
  (check-equal?
   (nexts "a b c a c. d e")
   (hash "a" (set "b" "c") "b" (set "c") "c" (set "a") "d" (set "e")))
  (check-equal?
   (prevs "a b c a c. d e")
   (hash "a" (set "c") "b" (set "a") "c" (set "b" "a") "e" (set "d")))
  (check-equal?
   (phrases "a b c d. a b e d.")
   '#hash(("d" . #hash(("c" . #hash(("b" . #hash(("a" . #hash()))))) ("e" . #hash(("b" . #hash(("a" . #hash())))))))))
  (check-equal?
   (vocab "a b c d. a b e d.")
   (set "d" "e" "b" "c" "a"))
  (define config (make-config "a b a c. d b e d."))
  (check-equal? (project-forward config "a")
                (set "b" "c"))
  (check-equal? (project-backward config "b")
                (set "a" "d")))