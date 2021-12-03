#lang racket
(require "ortho.rkt" racket/hash)
(provide make-repo add-ortho find-by-size-and-origin find-by-size-and-hop repo-set-subtract)

(struct repo (origins hops))

(define (make-repo)
  (repo (hash) (hash)))

(define (repo-set-subtract r l)
  (filter (λ (o) (not (ortho-in-repo r o))) l))

(define (ortho-in-repo r o)
  (define possibles (find-by-size-and-origin r (ortho-size o) (ortho-origin o)))
  (for/or ([other possibles])
    (equal? o other)))

(define (add-ortho r o)
  (define size-to-insert (ortho-size o))
  (define new-origins (hash-union
                       (repo-origins r)
                       (hash (cons size-to-insert (ortho-origin o)) (set o))
                       #:combine set-union))
  
  (define new-hops (for/fold ([acc (repo-hops r)])
                             ([cur (ortho-hops o)])
                     (hash-union
                      acc
                      (hash (cons size-to-insert cur) (set o))
                      #:combine set-union)))
  
  (repo new-origins new-hops))

(define (find-by-size-and-origin r size origin)
  (hash-ref (repo-origins r) (cons size origin) (set)))

(define (find-by-size-and-hop r size hop)
  (hash-ref (repo-hops r) (cons size hop) (set)))

(module+ test
  (require rackunit)
  (define repo (make-repo))
  (define ortho (make-ortho "a" "b" "c" "d"))
  (define added (add-ortho repo ortho))
  (define size (ortho-size ortho))
  (define origin (ortho-origin ortho))
  (define hops (ortho-hops ortho))
  (define other-ortho (make-ortho "x" "y" "z" "b"))
  
  (check-equal? (find-by-size-and-origin repo size origin)
                (set))
  (check-equal? (find-by-size-and-origin added size origin)
                (set ortho))
  (check-equal? (find-by-size-and-hop added size (set-first hops))
                (set ortho))
  (check-equal? (find-by-size-and-hop repo size (set-first hops))
                (set))
  (check-equal? (repo-set-subtract added (list ortho other-ortho))
                (list other-ortho)))
                