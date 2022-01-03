#lang racket
(require "config.rkt" "ortho.rkt" "repo.rkt" racket/hash threading)

(struct correspondence (ortho mapping))
(struct projection-pair (first second))

(define (fold-up config repo ortho)
  (apply set (append (repo-set-subtract repo (find-forwards config repo ortho)) (repo-set-subtract repo (find-backwards config repo ortho)))))

(define (find-forwards config repo ortho)
  (find config repo ortho project-forward check-diagonals-forward))

(define (find-backwards config repo ortho)
  (find config repo ortho project-backward check-diagonals-backward))

(define (find config repo ortho projection-function check-diagonals)
  (define potentials (for/fold ([acc (set)])
                               ([origin-to-search (projection-function config (ortho-origin ortho))])
                       (set-union acc (find-by-size-and-origin repo (ortho-size ortho) origin-to-search))))
  (~>
   potentials
   (make-mappings _ config ortho projection-function)
   (ungroup (set->list potentials) _ null)
   (filter (λ (corr) (check-projection config ortho corr projection-function)) _)
   (filter (λ (corr) (check-diagonals ortho corr)) _)
   (map (λ (right) (ortho-zip-up ortho (correspondence-ortho right) (correspondence-mapping right))) _)))

(define (check-diagonals-forward ortho corr)
  (for/and ([left (cdr (ortho-get-names-in-buckets ortho))]
            [right (drop-right (ortho-get-names-in-buckets (correspondence-ortho corr)) 1)])
    (set-disjoint? left right)))

(define (check-diagonals-backward ortho corr)
  (for/and ([right (drop-right (ortho-get-names-in-buckets ortho) 1)]
            [left (cdr (ortho-get-names-in-buckets (correspondence-ortho corr)))])
    (set-disjoint? left right)))

(define (set-disjoint? s1 s2)
  (set-empty? (set-intersect s1 s2)))

(define (make-mappings potential-forwards config ortho projection-function)
  (for/list ([potential-forward potential-forwards])
    (find-mapping
     (ortho-hops potential-forward)
     (set-map (ortho-hops ortho) (λ (hop) (projection-pair hop (projection-function config hop)))))))

(define (check-projection config ortho corr project-function)
  (for/and ([name-and-location (ortho-location-pairs ortho)])
    (check-maps (car name-and-location) (cdr name-and-location) (correspondence-ortho corr) (correspondence-mapping corr) config project-function)))

(define (check-maps name location potential-forward mapping config project-function)
  (set-member? (project-function config name) (ortho-name-at-location potential-forward (ortho-location-translate location mapping))))

(define (ungroup potential-forwards mappings acc)
  (cond
    [(null? mappings) acc]
    [(null? (car mappings)) (ungroup (cdr potential-forwards) (cdr mappings) acc)]
    [else (ungroup potential-forwards
                   (cons (cdr (car mappings)) (cdr mappings))
                   (cons
                    (correspondence (car potential-forwards) (car (car mappings)))
                    acc))]))

(define (find-mapping hop-of-potential-forward from-hop-projection-pairs)
  (filter
   identity
   (map
    (λ (potential) (build-mapping potential from-hop-projection-pairs (hash)))
    (permutations
     (set->list hop-of-potential-forward)))))

(define (build-mapping forward ppairs acc)
  (cond
    [(empty? forward) acc]
    [(set-member? (projection-pair-second (car ppairs)) (car forward)) (build-mapping (cdr forward) (cdr ppairs)
                                                                                      (hash-union acc
                                                                                                  (hash (projection-pair-first (car ppairs)) (car forward))))]
    [else #f]))

(module+ test
  (require rackunit)
  (define ortho (make-ortho "a" "b" "c" "d"))
  (define ortho-r (make-ortho "e" "f" "g" "h"))
  (define config (make-config "a b. c d. a c. b d. e f. g h. e g. f h. a e. b f. c g. d h."))
  (define repo (make-repo (list ortho-r)))
  (check-equal? (fold-up config repo ortho)
                (set (ortho-zip-up ortho ortho-r (hash "b" "f" "c" "g"))))
  (define repo-two (make-repo (list ortho)))
  (check-equal? (fold-up config repo-two ortho-r)
                (set (ortho-zip-up ortho-r ortho (hash "f" "b" "g" "c")))))
