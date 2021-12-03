#lang racket
(require "config.rkt" "ortho.rkt" "repo.rkt" racket/hash threading)

(define (fold-up config repo ortho)
  (apply set (repo-set-subtract repo (find-forwards config repo ortho)) (repo-set-subtract repo (find-backwards config repo ortho))))

(define (find-forwards config repo ortho)
  (define potential-forwards (find-by-size-and-origin repo (ortho-size ortho) (project-forward config (ortho-origin ortho))))
  (~>
   potential-forwards
   (make-corrs _ config ortho)
   (ungroup potential-forwards _ null)
   (filter (λ (corr) (check-projection config ortho corr)) _)
   (filter (λ (corr) (check-diagonals ortho corr)) _)))

(define (check-diagonals ortho corr)
  (define potential-forward (car corr))
  (for/and ([left (cdr (ortho-get-names-in-buckets ortho))]
            [right (drop-right (ortho-get-names-in-buckets potential-forward))])
    (set-disjoint? left right)))

(define (set-disjoint? s1 s2)
  (set-empty? (set-intersect s1 s2)))

(define (make-corrs potential-forwards config ortho)
  (for/list ([potential-forward potential-forwards])
    (findCorrespondence
     (ortho-hops potential-forward)
     (set-map (λ (hop) (cons hop (project-forward config hop))) (ortho-hops ortho)))))

(define (check-projection config ortho corr)
  (define potential-forward (car corr))
  (define mapping (cdr corr))
  (for/and ([name-and-location (ortho-location-pairs ortho)])
    (check-maps (car name-and-location) (cdr name-and-location) potential-forward mapping config)))

(define (check-maps name location potential-forward mapping config)
  (define translated-location (ortho-location-translate location mapping))
  (define name-at-location (ortho-name-at-location potential-forward translated-location))
  (define desired (project-forward config name))
  (set-member? name-at-location desired))

(define (ungroup potential-forwards corrs acc)
  (cond
    [(null? corrs) acc]
    [(null? (car corrs)) (ungroup (cdr potential-forwards) (cdr corrs) acc)]
    [else (ungroup potential-forwards
                   (cons (cdr (car (corrs))) (cdr corrs))
                   (cons
                    (cons (car potential-forwards) (car (car corrs)))
                    acc))])) 
  
  
(define (find-backwards config repo ortho)
  1)

(define (findCorrespondence hop-of-potential-forward from-hop-projection-pairs)
  (filter
   (λ (x) (not (eq? #f x)))
   (map
    (λ (potential) (findCorr potential from-hop-projection-pairs (hash)))
    (permutations
     (set->list hop-of-potential-forward)))))

(define (findCorr forward ppair acc)
  (cond
    [(empty? forward) acc]
    [(set-member? (cdr (car ppair)) (car forward)) (findCorr (cdr forward) (cdr ppair)
                                                             (hash-union acc
                                                                         (hash (car (car ppair)) (car forward))))]
    [else #f]))
