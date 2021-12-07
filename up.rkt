#lang racket
(require "config.rkt" "ortho.rkt" "repo.rkt" racket/hash threading)

(define (fold-up config repo ortho)
  (apply set (repo-set-subtract repo (find-forwards config repo ortho)) (repo-set-subtract repo (find-backwards config repo ortho))))

(define (find-forwards config repo ortho)
  (define potential-forwards (for/fold ([acc (set)])
                                       ([origin-to-search (project-forward config (ortho-origin ortho))])
                               (set-union acc (find-by-size-and-origin repo (ortho-size ortho) origin-to-search))))
  
  (define corrs (make-corrs potential-forwards config ortho))
  (define better-corrs (ungroup potential-forwards corrs null))
  (define projects (filter (λ (corr) (check-projection config ortho corr)) better-corrs))
  (define found (filter (λ (corr) (check-diagonals ortho corr)) projects))
  (map (λ (right) (ortho-zip-up ortho right better-corrs)) found))

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

(module+ test
  (require rackunit)
  (define ortho (make-ortho "a" "b" "c" "d"))
  (define ortho-r (make-ortho "e" "f" "g" "h"))
  (define config (make-config "a b. c d. a c. b d. e f. g h. e g. f h. a e. b f. c g. d h."))
  (define repo (make-repo (list ortho-r)))
  (check-equal? (fold-up config repo ortho)
                (list (ortho-zip-up ortho ortho-r (hash "a" "e" "b" "f" "c" "g" "d" "h")))))