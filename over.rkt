#lang racket
(require "config.rkt" "ortho.rkt" "repo.rkt" racket/hash threading)

(struct ortho-and-axis (ortho axis))
(struct shift-mapping (ortho source-axis target-axis))
(struct shifted-pair (source target shifted-source shifted-target source-axis target-axis))
(struct mapping (source target shifted-source shifted-target correspondence shift-axis)#:transparent)

(define (fold-over config repo ortho)
  (apply set (append (repo-set-subtract repo (find-forwards config repo ortho)) (repo-set-subtract repo (find-backwards config repo ortho)))))

(define (find-forwards config repo ortho)
  (define potentials (set->list (apply set-union (set-map (ortho-hops ortho) (λ (origin) (apply set (set-map (find-by-size-and-origin repo (ortho-size ortho) origin) (λ (o) (ortho-and-axis o origin)))))))))
  (define potential-shift-mappings (flatten (map (λ (p) (make-ortho-axis-mapping p)) potentials)))
  (define shifted-pairs (map (λ (mapping) (make-shifted-pair ortho mapping)) potential-shift-mappings))
  (define mappings (flatten (map (λ (pair) (make-mappings pair)) shifted-pairs)))
  (define with-overlapping-centers (filter centers-overlap mappings))
  (define winners (filter ((curry phrases-work) config) with-overlapping-centers))
  (map (λ (winner) (ortho-zip-over (mapping-source winner) (mapping-target winner) (mapping-shift-axis winner) (mapping-correspondence winner))) winners))

(define (phrases-work config mapping)
  (define overlap-axis (mapping-shift-axis mapping))
  (define corr (mapping-correspondence mapping))
  (for/and ([phrase-end-name-and-location (get-end-of-each-phrase (mapping-target mapping) (hash-ref (hash-reverse corr) overlap-axis))])
    (phrase-works config overlap-axis (mapping-correspondence mapping) (mapping-source mapping) phrase-end-name-and-location)))

(define (phrase-works config overlap-axis correspondence ortho phrase-end-name-and-location)
  (define starting-location (ortho-location-translate (cdr phrase-end-name-and-location) correspondence))
  (define desired-name (ortho-name-at-location ortho starting-location))
  (define trie (config-phrase-hop config (car phrase-end-name-and-location)))
  (if (config-phrase-hop-contains-name trie desired-name)
      (handle-deep-phrase config trie overlap-axis ortho starting-location)
      #f))

(define (handle-deep-phrase config trie overlap-axis ortho location)
  (if (eq? #f location)
      #t
      (if (config-phrase-hop-contains-name trie (ortho-name-at-location ortho location))
          (handle-deep-phrase config (config-step-trie trie (ortho-name-at-location ortho location)) overlap-axis ortho (ortho-shift-location location overlap-axis))
          #f)))

(define (centers-overlap mapping)
  (define source (mapping-shifted-source mapping))
  (define corr (mapping-correspondence mapping))
  (for/and ([target-name-location-pair (ortho-location-pairs (mapping-shifted-target mapping))])
    (equal? (car target-name-location-pair)
            (ortho-name-at-location source (ortho-location-translate (cdr target-name-location-pair) corr)))))
    
(define (make-mappings pair)
  (define source (shifted-pair-source pair))
  (define target (shifted-pair-target pair))
  (define shifted-source (shifted-pair-shifted-source pair))
  (define shifted-target (shifted-pair-shifted-target pair))
  (define source-axis (shifted-pair-source-axis pair))
  (define target-axis (shifted-pair-target-axis pair))
  (define target-axis-location (ortho-name-to-location target-axis))
  (define all-target-axes (ortho-hops-name-location-pairs shifted-target))
  (define all-target-axes-but-shift-axis (subtract-ortho-pair-by-location all-target-axes target-axis-location))
  (define guesses (permutations all-target-axes-but-shift-axis))
  (define mappings (map (λ (guess) (pairs-to-mapping (subtract-ortho-pair-by-location (ortho-hops-name-location-pairs shifted-source) (ortho-name-to-location source-axis)) guess)) guesses))
  (define complete-mappings (map (λ (h) (hash-union h (hash target-axis source-axis) #:combine (λ (v1 v2) v1))) (filter identity mappings)))
  (map (λ (m) (mapping source target (shifted-pair-shifted-source pair) (shifted-pair-shifted-target pair) m source-axis)) complete-mappings))

(define (pairs-to-mapping source target)
  (for/fold ([acc (hash)])
            ([source-pair source]
             [target-pair target])
    #:break (false? acc)
    (if (eq? (car source-pair) (car target-pair))
        (hash-union acc (hash (ortho-singleton-location-to-name (cdr target-pair)) (ortho-singleton-location-to-name (cdr source-pair))) #:combine (λ (v1 v2) v1))
        #f)))

(define (subtract-ortho-pair-by-location pairs location)
  (filter (λ (pair) (not (equal? (cdr pair) location))) pairs))

(define (make-shifted-pair source mapping)
  (define target (shift-mapping-ortho mapping))
  (define source-shift (shift-mapping-source-axis mapping))
  (define target-shift (shift-mapping-target-axis mapping))
  (define shifted-source (ortho-shift-left source source-shift))
  (define shifted-target (ortho-shift-right target target-shift))
  (shifted-pair source target shifted-source shifted-target source-shift target-shift))

(define (make-ortho-axis-mapping p)
  (define ortho (ortho-and-axis-ortho p))
  (define axis (ortho-and-axis-axis p))
  (set-map (ortho-hops ortho) (λ (h) (shift-mapping ortho axis h))))

(define (find-backwards config repo ortho)
  (list)) ; todo define

(module+ test
  (require rackunit)
  (define ortho (make-ortho "a" "b" "c" "d"))
  (define ortho-1 (make-ortho "b" "e" "d" "f"))
  (define ortho-2 (make-ortho "e" "a" "f" "c"))
  (define ortho-3 (make-ortho "e" "f" "a" "b"))
  (define ortho-4 (make-ortho "c" "d" "e" "f"))
  (define config-1 (make-config "a b e. c d f. a c. b d. e f."))
  (define config-2 (make-config "e a b. f c d. e f. a c. b d."))
  (define config-3 (make-config "e f. a b. c d. e a c. f b d."))
  (define config-4 (make-config "a b. c d. e f. a c e. b d f."))
  (define repo (make-repo (list ortho)))
  ;(check-equal? (fold-over config-1 repo ortho-1)
  ;              (set (ortho-zip-over ortho ortho-1 "b" (hash "e" "b" "d" "c"))))) ; todo add this back for backward
  (check-equal? (fold-over config-2 repo ortho-2)
                (set (ortho-zip-over ortho-2 ortho "a" (hash "b" "a" "c" "f"))))
  (check-equal? (fold-over config-3 repo ortho-3)
                (set (ortho-zip-over ortho-3 ortho "a" (hash "c" "a" "b" "f"))))
  ; (check-equal? (fold-over config-4 repo ortho-4)
  ;        (set (ortho-zip-over ortho ortho-4 "c" (hash "e" "c" "d" "b"))))) ; todo add this back for backward
  )


; a b     b e       a b e
; c d     d f  =>   c d f

; a b     e a       e a b
; c d     f c  =>   f c d

; a b     e f       e f
; c d     a b  =>   a b
;                   c d

; a b     c d       a b
; c d     e f  =>   c d
;                   e f
