#lang racket
(require "config.rkt" "ortho.rkt" "repo.rkt" racket/hash threading)

(struct ortho-and-axis (ortho axis))
(struct shift-mapping (ortho source-axis target-axis))
(struct shifted-pair (source target shifted-source shifted-target source-axis target-axis))
(struct mapping (source target shifted-source shifted-target correspondence shift-axis))

(define (fold-over config repo ortho)
  (apply set (append (repo-set-subtract repo (find-forwards config repo ortho)) (repo-set-subtract repo (find-backwards config repo ortho)))))

(define (find-forwards config repo ortho)
  (define potentials (set-map (ortho-hops ortho) (λ (origin) (ortho-and-axis (find-by-size-and-origin repo (ortho-size ortho) origin) origin))))
  (define potential-shift-mappings (map (λ (p) (make-ortho-axis-mapping p)) potentials))
  (define shifted-pairs (map (λ (mapping) (make-shifted-pair ortho mapping)) potential-shift-mappings))
  (define mappings (flatten (map (λ (pair) (make-mappings pair)) shifted-pairs))) 
  (define with-overlapping-centers (filter centers-overlap mappings))
  (define winners (filter ((curry phrases-work) config) with-overlapping-centers))
  (map (λ (winner) (ortho-zip-over (mapping-source winner) (mapping-target winner) (mapping-shift-axis winner) (mapping-correspondence winner))) winners))

(define (phrases-work config mapping)
  (define overlap-axis (mapping-shift-axis mapping))
  (for/and ([phrase-end-name-and-location (get-end-of-each-phrase (mapping-target mapping) (hash-ref (mapping-correspondence mapping) overlap-axis))])
    (phrase-works config overlap-axis (mapping-correspondence mapping) (mapping-source mapping) phrase-end-name-and-location)))

(define (phrase-works config overlap-axis correspondence ortho phrase-end-name-and-location)
  (define starting-location (ortho-map-location (cdr phrase-end-name-and-location)))
  (define desired-name (ortho-name-at-location ortho starting-location))
  (define trie (config-phrase-hop config (car phrase-end-name-and-location)))
  (if (config-phrase-hop-contains-name trie desired-name)
      (handle-deep-phrase config trie overlap-axis (ortho-shift-location starting-location overlap-axis))
      #f))

(define (handle-deep-phrase config trie overlap-axis location)
  (if (eq? #t location)
      #t
      (if (config-phrase-hop-contains-name trie (ortho-name-at-location ortho location))
          (handle-deep-phrase config (config-step-trie trie (ortho-name-at-location ortho location)) overlap-axis (ortho-shift-location location overlap-axis))
          #f)))

(define (centers-overlap mapping)
  (for/and ([name-location-pair (ortho-location-pairs mapping-shifted-target)])
    (eq?
     (ortho-get-name-by-location mapping-shifted-source (hash-ref mapping-correspondence (car name-location-pair) #f))
     (cdr name-location-pair))))

(define (make-mappings pair)
  (define source (shifted-pair-source pair))
  (define target (shifted-pair-target pair))
  (define source-axis (shifted-pair-source-axis pair))
  (define target-axis (shifted-pair-target-axis pair))
  (define guesses (permutations (subtract-ortho-pair-by-location (ortho-hops-name-location-pairs target) (ortho-name-to-location target-axis))))
  (define mappings (map (λ (guess) (pairs-to-mapping (subtract-ortho-pair-by-location (ortho-hops-name-location-pairs source) (ortho-name-to-location source-axis)) guess)) guesses))
  (define complete-mappings (map (λ (h) (hash-union h (hash target-axis source-axis) #:combine (λ (v1 v2) v1))) (filter identity mappings)))
  (map (λ (m) (mapping source target (shifted-pair-shifted-source pair) (shifted-pair-shifted-target pair) m source-axis))))

(define (pairs-to-mapping source target)
  (for/fold ([acc (hash)])
            ([source-pair source]
             [target-pair target])
    #:break (false? acc)
    (if (eq? (car source-pair) (car target-pair))
        (hash-union acc (hash (cdr target-pair) (cdr source-pair)) #:combine (λ (v1 v2) v1))
        #f)))

(define (subtract-ortho-pair-by-location pairs location)
  (filter (λ (pair) (not (eq? (cdr pair) location))) pairs))

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
  (map (λ (h) (shift-mapping ortho axis h)) (ortho-hops ortho)))

(define (find-backwards config repo ortho)
  1)

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
  (check-equal? (fold-over config-1 repo ortho-1)
                (set (ortho-zip-over ortho ortho-1 "b" (hash "e" "b" "d" "c"))))
  (check-equal? (fold-over config-2 repo ortho-2)
                (set (ortho-zip-over ortho-2 ortho "a" (hash "b" "a" "c" "f"))))
  (check-equal? (fold-over config-3 repo ortho-3)
                (set (ortho-zip-over ortho-3 ortho "a" (hash "c" "a" "b" "f"))))
  (check-equal? (fold-over config-4 repo ortho-4)
                (set (ortho-zip-over ortho ortho-4 "c" (hash "e" "c" "d" "b")))))

; a b     b e       a b e       a()    b(b)             b()  e(e)
; c d     d f  =>   c d f       c(c)   d(bc)            d(d) f(de) first know that the bs overlap. then look at ds. those overlap too. path for lhs d with shift is c, so c maps to d. e is the remainder axis, so b maps to it.

; a b     e a       e a b
; c d     f c  =>   f c d

; a b     e f       e f
; c d     a b  =>   a b
;                   c d

; a b     c d       a b
; c d     e f  =>   c d
;                   e f


;a() b(b)              b() i(i) 
;c(c) d(bc)            d(d) j(di)

;e(e) f(be)            f(f) k(fi) 
;g(ce) h(bce)          h(df) m(dfi)


;1. we are zipping on axis b (hop matches origin)
;2. We don’t know the RHS axis in question. It could be i, d, or f. Choose all of them nondeterministically.
;3. For the case where we choose i, shift out one i. and record that b=>i
;4. The remaining question is: How does ce correspond to df? (perhaps both ways. This could match multiple)
;5. Correspondence is done by exact match. That is, since d == d, the paths must correspond. Look at single distance things after shifts, and that is your correspondence
;6. Check the other locations across the correspondence for matching
;7. Rewrite this algorithm to eliminate the concept of shifting. This can be done by logically shifting. That is, simply ignore things that are shifted out.


;b()            b()
;d(c)           d(d)

;f(e)           f(f)
;h(ce)          h(df)



  ;Phrases check:

;a() b(b)       b() i(i) 
;c(c) d(bc)            d(d) j(di)

;e(e) f(be)             f(f) k(fi) 
;g(ce) h(bce)        h(df) m(dfi)


;1. Get the overlap axis. In this case it is b, and mapping is {b=i, c=d, e=f, g=h}
;2. Look up the axis that corresponds to the overlap axis. In this case it is i
;3. Find the max number of is
;4. Find all nodes with the max number of is
;5. For each node:
;    1. map the location over to the source
;    2. do a hop and see if that name is in the phrase trie
;    3. if it is not, fail
;    4. if it is, take the location from the last hop, subtract off one instance of the overlap axis, look up the corresponding name, and hop on that
;    5. see if the hop lead to something. If not, fail. If so, return to step 4