#lang racket

(require rebellion/collection/multiset threading)
(provide make-ortho ortho-size ortho-origin ortho-hops ortho-location-pairs ortho-location-translate ortho-name-at-location ortho-get-names-in-buckets ortho-zip-up hash-reverse)

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

(struct ortho (data)
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (and (equal?-recur (ortho-data a) (ortho-data b))))
   (define (hash-proc a hash-recur)
     (+ (hash-recur (ortho-data a))))
   (define (hash2-proc a hash2-recur)
     (+ (hash2-recur (ortho-data a))))])

(define (ortho-get-names-in-buckets ortho)
  (map (λ (x) (list->set (set-map x node-name))) (ortho-data ortho)))

(define (ortho-name-at-location ortho location)
  (for/first ([item (list-ref (ortho-data ortho) (multiset-size location))]
              #:when (equal? (node-location item) location))
    (node-name item)))

(define (ortho-location-translate location mapping)
  (for/multiset ([loc (in-multiset location)])
    (hash-ref mapping loc)))

(define (ortho-location-pairs o)
  (set-map (apply set-union (ortho-data o)) (λ (n) (cons (node-name n) (node-location n)))))

(define (ortho-origin o)
  (~>
   o
   (ortho-data)
   (car)
   (set-first)
   (node-name)))

(define (make-ortho a b c d)
  (ortho 
   (list
    (set (node a (multiset)))
    (set (node b (multiset b)) (node c (multiset c)))
    (set (node d (multiset b c))))))

(define (ortho-size o)
  (define corner (set-first (last (ortho-data o))))
  (define numbers (hash-values (multiset-frequencies (node-location corner))))
  (apply multiset numbers))

(define (ortho-hops o)
  (apply set (set-map (cadr (ortho-data o)) node-name)))

(define (ortho-not-hops-or-origin o)
  (cdr (cdr (ortho-data o))))

(define (map-ortho-locations o mapping)
  (ortho
   (for/list ([s (ortho-data o)])
     (for/set ([n s])
       (node (node-name n) (ortho-location-translate (node-location n) mapping))))))

(define (shift-ortho new-axis o)
  (cons (set)
        (for/list ([s (ortho-data o)])
          (for/set ([n s])
            (node (node-name n) (multiset-add (node-location n) new-axis))))))

(define (hash-reverse mapping)
  (make-hash (map swap (hash->list mapping))))

(define (swap p)
  (cons (cdr p) (car p)))

(define (ortho-zip-up l r mapping)
  (define translated (map-ortho-locations r (hash-reverse mapping)))
  (define augmented (shift-ortho (ortho-origin r) translated))
  (ortho (for/list ([left-set (append (ortho-data l) (list (set)))]
             [right-set augmented])
    (set-union left-set right-set))))

(define (ortho-zip-over l r mapping)
  1)

(module+ test
  (require rackunit)
  (define ortho1 (make-ortho "a" "b" "c" "d"))
  (define ortho2 (make-ortho "a" "c" "b" "d"))
  (define ortho3 (make-ortho "e" "f" "g" "h"))
  (define ortho4 (make-ortho "b" "e" "d" "f"))
  (check-equal? ortho1 ortho2)
  (check-equal? (ortho-origin ortho1) "a")
  (check-equal? (ortho-size ortho1) (multiset 1 1))
  (check-equal? (ortho-hops ortho1) (set "b" "c"))
  (check-equal? (ortho-not-hops-or-origin ortho1) (list (set (node "d" (multiset "c" "b")))))
  (check-equal? (ortho-get-names-in-buckets ortho1) (list (set "a") (set "b" "c") (set "d")))
  (check-equal? (ortho-name-at-location ortho1 (multiset "b" "c")) "d")
  (check-equal? (ortho-location-translate (multiset "a" "b") (hash "a" "c" "b" "d")) (multiset "c" "d"))
  (check-equal? (apply set (ortho-location-pairs ortho1)) (apply set (list (cons "c" (multiset "c")) (cons "b" (multiset "b")) (cons "a" (multiset)) (cons "d" (multiset "c" "b")))))
  (check-equal? (ortho-zip-up ortho1 ortho3 (hash "a" "e" "b" "f" "c" "g" "d" "h"))
                (ortho (list (set (node "a" (multiset))) (set (node "e" (multiset "e")) (node "b" (multiset "b")) (node "c" (multiset "c"))) (set (node "g" (multiset "e" "c")) (node "f" (multiset "e" "b")) (node "d" (multiset "c" "b"))) (set (node "h" (multiset "e" "c" "b"))))))
  (check-equal? (ortho-zip-over ortho1 ortho4 (hash "e" "b"))
                (ortho (list (set (node "a" (multiset))) (set (node "b" (multiset "b")) (node "c" (multiset "c"))) (set (node "e" (multiset "b" "b")) (node "d" (multiset "b" "c"))) (node "f" (multiset "b" "b" "c"))))))