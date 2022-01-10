#lang racket

(require rebellion/collection/multiset threading)
(provide make-ortho ortho-size ortho-origin ortho-hops ortho-location-pairs ortho-location-translate ortho-name-at-location ortho-get-names-in-buckets ortho-zip-up hash-reverse ortho-zip-over ortho-shift-left ortho-shift-right ortho-hops-name-location-pairs ortho-name-to-location get-end-of-each-phrase ortho-shift-location ortho-singleton-location-to-name)

(struct node (name location)#:transparent
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

(struct ortho (data) #:transparent
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (and (equal?-recur (ortho-data a) (ortho-data b))))
   (define (hash-proc a hash-recur)
     (+ (hash-recur (ortho-data a))))
   (define (hash2-proc a hash2-recur)
     (+ (hash2-recur (ortho-data a))))])

(define (ortho-singleton-location-to-name loc)
  (car (multiset->list loc)))

(define (ortho-shift-location location axis)
  (if (= 0 (multiset-size location))
      #f
      (multiset-remove location axis)))

(define (get-end-of-each-phrase o axis)
  (define nodes (ortho-location-pairs o))
  (define axis-count (for/fold ([maximum 0])
                               ([node nodes])
                       (if (> (multiset-frequency (cdr node) axis)
                              maximum)
                           (multiset-frequency (cdr node) axis)
                           maximum)))
  (filter (λ (node) (= (multiset-frequency (cdr node) axis)
                       axis-count)) nodes))
  

(define (ortho-name-to-location name)
  (multiset name))

(define (ortho-hops-name-location-pairs o)
  (set-map (car (cdr (ortho-data o))) (λ (n) (cons (node-name n) (node-location n)))))

(define (ortho-shift-left o axis)
  (define cleaned (for/list ([node-set (ortho-data o)])
                    (filter (λ (n) (positive? (multiset-frequency (node-location n) axis))) (set->list node-set))))
  (define shifted (for/list ([node-set cleaned])
                    (apply set (set-map node-set (λ (n) (node (node-name n) (multiset-remove (node-location n) axis)))))))
  (ortho (cdr shifted)))

(define (ortho-shift-right o axis)
  (define nodes (ortho-location-pairs o))
  (define axis-count (for/fold ([maximum 0])
                               ([node nodes])
                       (if (> (multiset-frequency (cdr node) axis)
                              maximum)
                           (multiset-frequency (cdr node) axis)
                           maximum)))
  (define result-pairs (filter (λ (node) (not (= (multiset-frequency (cdr node) axis)
                       axis-count))) nodes))
  (define almost-ortho (for/fold ([ortho (build-list (length (ortho-data o)) (λ (_) (set)))])
            ([name-and-location result-pairs])
    (list-update ortho (multiset-size (cdr name-and-location)) (λ (s) (set-add s (node (car name-and-location) (cdr name-and-location)))))))
  (define ans (if (set-empty? (last almost-ortho))
                  (drop-right almost-ortho 1)
                  almost-ortho))
  (ortho ans))

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

(define (ortho-zip-over l r combine-axis mapping)
  (define nodes-at-end-of-axis (get-nodes-at-end r combine-axis))
  (define shifted (map (λ (x) (add-to-node x combine-axis)) nodes-at-end-of-axis))
  (define mapped (map (λ (x) (map-node-location x mapping)) shifted))
  (add-nodes-to-ortho l mapped))

(define (add-to-node n axis)
  (node (node-name n) (multiset-add (node-location n) axis)))

(define (add-nodes-to-ortho o nodes)
  (ortho
   (for/fold ([acc (ortho-data o)])
             ([n nodes])
     (if (= (length acc)
            (multiset-size (node-location n)))
         (append acc (list (set n)))
         (list-update acc (multiset-size (node-location n)) (λ (x) (set-add x n)))))))

(define (map-node-location n mapping)
  (define location (node-location n))
  (define name (node-name n))
  (node name
        (for/multiset ([x (in-multiset location)])
          (hash-ref mapping x x))))
          

(define (get-nodes-at-end ortho axis)
  (define-values (_ answer)
    (for/fold ([number 0]
               [nodes null])
              ([p (ortho-location-pairs ortho)])
      (cond
        [(< (multiset-frequency (cdr p) axis) number) (values number nodes)]
        [(= (multiset-frequency (cdr p) axis) number) (values number (cons (node (car p) (cdr p)) nodes))]
        [else (values (multiset-frequency (cdr p) axis) (list (node (car p) (cdr p))))])))
  answer)

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
  (check-equal? (ortho-zip-over ortho1 ortho4 "b" (hash "e" "b" "d" "c"))
                (ortho (list
                        (set (node "a" (multiset)))
                        (set (node "b" (multiset "b")) (node "c" (multiset "c")))
                        (set (node "e" (multiset "b" "b")) (node "d" (multiset "b" "c")))
                        (set (node "f" (multiset "b" "b" "c"))))))
  (check-equal? (ortho-shift-location (multiset "a" "b" "b") "b")
                (multiset "a" "b"))
  (check-equal? (ortho-shift-location (multiset) "b") #f)
  (check-equal? (get-end-of-each-phrase ortho1 "b")
                (list (cons "b" (multiset "b")) (cons "d" (multiset "c" "b"))))
  (check-equal? (ortho-name-to-location "a") (multiset "a"))
  (check-equal? (apply set (ortho-hops-name-location-pairs ortho1)) (set (cons "b" (multiset "b")) (cons "c" (multiset "c"))))
  (check-equal? (ortho-shift-left ortho1 "b") (ortho (list (set (node "b" (multiset))) (set (node "d" (multiset "c"))))))
  (check-equal? (ortho-shift-right ortho1 "b") (ortho (list (set (node "a" (multiset))) (set (node "c" (multiset "c"))))))
  (check-equal? (ortho-singleton-location-to-name (multiset "b")) "b"))