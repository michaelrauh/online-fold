#lang typed/racket
(require "cleaner.rkt")

(require/typed racket/hash
               [(hash-union fanout-union) ([(Immutable-HashTable String (Setof String))]
                                           [#:combine ((Setof String) (Setof String) → (Setof String))]
                                           #:rest (Immutable-HashTable String (Setof String))
                                           . ->* .
                                           (Immutable-HashTable String (Setof String)))])

(require/typed racket/hash
               [(hash-union nested-union) ([Nested-HashTable]
                                           [#:combine (Nested-HashTable Nested-HashTable → Nested-HashTable)]
                                           #:rest Nested-HashTable
                                           . ->* .
                                           Nested-HashTable)])

;(provide make-config project-forward project-backward)

(define-type Nested-HashTable (U (Immutable-HashTable Any Any) (Immutable-HashTable String Nested-HashTable)))

(struct config ([next : (Immutable-HashTable String (Setof String))] [prev : (Immutable-HashTable String (Setof String))] [phrase : Nested-HashTable] [vocab : (Setof String)])
  #:transparent)

(: project-forward (config String  -> (Setof String)))
(define (project-forward c s)
  (hash-ref (config-next c) s (λ () ((inst set String)))))

;(define (make-config s)
;  (config
;   (nexts s)
;   (prevs s)
;   (phrases s)
;   (vocab s)))

(: make-sliding-tuple (String -> (Listof (Pairof String String))))
(define (make-sliding-tuple s)
  (apply append (map zip (clean-sentences s))))

(: zip ((Listof String) -> (Listof (Pairof String String))))
(define (zip x)
  (if (empty? x)
      null
      (for/list ([i (in-list x)] [j (in-list (cdr x))])
        (cons i j))))

(: nexts (String -> (HashTable String (Setof String))))
(define (nexts s)
  (for/fold ([acc ((inst hash String (Setof String)))])
            ([tup (in-list (make-sliding-tuple s))])
    (fanout-union
     (hash (car tup) (set (cdr tup)))
     acc
     #:combine set-union)))

(: prevs (String -> (HashTable String (Setof String))))
(define (prevs s)
  (for/fold ([acc ((inst hash String (Setof String)))])
            ([tup (in-list (make-sliding-tuple s))])
    (fanout-union
     (hash (cdr tup) (set (car tup)))
     acc
     #:combine set-union)))

(: dict-add (Nested-HashTable (Listof String) -> Nested-HashTable))
(define (dict-add d l)
  (if (empty? l)
      d
      (nested-union d
                    (hash (car l) (dict-add (layer-down d (car l)) (cdr l)))
                    #:combine (λ (a b) (rec-union a b)))))

(: layer-down (Nested-HashTable String -> HashTable))
(define (layer-down d s)
  (if (hash-has-key? d s)
      (hash-ref d s)
      (inst hash Any Any)))

(hash-ref (hash "a" "b") "x" (λ () "c"))

(: rec-union (Nested-HashTable Nested-HashTable -> Nested-HashTable))
(define (rec-union l r)
  (nested-union l r
                #:combine (λ (a b) (rec-union a b))))

;(define (phrases s)
;  (for/fold ([d (hash)])
;            ([l (map reverse (clean-sentences s))])
;    (dict-add d l)))

;(define (vocab s)
;  (apply set-union (map list->set (clean-sentences s))))

;(module+ test
;  (require rackunit)
;  (define conf (make-config "a b c. b e"))
;  (check-equal?
;   conf
;   (config
;    (hash "a" (set "b") "b" (set "e" "c"))
;    (hash "b" (set "a") "c" (set "b") "e" (set "b"))
;    '#hash(("c" . #hash(("b" . #hash(("a" . #hash())))))
;           ("e" . #hash(("b" . #hash()))))
;    (set "e" "b" "c" "a")))
;  (check-equal?
;   (project-forward conf "a")
;   (set "b"))
;  (check-equal?
;   (project-forward conf "z")
;   (set))
;  (check-equal?
;   (project-backward conf "b")
;   (set "a"))
;  (check-equal?
;   (project-backward conf "z")
;   (set)))