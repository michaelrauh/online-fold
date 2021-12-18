#lang racket
(require "cleaner.rkt" racket/hash)
(provide make-config project-forward project-backward)

(struct config (next prev phrase vocab)
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (and (equal?-recur (config-next a) (config-next b))
          (equal?-recur (config-prev a) (config-prev b))
          (equal?-recur (config-phrase a) (config-phrase b))
          (equal?-recur (config-vocab a) (config-vocab b))))
   (define (hash-proc a hash-recur)
     (+ (hash-recur (config-next a))
        (* 3 (hash-recur (config-prev a)))
        (* 5 (hash-recur (config-phrase a)))
        (* 7 (hash-recur (config-vocab a)))))
   (define (hash2-proc a hash2-recur)
     (+ (hash2-recur (config-next a))
        (hash2-recur (config-prev a))
        (hash2-recur (config-phrase a))
        (hash2-recur (config-vocab a))))])

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
            ([l (set-map (apply set-union (map make-phrases (clean-sentences s))) reverse)])
    (dict-add d l)))

(define (vocab s)
  (apply set-union (map list->set (clean-sentences s))))

(define (tails raw)
  (if (= 1 (length raw))
      (set raw)
      (set-union (set raw) (tails (cdr raw)))))

(define (make-phrases raw)
  (for/fold ([phrases (set)])
            ([i (range 1 (add1 (length raw)))])
    (set-union phrases (tails (take raw i)))))

(module+ test
  (require rackunit)
  (define conf (make-config "a b c. b e"))
  (check-equal?
   conf
   (config
    (hash "a" (set "b") "b" (set "e" "c"))
    (hash "b" (set "a") "c" (set "b") "e" (set "b"))
    '#hash(("a" . #hash())
           ("b" . #hash(("a" . #hash())))
           ("c" . #hash(("b" . #hash(("a" . #hash())))))
           ("e" . #hash(("b" . #hash()))))
    (set "e" "b" "c" "a")))
  (check-equal?
   (project-forward conf "a")
   (set "b"))
  (check-equal?
   (project-forward conf "z")
   (set))
  (check-equal?
   (project-backward conf "b")
   (set "a"))
  (check-equal?
   (project-backward conf "z")
   (set)))