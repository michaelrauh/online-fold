#lang racket

(require "atom-smasher.rkt" math)

(struct phrases (by-first by-second raw)
   #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     ; compare a and b
     (and (equal?-recur (phrases-by-first a) (phrases-by-first b))
          (equal?-recur (phrases-by-second a) (phrases-by-second b))
          (equal?-recur (phrases-raw a) (phrases-raw b))))
   (define (hash-proc a hash-recur)
     ; compute primary hash code of a
     (+ (hash-recur (state-lhs-center-to-ortho a))
        (* 3 (hash-recur (phrases-by-first a)))
        (* 11 (hash-recur (phrases-by-second a)))
        (* 13 (hash-recur (phrases-raw a)))))
   (define (hash2-proc a hash2-recur)
     ; compute secondary hash code of a
     (+ (hash2-recur (phrases-by-first a))
        (hash2-recur (phrases-by-second a))
        (hash2-recur (phrases-raw a))))])
(struct state (lhs-center-to-ortho rhs-center-to-ortho boxes phrases raw increment)
   #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     ; compare a and b
     (and (equal?-recur (state-lhs-center-to-ortho a) (state-lhs-center-to-ortho b))
          (equal?-recur (state-rhs-center-to-ortho a) (state-rhs-center-to-ortho b))
          (equal?-recur (state-boxes a) (state-boxes b))
          (equal?-recur (state-phrases a) (state-phrases b))
          (equal?-recur (state-raw a) (state-raw b))
          (equal?-recur (state-increment a) (state-increment b))))
   (define (hash-proc a hash-recur)
     ; compute primary hash code of a
     (+ (hash-recur (state-lhs-center-to-ortho a))
        (* 3 (hash-recur (state-rhs-center-to-ortho a)))
        (* 11 (hash-recur (state-boxes a)))
        (* 13 (hash-recur (state-phrases a)))
        (* 17 (hash-recur (state-raw a)))
        (* 19 (hash-recur (state-increment a)))))
   (define (hash2-proc a hash2-recur)
     ; compute secondary hash code of a
     (+ (hash2-recur (state-lhs-center-to-ortho a))
        (hash2-recur (state-rhs-center-to-ortho a))
        (hash2-recur (state-boxes a))
        (hash2-recur (state-phrases a))
        (hash2-recur (state-raw a))
        (hash2-recur (state-increment a))))])
(provide (struct-out state) (struct-out phrases) (struct-out ortho) drive)

; assumption - raw is nonempty. Only 2x2 are desired.
(define (drive s cur)
  (define p (state-phrases s))
  (define prev (last (state-raw s)))
  (define new-raw (append (state-raw s) (list cur)))
  (define new-phrases (set-union (phrases-raw (state-phrases s)) (tails new-raw)))
  (define new-next (hash-update (phrases-by-first p) prev (λ (s) (set-add s cur)) (set)))
  (define new-prev (hash-update (phrases-by-second p) cur (λ (s) (set-add s prev)) (set)))
  (define increment (make-boxes cur new-next new-prev))
  (define lhs-center-to-ortho (for/fold ([centers (state-lhs-center-to-ortho s)])
                                        ([box increment])
                                (hash-update centers (ortho-lhs-center box) (λ (s) (set-add s box)) (set))))
  (define rhs-center-to-ortho (for/fold ([centers (state-rhs-center-to-ortho s)])
                                        ([box increment])
                                (hash-update centers (ortho-rhs-center box) (λ (s) (set-add s box)) (set))))
  (define new-boxes (hash-update (state-boxes s) '(2 2) (λ (s) (set-union s increment)) (set)))
  (state lhs-center-to-ortho rhs-center-to-ortho new-boxes (phrases new-next new-prev new-phrases) new-raw increment))

(define (drive-phrases p cur raw)
  (define prev (last raw))
  (define new-raw (append raw (list cur)))
  (define new-phrases (set-union (phrases-raw p) (tails new-raw)))
  (define new-next (hash-update (phrases-by-first p) prev (λ (s) (set-add s cur)) (set)))
  (define new-prev (hash-update (phrases-by-second p) cur (λ (s) (set-add s prev)) (set)))
  (values (phrases new-next new-prev new-phrases) new-raw))

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
  (define next #hash(("a" . (set "b" "c")) ("b" . (set "c")) ("c" . (set "d" "b")) ("d" . (set "a"))))
  (define prev #hash(("a" . (set "d")) ("b" . (set "a" "c")) ("c" . (set "a" "b")) ("d" . (set "c"))))
  (define tree (make-phrases '("a" "b" "c" "d" "a" "c" "b")))
  (define res (drive (state #hash() #hash() #hash() (phrases next prev tree)
                            '("a" "b" "c" "d" "a" "c" "b") (set)) "d"))

  (check-equal? (state-lhs-center-to-ortho res)
                (hash
                 (array #[#["a"] #["b"]])
                 (set
                  (ortho
                   (array #[#["a" "c"] #["b" "d"]])
                   (array #[#["a"] #["b"]])
                   (array #[#["c"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d"))))
                 (array #[#["a"] #["c"]])
                 (set
                  (ortho
                   (array #[#["a" "b"] #["c" "d"]])
                   (array #[#["a"] #["c"]])
                   (array #[#["b"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d"))))))
  (check-equal? (state-rhs-center-to-ortho res)
                (hash
                 (array #[#["c"] #["d"]])
                 (set
                  (ortho
                   (array #[#["a" "c"] #["b" "d"]])
                   (array #[#["a"] #["b"]])
                   (array #[#["c"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d"))))
                 (array #[#["b"] #["d"]])
                 (set
                  (ortho
                   (array #[#["a" "b"] #["c" "d"]])
                   (array #[#["a"] #["c"]])
                   (array #[#["b"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d"))))))
  (check-equal? (phrases-by-first (state-phrases res))
                #hash(("a" . (set "b" "c"))
                      ("b" . ("d" set "c"))
                      ("c" . (set "d" "b"))
                      ("d" . (set "a"))))
  (check-equal? (phrases-by-second (state-phrases res))
                #hash(("a" . (set "d"))
                      ("b" . (set "a" "c"))
                      ("c" . (set "a" "b"))
                      ("d" . ("b" set "c"))))
  (check-equal? (state-boxes res)
                (hash
                 '(2 2)
                 (set
                  (ortho
                   (array #[#["a" "b"] #["c" "d"]])
                   (array #[#["a"] #["c"]])
                   (array #[#["b"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d")))
                  (ortho
                   (array #[#["a" "c"] #["b" "d"]])
                   (array #[#["a"] #["b"]])
                   (array #[#["c"] #["d"]])
                   (list (set "a") (set "b" "c") (set "d"))))))
  (check-true (set-member? (phrases-raw (state-phrases res)) (list "a" "b" "c" "d" "a" "c" "b" "d")))
  (check-equal? (state-raw res) '("a" "b" "c" "d" "a" "c" "b" "d"))
  (check-equal? (state-increment res)
                (set
                 (ortho
                  (array #[#["a" "b"] #["c" "d"]])
                  (array #[#["a"] #["c"]])
                  (array #[#["b"] #["d"]])
                  (list (set "a") (set "b" "c") (set "d")))
                 (ortho
                  (array #[#["a" "c"] #["b" "d"]])
                  (array #[#["a"] #["b"]])
                  (array #[#["c"] #["d"]])
                  (list (set "a") (set "b" "c") (set "d"))))))
