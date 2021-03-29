#lang racket
(require "driver.rkt" math)

; TODO consider making shapes vectors instead of lists

(define (combine cur s)
  (define shape (vector->list (array-shape cur)))
  (define combine-candidates (hash-ref (state-boxes s) shape)) ; failure result?
  (define refined-candidates (filter (λ (b) (next-filter (state-next s) (apply * shape) cur b)) combine-candidates))
  (define selected-candidates (filter (λ (b) (diagonal-filter cur b)) refined-candidates))
  (map (λ (b) (combine-winners cur b)) selected-candidates))

(define (next-filter next volume cur candidate)
  (define flat-cur (array-flatten cur))
  (define flat-candidate (array-flatten candidate))
  (for/and ([i (range volume)])
    (define from-word (vector-ref flat-cur i))
    (define target-word (vector-ref flat-candidate i))
    (define to-words (hash-ref next from-word))
    (set-member? to-words target-word)))

(define (diagonal-filter cur candidate)
  (define left-diags (cdr (ortho-diagonals cur)))
  (define right-diags (drop-right (ortho-diagonals candidate) 1))
  (for/and ([l left-diags]
            [r right-diags])
    (set-empty? (set-intersect l r))))

; TODO flip wich center is saved and which is in the map to make this work
(define (combine-winners cur other)
  (define data (array #[(ortho-data cur) (ortho-data other)]))
  (define center (array #[(ortho-center cur) (ortho-center other)]))
  (define left-diags (cdr (ortho-diagonals cur)))
  (define right-diags (drop-right (ortho-diagonals other) 1))
  
  (define diagonal-center (for/list ([left-set left-diags]
                                     [right-set right-diags])
                            (set-union left-set right-set)))
  (define diagonal (append (list (car (ortho-diagonals cur))) diagonal-center (last (ortho-diagonals other))))
  (ortho data center diagonal))
    