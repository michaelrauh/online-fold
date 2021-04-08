#lang racket
(require "driver.rkt" 2htdp/batch-io threading suffixtree racket/trace math)
(provide calculate input-strings make-empty-state)

; assumption - stringbreakingpoint does not occur in any real text. Phrases terminate at text breaks. Broken phrases are not desired in output.
(define (safe-drive s cur)
  (cond
    [(equal? "stringbreakingpoint" cur) (state (state-centers s) (state-next s) (state-prev s) (state-boxes s) (state-phrases s) (list) (set))]
    [(empty? (state-raw s)) (state (state-centers s) (state-next s) (state-prev s) (state-boxes s) (state-phrases s) (append (state-raw s) (list cur)) (set))]
    [else (drive s cur)]))

; assumption - double newlines and periods are breaking points. Casing is not important. Punctuation is not important.
(define (input-strings s)
  (~>
   s
   (string-replace _ "\n\n" " stringbreakingpoint ")
   (string-replace _ "." " stringbreakingpoint ")
   (string-split)
   (map (λ (s) (string-replace s #px"\\W" "")) _)
   (map string-downcase _)))

(define (input-from-file)
  (input-strings (read-file "example.txt")))

(define (calculate input-strings state)
  (cond
    [(empty? input-strings) state]
    [else (begin
            (define step (safe-drive state (car input-strings)))
            (calculate (cdr input-strings) step))]))

(define (make-empty-state)
  (state #hash() #hash() #hash() #hash() (make-tree) (list) (set)))

(module+ test
  (require rackunit)
  (check-equal? (state-increment (calculate (input-strings "a b c d a c b d") (make-empty-state)))
                (set (ortho
                      (array #[#["a" "b"] #["c" "d"]])
                      (array #[#["b"] #["d"]])
                      (list (set "a") (set "b" "c") (set "d")))
                     (ortho (array #[#["a" "c"] #["b" "d"]])
                            (array #[#["c"] #["d"]])
                            (list (set "a") (set "b" "c") (set "d")))))
  (check-equal? (state-increment (calculate (input-strings "a b c d a c b d e") (make-empty-state)))
                (set)))
