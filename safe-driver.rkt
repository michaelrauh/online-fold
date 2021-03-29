#lang racket
(require "driver.rkt" 2htdp/batch-io threading suffixtree racket/trace)
(provide calculate input-strings make-empty-state)

(define (safe-drive s cur)
  (cond
    [(equal? "stringbreakingpoint" cur) (state (state-centers s) (state-next s) (state-prev s) (state-boxes s) (state-phrases s) (list) (set))]
    [(empty? (state-raw s)) (state (state-centers s) (state-next s) (state-prev s) (state-boxes s) (state-phrases s) (append (state-raw s) (list cur)) (set))]
    [else (drive s cur)]))

(define (input-strings)
  (~>
   (read-file "example.txt")
   (string-replace _ "\n\n" " stringbreakingpoint ")
   (string-replace _ "." " stringbreakingpoint ")
   (string-split)
   (map (λ (s) (string-replace s #px"\\W" "")) _)
   (map string-downcase _)))

(define (calculate input-strings state)
  (cond
    [(empty? input-strings) state]
    [else (begin
            (define step (safe-drive state (car input-strings)))
            (calculate (cdr input-strings) step))]))

(define (make-empty-state)
  (state #hash() #hash() #hash() (set) (make-tree) (list) (set)))

(calculate (input-strings) (make-empty-state))