#lang racket
(require "driver.rkt" 2htdp/batch-io threading suffixtree racket/trace)
(provide calculate input-strings make-empty-state)

(require (only-in pfds/deque/bankers
                  deque
                  enqueue
                  [empty? deque-empty?]))

(define (safe-drive s cur)
  (cond
    [(equal? "stringbreakingpoint" cur) (state (state-centers s) (state-next s) (state-prev s) (state-boxes s) (state-phrases s) (deque) (set))]
    [(deque-empty? (state-raw s)) (state (state-centers s) (state-next s) (state-prev s) (state-boxes s) (state-phrases s) (enqueue cur (state-raw s)) (set))]
    [else (drive s cur)]))

(define (input-strings)
  (~>
   (read-file "example.txt")
   (string-replace _ "\n\n" " stringbreakingpoint ")
   (string-split)
   (map (λ (s) (string-replace s #px"\\W" "")) _)
   (map string-downcase _)))

(define (calculate input-strings state)
  (cond
    [(empty? input-strings) (displayln "done")]
    [else (begin
            (displayln (car input-strings))
            (define step (safe-drive state (car input-strings)))
            (calculate (cdr input-strings) step))]))

(define (make-empty-state)
  (state #hash() #hash() #hash() (set) (make-tree) (deque) (set)))