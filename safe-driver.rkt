#lang racket
(require "driver.rkt" pfds/deque/bankers)

(struct state (centers next prev boxes phrases raw increment) #:transparent)
(define (safe-drive s cur)
  (cond
    [(= 'break cur) (state (state-centers s) (state-next s) (state-prev s) (state-boxes s) (state-phrases s) (deque) (set))]
    [(empty? (state-raw s)) (state (state-centers s) (state-next s) (state-prev s) (state-boxes s) (state-phrases s) (enqueue cur (state-raw s)) (set))]
    [else (drive s cur)]))