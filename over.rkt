#lang racket
(require "config.rkt" "ortho.rkt" "repo.rkt" racket/hash threading)

(module+ test
  (require rackunit)
  (define ortho (make-ortho "a" "b" "c" "d"))
  (define ortho-1 (make-ortho "b" "e" "d" "f"))
  (define ortho-2 (make-ortho "e" "a" "f" "c"))
  (define ortho-3 (make-ortho "e" "f" "a" "b"))
  (define ortho-4 (make-ortho "c" "d" "e" "f"))
  (define config-1 (make-config "a b e. c d f. a c. b d. e f."))
  (define config-2 (make-config "e a b. f c d. e f. a c. b d."))
  (define config-3 (make-config "e f. a b. c d. e a c. f b d."))
  (define config-4 (make-config "a b. c d. e f. a c e. b d f."))
  (define repo (make-repo (list ortho)))
  (check-equal? (fold-over config-1 repo ortho-1)
                (set (ortho-zip-over ortho ortho-1 (hash "e" "b"))))
  (check-equal? (fold-over config-2 repo ortho-2)
                (set (ortho-zip-over ortho-2 ortho (hash "b" "a"))))
  (check-equal? (fold-over config-3 repo ortho-3)
                (set (ortho-zip-over ortho-3 ortho (hash "c" "a"))))
  (check-equal? (fold-over config-4 repo ortho-4)
                (set (ortho-zip-over ortho ortho-4 (hash "e" "c")))))

; a b     b e       a b e
; c d     d f  =>   c d f

; a b     e a       e a b
; c d     f c  =>   f c d

; a b     e f       e f
; c d     a b  =>   a b
;                   c d

; a b     c d       a b
; c d     e f  =>   c d
;                   e f