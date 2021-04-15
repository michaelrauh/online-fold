#lang racket
(require "driver.rkt" 2htdp/batch-io threading suffixtree racket/trace math "grow-dimension.rkt" "up-dimension.rkt")
(provide calculate input-strings make-empty-state)

; assumption - stringbreakingpoint does not occur in any real text. Phrases terminate at text breaks. Broken phrases are not desired in output.
(define (safe-drive s cur)
  (cond
    [(equal? "stringbreakingpoint" cur) (state (state-centers s) (state-next s) (state-prev s) (state-boxes s) (state-phrases s) (list) (set))]
    [(empty? (state-raw s)) (state (state-centers s) (state-next s) (state-prev s) (state-boxes s) (state-phrases s) (append (state-raw s) (list cur)) (set))]
    [else (process s cur)]))

(define (process s cur)
  (define base-state (drive s cur))
  (if (empty? (state-increment base-state))
      base-state
      (process-increment base-state)))

(struct up (state))
(struct over (state))

(define (process-increment s)
  (define increment (state-increment s))
  (define work-queue (for/fold ([queue (list)])
                               ([b increment])
                       (append queue (list (over b)) (list (up b)))))
  (sift s work-queue))

(define (sift s work-queue)
  (displayln work-queue)
  (if (empty? work-queue)
      s
      (let ([step (drive-generic s (car work-queue))])
        (sift step (append (decorate (state-increment step)) (cdr work-queue))))))

(define (drive-generic s cur)
  (cond
    [(over? cur) (drive-in s (over-state cur))]
    [(up? cur) (drive-up s (up-state cur))]))

(define (decorate incs)
  (for/fold ([ans (list)])
            ([inc incs])
    (if (base-dim inc)
        (append (list (over inc)) (list (up inc)) ans)
        (append (list (over inc)) ans))))

(define (base-dim inc)
  (empty? (filter (λ (x) (= 2 x)) (vector->list (array-shape (ortho-data inc))))))

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
;  (check-equal? (state-boxes (calculate (input-strings "a b c d a c b d") (make-empty-state)))
;                (hash
;                 '(2 2)
;                 (set
;                  (ortho
;                   (array #[#["a" "b"] #["c" "d"]])
;                   (array #[#["b"] #["d"]])
;                   (list (set "a") (set "b" "c") (set "d")))
;                  (ortho
;                   (array #[#["a" "c"] #["b" "d"]])
;                   (array #[#["c"] #["d"]])
;                   (list (set "a") (set "b" "c") (set "d"))))
;                 '(2 2 2)
;                 (set)))
;  (check-equal? (state-boxes (calculate (input-strings "a b c d a c b d e") (make-empty-state)))
;                (hash
;                 '(2 2)
;                 (set
;                  (ortho
;                   (array #[#["a" "b"] #["c" "d"]])
;                   (array #[#["b"] #["d"]])
;                   (list (set "a") (set "b" "c") (set "d")))
;                  (ortho
;                   (array #[#["a" "c"] #["b" "d"]])
;                   (array #[#["c"] #["d"]])
;                   (list (set "a") (set "b" "c") (set "d"))))
;                 '(2 2 2)
;                 (set)))


; a b c  g h i
; d e f  j k l

; TODO figure out why the above is not found.
; a b   g h
; d e   j k
; 2x2x2 is missing the above entry so the expand fails.
  (check-equal? (hash-ref (state-boxes (calculate (input-strings "a b c d e f g h i j k l a d b e c f g j h k i l a g b h c i d j e k f l") (make-empty-state))) '(2 2 3))
                5))
