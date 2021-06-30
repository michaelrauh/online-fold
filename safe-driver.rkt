#lang racket
(require "driver.rkt" 2htdp/batch-io threading racket/trace math profile "grow-dimension.rkt" "up-dimension.rkt")
(provide calculate input-strings make-empty-state)

; assumption - stringbreakingpoint does not occur in any real text. Phrases terminate at text breaks. Broken phrases are not desired in output.
(define (safe-drive s phrases cur)
  (cond
    [(equal? "stringbreakingpoint" cur) (state (state-lhs-center-to-ortho s) (state-rhs-center-to-ortho s) (state-next s) (state-prev s) (state-boxes s) (list) (set))]
    [(empty? (state-raw s)) (state (state-lhs-center-to-ortho s) (state-rhs-center-to-ortho s) (state-next s) (state-prev s) (state-boxes s) (append (state-raw s) (list cur)) (set))]
    [else (process s phrases cur)]))

(define (process s phrases cur)
  (define base-state (drive s phrases cur))
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
  (time (sift s work-queue 0)))

(define (sift s work-queue cycles)
  (if (empty? work-queue)
      (begin (displayln cycles)
      s)
      (let ([step (drive-generic s (car work-queue))])
        (sift step (append (decorate (state-increment step)) (cdr work-queue)) (add1 cycles)))))

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
   (string-replace _ "." " stringbreakingpoint ")
   (string-split)
   (map (λ (s) (string-replace s #px"\\W" "")) _)
   (map string-downcase _)))

(define (input-from-file)
  (input-strings (read-file "example.txt")))

(define (calculate input-strings state phrases)
  (cond
    [(empty? input-strings) state]
    [else (begin
            (define step (safe-drive state phrases (car input-strings)))
            (calculate (cdr input-strings) step phrases))]))

(define (make-empty-state)
  (state #hash() #hash() #hash() #hash() #hash() (list) (set)))

(define (tails raw)
  (if (= 1 (length raw))
      (set raw)
      (set-union (set raw) (tails (cdr raw)))))

(define (make-phrases raw)
  (for/fold ([phrases (set)])
            ([i (range 1 (add1 (length raw)))])
    (set-union phrases (tails (take raw i)))))

(define (make-all-phrases input-strings)
  (define clean (dropf input-strings (λ (x) (equal? x "stringbreakingpoint"))))
    (if (empty? clean)
        (set)
         (let-values ([(start end) (splitf-at clean (λ (x) (not (equal? x "stringbreakingpoint"))))])
        (set-union
         (make-phrases start)
         (make-all-phrases end)))))

;(module+ test
;  ; a b c  g h i
;  ; d e f  j k l
;  
;  (require rackunit)
;  (define final-ortho (ortho (array #[#[#["a" "b" "c"] #["d" "e" "f"]] #[#["g" "h" "i"] #["j" "k" "l"]]]) (array #[#[#["a" "b"] #["d" "e"]] #[#["g" "h"] #["j" "k"]]]) (array #[#[#["b" "c"] #["e" "f"]] #[#["h" "i"] #["k" "l"]]]) (list (set "a") (set "d" "b" "g") (set "h" "c" "e" "j") (set "k" "i" "f") (set "l"))))
;  (define s (calculate (input-strings "a b c d e f g h i j k l a d b e c f g j h k i l a g b h c i d j e k f") (make-empty-state) (make-all-phrases (input-strings "a b c d e f g h i j k l a d b e c f g j h k i l a g b h c i d j e k f l"))))
;  (define ans-one (hash-ref (state-boxes s) '(2 2 3) (set)))
;  (check-false (set-member? ans-one final-ortho))
;  (define final-state (calculate (input-strings "l") s (input-strings "a b c d e f g h i j k l a d b e c f g j h k i l a g b h c i d j e k f l"))) ; todo track down where the wrong data is in a place. Type error somewhere.
;  (define ans-two (hash-ref (state-boxes final-state) '(2 2 3)))
;  (check-true (set-member? ans-two final-ortho)))