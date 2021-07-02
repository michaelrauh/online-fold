#lang racket
(require "driver.rkt" 2htdp/batch-io threading math "grow-dimension.rkt" "up-dimension.rkt" racket/hash racket/trace)
(provide calculate input-strings make-empty-state)

; assumption - stringbreakingpoint does not occur in any real text. Phrases terminate at text breaks. Broken phrases are not desired in output.
(define (safe-drive s cur)
  (process s cur))

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

(define (input-words s)
  (~>
   s
   (string-split)
   (map (λ (s) (string-replace s #px"\\W" "")) _)
   (map string-downcase _)
   (remove-duplicates _)
   (sort _ string<?)))

(define (input-from-file)
  (input-strings (read-file "example.txt")))

(define (calculate input-strings state)
  (cond
    [(empty? input-strings) state]
    [else (begin
            (define step (safe-drive state (car input-strings)))
            (calculate (cdr input-strings) step))]))

(define (make-empty-state prev next phrases)
  (state #hash() #hash() next prev #hash() phrases (set)))

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

(define (make-all-nexts input-strings)
  (define clean (dropf input-strings (λ (x) (equal? x "stringbreakingpoint"))))
  (if (empty? clean)
      (hash)
      (let-values ([(start end) (splitf-at clean (λ (x) (not (equal? x "stringbreakingpoint"))))])
        (hash-union (get-nexts start)
                    (make-all-nexts end)
                    #:combine set-union))))

(define (make-all-prevs input-strings)
  (define clean (dropf input-strings (λ (x) (equal? x "stringbreakingpoint"))))
  (if (empty? clean)
      (hash)
      (let-values ([(start end) (splitf-at clean (λ (x) (not (equal? x "stringbreakingpoint"))))])
        (hash-union (get-prevs start)
                    (make-all-prevs end)
                    #:combine set-union))))

(define (get-nexts strs)
  (if (< (length strs) 2) (hash)
      (hash-union (hash (car strs) (set (cadr strs)))
                  (get-nexts (cdr strs))
                  #:combine set-union))) ; todo make this a fold, refactor repeats out, consider using hash union in other parts of code

(define (get-prevs strs)
  (if (< (length strs) 2) (hash)
      (hash-union (hash (cadr strs) (set (car strs)))
                  (get-prevs (cdr strs))
                  #:combine set-union)))


;(define s (calculate (input-words (read-file "example.txt")) (make-empty-state (make-all-prevs (input-from-file)) (make-all-nexts (input-from-file)) (make-all-phrases (input-from-file)))))
;(state-boxes s)
;(hash-keys (state-boxes s))

(define (merge s1 s2)
  (define lhs-center-to-ortho (hash-union (state-lhs-center-to-ortho s1) (state-lhs-center-to-ortho s2) #:combine set-union))
  (define rhs-center-to-ortho (hash-union (state-rhs-center-to-ortho s1) (state-rhs-center-to-ortho s2) #:combine set-union))
  (define next (hash-union (state-next s1) (state-next s2) #:combine set-union))
  (define prev (hash-union (state-prev s1) (state-prev s2) #:combine set-union))
  (define boxes (hash-union (state-boxes s1) (state-boxes s2) #:combine set-union))
  (define phrases (set-union (state-phrases s1) (state-phrases s2)))
  (define increment (set))
  (state lhs-center-to-ortho rhs-center-to-ortho next prev boxes phrases increment))

(define (calculator filename)
  (define contents (read-file filename))
  (define words (input-words contents))
  (define strings (input-strings (read-file filename)))
  (define seeded-state (make-empty-state (make-all-prevs strings) (make-all-nexts strings) (make-all-phrases strings)))
  (calculate words seeded-state))

(define (merge-run f1 f2)
  (define s (merge (calculator f1) (calculator f2)))
  (calculate (append (input-words (read-file f1)) (input-words (read-file f2))) s)) ; todo make appended input words sorted and unique

(trace safe-drive)
(merge-run "example1.txt" "example2.txt")

  
(module+ test
  ; a b c  g h i
  ; d e f  j k l
  
  (require rackunit)
  (define full-input-one (input-strings  "a b c. d e f. g h i. j k l. a g."))
  (define partial-input-one (input-words "a b c. d e f. g h i. j k l. a g."))
  (define full-input-two (input-strings  "a d. b e. c f. g j. h k. i l. b h. c i. d j. e k. f l."))
  (define partial-input-two (input-words "a d. b e. c f. g j. h k. i l. b h. c i. d j. e k. f l."))
  (define final-ortho (ortho (array #[#[#["a" "b" "c"] #["d" "e" "f"]] #[#["g" "h" "i"] #["j" "k" "l"]]]) (array #[#[#["a" "b"] #["d" "e"]] #[#["g" "h"] #["j" "k"]]]) (array #[#[#["b" "c"] #["e" "f"]] #[#["h" "i"] #["k" "l"]]]) (list (set "a") (set "d" "b" "g") (set "h" "c" "e" "j") (set "k" "i" "f") (set "l"))))
  
  (define run-one (calculate partial-input-one (make-empty-state (make-all-prevs full-input-one) (make-all-nexts full-input-one) (make-all-phrases full-input-one))))
  (define run-two (calculate partial-input-two (make-empty-state (make-all-prevs full-input-two) (make-all-nexts full-input-two) (make-all-phrases full-input-two))))
  (define final-state (calculate (input-words "a b c. d e f. g h i. j k l. a d. b e. c f. g j. h k. i l. a g. b h. c i. d j. e k. f l.") (merge run-one run-two)))
  (define ans-two (hash-ref (state-boxes final-state) '(2 2 3)))
  (check-true (set-member? ans-two final-ortho)))