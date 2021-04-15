#lang racket

(define (make-scheduler)
  (define (scheduler success)
    (if (not success)
        #f
        (build-response)))
  scheduler)

(define (build-response)
  (define (next-scheduler next-success)
    (if (not next-success)
        (values '(2 2 2) null)
        (build-response)))
  (values '(2 3) next-scheduler))

(module+ test
  (require rackunit)
  (define sched (make-scheduler))
  (check-false (sched #f)))

(module+ test
  (require rackunit)
  (define sched2 (make-scheduler))
  (define-values (resp sched3) (sched2 #t))
  (check-equal? '(2 3) resp)
  (define-values (resp2 sched4) (sched3 #f))
  (check-equal? resp2 '(2 2 2)))

; ways to frame this problem:
; 1. take in state and a bool and return new state and the next place to check
; 2. take a list of all past bools and returns the next place to check
; 3. take in a bool and return the next place to check as well as a scheduler that will take another bool