#lang racket
(require "config.rkt" "ortho.rkt" "repo.rkt" threading)

(define (fold-up config repo ortho)
  (define potential-forwards (find-by-size-and-origin repo (ortho-size ortho) (project-forward config (get-origin ortho))))
  (define potential-backwards (find-by-size-and-origin repo (ortho-size ortho) (project-backward config (get-origin ortho))))
  1)