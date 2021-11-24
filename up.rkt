#lang racket
(require "config.rkt" "ortho.rkt" "repo.rkt" threading)

(define (fold-up config repo ortho)
  (define potential-forwards (find-by-size-and-origin repo (ortho-size ortho) (project-forward config (ortho-origin ortho))))
  (define potential-backwards (find-by-size-and-origin repo (ortho-size ortho) (project-backward config (ortho-origin ortho))))
  1)