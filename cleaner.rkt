#lang racket
(require threading)

(define (clean-sentences s)
  (define sentences (string-split s #px"\\.|\\?|\\!"))
  (map (compose1 string-split clean) sentences))
(provide clean-sentences)

(define (clean s)
  (~>
   s
   (string-replace _ #px";|\'|:|\\," "")
   (string-normalize-spaces)
   (string-downcase)))


(module+ test
  (require rackunit)
  (check-equal? (clean-sentences "A \n\tb ,; ' : c? D e! F g.") '(("a" "b" "c") ("d" "e") ("f" "g"))))