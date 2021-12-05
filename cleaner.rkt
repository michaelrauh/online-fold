#lang typed/racket
(require threading)

(: clean-sentences (-> String (Listof (Listof String))))
(define (clean-sentences s)
  (map (compose1 string-split clean) (string-split s #px"\\.|\\?|\\!")))
(provide clean-sentences)

(: clean (-> String String))
(define (clean s)
  (~>
   s
   (string-replace _ #px";|\'|:|\\," "")
   (string-normalize-spaces)
   (string-downcase)))


(module+ test
  (require typed/rackunit)
  (check-equal? (clean-sentences "A \n\tb ,; ' : c? D e! F g.") '(("a" "b" "c") ("d" "e") ("f" "g"))))