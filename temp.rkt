#lang racket
;(require suffixtree)
;(define tree (make-tree))
;(tree-add! tree (vector->label/with-sentinel (list->vector (list "a" "b" "c"))))
;(tree-contains? tree (vector->label (list->vector (list "a"))))

(require pfds/queue/bankers)



(queue->list (enqueue 10 (queue 4 5 6)))