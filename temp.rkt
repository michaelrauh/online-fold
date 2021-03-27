#lang racket
(require suffixtree)
(define tree (make-tree))
(tree-add! tree (vector->label/with-sentinel (list->vector (list "a" "b" "c"))))
(tree-contains? tree (vector->label (list->vector (list "a"))))

(require pfds/deque/bankers)

(last (enqueue 2 (enqueue 1 (deque))))
(deque->list (enqueue 2 (enqueue 1 (deque))))