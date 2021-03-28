#lang racket

(require profile "safe-driver.rkt")
(profile-thunk (thunk (calculate (input-strings) (make-empty-state))))