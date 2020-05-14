#lang racket/base

(module+ test
  (require rackunit))

(module+ test
  (check-equal? (+ 2 2) 4))

(module+ main
  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who))))
