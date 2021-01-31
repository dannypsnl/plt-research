#lang racket

(provide (except-out (all-from-out racket)
                     #%app)
         (rename-out [@#%app #%app]))
(require syntax/parse/define)

(define-syntax-parser @#%app
  [(_ f:expr arg*:expr ...)
   (displayln (format "applying ~a" #'f))
   #'(#%app f arg* ...)])
