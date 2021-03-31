#lang racket

(provide #%app)

(require (rename-in racket [#%app origin-app])
         syntax/parse/define
         (for-syntax "env.rkt"))

(define-syntax-parser #%app
  [(_ f e* ...)
   (check-app #'(f e* ...))
   #'(origin-app f e* ...)])
