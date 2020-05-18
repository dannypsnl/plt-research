#lang typed/racket

(provide typ
         typ:builtin)

(struct typ [] #:transparent)
(struct typ:builtin typ [(name : String)] #:transparent)
