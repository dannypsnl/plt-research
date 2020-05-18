#lang typed/racket

(provide typ
         typ:builtin)

(struct typ [])
(struct typ:builtin typ [(name : String)])
