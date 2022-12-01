#lang racket
(require "parse.rkt"
         "core.rkt")

(module+ main
  (require racket/cmdline)

  (command-line
   #:program "whisper"
   #:args ()
   ; let Nat : U = (N : U) -> (N -> N) -> N -> N;
   (define tm (parse
               #'(let id : ((A : U) -> (A -> A)) = (lam A x => x)
                   in (id U))))
   (define ty (infer-empty tm))
   (printf "type of term:~n~n~a~n~nis~n~n~a~n" tm (readback '() ty))))
