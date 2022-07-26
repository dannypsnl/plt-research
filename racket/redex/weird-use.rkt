#lang racket
(require redex)

(define-language weird-lang
  (M maintainer1
     maintainer2)
  (E editor1))

(define weird-m-red
  (reduction-relation
   weird-lang
   (--> maintainer1 maintainer2
        "I'm your user")
   (--> maintainer2 maintainer1
        "The positions you're using identifier: xxx")
   (--> editor1 maintainer2
        "renaming")))

(traces weird-m-red
        (term editor1))
