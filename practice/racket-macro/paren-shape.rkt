#lang racket

(require syntax/parse
         syntax/parse/class/paren-shape)

(syntax-parse #'(1 2 . "three")
  [[~parens a ... . rst]
   (cons #'(a ...) #'rst)])
(syntax-parse #'[1 2 . "three"]
  [[~brackets a ... . rst]
   (cons #'(a ...) #'rst)])
(syntax-parse #'{1 2 . "three"}
  [[~braces a ... . rst]
   (cons #'(a ...) #'rst)])

;;; case: error
#;(syntax-parse #'(1 2 . "three")
  [[~brackets a ... . rst]
   (cons #'(a ...) #'rst)])
#;(syntax-parse #'{1 2 . "three"}
  [(~parens a ... . rst)
   (cons #'(a ...) #'rst)])
