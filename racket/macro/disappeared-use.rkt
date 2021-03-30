;;; modify from https://github.com/yjqww6/macrology/blob/master/draw-arrow.md
#lang racket

(require syntax/parse/define)

(define-syntax foo (syntax-rules ()))

(define-syntax-parser bar #:track-literals
  [(_ (~literal foo)) #''ok])

(bar foo)

(define-syntax (arrow-art stx)
  (syntax-case stx ()
    [(_ id ...)
     (syntax-property
      (syntax-property
       #'(void)
       'disappeared-use
       (map syntax-local-introduce (syntax->list #'(id ...))))
      'disappeared-binding
      (map syntax-local-introduce (syntax->list #'(id ...))))]))

(arrow-art a     a
          
              a)

(arrow-art b     b

              b


              b)
