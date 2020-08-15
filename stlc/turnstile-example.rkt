#lang racket

(require "turnstile.rkt")

((λ ([e : Int]) e) 1)
((λ ([e : String]) e) "hello")
(ann 10 : Int)
