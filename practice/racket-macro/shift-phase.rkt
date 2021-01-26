#lang racket

; import with no phase shift
(require "a.rkt")
; shift phase by +1
(require (for-syntax "a.rkt"))
; shift phase by -1
(require (for-template "a.rkt"))
; shift phase by +5
(require (for-meta 5 "a.rkt"))
