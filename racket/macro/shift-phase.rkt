#lang racket/base

; import with no phase shift
(require racket/match)
; shift phase by +1
(require (for-syntax racket/match))
; shift phase by -1
(require (for-template racket/match))
; shift phase by +5
(require (for-meta 5 racket/match))
