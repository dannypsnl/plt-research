#lang info
(define collection "whisper")
(define deps '("base"
               "reporter"))
(define build-deps '("rackunit-lib"
                     "scribble-lib" "racket-doc"
                     ; coverage
                     "cover" "cover-badge"))
(define pkg-desc "Description Here")
(define version "0.0")
(define license '(Apache-2.0 OR MIT))
(define pkg-authors '(dannypsnl))
