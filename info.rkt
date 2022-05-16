#lang info
(define collection "glossolalia")
(define deps '("beautiful-racket-macro"
               "base"
               "rackunit-lib"
               "math-lib"
               "brag"
               "beautiful-racket"
               "beautiful-racket-lib"
               "br-parser-tools-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/glossolalia.scrbl" ())))
(define pkg-desc "A language for generating words in constructed languages")
(define version "1.0")
(define pkg-authors '(robkleffner))
