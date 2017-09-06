#lang info
(define collection "trinity")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/trinity.scrbl" ())))
(define pkg-desc "A language for generating words in constructed languages")
(define version "0.0")
(define pkg-authors '(gamec))
