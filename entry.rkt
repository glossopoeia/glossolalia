#lang racket

(require racket/cmdline)
(require "tokenizer.rkt" "parser.rkt" "generator.rkt")

(define input-file-path
    (command-line
        #:program "glossolalia"
        #:args (filename)
        filename))
(define input-file
    (open-input-file input-file-path))

(define stx (syntax->datum (parse input-file-path (make-tokenizer input-file input-file-path))))
(interpret stx)