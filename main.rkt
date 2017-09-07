#lang racket/base
(require "parser.rkt" "tokenizer.rkt")
(require br/syntax)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-bindings
   #`(module trinity-mod trinity/expander
       #,parse-tree)))

(module+ reader
  (provide read-syntax))
