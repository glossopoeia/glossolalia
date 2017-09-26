#lang racket
(require "parser.rkt" "tokenizer.rkt")
(require syntax/strip-context)
(require br/define)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port path)))
  (strip-context
   #`(module glossolalia-parser-mod glossolalia/parser-only
       #,parse-tree)))
(module+ reader (provide read-syntax))

(define-macro (parser-only-mb PARSE-TREE)
  #'(#%module-begin
     'PARSE-TREE))
(provide (rename-out [parser-only-mb #%module-begin]))