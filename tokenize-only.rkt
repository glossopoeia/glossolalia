#lang racket
(require brag/support "tokenizer.rkt")
(require br/syntax br/define)

(define (read-syntax path port)
  (define tokens (apply-tokenizer make-tokenizer port))
  (strip-bindings
   #`(module glossolalia-tokens-mod glossolalia/tokenize-only
       #,@tokens)))
(module+ reader (provide read-syntax))

(define-macro (tokenize-only-mb TOKEN ...)
  #'(#%module-begin
     (list TOKEN ...)))
(provide (rename-out [tokenize-only-mb #%module-begin]))