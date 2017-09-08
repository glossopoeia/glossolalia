#lang racket
(require "parser.rkt" "tokenizer.rkt" "roulette-wheel.rkt")
(require br/syntax)

(define (read-syntax path port)
    (define parse-tree (parse path (make-tokenizer port path)))
    (strip-bindings
        #`(module trinity-mod trinity/expander
            #,parse-tree)))

(define (interpret stx)
    (match stx
        [(list 't-file cats structs rules freqs gen)
         (define categories (get-categories cats))
         (define structures (get-structures structs))
         (define rules (make-rules rules))
         (define freqs (make-frequencies freqs categories))
         (define-values (seed count) (get-config gen))]))

(module+ reader
    (provide read-syntax))
