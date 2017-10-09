#lang racket
(require "parser.rkt" "tokenizer.rkt")
(require br/syntax)

(define (read-syntax path port)
    (define parse-tree (syntax->datum (parse path (make-tokenizer port path))))
    (datum->syntax #f `(module glossolalia-mod glossolalia/generator ,parse-tree)))

(define (get-info port mod line col pos)
    (define (handle-query key default)
        (case key
            [(color-lexer)
             (dynamic-require 'glossolalia/colorer 'glossolalia-colorer)]
            [else default]))
    handle-query)

(module+ reader
    (provide read-syntax get-info))