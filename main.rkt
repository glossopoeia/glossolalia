#lang racket
(require "parser.rkt" "tokenizer.rkt" "roulette-wheel.rkt")
(require math/distributions br/syntax)

(define (read-syntax path port)
    (define parse-tree (syntax->datum (parse path (make-tokenizer port path))))
    (datum->syntax #f `(module glossolalia-mod glossolalia/generator ,parse-tree)))

(module+ reader
    (provide read-syntax))