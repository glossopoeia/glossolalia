#lang racket

(require "lexer.rkt" brag/support)
#|
(provide glossolalia-colorer)



(define (glossolalia-colorer port)
    (define (handle-lexer-error excn)
        (define excn-srclocs (exn:fail:read-srclocs excn))
        (srcloc-token (token 'ERROR) (car excn-srclocs)))
    (define srcloc-tok
        (with-handlers ([exn:fail:read? handle-lexer-error])
            (glossolalia-lexer port)))
    (match srcloc-tok
        [(? eof-object?) (values srcloc-tok 'eof #f #f #f)]
        [else
            (match-define
                (srcloc-token
                    (token-struct type val _ _ _ _ _)
                    (srcloc _ _ _ posn span)) srcloc-tok)
            (define start posn)
            (define end (+ start span))
            (define cat
                (match type
                    ['UNARY-RULE-NAME 'string]
                    ['BINARY-RULE-NAME 'string]
                    ['TERNARY-RULE-NAME 'string]
                    ['TERNARY-INDIRECT 'string]
                    ['ERROR 'error]
                    ['INTEGER 'constant]
                    ['PERCENTAGE 'constant]
                    ['COMMENT 'comment]
                    ['GROUP-NAME 'hash-colon-keyword]
                    ['SYLLABLE-NAME 'hash-colon-keyword]
                    [else 'no-color]
            (values val cat #f start end)])))
|#