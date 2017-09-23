#lang racket
(require brag/support)

(define (percent-string->number s)
    (define no-percent (substring s 0 (sub1 (string-length s))))
    (/ (if (string-contains? no-percent ".")
           (string->number no-percent)
           (string->number (string-append no-percent ".0")))
        100))

(define trinity-lexer
    (lexer-srcloc
        [(eof) (return-without-srcloc eof)]
        [(:or "\n" "\r\n") (token 'NEWLINE lexeme)]
        [whitespace (token 'WS #:skip? #t)]
        [(from/to "--" "\n") (token lexeme #:skip? #t)]
        [(:or "Categories" "Structures" "Rules" "Frequencies" "Generate" "Seed" "Count" "Longest" "Shortest" "Mode" "," "=")
         (token lexeme lexeme)]
        [(:or "never-starts-word" "never-ends-word" "never-in-middle-of-word" "never-doubled" "only-starts-word" "only-ends-word")
         (token 'UNARY-RULE-NAME (string->symbol lexeme))]
        [(:or "never-followed-by" "never-preceded-by" "never-adjacent-to" "never-in-same-word-as" "only-followed-by" "only-preceded-by")
         (token 'BINARY-RULE-NAME (string->symbol lexeme))]
        [(:seq numeric (:? numeric) (:? (:seq "." (:* numeric))) "%")
         (token 'PERCENTAGE (percent-string->number lexeme))]
        [(:+ numeric)
         (token 'INTEGER (string->number lexeme))]
        [(:+ (:or "'" alphabetic))
         (token 'SOUND-NAME lexeme)]
        [(:seq "@" (:+ (:or alphabetic "-")))
         (token 'GROUP-NAME (string->symbol lexeme))]
        [(:seq "$" (:+ (:or alphabetic "-")))
         (token 'STRUCTURE-NAME (string->symbol lexeme))]))

(provide trinity-lexer)