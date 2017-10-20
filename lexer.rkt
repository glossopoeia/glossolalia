#lang racket
(require brag/support)

(define (percent-string->number s)
    (define no-percent (substring s 0 (sub1 (string-length s))))
    (/ (if (string-contains? no-percent ".")
           (string->number no-percent)
           (string->number (string-append no-percent ".0")))
        100))

(define glossolalia-lexer
    (lexer-srcloc
        [(eof) (return-without-srcloc eof)]
        [whitespace (token 'WS lexeme #:skip? #t)]
        [(from/to "--" "\n") (token 'COMMENT lexeme #:skip? #t)]
        [(:or "Sounds" "Syllables" "Rules" "Configuration" "Seed" "Count" "Longest" "Shortest" "Mode" "Output" "," "=" ":" "|")
         (token lexeme lexeme)]
        [(:or "never-starts-word" "never-ends-word" "never-in-middle-of-word" "never-doubled" "only-starts-word" "only-ends-word")
         (token 'UNARY-RULE-NAME (string->symbol lexeme))]
        [(:or "never-followed-by" "never-preceded-by" "never-adjacent-to" "never-in-same-word-as" "only-followed-by" "only-preceded-by")
         (token 'BINARY-RULE-NAME (string->symbol lexeme))]
        [(:or "becomes" "prepends" "appends")
         (token 'TERNARY-RULE-NAME (string->symbol lexeme))]
        [(:or "before" "after")
         (token 'TERNARY-INDIRECT (string->symbol lexeme))]
        [(:seq numeric (:? numeric) (:? (:seq "." (:* numeric))) "%")
         (token 'PERCENTAGE (percent-string->number lexeme))]
        [(:+ numeric)
         (token 'INTEGER (string->number lexeme))]
        [(:+ (:or "'" alphabetic))
         (token 'SOUND-NAME lexeme)]
        [(:seq "@" (:+ (:or alphabetic "-")))
         (token 'GROUP-NAME (string->symbol lexeme))]
        [(:seq "$" (:+ (:or alphabetic "-")))
         (token 'SYLLABLE-NAME (string->symbol lexeme))]))

(provide glossolalia-lexer)