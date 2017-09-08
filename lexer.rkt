#lang racket
(require brag/support)

(define trinity-lexer
    (lexer-srcloc
        [(eof) (return-without-srcloc eof)]
        [(:or "\n" "\r\n") (token 'NEWLINE lexeme)]
        [whitespace (token 'WS #:skip? #t)]
        [(from/to "--" "\n") (token lexeme #:skip? #t)]
        [(:or "Categories" "Structures" "Rules" "Frequencies" "Generate" "Seed" "Count" "," "." "=" ":")
         (token lexeme lexeme)]
        [(:or "never-starts-word" "never-ends-word" "only-starts-word" "only-ends-word")
         (token 'UNARY-RULE-NAME (string->symbol lexeme))]
        [(:or "never-followed-by" "never-preceded-by" "never-in-same-word-as" "always-followed-by" "always-preceded-by")
         (token 'BINARY-RULE-NAME (string->symbol lexeme))]
        [(:seq (:+ numeric) (:? (:seq "." (:* numeric))) "%")
         (token 'PERCENTAGE (string->number (substring lexeme 0 (sub1 (string-length lexeme)))))]
        [(:+ numeric)
         (token 'INTEGER (string->number lexeme))]
        [(:+ lower-case)
         (token 'SOUND-NAME lexeme)]
        [(:seq "@" (:+ alphabetic))
         (token 'GROUP-NAME lexeme)]))

(provide trinity-lexer)