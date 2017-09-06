#lang racket
(require brag/support)

(define trinity-lexer
    (lexer-srcloc
        [(eof) (return-without-srcloc eof)]
        ["\n" (token 'NEWLINE lexeme)]
        [(from/to "--" "\n") (token lexeme #:skip? #t)]
        [(:+ whitespace) (token lexeme #:skip? #t)]
        [(:or "Categories" "Structures" "Rules" "Frequencies" "Generate" "Seed" "Count" "," "." "=" ":")
         (token lexeme lexeme)]
        [(:or "never-starts-word" "never-ends-word" "only-starts-word" "only-ends-word")
         (token 'UNARY-RULE-NAME lexeme)]
        [(:or "never-followed-by" "never-preceded-by" "never-in-same-word-as" "always-followed-by" "always-preceded-by")
         (token 'BINARY-RULE-NAME lexeme)]
        [(:seq (:+ numeric) (:? (:seq "." (:* numeric))) "%")
         (token 'PERCENTAGE (substring lexeme 0 (sub1 (string-length lexeme))))]
        [(:+ numeric)
         (token 'INTEGER (string->number lexeme))]
        [(:+ lower-case)
         (token 'SOUND-NAME lexeme)]
        [(:seq "@" (:+ alphabetic))
         (token 'GROUP-NAME lexeme)]))

(provide trinity-lexer)