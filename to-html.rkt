#lang racket
(require brag/support "tokenizer.rkt")
(require br/syntax br/define)

(define (read-syntax path port)
  (define tokens (apply-tokenizer make-tokenizer port))
  (datum->syntax #f
    `(module glossolalia-tokens-mod glossolalia/to-html ,tokens)))
(module+ reader (provide read-syntax))

(define-macro (to-html-mb TOKEN ...)
  #'(#%module-begin
     (define code (string-append* "#lang glossolalia" (map htmlify (quote TOKEN ...))))
     (define out (open-output-file "./rendered.html" #:exists 'replace))
     (displayln code out)
     (close-output-port out)))
(provide (rename-out [to-html-mb #%module-begin]))

(define (htmlify token)
    (match token
        [(? eof-object?) ""]
        [else
         (match-define (srcloc-token (token-struct type val _ _ _ _ _) (srcloc _ _ _ posn span)) token)
         (match type
            ['UNARY-RULE-NAME (keyword (symbol->string val))]
            ['BINARY-RULE-NAME (keyword (symbol->string val))]
            ['TERNARY-RULE-NAME (keyword (symbol->string val))]
            ['TERNARY-INDIRECT (keyword (symbol->string val))]
            ['INTEGER (number->string val)]
            ['PERCENTAGE (string-append (number->string (* 100 val)) "%")]
            ['COMMENT (comment val)]
            ['GROUP-NAME (special (symbol->string val))]
            ['SYLLABLE-NAME (special (symbol->string val))]
            ['Sounds (keyword val)]
            ['Syllables (keyword val)]
            ['Rules (keyword val)]
            ['Configuration (keyword val)]
            ['Seed (keyword val)]
            ['Cound (keyword val)]
            ['Longest (keyword val)]
            ['Shortest (keyword val)]
            ['Mode (keyword val)]
            [else val])]))

(define (keyword str)
    (string-append "<span class=\"keyword\">" str "</span>"))

(define (comment str)
    (string-append "<span class=\"comment\">" str "</span>"))

(define (special str)
    (string-append "<span class=\"special\">" str "</span>"))