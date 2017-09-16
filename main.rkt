#lang racket
(require "parser.rkt" "tokenizer.rkt" "roulette-wheel.rkt")
(require br/syntax)

(module+ test
    (require rackunit))

(struct config (seed count longest) #:transparent)
(struct sound (ortho group) #:transparent)

(define (read-syntax path port)
    (define parse-tree (parse path (make-tokenizer port path)))
    `(module trinity-mod racket ,(datum->syntax #f (interpret (syntax->datum parse-tree)))))

(module+ reader
    (provide read-syntax))

(define (interpret stx)
    (match stx
        [(list 't-file cats structs rules freqs gen)
         (define categories (get-categories cats))
         (define structures (get-structures structs))
         (define rule-funcs (make-rules rules))
         (define wheels (make-frequencies freqs categories))
         (define config (get-config gen))
         (generate config structures wheels rule-funcs)
         #t]))

(define (get-categories stx)
    (define (get-category stx)
        (match stx
            [(list 't-category group-name sounds ...)
             (cons group-name sounds)]))

    (match stx
        [(list 't-categories cats ...)
         (make-immutable-hash (map get-category cats))]))

(module+ test
    (check-equal?
        (get-categories '(t-categories (t-category "@hi" "a" "b" "c")))
        (make-immutable-hash (list (cons "@hi" (list "a" "b" "c")))))
    (check-equal?
        (get-categories '(t-categories (t-category "@hi" "a") (t-category "@bye" "b")))
        (make-immutable-hash (list (cons "@hi" (list "a")) (cons "@bye" (list "b"))))))

(define (get-structures stx)
    (define (get-structure stx)
        (match stx
            [(list 't-structure groups ...)
             groups]))
    
    (match stx
        [(list 't-structures structs ...)
         (map get-structure structs)]))

(define (make-rules stx)
    (define rule-templates (hash
        'never-starts-word      never-starts-word
        'never-ends-word        never-ends-word
        'only-starts-word       only-starts-word
        'only-ends-word         only-ends-word
        'never-followed-by      never-followed-by
        'never-preceded-by      never-preceded-by
        'never-in-same-word-as  never-in-same-word-as
        'always-followed-by     always-followed-by
        'always-preceded-by     always-preceded-by))
    
    (define (get-rule-args stx)
        (match stx
            [(list 't-rule-args args ...)
             args]))

    (define (make-rule stx)
        (match stx
            [(list 't-unary-rule args name)
             (curry (hash-ref rule-templates name)
                    (get-rule-args args))]
            [(list 't-binary-rule l-args name r-args)
             (curry (hash-ref rule-templates name)
                    (get-rule-args l-args)
                    (get-rule-args r-args))]))

    (match stx
        [(list 't-rules rules ...)
         (map make-rule rules)]))

;; make-frequencies : Syntax, Hash GroupName (List Ortho) -> Hash GroupName (Roulette Ortho)
(define (make-frequencies stx cats)
    ;; get-frequencies : Syntax -> Hash GroupName (List (Ortho, Decimal))
    (define (get-frequencies stx)
        (match stx
            [(list 't-frequencies group-freqs ...)
             (make-immutable-hash (map get-group-frequencies group-freqs))]))
    
    ;; get-group-frequencies : Syntax -> (GroupName, List (Ortho, Decimal))
    (define (get-group-frequencies stx)
        (match stx
            [(list 't-group-frequency group-name sound-freqs ...)
             (cons group-name (append* (map get-sound-frequency sound-freqs)))]))
    
    ;; get-sound-frequencies : Syntax -> List (Ortho, Decimal)
    (define (get-sound-frequency stx)
        (match stx
            [(list 't-sound-frequency orthos ... freq)
             (map (curryr cons freq) orthos)]))
    
    (define specified (get-frequencies stx))
    (make-immutable-hash
        (for/list ([(k v) (in-hash cats)])
            (cons k
                (if (hash-has-key? specified k)
                    (let* ([part (hash-ref specified k)]
                           [part-orthos (map car part)]
                           [remain (filter (lambda (x) (not (member x part-orthos))) v)])
                        (make-partial-roulette part remain))
                    (make-partial-roulette empty v))))))

;; get-config : Syntax -> Config
(define (get-config stx)
    (match stx
        [(list 't-generate seed count longest)
         (if (< longest 1)
             (error 'get-config "'Longest' field must have value > 1")
             (config seed count longest))]))

(define (generate config structs freqs rules)
    (random-seed (config-seed config))
    (define count (config-count config))
    (define words (list))
    (let loop ()
        (define new (generate-words config structs freqs))
        (set! words (append words (filter (curry obey-rules rules) new)))
        (when (< (length words) count) (loop)))
    (set! words (take words count))
    (displayln (map sound-word->string-word words))
    (define out (open-output-file "./generated.txt" #:exists 'replace))
    (for ([l (in-list words)])
        (displayln (sound-word->string-word l) out))
    (close-output-port out))

;; generate-words : Config, List Structure, Hash GroupName (Roulette Ortho) -> List (List Sound)
(define (generate-words config structs freqs)
    ;; generate-word : Void -> List Sound
    (define (generate-word)
        (define word-len (random 1 (config-longest config)))
        (define struct-count (length structs))
        (append*
            (for/list ([i (in-range word-len)])
                (define syll (list-ref structs (random struct-count)))
                (for/list ([p (in-list syll)])
                    (sound (sample-roulette (hash-ref freqs p)) p)))))

    (for/list ([i (in-range (config-count config))])
        (generate-word)))

;; obey-rules : List (List Sound -> Bool), List Sound -> Bool
(define (obey-rules rules word)
    (for/and ([r (in-list rules)])
        (r word)))

(module+ test
    (check-true (obey-rules (list) (list)))
    (check-true (obey-rules (list (lambda (x) #t)) (list (sound "a" "h"))))
    (check-false (obey-rules (list (lambda (x) #f)) (list (sound "b" "h")))))

;; sound-word->string-word : List Sound -> String
(define (sound-word->string-word sw)
    (string-append* (map sound-ortho sw)))

(module+ test
    (check-equal? (sound-word->string-word (list)) "")
    (check-equal? (sound-word->string-word (list (sound "a" "h") (sound "t" "e"))) "at"))

;; ================================================
;; RULE TEMPLATES
;; ================================================

;; has-ortho? : List Sound, Ortho -> Bool
(define (has-ortho? word ortho)
    (memf (lambda (s) (string=? (sound-ortho s) ortho)) word))

;; contains-adjacent? : List Sound, Ortho, Ortho -> Bool
(define (contains-adjacent? word l r)
    (cond
        [(< (length word) 2) #f]
        [(and (string=? (sound-ortho (first word)) l) (string=? (sound-ortho (second word)) r)) #t]
        [else (contains-adjacent? (cdr word) l r)]))

(define (never-starts-word args word)
    (for/and ([s (in-list args)])
        (not (string=? (sound-ortho (first word)) s))))

(define (never-ends-word args word)
    (for/and ([s (in-list args)])
        (not (string=? (sound-ortho (last word)) s))))

(define (only-starts-word args word)
    (for/and ([s (in-list args)])
        (if (has-ortho? word s)
            (string=? (sound-ortho (first word)) s)
            #t)))

(define (only-ends-word args word)
    (for/and ([s (in-list args)])
        (if (has-ortho? word s)
            (string=? (sound-ortho (last word)) s)
            #t)))

(define (never-followed-by l-args r-args word)
    (for/and ([l (in-list l-args)])
        (for/and ([r (in-list r-args)])
            (not (contains-adjacent? word l r)))))

(define (never-preceded-by l-args r-args word)
    (for/and ([l (in-list l-args)])
        (for/and ([r (in-list r-args)])
            (not (contains-adjacent? word r l)))))

(define (never-in-same-word-as l-args r-args word)
    (for/and ([l (in-list l-args)])
        (for/and ([r (in-list r-args)])
            (not (and (has-ortho? word l) (has-ortho? word r))))))

(define (always-followed-by l-args r-args word)
    (for/and ([l (in-list l-args)])
        (for/and ([r (in-list r-args)])
            (if (has-ortho? word l)
                (contains-adjacent? word l r)
                #t))))

(define (always-preceded-by l-args r-args word)
    (for/and ([l (in-list l-args)])
        (for/and ([r (in-list r-args)])
            (if (has-ortho? word l)
                (contains-adjacent? word r l)
                #t))))

(module+ test
    (define (simple-test-word s)
        (map (lambda (c) (sound (string c) "test")) (string->list s)))

    (check-true (never-starts-word (list "a" "b") (simple-test-word "hello")))
    (check-false (never-starts-word (list "a" "b") (simple-test-word "all")))
    (check-false (never-starts-word (list "a" "b") (simple-test-word "batman")))
    (check-true (never-starts-word (list "a" "b") (list (sound "aa" "uh") (sound "r" "n") (sound "l" "b"))))
    
    (check-true (never-ends-word (list "k") (simple-test-word "salt")))
    (check-false (never-ends-word (list "k") (simple-test-word "pick")))
    
    (check-true (only-starts-word (list "k") (simple-test-word "fail")))
    (check-true (only-starts-word (list "k") (simple-test-word "kill")))
    (check-false (only-starts-word (list "k") (simple-test-word "pickle")))
    
    (check-true (only-ends-word (list "k") (simple-test-word "fail")))
    (check-true (only-ends-word (list "k") (simple-test-word "sick")))
    (check-false (only-ends-word (list "k") (simple-test-word "pickle")))
    
    (check-true (never-followed-by (list "k") (list "g") (simple-test-word "hodor")))
    (check-true (never-followed-by (list "k") (list "g") (simple-test-word "akngu")))
    (check-false (never-followed-by (list "k") (list "g") (simple-test-word "bakgi")))
    
    (check-true (never-in-same-word-as (list "a") (list "i") (simple-test-word "seli")))
    (check-true (never-in-same-word-as (list "a") (list "i") (simple-test-word "halo")))
    (check-true (never-in-same-word-as (list "a") (list "i") (simple-test-word "mule")))
    (check-false (never-in-same-word-as (list "a") (list "i") (simple-test-word "hail")))

    (check-true (never-preceded-by (list "a") (list "b") (simple-test-word "aloe")))
    (check-true (never-preceded-by (list "a") (list "b") (simple-test-word "aberstwyth")))
    (check-false (never-preceded-by (list "a") (list "b") (simple-test-word "bar")))
    
    (check-true (always-followed-by (list "a") (list "b") (simple-test-word "right")))
    (check-true (always-followed-by (list "a") (list "b") (simple-test-word "belle")))
    (check-true (always-followed-by (list "a") (list "b") (simple-test-word "abbey")))
    (check-false (always-followed-by (list "a") (list "b") (simple-test-word "allow")))
    
    (check-true (always-preceded-by (list "a") (list "b") (simple-test-word "bat")))
    (check-true (always-preceded-by (list "a") (list "b") (simple-test-word "bed")))
    (check-false (always-preceded-by (list "a") (list "b") (simple-test-word "cat"))))