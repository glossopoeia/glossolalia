#lang racket
(require "parser.rkt" "tokenizer.rkt" "roulette-wheel.rkt")
(require math/distributions br/syntax)

(module+ test
    (require rackunit))

(struct config (seed count word-len-dist) #:transparent)
(struct sound (ortho group) #:transparent)

(define (read-syntax path port)
    (define parse-tree (parse path (make-tokenizer port path)))
    `(module trinity-mod racket ,(datum->syntax #f (interpret (syntax->datum parse-tree)))))

(module+ reader
    (provide read-syntax))

(define (interpret stx)
    (match stx
        [(list 't-file cats structs rules gen)
         (define categories (get-categories cats))
         (define structures (get-structures structs))
         (define rule-funcs (make-rules rules))
         (define config (get-config gen))
         (generate config structures categories rule-funcs)
         #t]))

;; get-categories : Syntax -> Hash GroupName (Roulette Ortho)
(define (get-categories stx)
    (define (get-unspecified split)
        (cond
            [(and (> (length split) 1) (string? (first (first split))))
             (first split)]
            [(> (length split) 1)
             (second split)]
            [(string? (first (first split)))
             (first split)]
            [else empty]))
    
    (define (get-specified split)
        (cond
            [(and (> (length split) 1) (string? (first (first split))))
             (second split)]
            [(> (length split) 1)
             (first split)]
            [(string? (first (first split)))
             empty]
            [else (first split)]))

    ;; get-category : Syntax -> (GroupName, Roulette Ortho)
    (define (get-category stx)
        (match stx
            [(list 't-category group-name sounds ...)
             (define split (group-by pair? (map get-category-sound sounds)))
             (define specified (get-specified split))
             (define unspecified (get-unspecified split))
             (cons group-name (make-partial-roulette specified unspecified))]))
    
    ;; get-category-sound : Syntax -> Ortho | (Ortho, Decimal)
    (define (get-category-sound stx)
        (match stx
            [(list 't-cat-sound sound-name)
             sound-name]
            [(list 't-cat-sound sound-name freq)
             (cons sound-name freq)]))

    (match stx
        [(list 't-categories cats ...)
         (make-immutable-hash (map get-category cats))]))

;; get-structures : Syntax -> Roulette Structure
(define (get-structures stx)
    (define (get-unspecified split)
        (cond
            [(and (> (length split) 1) (list? (first (first split))))
             (first split)]
            [(> (length split) 1)
             (second split)]
            [(list? (first (first split)))
             (first split)]
            [else empty]))
    
    (define (get-specified split)
        (cond
            [(and (> (length split) 1) (list? (first (first split))))
             (second split)]
            [(> (length split) 1)
             (first split)]
            [(list? (first (first split)))
             empty]
            [else (first split)]))

    ;; get-structure : Syntax -> List Structure | (List Structure, Decimal)
    (define (get-structure stx)
        (match stx
            [(list 't-structure groups ...)
             groups]
            [(list 't-structure-perc groups ... freq)
             (cons groups freq)]))
    
    (match stx
        [(list 't-structures structs ...)
         (define split (group-by list? (map get-structure structs)))
         (define specified (get-specified split))
         (define unspecified (get-unspecified split))
         (make-partial-roulette specified unspecified)]))

(define (make-rules stx)
    (define rule-templates (hash
        'never-starts-word          never-starts-word
        'never-ends-word            never-ends-word
        'never-followed-by          never-followed-by
        'never-preceded-by          never-preceded-by
        'never-in-same-word-as      never-in-same-word-as
        'never-doubled              never-doubled
        'never-adjacent-to          never-adjacent-to
        'never-in-middle-of-word    never-in-middle-of-word
        'only-starts-word           only-starts-word
        'only-ends-word             only-ends-word
        'only-followed-by         only-followed-by
        'only-preceded-by         only-preceded-by))
    
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

;; get-config : Syntax -> Config
(define (get-config stx)
    (define (verify-valid-distribution shortest longest mode)
        (cond
            [(or (< shortest 1) (< longest 1))
             (error 'verify-valid-distribution "Both 'Longest' and 'Shortest' must have value >= 1")]
            [(< longest shortest)
             (error 'verify-valid-distribution "Longest must be greater than or equal to Shortest")]
            [(< longest mode)
             (error 'verify-valid-distribution "Longest must be greater than or equal to Mode")]
            [(< mode shortest)
             (error 'verify-valid-distribution "Mode must be greater than or equal to Shortest")]
            [else (void)]))
    
    (define (get-with-default sym default items)
        (for/fold ([res default])
                  ([stx (in-list items)]
                   #:when (symbol=? (first stx) sym))
            (second stx)))

    (define (get-seed items) (get-with-default 't-seed 9001 items))
    (define (get-count items) (get-with-default 't-count 100 items))
    (define (get-word-len-dist items)
        (define shortest (get-with-default 't-shortest 1 items))
        (define longest (add1 (get-with-default 't-longest 5 items)))
        (define mode (get-with-default 't-mode (* 0.5 (+ longest shortest)) items))
        (verify-valid-distribution shortest longest mode)
        (triangle-dist shortest longest mode))

    (match stx
        [(list 't-generate items ...)
         (config (get-seed items) (get-count items) (get-word-len-dist items))]))

;; generate : Config, Roulette Structure, Hash GroupName (Roulette Ortho), List Rule -> Void
(define (generate config structs freqs rules)
    (random-seed (config-seed config))

    (define words (generate-words config structs freqs rules))

    ;(displayln (map sound-word->string-word words))
    (define out (open-output-file "./generated.txt" #:exists 'replace))
    (for ([l (in-list words)])
        (displayln (sound-word->string-word l) out))
    (close-output-port out))

;; generate-words : Config, Roulette Structure, Hash GroupName (Roulette Ortho), List Rule -> List (List Sound)
(define (generate-words config structs freqs rules)
    (define syllable-dist (config-word-len-dist config))
    (for/fold ([words (list)])
              ([i (in-range (config-count config))])
              (append words (list (generate-word-under-rules words syllable-dist structs freqs rules)))))

;; generate-word-under-rules : List (List Sound), Distribution, Roulette Structure, Hash GroupName (Roulette Ortho), List Rule -> List Sound
(define (generate-word-under-rules existing syllable-dist structs freqs rules)
    (define maybe (generate-word syllable-dist structs freqs))
    (if (and (obey-rules rules maybe) (not (member maybe existing)))
        maybe
        (generate-word-under-rules existing syllable-dist structs freqs rules)))

;; generate-word : Distribution, Roulette Structure, Hash GroupName (Roulette Ortho) -> List Sound
(define (generate-word syllable-dist structs freqs)
    (define word-len (floor (sample syllable-dist)))
    (append*
        (for/list ([i (in-range word-len)])
            (for/list ([p (in-list (sample-roulette structs))])
                (sound (sample-roulette (hash-ref freqs p)) p)))))

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

;; sound-has-ortho? : List Sound, Ortho -> Bool
(define (sound-has-ortho? sound ortho)
    (string=? (sound-ortho sound) ortho))

;; sound-has-group? : List Sound, GroupName -> Bool
(define (sound-has-group? sound group)
    (symbol=? (sound-group sound) group))

;; sound-has-prop? : List Sound, Ortho | GroupName -> Bool
(define (sound-has-prop? sound orth-or-group)
    (if (symbol? orth-or-group)
        (sound-has-group? sound orth-or-group)
        (sound-has-ortho? sound orth-or-group)))

;; word-has-ortho? : List Sound, Ortho -> Bool
(define (word-has-ortho? word ortho)
    (memf (lambda (s) (string=? (sound-ortho s) ortho)) word))

;; word-has-group? : List Sound, GroupName -> Bool
(define (word-has-group? word group)
    (memf (lambda (s) (symbol=? (sound-group s) group)) word))

;; word-has-prop? : List Sound, Ortho | GroupName -> Bool
(define (word-has-prop? word orth-or-group)
    (if (symbol? orth-or-group)
        (word-has-group? word orth-or-group)
        (word-has-ortho? word orth-or-group)))

;; contains-pair? : List Sound, Ortho | GroupName, Ortho | GroupName -> Bool
(define (contains-pair? word l r)
    (cond
        [(< (length word) 2) #f]
        [(and (sound-has-prop? (first word) l) (sound-has-prop? (second word) r)) #t]
        [else (contains-pair? (rest word) l r)]))

;; each-followed-by-one-of? : List Sound, Ortho | GroupName, List (Ortho | GroupName) -> Bool
(define (each-followed-by-one-of? word l rs)
    (cond
        [(< (length word) 2) (not (sound-has-prop? (first word) l))]
        [(sound-has-prop? (first word) l)
         (and (ormap (curry sound-has-prop? (second word)) rs)
              (each-followed-by-one-of? (rest (rest word)) l rs))]
        [else (each-followed-by-one-of? (rest word) l rs)]))

;; each-preceded-by-one-of? : List Sound, Ortho | GroupName, List (Ortho | GroupName) -> Bool
(define (each-preceded-by-one-of? word l rs)
    (cond
        [(< (length word) 2) #t]
        [(sound-has-prop? (second word) l)
         (and (ormap (curry sound-has-prop? (first word)) rs)
              (each-preceded-by-one-of? (rest (rest word)) l rs))]
        [else (each-preceded-by-one-of? (rest word) l rs)]))

(define (never-starts-word args word)
    (for/and ([s (in-list args)])
        (not (sound-has-prop? (first word) s))))

(define (never-ends-word args word)
    (for/and ([s (in-list args)])
        (not (sound-has-prop? (last word) s))))

(define (never-followed-by l-args r-args word)
    (for/and ([l (in-list l-args)])
        (for/and ([r (in-list r-args)])
            (not (contains-pair? word l r)))))

(define (never-preceded-by l-args r-args word)
    (for/and ([l (in-list l-args)])
        (for/and ([r (in-list r-args)])
            (not (contains-pair? word r l)))))

(define (never-in-same-word-as l-args r-args word)
    (for/and ([l (in-list l-args)])
        (define has-left (word-has-prop? word l))
        (for/and ([r (in-list r-args)])
            (nand has-left (word-has-prop? word r)))))

(define (never-doubled args word)
    (for/and ([l (in-list args)])
        (not (contains-pair? word l l))))

(define (never-adjacent-to l-args r-args word)
    (for/and ([l (in-list l-args)])
        (for/and ([r (in-list r-args)])
            (nor (contains-pair? word l r) (contains-pair? word r l)))))

(define (never-in-middle-of-word args word)
    (for/and ([l (in-list args)])
        (not (word-has-prop? (drop-right (rest word) 1) l))))

(define (only-starts-word args word)
    (for/and ([s (in-list args)])
        (not (word-has-prop? (rest word) s))))

(define (only-ends-word args word)
    (for/and ([s (in-list args)])
        (not (word-has-prop? (drop-right word 1) s))))

(define (only-followed-by l-args r-args word)
    (for/and ([l (in-list l-args)])
        (implies
            (word-has-prop? word l)
            (each-followed-by-one-of? word l r-args))))

(define (only-preceded-by l-args r-args word)
    (for/and ([l (in-list l-args)])
        (implies
            (word-has-prop? word l)
            (each-preceded-by-one-of? word l r-args))))

(module+ test
    (define (simple-test-word s)
        (map (lambda (c) (sound (string c) "test")) (string->list s)))

    (check-true (never-starts-word (list "a" "b") (simple-test-word "hello")))
    (check-false (never-starts-word (list "a" "b") (simple-test-word "all")))
    (check-false (never-starts-word (list "a" "b") (simple-test-word "batman")))
    (check-true (never-starts-word (list "a" "b") (list (sound "aa" "uh") (sound "r" "n") (sound "l" "b"))))
    
    (check-true (never-ends-word (list "k") (simple-test-word "salt")))
    (check-false (never-ends-word (list "k") (simple-test-word "pick")))
    (check-true (never-ends-word (list 'con) (list (sound "b" 'con) (sound "a" 'vow))))
    (check-false (never-ends-word (list 'con) (list (sound "a" 'vow) (sound "b" 'con))))
    
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

    (check-true (never-doubled (list "a") (simple-test-word "alalala")))
    (check-false (never-doubled (list "a") (simple-test-word "aardvark")))

    (check-true (never-adjacent-to (list "a") (list "b") (simple-test-word "belay")))
    (check-true (never-adjacent-to (list "b") (list "a") (simple-test-word "belay")))
    (check-false (never-adjacent-to (list "a") (list "b") (simple-test-word "abbey")))
    (check-false (never-adjacent-to (list "a") (list "b") (simple-test-word "baron")))
    (check-false (never-adjacent-to (list "b") (list "a") (simple-test-word "abbey")))
    (check-false (never-adjacent-to (list "b") (list "a") (simple-test-word "baron")))
    (check-true (never-adjacent-to (list "a" "l") (list "b") (simple-test-word "belay")))
    (check-false (never-adjacent-to (list "a" "l") (list "b") (simple-test-word "baron")))
    (check-false (never-adjacent-to (list "a" "l") (list "b") (simple-test-word "blade")))

    (check-true (never-in-middle-of-word (list "a") (simple-test-word "abort")))
    (check-true (never-in-middle-of-word (list "e") (simple-test-word "rake")))
    (check-false (never-in-middle-of-word (list "a") (simple-test-word "barrel")))

    (check-true (only-starts-word (list "k") (simple-test-word "fail")))
    (check-true (only-starts-word (list "k") (simple-test-word "kill")))
    (check-false (only-starts-word (list "k") (simple-test-word "pickle")))
    (check-false (only-starts-word (list "k") (simple-test-word "kek")))
    
    (check-true (only-ends-word (list "k") (simple-test-word "fail")))
    (check-true (only-ends-word (list "k") (simple-test-word "sick")))
    (check-false (only-ends-word (list "k") (simple-test-word "pickle")))
    (check-false (only-ends-word (list "k") (simple-test-word "kek")))
    
    (check-true (only-followed-by (list "a") (list "b") (simple-test-word "right")))
    (check-true (only-followed-by (list "a") (list "b") (simple-test-word "belle")))
    (check-true (only-followed-by (list "a") (list "b") (simple-test-word "abbey")))
    (check-true (only-followed-by (list "a") (list "b" "d") (simple-test-word "adder")))
    (check-false (only-followed-by (list "a") (list "b") (simple-test-word "allow")))
    (check-false (only-followed-by (list "n") (list "e") (simple-test-word "connect")))
    (check-false (only-followed-by (list "t") (list "h") (simple-test-word "cat")))
    
    (check-true (only-preceded-by (list "a") (list "b") (simple-test-word "bat")))
    (check-true (only-preceded-by (list "a") (list "b") (simple-test-word "bed")))
    (check-false (only-preceded-by (list "a") (list "b") (simple-test-word "cat"))))