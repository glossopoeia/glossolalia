#lang racket

(require "roulette-wheel.rkt")
(require math/distributions br/syntax br/define)

(module+ test
    (require rackunit))

(define-macro (g-module-begin PARSE-TREE)
  #'(#%module-begin
     (interpret (quote PARSE-TREE))))
(provide (rename-out [g-module-begin #%module-begin]))

;; type Config = Integer, Integer, TriangleDistribution, String
(struct config (seed count word-len-dist outfile) #:transparent)

;; type Ortho = String
;; type GroupName = Symbol
;; type Sound = Ortho, GroupName
(struct sound (ortho group) #:transparent)

;; type SyllableName = Symbol
;; type Syllable = SyllableName, List Sound
(struct syllable (name sounds) #:transparent)

(provide interpret)

;; interpret : Syntax -> Void
;; Entry point for the word generator, creates the sampling data structures
;; from the abstract syntax and generates the list of words.
(define (interpret stx)
    (match stx
        [(list 't-file cats sylls rules gen)
         (define categories (get-categories cats))
         (define syllable-defs (get-syllables sylls (hash-keys categories)))
         (define rule-funcs (make-rules rules (hash-keys categories) (map car (hash-keys syllable-defs))))
         (define config (get-config gen))
         (displayln "Generating...")
         (flush-output)
         (generate config syllable-defs categories rule-funcs)]))

;; get-categories : Syntax -> Hash GroupName (Roulette Ortho)
;; Converts the category abstract syntax into a map from group names to roulette wheels of sound names.
(define (get-categories stx)
    ;; get-category : Syntax -> (GroupName, Roulette Ortho)
    (define (get-category stx)
        (match stx
            [(list 't-category group-name sounds ...)
             (cons group-name (make-partial-roulette-mixed (map get-category-sound sounds)))]))
    
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

;; get-syllables : Syntax, List GroupName -> Roulette (SyllableName, Roulette (List GroupName))
;; Convert the syllable abstract syntax into a roulette of roulettes for selecting a syllable group,
;; then selecting a particular variant from within that group.
(define (get-syllables stx cat-names)
    ;; check-undefined : List GroupName -> Void
    (define (check-undefined syll-name groups)
        (for ([name (in-list groups)]
              #:when (not (member name cat-names)))
            (error 'get-syllables (string-append
                                    "undefined group name '"
                                    (symbol->string name)
                                    "' found in syllable definition '"
                                    (symbol->string syll-name) "'"))))

    ;; get-syllable : Syntax -> (SyllableName, Roulette (List GroupName)) | ((SyllableName, Roulette (List GroupName)), Decimal)
    (define (get-syllable stx)
        (match stx
            [(list 't-syllable syll-name variants ...)
             (cons syll-name (make-partial-roulette-mixed (map (curry get-variant syll-name) variants)))]
            [(list 't-syllable-perc syll-name freq variants ...)
             (cons
                (cons syll-name (make-partial-roulette-mixed (map (curry get-variant syll-name) variants)))
                freq)]))
    
    ;; get-variant : Syntax -> List GroupName | (List GroupName, Decimal)
    (define (get-variant syll-name stx)
        (match stx
            [(list 't-syll-con groups ...)
             (check-undefined syll-name groups)
             groups]
            [(list 't-syll-con-perc groups ... freq)
             (check-undefined syll-name groups)
             (cons groups freq)]))
    
    (match stx
        [(list 't-syllables sylls ...)
         (make-partial-roulette-mixed (map get-syllable sylls))]))

;; make-rules : Syntax -> (List Sound -> Bool, List Sound -> List Sound)
;; Converts the rule abstract syntax into a list of filtering trules and a list of transformation rules.
(define (make-rules stx cat-names syll-names)
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
        'only-followed-by           only-followed-by
        'only-preceded-by           only-preceded-by
        'becomes-before             becomes-before
        'becomes-after              becomes-after
        'prepends-before            prepends-before
        'prepends-after             prepends-after
        'appends-before             appends-before
        'appends-after              appends-after))
    
    (define (warn-undefined args defs)
        (for ([a (in-list args)]
              #:when (and (symbol? a) (not (member a defs))))
            (displayln (string-append "Warning: "
                                      (symbol->string a)
                                      " is not defined, did you mean something else?"))))
    
    ;; get-rule-args : Syntax -> List (GroupName | Ortho) | List SyllableName
    (define (get-rule-args stx)
        (match stx
            [(list 't-rule-args args ...)
             (warn-undefined args cat-names)
             args]
            [(list 't-srule-args args ...)
             (warn-undefined args syll-names)
             args]))
    
    (define (filter-rule r) (cons #t r))
    (define (transform-rule r) (cons #f r))

    ;; make-rule : Syntax -> (Bool, (List Syllable -> Bool) | (List Sound -> List Sound))
    ;; If the first part of the pair is true, the rule is a filter.
    ;; If the first part of the pair is false, the rule is a transformer.
    (define (make-rule stx)
        (match stx
            [(list 't-unary-rule args name)
             (filter-rule
                (make-word-rule
                    (curry (hash-ref rule-templates name)
                           (get-rule-args args))))]
            [(list 't-binary-rule l-args name r-args)
             (filter-rule
                (make-word-rule
                    (curry (hash-ref rule-templates name)
                           (get-rule-args l-args)
                           (get-rule-args r-args))))]
            [(list 't-ternary-rule l-args name m-args ... ind r-args)
             (define rule-name (string->symbol (string-append (symbol->string name) "-" (symbol->string ind))))
             (transform-rule
                (curry (hash-ref rule-templates rule-name)
                       (get-rule-args l-args)
                       (map no-group-sound m-args)
                       (get-rule-args r-args)))]
            [(list 't-unary-srule args name)
             (filter-rule
                (curry (hash-ref rule-templates name)
                       (get-rule-args args)))]
            [(list 't-binary-srule l-args name r-args)
             (filter-rule
                (curry (hash-ref rule-templates name)
                       (get-rule-args l-args)
                       (get-rule-args r-args)))]))

    (match stx
        [(list 't-rules rules ...)
         ;; this is essentially 'split-by is-filter? rules', but it's not a default racket function...
         (define-values (filters transformers)
            (for/fold ([filters empty] [transformers empty])
                      ([item (in-list (map make-rule rules))])
                (if (car item)
                    (values (cons (cdr item) filters) transformers)
                    (values filters (cons (cdr item) transformers)))))
         (cons filters (reverse transformers))]))

;; get-config : Syntax -> Config
;; Converts the configuartion abstract syntax into a configuartion structure.
(define (get-config stx)
    (define default-seed 9001)
    (define default-count 100)
    (define default-shortest 1)
    (define default-longest 5)
    (define default-output "generated")

    (define (verify-valid-distribution shortest longest mode)
        (cond
            [(or (< shortest 1))
             (error 'verify-valid-distribution "'Shortest' must have value >= 1")]
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

    (define (get-seed items) (get-with-default 't-seed default-seed items))
    (define (get-count items) (get-with-default 't-count default-count items))
    (define (get-word-len-dist items)
        (define shortest (get-with-default 't-shortest default-shortest items))
        (define longest (add1 (get-with-default 't-longest default-longest items)))
        (define mode (get-with-default 't-mode (* 0.5 (+ longest shortest)) items))
        (verify-valid-distribution shortest longest mode)
        (triangle-dist shortest longest mode))
    (define (get-outfile items) (string-append (get-with-default 't-output default-output items) ".txt"))

    (match stx
        [(list 't-generate items ...)
         (config (get-seed items) (get-count items) (get-word-len-dist items) (get-outfile items))]))

;; generate : Config, Roulette (SyllableName, Roulette (List GroupName)), Hash GroupName (Roulette Ortho), (List Filter, List Transformer) -> Void
;; Generates the list of words and saves them to a file.
(define (generate config sylls freqs rules)
    (when (string-contains? (config-outfile config) "'")
        (error 'generate "Output filename cannot contain an apostrophe"))

    (random-seed (config-seed config))

    (define filters (car rules))      ;; List (List Syllable -> Bool)
    (define transformers (cdr rules)) ;; List (List Sound -> List Sound)
    (define syllable-dist (config-word-len-dist config)) ;; TriangleDistribution
    (define max-gas 5000) ;; Integer

    ;; generate-words : Void -> List (List Sound)
    ;; Generates a random list of words.
    (define (generate-words)
        (for/fold ([words (list)])
                  ([i (in-range (config-count config))])
                  (append words (list (generate-word-under-rules words max-gas)))))

    ;; generate-word-under-rules : List (List Sound), Integer -> List Sound
    ;; Generates a word obeying the filter rules and applied under the transformation rules. If no valid
    ;; word is found after 'max-gas' attempts at generating a new word, the program quits with an error.
    ;; This helps keep the program terminating.
    (define (generate-word-under-rules existing gas-left)
        (if (<= gas-left 0)
            (error 'generate-word "Ran out of gas while trying to generate a word, too many failed attempts!")
            (let ([maybe (generate-word)])
                (if (obey-rules filters maybe)
                    (let ([transformed (apply-transformers transformers (syllable-word->sound-word maybe))])
                        (if (not (member transformed existing))
                            transformed
                            (generate-word-under-rules existing (sub1 gas-left))))
                    (generate-word-under-rules existing (sub1 gas-left))))))
    
    ;; generate-word : Void -> List Syllable
    (define (generate-word)
        ;; generate-sounds : List GroupName -> List Sound
        (define (generate-sounds gs)
            (for/list ([p (in-list gs)])
                (sound (sample-roulette (hash-ref freqs p)) p)))

        ;; generate-syllable : Void -> Syllable
        (define (generate-syllable)
            (define syll (sample-roulette sylls))
            (syllable (car syll) (generate-sounds (sample-roulette (cdr syll)))))

        (define word-len (floor (sample syllable-dist)))
        (for/list ([i (in-range word-len)])
            (generate-syllable)))

    (define words (generate-words))
    (define string-words (map sound-word->string-word words))

    ;(displayln (map sound-word->string-word words))
    (define out (open-output-file (config-outfile config) #:exists 'replace #:mode 'text))
    (for ([l (in-list string-words)])
        (displayln l out))
    (close-output-port out)
    
    (displayln (string-append "Generated words successfully, check '" (path->string (current-directory)) (config-outfile config) "' for the results."))
    (void))

;; obey-rules : List (List Sound -> Bool), List Sound -> Bool
(define (obey-rules rules word)
    (for/and ([r (in-list rules)])
        (r word)))

(module+ test
    (check-true (obey-rules (list) (list)))
    (check-true (obey-rules (list (lambda (x) #t)) (list (sound "a" "h"))))
    (check-false (obey-rules (list (lambda (x) #t) (lambda (x) #f)) (list (sound "b" "h")))))

;; apply-transformers : List (List Sound -> List Sound), List Sound -> List Sound
(define (apply-transformers rules word)
    (for/fold ([new word])
              ([transform (in-list rules)])
        (transform new)))

(module+ test
    (check-equal? (apply-transformers (list (lambda (x) (string-append "a" x))) "bba") "abba")
    (check-equal?
        (apply-transformers
            (list (lambda (x) (string-append "b" x))
                  (lambda (x) (string-append "a" x)))
            "ba")
        "abba"))

;; syllable-word->string-word : List Syllable -> String
(define (syllable-word->string-word sw)
    (sound-word->string-word (syllable-word->sound-word sw)))

;; syllable-word->sound-word : List Syllable -> List Sound
(define (syllable-word->sound-word sw)
    (append* (map syllable-sounds sw)))

;; sound-word->string-word : List Sound -> String
(define (sound-word->string-word sw)
    (string-append* (map sound-ortho sw)))

;; ================================================
;; RULE TEMPLATES
;; ================================================

;; sound-has-ortho? : Sound, Ortho -> Bool
(define (sound-has-ortho? sound ortho)
    (string=? (sound-ortho sound) ortho))

;; sound-has-group? : Sound, GroupName -> Bool
(define (sound-has-group? sound group)
    (symbol=? (sound-group sound) group))

;; sound-has-prop? : Sound, Ortho | GroupName -> Bool
(define (sound-has-prop? sound orth-or-group)
    (if (symbol? orth-or-group)
        (sound-has-group? sound orth-or-group)
        (sound-has-ortho? sound orth-or-group)))

;; syllable-has-prop? : Syllable, SyllableName -> Bool
(define (syllable-has-prop? syll syll-name)
    (symbol=? syll-name (syllable-name syll)))

;; part-has-prop? : Syllable | Sound, SyllableName | Ortho | GroupName -> Bool
(define (part-has-prop? part prop)
    (if (syllable? part)
        (syllable-has-prop? part prop)
        (sound-has-prop? part prop)))

;; sound-word-has-ortho? : List Sound, Ortho -> Bool
(define (sound-word-has-ortho? word ortho)
    (memf (lambda (s) (string=? (sound-ortho s) ortho)) word))

;; sound-word-has-group? : List Sound, GroupName -> Bool
(define (sound-word-has-group? word group)
    (memf (lambda (s) (symbol=? (sound-group s) group)) word))

;; sound-word-has-prop? : List Sound, Ortho | GroupName -> Bool
(define (sound-word-has-prop? word orth-or-group)
    (if (symbol? orth-or-group)
        (sound-word-has-group? word orth-or-group)
        (sound-word-has-ortho? word orth-or-group)))

;; syllable-word-has-prop? : List Syllable, SyllableName -> Bool
(define (syllable-word-has-prop? word syll-name)
    (memf (lambda (s) (symbol=? (syllable-name s) syll-name)) word))

;; word-has-prop? : List Sound | List Syllable, Ortho | GroupName | SyllableName -> Bool
(define (word-has-prop? word prop)
    (cond
        [(empty? word) #f]
        [(syllable? (first word))
         (syllable-word-has-prop? word prop)]
        [else
         (sound-word-has-prop? word prop)]))

;; contains-pair? : List Sound | List Syllable, Ortho | GroupName | SyllableName, Ortho | GroupName | SyllableName -> Bool
(define (contains-pair? word l r)
    (cond
        [(< (length word) 2) #f]
        [(and (part-has-prop? (first word) l) (part-has-prop? (second word) r)) #t]
        [else (contains-pair? (rest word) l r)]))

;; each-followed-by-one-of? : List Sound | List Syllable, Ortho | GroupName | SyllableName, List (Ortho | GroupName) | List SyllableName -> Bool
(define (each-followed-by-one-of? word l rs)
    (cond
        [(< (length word) 1) #t]
        [(< (length word) 2) (not (part-has-prop? (first word) l))]
        [(part-has-prop? (first word) l)
         (and (ormap (curry part-has-prop? (second word)) rs)
              (each-followed-by-one-of? (rest (rest word)) l rs))]
        [else (each-followed-by-one-of? (rest word) l rs)]))

;; each-preceded-by-one-of? : List Sound | List Syllable, Ortho | GroupName | SyllableName, List (Ortho | GroupName) | List SyllableName -> Bool
(define (each-preceded-by-one-of? word l rs)
    (cond
        [(< (length word) 2) #t]
        [(part-has-prop? (second word) l)
         (and (ormap (curry part-has-prop? (first word)) rs)
              (each-preceded-by-one-of? (rest (rest word)) l rs))]
        [else (each-preceded-by-one-of? (rest word) l rs)]))

;; replace-right : List Sound, Ortho | GroupName, Ortho | GroupName, List Sound -> List Sound
(define (replace-right word pl pr newr)
    (cond
        [(< (length word) 2) word]
        [(and (sound-has-prop? (first word) pl) (sound-has-prop? (second word) pr))
         (cons (first word) (append newr (replace-right (rest (rest word)) pl pr newr)))]
        [else (cons (first word) (replace-right (rest word) pl pr newr))]))

;; replace-left : List Sound, Ortho | GroupName, Ortho | GroupName, List Sound -> List Sound
(define (replace-left word pl pr newl)
    (cond
        [(< (length word) 2) word]
        [(and (sound-has-prop? (first word) pl) (sound-has-prop? (second word) pr))
         (append newl (cons (second word) (replace-left (rest (rest word)) pl pr newl)))]
        [else (cons (first word) (replace-left (rest word) pl pr newl))]))

;; insert-left : List Sound, Ortho | GroupName, Ortho | GroupName, List Sound -> List Sound
(define (insert-left word pl pr new)
    (cond
        [(< (length word) 2) word]
        [(and (sound-has-prop? (first word) pl) (sound-has-prop? (second word) pr))
         (append new (list* (first word) (second word) (insert-left (rest (rest word)) pl pr new)))]
        [else (cons (first word) (insert-left (rest word) pl pr new))]))

;; insert-right : List Sound, Ortho | GroupName, Ortho | GroupName, List Sound -> List Sound
(define (insert-right word pl pr new)
    (cond
        [(< (length word) 2) word]
        [(and (sound-has-prop? (first word) pl) (sound-has-prop? (second word) pr))
         (list* (first word) (second word) (append new (insert-right (rest (rest word)) pl pr new)))]
        [else (cons (first word) (insert-right (rest word) pl pr new))]))

;; insert-between : List Sound, Ortho | GroupName, Ortho | GroupName, List Sound -> List Sound
(define (insert-between word pl pr new)
    (cond
        [(< (length word) 2) word]
        [(and (sound-has-prop? (first word) pl) (sound-has-prop? (second word) pr))
         (cons (first word) (append new (cons (second word) (insert-between (rest (rest word)) pl pr new))))]
        [else (cons (first word) (insert-between (rest word) pl pr new))]))

;; make-word-rule : (List Syll -> Bool) -> (List Sound -> Bool)
(define (make-word-rule rule)
    (lambda (w) (rule (syllable-word->sound-word w))))

;; no-group-sound : Ortho -> Sound
(define (no-group-sound ortho) (sound ortho (void)))

(define (never-starts-word args word)
    (for/and ([s (in-list args)])
        (not (part-has-prop? (first word) s))))

(define (never-ends-word args word)
    (for/and ([s (in-list args)])
        (not (part-has-prop? (last word) s))))

(define (never-followed-by l-args r-args word)
    (for*/and ([l (in-list l-args)]
               [r (in-list r-args)])
        (not (contains-pair? word l r))))

(define (never-preceded-by l-args r-args word)
    (for*/and ([l (in-list l-args)]
               [r (in-list r-args)])
        (not (contains-pair? word r l))))

(define (never-in-same-word-as l-args r-args word)
    (nand
        (for/or ([l (in-list l-args)])
            (word-has-prop? word l))
        (for/or ([r (in-list r-args)])
            (word-has-prop? word r))))

(define (never-doubled args word)
    (for/and ([l (in-list args)])
        (not (contains-pair? word l l))))

(define (never-adjacent-to l-args r-args word)
    (for*/and ([l (in-list l-args)]
               [r (in-list r-args)])
        (nor (contains-pair? word l r) (contains-pair? word r l))))

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

(define (becomes-before target-args dest-args pos-args word)
    (for*/fold ([new word])
               ([targ (in-list target-args)]
                [pos (in-list pos-args)])
        (replace-left new targ pos dest-args)))

(define (becomes-after target-args dest-args pos-args word)
    (for*/fold ([new word])
               ([targ (in-list target-args)]
                [pos (in-list pos-args)])
        (replace-right new pos targ dest-args)))

(define (prepends-before target-args dest-args pos-args word)
    (for*/fold ([new word])
               ([targ (in-list target-args)]
                [pos (in-list pos-args)])
        (insert-left new targ pos dest-args)))

(define (prepends-after target-args dest-args pos-args word)
    (for*/fold ([new word])
               ([targ (in-list target-args)]
                [pos (in-list pos-args)])
        (insert-between new pos targ dest-args)))

(define (appends-before target-args dest-args pos-args word)
    (for*/fold ([new word])
               ([targ (in-list target-args)]
                [pos (in-list pos-args)])
        (insert-between new targ pos dest-args)))

(define (appends-after target-args dest-args pos-args word)
    (for*/fold ([new word])
               ([targ (in-list target-args)]
                [pos (in-list pos-args)])
        (insert-right new pos targ dest-args)))

(module+ test
    (define (simple-test-word s)
        (map (lambda (c) (sound (string c) (void))) (string->list s)))

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
    (check-false (never-in-same-word-as (list "a" "o") (list "i" "y") (simple-test-word "boy")))
    (check-false (never-in-same-word-as (list "a" "o" "e") (list "i" "y") (simple-test-word "eby")))

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
    (check-false (only-preceded-by (list "a") (list "b") (simple-test-word "cat")))
    
    (check-equal?
        (becomes-before (list "a") (list (sound "e" (void))) (list "b") (simple-test-word "abazab"))
        (simple-test-word "ebazeb"))
    (check-equal?
        (becomes-after (list "b") (list (sound "c" (void))) (list "a") (simple-test-word "abebab"))
        (simple-test-word "acebac"))
    (check-equal?
        (prepends-before (list "b") (list (sound "a" (void))) (list "d") (simple-test-word "bdecbdelbt"))
        (simple-test-word "abdecabdelbt"))
    (check-equal?
        (prepends-after (list "b") (list (sound "a" (void))) (list "d") (simple-test-word "dbecdbelbt"))
        (simple-test-word "dabecdabelbt"))
    (check-equal?
        (appends-before (list "b") (list (sound "a" (void))) (list "d") (simple-test-word "bdecbdelbt"))
        (simple-test-word "badecbadelbt"))
    (check-equal?
        (appends-after (list "b") (list (sound "a" (void))) (list "d") (simple-test-word "dbecdbelbt"))
        (simple-test-word "dbaecdbaelbt")))