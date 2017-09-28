#lang racket
(require racket/flonum)

(module+ test
    (require rackunit))

(provide
    make-partial-roulette-mixed
    make-partial-roulette
    make-full-roulette
    sample-roulette)

;; type Roulette = Hash Any Decimal

;; make-partial-roulette-mixed : List ((Any, Decimal), (Not (Any, Decimal))) -> Roulette
;; Same as make-partial-roulette, except elements with specified weights are mixed into
;; the same list as those with unspecified weights. Note that this function cannot distinguish
;; between a weighted element, and an unweighted element that is a pair in which the second
;; element is a number: the latter will always be treated as a weighted element.
(define (make-partial-roulette-mixed mixed)
    (define-values (weighted unweighted)
        (for/fold ([weighted empty] [unweighted empty])
                  ([item (in-list mixed)])
            (if (and (pair? item) (number? (cdr item)))
                (values (cons item weighted) unweighted)
                (values weighted (cons item unweighted)))))
    (make-partial-roulette weighted unweighted))

;; make-partial-roulette : List (Any, Decimal), List Any -> Roulette
;; Creates an abstract roulette wheel selection data structure from a list of value-weight pairs
;; and a list of unweight values. At least one of the lists must be non-empty, and if the second
;; list contains elements, the sum of the weights in the first list should be < 1, otherwise the
;; sum of the weights should be <= 1. The unweighted elements are weighted with equal divisions
;; of the weight remaining when the sum of the first list is subtracted from 1, i.e.
;; W(unweighted-element) = (1 - sum-of-weights-of-first-list) / (length-of-second-list)
(define (make-partial-roulette weighted unweighted)
    (define weight-sum (sum-weights weighted))
    (cond
        [(and (empty? weighted) (empty? unweighted))
         (error 'make-partial-roulette "at least one of the input lists must be non-empty")]
        [(and (not (empty? unweighted)) (>= weight-sum 1))
         (error 'make-partial-roulette
                (string-append "if unweighted elements are present, the sum of the weights in the weighted list must be < 1, got "
                               (number->string weight-sum)))]
        [(> (length unweighted) 0)
         (define partial-weight (/ (- 1 weight-sum) (length unweighted)))
         (make-full-roulette
            (append weighted
                    (for/list ([v (in-list unweighted)])
                        (cons v partial-weight))))]
        [else
         (make-full-roulette weighted)]))

;; make-full-roulette : List (Any, Decimal) -> Roulette
;; Creates an abstract roulette wheel selection data structure from a list of value-weight pairs.
;; The list of pairs must be non-empty, and the sum of the weights must be <= 1.
(define (make-full-roulette items)
    (cond
        [(empty? items)
         (error 'make-full-roulette "the input list cannot be empty")]
        [(> (sum-weights items) 1.0001)
         (error 'make-full-roulette (string-append "the sum of the weights in the input must be <= 1, got " (number->string (sum-weights items))))]
        [else
         (make-hash items)]))

;; sample-roulette : Roulette -> Any
;; Chooses a random element from the roulette based on the weights from the input.
;; Simple linear choosing.
(define (sample-roulette roulette)
    (define rn (random))
    
    (let-values
        ([(s out)
            (for/fold ([sum 0] [out (void)])
                      ([(k v) (in-hash roulette)]
                      #:break (>= sum rn))
                (values (+ sum v) k))])
        out))

;; sum-weights : List (Any, Decimal) -> Decimal
(define (sum-weights assocs)
    (for/fold ([sum 0])
              ([val-weight (in-list assocs)])
        (+ sum (cdr val-weight))))

(module+ test
    (check-equal? (sum-weights (list)) 0)
    (check-= (sum-weights (list (cons "a" .3) (cons "b" .6))) .9 .001)
    
    (check-exn exn:fail? (lambda () (make-partial-roulette (list) (list))))
    (check-exn exn:fail? (lambda () (make-partial-roulette (list (cons "a" 1)) (list "b"))))
    (check-exn exn:fail? (lambda () (make-full-roulette (list))))
    (check-exn exn:fail? (lambda () (make-full-roulette (list (cons "a" 1.001)))))
    (check-exn exn:fail? (lambda () (make-full-roulette (list (cons "a" 0.5) (cons "b" 0.501)))))
    
    (check-equal?
        (make-partial-roulette (list (cons "a" (/ 3 10)) (cons "b" (/ 6 10))) (list))
        (make-hash (list (cons "a" (/ 3 10)) (cons "b" (/ 6 10)))))
    (check-equal?
        (make-partial-roulette (list) (list "a" "b"))
        (make-hash (list (cons "a" (/ 1 2)) (cons "b" (/ 1 2)))))
    (check-equal?
        (make-partial-roulette (list (cons "a" (/ 3 10))) (list "c" "d"))
        (make-hash (list (cons "a" (/ 3 10)) (cons "c" (/ 7 20)) (cons "d" (/ 7 20)))))
    
    (check-not-exn (lambda () (sample-roulette (make-full-roulette (list (cons "a" 1))))))
    (check-equal? (sample-roulette (make-full-roulette (list (cons "a" 1)))) "a"))