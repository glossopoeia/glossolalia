#lang racket
(require br/define)

(provide #%datum)
(define (never-followed-by word ls rs) (list))
(provide never-followed-by)
(define (never-in-same-word-as word ls rs) (list))
(provide never-in-same-word-as)

(define-macro (trinity-module-begin (t-file ITEMS ...))
    #'(#%module-begin
        ITEMS ...))
(provide (rename-out [trinity-module-begin #%module-begin]))

(define-macro (t-categories CATS ...)
    #'(define categories (make-hash (list CATS ...))))
(provide t-categories)

(define-macro (t-category NAME SOUNDS ...)
    #'(cons NAME (list SOUNDS ...)))
(provide t-category)

(define-macro (t-structures STRUCTS ...)
    #'(define structures (list STRUCTS ...)))
(provide t-structures)

(define-macro (t-structure GROUPS ...)
    #'(list GROUPS ...))
(provide t-structure)

(define-macro (t-rules RULES ...)
    #'(define rules (list RULES ...)))
(provide t-rules)

(define-macro (t-unary-rule ARGS RULE)
    #'(lambda (x) (RULE x ARGS)))
(provide t-unary-rule)

(define-macro (t-binary-rule LEFTS RULE RIGHTS)
    #'(lambda (x) (RULE x LEFTS RIGHTS)))
(provide t-binary-rule)

(define-macro (t-rule-args ARGS ...)
    #'(list ARGS ...))
(provide t-rule-args)

(define-macro (t-frequencies WHAT ...)
    #'(displayln "Group frequency encountered"))
(provide t-frequencies)

(define-macro (t-generate WHAT ...)
    #'(begin
        (displayln "Generate encountered")
        (displayln categories)
        (displayln structures)
        (displayln (hash-ref categories "@init"))))
(provide t-generate)