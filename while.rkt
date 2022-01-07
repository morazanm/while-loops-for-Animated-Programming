#lang racket

(require (for-syntax syntax/parse))

;(provide while)


;   Syntax for Animated Programming
;   Purpose: Provide while loop construct that returns (void)
;
;   (while <expr>
;     <expr>^+)

;(define-syntax while
;  (syntax-rules ()
;    ((while condition body ...)
;     (let loop ()
;       (if condition
;           (begin
;             body 
;             ...
;             (loop))
;           (void))))))

(define-syntax (while stx)
  (syntax-parse stx
    [(_ condition body ...+)
     #`(let loop ()
         (if condition
             (begin
               body ...
               (loop))
             (void)))]
    [(_ condition)
     (error (format "Invalid while syntax: Missing body.\n Expected syntax: (while <expr> <expr>^+).\n Given: (while ~s)" (syntax->datum #'condition)))]
    [else
     (error (format "Invalid while syntax: Missing driver and body.\n Expected syntax: (while <expr> <expr>^+).\n Given: (while)"))]))

