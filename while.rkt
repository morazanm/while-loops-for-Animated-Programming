#lang racket

(require (for-syntax syntax/parse))

(provide while)


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
    [(_ condition:expr body:expr ...+)
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

(define x 10)
(define res 0)
(begin
    (while (< 0 x)
           (set! res (+ res x))
           (set! x (sub1 x)))
    res)

(begin
    (while 7 "5"))

