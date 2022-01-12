#lang scribble/manual

(require scribble/base)


@;{This is the documentation for the while loop
   syntax used in the textbook Animated Programming.}

@title{While Loops for Animated Programming}
@author[(author+email "Marco T. Morazan" "morazanm@shu.edu")]
@defmodule[while]

This teachpack provides the necessary syntax to write while loops
as done in the textbook @italic{Animated Programming: Intermediate Program Design Using Video Game Development}.

@table-of-contents[]

@section{Syntax}

@defproc[(while [test expression] 
           [body (cons expression (listof expression))])
         (void)]
        If @italic{test} evaluates to @racket{#true} then the @italic{body} is evaluated. Once
         the @italic{body} is evaluated control returns to the top of the
         loop and the process is repeated until the @italic{test} evaluates
         to @racket{#false}.

@section{Examples}

@subsection{Computing Factorial}

@#reader scribble/comment-reader
@(racketblock
(define (fact-while n)
  (local [; natnum
          ; Purpose: The last natnum multiplied into acc
          (define k (void))
          ; natnum
          ; Purpose: The product of natnums in k+1..n
          (define acc (void))]
    (begin
      (set! k n)
      (set! acc 1)
      ; INV: acc = Π i for i=k+1..n  AND k>=0
      (while (not (= k 0))
             ; acc = Π i for i=k+1..n AND k>0
             (set! acc (* k acc))
             ; acc = Π i for i=k..n AND k>0
             (set! k (sub1 k))
             ; acc = Π i for i=k+1..n AND k>=0
             )
      ; acc = Π i for i=k+1..n AND k=0
      ; ==> acc = Π i for i=1..n 
      ; ==> acc = n!
      acc))
  ; Termination: k starts with the value of the natnum n.
  ; Every loop iteration decreases k by 1 and remains a
  ; a natural number. Eventually, k becomes 0 and the loop
  ; terminates.
  )

(check-expect (fact-while 0) 1)
(check-expect (fact-while 3) 6)
(check-expect (fact-while 5) 120))


@subsection{List Contains}

@#reader scribble/comment-reader
@(racketblock
; contains?-while: x (listof X) --> boolean
; Purpose: To determine if x is in the given list
(define (contains?-while x L)
  (local [; (listof X)
          ; Purpose: To store the unprocessed portion of L
          (define l (void))
          ; Boolean
          ; Purpose: Flag to indicate if x is in L
          (define res (void))]
    (begin
      (set! l L)
      (set! res #f)
      ; INV: res ==> x ε L-l AND (not res) ==> x ∉ L-l
      (while (and (not (empty? l)) (not res))
             ; res ==> x ε L-l AND (not res) ==> x ∉ L-l AND res is false AND l is not empty
             (set! res (or (equal? x (first l)) res))
             ; res ==> x ε L-l AND (not res) ==> x ∉ L-l AND l is not empty
             (set! l (rest l))
             ; res ==> x ε L-l AND (not res) ==> x ∉ L-l
             )
      ; res ==> x ε L-l AND (not res) ==> x not ε L-l AND (l is empty or res is true)
      ; l is empty  ==> res ==> x ε L AND (not res) ==> x ∉ L
      ; res is true ==> true ==> x ε L-l AND false ==> x ∉ L-l
      ;             ==> x ε L
      res)))

(check-expect (contains?-while 'a '()) #false)
(check-expect (contains?-while 'j '(h s e a j l)) #true)
(check-expect (contains?-while 'z '(a b c)) #false))