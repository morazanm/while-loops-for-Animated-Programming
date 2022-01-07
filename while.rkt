;   Syntax for Animated Programming
;   Purpose: Provide while loop construct that returns (void)
;
;   (while <expr>
;     <expr>^*)



(module while racket

  (provide while)

  (define-syntax while
    (syntax-rules ()
      ((while condition body ...)
       (let loop ()
         (if condition
             (begin
               body 
               ...
               (loop))
             (void)))))))

