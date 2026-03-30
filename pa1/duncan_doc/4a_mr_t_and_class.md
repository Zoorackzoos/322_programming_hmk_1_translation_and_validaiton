# mr_t_and_class

fight or die


this is my transcription of mr T's pseudocode on friday.
```racket
(define (find-best-split e) 
    ...)
    
(define (take-n lst n)
    (cond
        [(= n 0) '()]
        [else (cons (car lst)
                    (take-n (cdr lst) (- n 1)))]  
    )
)

(define (drop-n lst n)
    (cond
        [(= n 0) lst]
        [else (drop-n (cdr lst) (- n 1))]
    )
)
    
(define (infix->prefix e)
    (cond)
        [(number? e) e]
        [(boolean-literal? e) e]
        [(unary-shape? e) ...]
        [(single-element? e) (unwrap (infix->prefix e))]
        [else
            ;; find best split operator funciton ;; '( 1 = 2 * 3 + 4)
            ;; split the list into 3 parts ((left) op (right)) for exaplme ((1 + 2 + 8 3) + (4))
            ;; swap first and second elements
            ;; call infix-prefix on the operands: (op (finfix->prefix first) (infix_>prefix third))
        ]
)
```

