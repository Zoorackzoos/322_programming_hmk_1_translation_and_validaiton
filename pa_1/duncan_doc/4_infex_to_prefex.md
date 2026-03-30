# infex_to_prefex.md

## the bus
everything is going to be fine.  
just fine.  

```text
1 + 1
->
+ 1 1 
        (list 1 2 3 'a "hello")
        ; Result: '(1 2 3 a "hello")
        
        (list (list 1 2) (list 3 4))
        ; Result: '((1 2) (3 4))
    (list (my-second e) (my-first e) )
    
1 + 1 + 1
->
+ 1 1 + 1
->
+ 1 ( 1 + 1 )
+ 1 ( + 1 1 )
    (if 
     ( > (length e) 3) ;; conditional 
      (infix->prefix (my-second e) (my-first e) '( (list (rest((rest e)) ) ) ') ) ;; if yes 
      (list (my-second e) (my-first e) ) ;; if no
    )
    
5 
'true
    (cond
        [
            (number? e) e
        ]
        [
            (boolean-literal? e)
                (cond
                    [
                    (equal? e #t) #t
                    (equal? e #f) #f
                    ]
                )
        ]
        (else 
            (if 
                ( > (length e) 3) ;; conditional 
                (infix->prefix (my-second e) (my-first e) '( (list (rest((rest e)) ) ) ') ) ;; if yes 
                (list (my-second e) (my-first e) ) ;; if no
            )
        )
    )
```
i'm going to plug in that final algo. 
1. ran into issue of extra "()" in the recursive call , that's in the else. 
2. in ```(rest((rest e)) )``` i get the following
```text
. . application: not a procedure;
 expected a procedure that can be applied to arguments
  given: '(+ 2 * 3)
```


