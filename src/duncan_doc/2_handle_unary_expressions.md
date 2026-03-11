# 2_handle_unary_expressions.md

```racket
;; ============================================================
;; validate-program
;;
;; Input:
;;   any Racket value
;;
;; Output:
;;   #t if valid
;;   otherwise the smallest offending symbol or subexpression
;;
;; Examples:
;;   (validate-program '(1 + 2))          => #t
;;   (validate-program '(1 + * 3))        => '*
;;   (validate-program '(1 < 2 > 3))      => '>
;;
;; TODO:
;;   implement this function
;; ============================================================

;; TODO: handle unary expressions
```
![fuck](https://media.tenor.com/t5bY3d1QQioAAAAM/hellfire-skeleton.gif)

[wtf is unary](2a_wtf_is_unary.md)

available functions made by mr. T that deal with unary
1. unary-op
2. unary-shape

```racket
;; returns true if: - , !. returns false if antyhging else
(define (unary-op? x)
  (or (equal? x '-)
      (equal? x '!)
      (equal? x 'not)))
```

```racket
;; this is a word nobody uses.
;;     https://docs.google.com/document/d/1HOnXb8v4rdqWKvQWRcCS--AHOB0hQhTt20cyo58_x1s/edit?usp=sharing
;; if e is a list, and has length 2, and the 1st element is either ! or -.
;;    like: -x, !x
;;    not: ++x, --x, ~x
(define (unary-shape? e)
  (and (list? e)
       (= (length e) 2)
       (unary-op? (my-first e))))
```

notes:  
1. i'm supposed to edit unary shape
2. i'm not supposed to edit unary-op

# day 2 
[if statements](https://docs.racket-lang.org/reference/if.html)  
i should have done something else with my life.  

## cond
the way cond works is that every ```[]``` is a if statement.  
just imagine that as a if statement if it were in a higher level language.  
you put a conditional in there,  

unfortunately the way cond works is that there is not else after the immteate if statement. the else happens at the end. 

## list length
a list's length is determined by spaces between strings. not the string length.  
coulda fooled me mate. 



[back](0_table_of_contents.md)