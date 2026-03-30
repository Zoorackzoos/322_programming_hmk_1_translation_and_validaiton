# 1_racket_multi_argument_methods.md

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
and takes multiple arguemtns.  
turns out primitive things take multiple arugments.  

[back](0_table_of_contents.md)