# 3_handle_binary_expressions

```racket
;; TODO: handle binary expressions
```

# wtf is binary
yeah yeah laugh tit up 
```text
Binary Arithmetic Operators 
(e + e) 
(e - e) 
(e * e) 
(e / e) 
```

available functions made by mr. T that deal with binary
1. binary-op
2. binary-shape

```racket
(define (binary-op? x)
  (or (arithmetic-op? x)
      (boolean-op? x)
      (comparison-op? x)))
```

```racket
(define (binary-shape? e)
  (and (three-part-shape? e)
       (binary-op? (my-second e))))
```

# scale of what's put into validate-program
if it's 1, it's a literal. that's fine.  

if it's 2:
```racket
'(- 1)
```
then it's unary. that's acceptable  

if it's 3:
```racket
'(1 + 1) ;;<-- good
'(1 * 1)
'(1 / 1)
'(1 + +) ;;<-- bad
```






