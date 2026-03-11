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








