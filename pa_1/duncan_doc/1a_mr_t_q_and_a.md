# 1a_mr_t_q_and_a

```racket
`duncan_validate_program_tests_unary
(length `(-x + -x) ) ;; 3
(length `(-x+-x) ) ;; 1
(validate-program `(- 1))
(validate-program `(! 1))
(validate-program `(- -1))
(validate-program `(! !1))
;;(validate-program `- x) ;; x: unbound identifier in: x
(validate-program `(- 1 -))
(validate-program `(! 1 -))
```

if valid-program == false then blow up
1. construct new list
2. "swap 1st and second element"
3. call itslef on 1st aand 3rd lement
1 + 2 -> + 1 2
swap 2nd and 1st element
why stop?
1 + 2 + 3
->
+ 1 2 + 3
  ->
+ 1 (+ 2 3)
  swap 1st and 2nd element
  precident = pemdas
  1 + 2 * 3
  ->
  1 + (2 * 3)
  determined by
    1. run thru full iist
    2. determine highes precidence value
    3. make new list based of ranking

    