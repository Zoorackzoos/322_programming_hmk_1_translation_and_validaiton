# 4_binary_detection_and_eval..md

```racket
;; if it's bigger than 3 items then we simplofy that using recursion down the line
;; when it gets to here it is 3 items
```
```racket
(println "mr.t pa2 grader tests")
;;(evaluate-prefix '(1 + 2))
;;(evaluate-prefix '(5 - 2))
;;(evaluate-prefix '(3 * 4))
;;(evaluate-prefix '(8 / 2)))
```
```racket
  ;; Boolean (8 pts)
  (run-test "Evaluation" "(true && false)" student-evaluate '(true && false) 'false 2)
  (run-test "Evaluation" "(true || false)" student-evaluate '(true || false) 'true 2)
  (run-test "Evaluation" "(! true)" student-evaluate '(! true) 'false 2)
  (run-test "Evaluation" "(not false)" student-evaluate '(not false) 'true 2)

  ;; Comparison (12 pts)
  (run-test "Evaluation" "(1 < 2)" student-evaluate '(1 < 2) 'true 2)
  (run-test "Evaluation" "(2 <= 2)" student-evaluate '(2 <= 2) 'true 2)
  (run-test "Evaluation" "(3 > 1)" student-evaluate '(3 > 1) 'true 2)
  (run-test "Evaluation" "(3 >= 4)" student-evaluate '(3 >= 4) 'false 2)
  (run-test "Evaluation" "(1 == 1)" student-evaluate '(1 == 1) 'true 2)
  (run-test "Evaluation" "(1 != 2)" student-evaluate '(1 != 2) 'true 2)
```

this is broken down into X conditionals
1. arithmatic
   1. \+
   2. \-
   3. \*
   4. \/ 
2. 1 v 1 boolean
   1. &&
   2. ||
3. 1 v 1 number comparison 
   1. \>
   2. \< 
   3. \>=
   4. \<=
4. equals and not equals
   1. ==
   2. !=

[table of contents](0_table_of_contents.md)  
[go back](3_abundence_of_println_statemetns.md)  
[next]()