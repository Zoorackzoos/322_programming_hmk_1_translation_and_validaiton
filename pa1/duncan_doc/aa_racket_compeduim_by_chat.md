# Racket Syntax Reference Compendium

This document is a practical reference for common Racket syntax. Each entry includes:

1. Title
2. General purpose
3. Example syntax with output

---

# 1. Program Header: `#lang`

## Purpose

Declares which language the file uses. Almost all beginner Racket programs start with `#lang racket`.

## Example

```racket
#lang racket
(+ 2 3)
```

Output:

```
5
```

---

# 2. Function Call

## Purpose

Racket uses **prefix notation**. The operator comes first, followed by arguments.

## Example

```racket
(+ 2 3)
```

Output

```
5
```

---

# 3. Defining Variables: `define`

## Purpose

Creates a variable binding.

## Example

```racket
(define x 10)
(+ x 5)
```

Output

```
15
```

---

# 4. Defining Functions

## Purpose

Creates reusable procedures.

## Example

```racket
(define (square x)
  (* x x))

(square 5)
```

Output

```
25
```

---

# 5. Lambda (Anonymous Functions)

## Purpose

Creates a function without naming it.

## Example

```racket
((lambda (x) (* x x)) 6)
```

Output

```
36
```

---

# 6. Conditional: `if`

## Purpose

Chooses between two expressions.

## Syntax

`(if condition true-expression false-expression)`

## Example

```racket
(if (> 5 3)
    "yes"
    "no")
```

Output

```
yes
```

---

# 7. Multiple Conditions: `cond`

## Purpose

Used when there are many conditional branches.

## Example

```racket
(cond
  [(> 5 10) "big"]
  [(> 5 3) "medium"]
  [else "small"])
```

Output

```
medium
```

---

# 8. Logical AND

## Purpose

Evaluates expressions left-to-right and stops on false.

## Example

```racket
(and (> 5 3) (< 5 10))
```

Output

```
#t
```

---

# 9. Logical OR

## Purpose

Returns the first true value.

## Example

```racket
(or #f #f 10)
```

Output

```
10
```

---

# 10. Lists

## Purpose

Creates a list data structure.

## Example

```racket
(list 1 2 3 4)
```

Output

```
'(1 2 3 4)
```

---

# 11. Quoting Data

## Purpose

Prevents evaluation and treats data literally.

## Example

```racket
'(1 2 3)
```

Output

```
'(1 2 3)
```

---

# 12. Accessing List Elements

## Purpose

Extract elements from lists.

## Example

```racket
(define L '(10 20 30))
(first L)
```

Output

```
10
```

---

# 13. Rest of List

## Purpose

Returns everything except the first element.

## Example

```racket
(rest '(1 2 3 4))
```

Output

```
'(2 3 4)
```

---

# 14. List Length

## Purpose

Counts elements in a list.

## Example

```racket
(length '(1 2 3 4))
```

Output

```
4
```

---

# 15. Checking Types

## Purpose

Determines the type of a value.

## Example

```racket
(list? '(1 2 3))
```

Output

```
#t
```

---

# 16. Local Variables: `let`

## Purpose

Creates temporary bindings.

## Example

```racket
(let ([x 5]
      [y 7])
  (+ x y))
```

Output

```
12
```

---

# 17. Recursion

## Purpose

Functions calling themselves.

## Example

```racket
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)
```

Output

```
120
```

---

# 18. Mapping Over Lists

## Purpose

Apply a function to each element.

## Example

```racket
(map square '(1 2 3 4))
```

Output

```
'(1 4 9 16)
```

---

# 19. Filtering Lists

## Purpose

Keep elements matching a predicate.

## Example

```racket
(filter even? '(1 2 3 4 5 6))
```

Output

```
'(2 4 6)
```

---

# 20. Folding (Reducing)

## Purpose

Combine list elements into one value.

## Example

```racket
(foldl + 0 '(1 2 3 4))
```

Output

```
10
```

---

# 21. Boolean Values

## Purpose

Logical truth values.

## Example

```racket
#t
#f
```

---

# 22. Equality

## Purpose

Compare values.

## Example

```racket
(= 5 5)
```

Output

```
#t
```

---

# 23. Strings

## Purpose

Text data.

## Example

```racket
(string-append "Hello " "World")
```

Output

```
"Hello World"
```

---

# 24. Printing

## Purpose

Display output.

## Example

```racket
(displayln "Hello")
```

Output

```
Hello
```

---

# 25. Comments

## Purpose

Ignored text for documentation.

## Example

```racket
;; This is a comment
(+ 2 3)
```

Output

```
5
```

---

# End of Basic Reference

This compendium covers the most common syntax needed for beginner to intermediate Racket programming.
