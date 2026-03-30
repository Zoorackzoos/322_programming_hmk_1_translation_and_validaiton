#lang racket

;; ============================================================================
;; DEMO INTERPRETER FOR SIMPLE ARITHMETIC EXPRESSION LANGUAGE
;; CSCE 322 - Programming Language Concepts
;;
;; This is a classroom demonstration of core interpreter concepts.
;; We implement a complete interpreter for the arithmetic language from L09:
;;
;;   Program → Expr
;;   Expr    → Number | (+ Expr Expr) | (- Expr Expr) | (* Expr Expr)
;;
;; The interpreter has two main components:
;;   1. Syntax Checker: validates expressions against the grammar
;;   2. Evaluator: executes expressions following operational semantics
;;
;; We also include an optional bonus demo:
;;   3. Translator (infix → prefix): shows how to convert human-readable
;;      infix notation like (1 + 2) into prefix notation like (+ 1 2).
;;      This is directly relevant to PA1!
;;
;; ============================================================================


;; ============================================================================
;; PART 1: SYNTAX CHECKER (valid-expr?)
;; ============================================================================
;;
;; This function implements a recursive descent parser. It checks whether
;; a quoted s-expression matches our grammar:
;;
;;   Expr → Number | (op Expr Expr) where op ∈ {+, -, *}
;;
;; The structure of this function mirrors the grammar structure directly.
;;

(define (valid-expr? expr)
  ;; An expression is valid if it's either:
  ;;   1. A number (atomic case), OR
  ;;   2. A list of form (op expr1 expr2) where op is an arithmetic operator
  ;;      and expr1, expr2 are both valid expressions (recursive case)

  (cond
    ;; Base case: a number is a valid expression
    ((number? expr) #t)

    ;; Recursive case: (op expr1 expr2) where op ∈ {+, -, *}
    ((pair? expr)
     ;; Check structure: must be a list with exactly 3 elements
     (and (= (length expr) 3)
          ;; First element must be an operator
          (valid-op? (car expr))
          ;; Second element must be a valid expression
          (valid-expr? (car (cdr expr)))
          ;; Third element must be a valid expression
          (valid-expr? (car (cdr (cdr expr))))))

    ;; Invalid: anything else (symbols, booleans, empty list, etc.)
    (else #f)))


;; Helper: check if something is a valid operator
;; This is a utility function that checks membership in the operator set.
(define (valid-op? op)
  (or (eq? op '+)
      (eq? op '-)
      (eq? op '*)))


;; ============================================================================
;; PART 2: EVALUATOR (eval-expr)
;; ============================================================================
;;
;; This is the core interpreter. It evaluates expressions according to
;; the operational semantics defined in the course notes:
;;
;;   [[n]]               = n                    (number is its own value)
;;   [[(+ e1 e2)]]       = [[e1]] + [[e2]]     (addition)
;;   [[(- e1 e2)]]       = [[e1]] - [[e2]]     (subtraction)
;;   [(* e1 e2)]]        = [[e1]] * [[e2]]     (multiplication)
;;
;; where [[·]] denotes the semantic evaluation function.
;;
;; The Racket implementation directly mirrors this formal specification:
;; we recursively evaluate subexpressions and apply the operation.
;;

(define (eval-expr expr)
  (cond
    ;; Semantic rule: [[n]] = n
    ;; A numeric literal evaluates to itself
    ((number? expr)
     expr)

    ;; Semantic rule: [[(+ e1 e2)]] = [[e1]] + [[e2]]
    ;; Addition: evaluate both operands, then add
    ((and (pair? expr) (eq? (car expr) '+))
     (+ (eval-expr (car (cdr expr)))
        (eval-expr (car (cdr (cdr expr))))))

    ;; Semantic rule: [[(- e1 e2)]] = [[e1]] - [[e2]]
    ;; Subtraction: evaluate both operands, then subtract
    ((and (pair? expr) (eq? (car expr) '-))
     (- (eval-expr (car (cdr expr)))
        (eval-expr (car (cdr (cdr expr))))))

    ;; Semantic rule: [[(* e1 e2)]] = [[e1]] * [[e2]]
    ;; Multiplication: evaluate both operands, then multiply
    ((and (pair? expr) (eq? (car expr) '*))
     (* (eval-expr (car (cdr expr)))
        (eval-expr (car (cdr (cdr expr))))))

    ;; Should not reach here if valid-expr? was true
    (else (error "invalid expression in eval-expr"))))


;; ============================================================================
;; OPTIONAL BONUS DEMO: INFIX → PREFIX TRANSLATOR (infix->prefix)
;; ============================================================================
;;
;; This is a simplified demo showing how to translate INFIX notation
;; (operator in the middle) to PREFIX notation (operator first).
;;
;;   Infix:   (1 + 2)             — how humans typically write math
;;   Prefix:  (+ 1 2)             — how our interpreter expects input
;;
;; WHY IS THIS RELEVANT?
;;   In PA1, you will build a more complete version of this translation.
;;   PA1 adds booleans, more operators, precedence, and associativity.
;;   This demo shows the core idea in its simplest form.
;;
;; THE KEY INSIGHT:
;;   In a 3-element infix list (expr1 op expr2), the operator is in
;;   the MIDDLE (position 2). We rearrange so the operator comes FIRST:
;;
;;     (expr1 op expr2)  →  (op expr1 expr2)
;;
;;   We do this recursively, so nested expressions are handled too.
;;
;; LIMITATION:
;;   This simple version only handles fully parenthesized expressions.
;;   e.g., '(1 + (2 * 3))  works
;;   but   '(1 + 2 * 3)    does NOT (PA1 handles this with precedence!)
;;

(define (infix->prefix expr)
  (cond
    ;; Base case: a number is already in "prefix" form — just return it
    ((number? expr) expr)

    ;; Recursive case: (expr1 op expr2) → (op expr1 expr2)
    ;; car   = expr1 (left operand)
    ;; cadr  = op    (operator — the middle element)
    ;; caddr = expr2 (right operand)
    ((and (list? expr) (= (length expr) 3))
     (list (cadr expr)                            ;; move operator to front
           (infix->prefix (car expr))             ;; recursively translate left
           (infix->prefix (caddr expr))))         ;; recursively translate right

    ;; Anything else is not a valid infix expression for this demo
    (else (error "invalid infix expression"))))


;; ============================================================================
;; PART 3: INTEGRATION - RUN (simple REPL wrapper)
;; ============================================================================
;;
;; This function ties everything together:
;;   1. Check syntax — does the input match our grammar?
;;   2. Evaluate (if valid) — compute the numeric result
;;   3. Print results
;;
;; This demonstrates the pipeline: validate → evaluate → display
;;

;; Helper: display the result of a valid expression
(define (display-result expr)
  (display "Expression: ")
  (display expr)
  (newline)
  (display "Result: ")
  (display (eval-expr expr))
  (newline))

(define (run expr)
  (cond
    ;; First, check if the expression is syntactically valid
    ((valid-expr? expr)
     ;; If valid, evaluate it and display
     (display-result expr))

    ;; If invalid, report a syntax error
    (else
     (display "Syntax error: invalid expression")
     (newline)
     (display "Expected: Number or (op expr expr) where op is +, -, or *")
     (newline))))


;; ============================================================================
;; TEST CASES
;; ============================================================================
;;
;; These tests demonstrate all the core functionality. Each test is
;; documented with its expected output. You can add these to the REPL
;; or run the file with `racket demo-interpreter.rkt`.
;;

(display "========== PART 1: SYNTAX CHECKING ==========")
(newline)

;; Test 1.1: Simple number (valid)
(display "Test 1.1: (valid-expr? '5)")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-expr? '5))

;; Test 1.2: Simple addition (valid)
(display "Test 1.2: (valid-expr? '(+ 1 2))")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-expr? '(+ 1 2)))

;; Test 1.3: Nested expression (valid)
(display "Test 1.3: (valid-expr? '(+ 1 (* 2 3)))")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-expr? '(+ 1 (* 2 3))))

;; Test 1.4: Missing operand (invalid)
(display "Test 1.4: (valid-expr? '(+ 1))")
(newline)
(display "Expected: #f")
(newline)
(display "Got:      ")
(displayln (valid-expr? '(+ 1)))

;; Test 1.5: Invalid operator (invalid)
(display "Test 1.5: (valid-expr? '(/ 4 2))")
(newline)
(display "Expected: #f")
(newline)
(display "Got:      ")
(displayln (valid-expr? '(/ 4 2)))

;; Test 1.6: Invalid structure - operator is not first (invalid)
(display "Test 1.6: (valid-expr? '(1 2 +))")
(newline)
(display "Expected: #f")
(newline)
(display "Got:      ")
(displayln (valid-expr? '(1 2 +)))

;; Test 1.7: Negative numbers (valid)
(display "Test 1.7: (valid-expr? '(+ -5 3))")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-expr? '(+ -5 3)))

(newline)
(display "========== BONUS DEMO: INFIX → PREFIX TRANSLATION ==========")
(newline)
(display "(This is a simplified version of what you will build in PA1!)")
(newline)
(newline)

;; Test 2.1: Simple number — stays the same in both notations
(display "Test 2.1: (infix->prefix 5)")
(newline)
(display "Expected: 5")
(newline)
(display "Got:      ")
(displayln (infix->prefix 5))

;; Test 2.2: Simple addition — operator moves from middle to front
;; Infix:  (1 + 2)  →  Prefix: (+ 1 2)
(display "Test 2.2: (infix->prefix '(1 + 2))")
(newline)
(display "Expected: (+ 1 2)")
(newline)
(display "Got:      ")
(displayln (infix->prefix '(1 + 2)))

;; Test 2.3: Nested expression — recursion handles inner expressions too
;; Infix:  (1 + (2 * 3))  →  Prefix: (+ 1 (* 2 3))
(display "Test 2.3: (infix->prefix '(1 + (2 * 3)))")
(newline)
(display "Expected: (+ 1 (* 2 3))")
(newline)
(display "Got:      ")
(displayln (infix->prefix '(1 + (2 * 3))))

;; Test 2.4: Both operands are nested
;; Infix:  ((1 + 2) * (5 - 3))  →  Prefix: (* (+ 1 2) (- 5 3))
(display "Test 2.4: (infix->prefix '((1 + 2) * (5 - 3)))")
(newline)
(display "Expected: (* (+ 1 2) (- 5 3))")
(newline)
(display "Got:      ")
(displayln (infix->prefix '((1 + 2) * (5 - 3))))

;; Test 2.5: Deeper nesting
;; Infix:  ((2 * 3) + (4 * 5))  →  Prefix: (+ (* 2 3) (* 4 5))
(display "Test 2.5: (infix->prefix '((2 * 3) + (4 * 5)))")
(newline)
(display "Expected: (+ (* 2 3) (* 4 5))")
(newline)
(display "Got:      ")
(displayln (infix->prefix '((2 * 3) + (4 * 5))))

;; NOTE: This simple infix->prefix requires full parenthesization.
;; In PA1, you will handle flat expressions like '(1 + 2 * 3)
;; using precedence and associativity rules!

(newline)
(display "========== PART 2: EVALUATION ==========")
(newline)

;; Test 3.1: Simple number
(display "Test 3.1: (eval-expr '5)")
(newline)
(display "Expected: 5")
(newline)
(display "Got:      ")
(displayln (eval-expr '5))

;; Test 3.2: Simple addition
(display "Test 3.2: (eval-expr '(+ 2 3))")
(newline)
(display "Expected: 5")
(newline)
(display "Got:      ")
(displayln (eval-expr '(+ 2 3)))

;; Test 3.3: Simple subtraction
(display "Test 3.3: (eval-expr '(- 10 4))")
(newline)
(display "Expected: 6")
(newline)
(display "Got:      ")
(displayln (eval-expr '(- 10 4)))

;; Test 3.4: Simple multiplication
(display "Test 3.4: (eval-expr '(* 3 4))")
(newline)
(display "Expected: 12")
(newline)
(display "Got:      ")
(displayln (eval-expr '(* 3 4)))

;; Test 3.5: Nested expression - the example from lecture
;; Semantic evaluation:
;;   [[(+ (- 4 1) (+ 2 4))]]
;;   = [[(- 4 1)]] + [[(+ 2 4)]]
;;   = (4 - 1) + (2 + 4)
;;   = 3 + 6
;;   = 9
(display "Test 3.5: (eval-expr '(+ (- 4 1) (+ 2 4)))")
(newline)
(display "Expected: 9")
(newline)
(display "Got:      ")
(displayln (eval-expr '(+ (- 4 1) (+ 2 4))))

;; Test 3.6: Expression with multiplication
(display "Test 3.6: (eval-expr '(+ 1 (* 2 3)))")
(newline)
(display "Expected: 7")
(newline)
(display "Got:      ")
(displayln (eval-expr '(+ 1 (* 2 3))))

;; Test 3.7: Complex nested expression
(display "Test 3.7: (eval-expr '(* (+ 1 2) (- 5 3)))")
(newline)
(display "Expected: 6  [because (1+2)*(5-3) = 3*2 = 6]")
(newline)
(display "Got:      ")
(displayln (eval-expr '(* (+ 1 2) (- 5 3))))

;; Test 3.8: Negative numbers
(display "Test 3.8: (eval-expr '(+ -5 3))")
(newline)
(display "Expected: -2")
(newline)
(display "Got:      ")
(displayln (eval-expr '(+ -5 3)))

(newline)
(display "========== PART 3: INTEGRATION (run) ==========")
(newline)

;; Test 4.1: Valid expression
(display "Test 4.1: (run '(+ 1 2))")
(newline)
(newline)
(run '(+ 1 2))
(newline)

;; Test 4.2: Complex valid expression
(display "Test 4.2: (run '(+ (- 4 1) (+ 2 4)))")
(newline)
(newline)
(run '(+ (- 4 1) (+ 2 4)))
(newline)

;; Test 4.3: Invalid expression
(display "Test 4.3: (run '(+ 1))  [missing operand]")
(newline)
(newline)
(run '(+ 1))
(newline)

;; Test 4.4: Another invalid expression
(display "Test 4.4: (run '(/ 4 2))  [invalid operator]")
(newline)
(newline)
(run '(/ 4 2))
(newline)

(display "========== ALL TESTS COMPLETE ==========")
(newline)
