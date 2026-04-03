#lang racket

;; ============================================================================
;; DEMO INTERPRETER 2: BOOLEANS, COMPARISONS, AND CONDITIONALS
;; CSCE 322 - Programming Language Concepts
;;
;; This builds on Demo Interpreter 1 (arithmetic expressions from L09/L10).
;; We INCREMENTALLY extend the language with three new features from L11:
;;
;;   1. Booleans    — true/false values with logic operators (and, or, not)
;;   2. Comparisons — bridge arithmetic to booleans (gt, lt, eq)
;;   3. Conditionals — if-then-else for decision-making
;;
;; The full grammar (from L11):
;;
;;   Program → Expr
;;   Expr    → AExpr | BExpr | IfExpr
;;   AExpr   → Number | (+ Expr Expr) | (- Expr Expr) | (* Expr Expr)
;;   BExpr   → true | false
;;   IfExpr  → (if CCond Expr Expr)
;;   CCond   → BCond | (and CCond CCond) | (or CCond CCond) | (not CCond)
;;   BCond   → (gt Expr Expr) | (lt Expr Expr) | (eq Expr Expr)
;;
;; For each feature we follow the same pattern:
;;   grammar → semantics → interpreter extension
;;
;; The interpreter has two main components:
;;   1. Syntax Checker: validates expressions against the extended grammar
;;   2. Evaluator: executes expressions following operational semantics
;;
;; KEY INSIGHT (from L11): Selective Evaluation
;;   - Arithmetic/boolean operators ALWAYS evaluate ALL subexpressions
;;   - Conditionals evaluate ONLY ONE branch (the one selected by the condition)
;;   - This distinction matters when a branch would cause an error!
;;
;; ============================================================================


;; ============================================================================
;; PART 1: SYNTAX CHECKER
;; ============================================================================
;;
;; We need checkers for each non-terminal in our grammar.
;; The grammar is stratified — Expr branches into AExpr, BExpr, IfExpr —
;; so we write a separate checker for each and combine them.
;;
;; Why stratify? It makes the grammar extensible. When we add a new
;; feature, we just add a new non-terminal and a new branch in Expr.
;;


;; ----------------------------------------------------------------------------
;; Operator helpers
;; ----------------------------------------------------------------------------

;; Arithmetic operators: +, -, *
(define (arith-op? op)
  (or (eq? op '+)
      (eq? op '-)
      (eq? op '*)))

;; Comparison operators: gt, lt, eq
;; These bridge numbers to booleans — they take Exprs and produce a boolean
(define (comp-op? op)
  (or (eq? op 'gt)
      (eq? op 'lt)
      (eq? op 'eq)))

;; Compound condition operators: and, or (binary), not (unary)
(define (logic-binary-op? op)
  (or (eq? op 'and)
      (eq? op 'or)))


;; ----------------------------------------------------------------------------
;; valid-expr? — top-level expression checker
;; ----------------------------------------------------------------------------
;; Expr → AExpr | BExpr | IfExpr
;; An expression is valid if it matches ANY of the three sub-grammars.

(define (valid-expr? expr)
  (or (valid-aexpr? expr)
      (valid-bexpr? expr)
      (valid-ifexpr? expr)))


;; ----------------------------------------------------------------------------
;; valid-aexpr? — arithmetic expressions
;; ----------------------------------------------------------------------------
;; AExpr → Number | (+ Expr Expr) | (- Expr Expr) | (* Expr Expr)
;;
;; Note: operands are Expr (not just AExpr), so you can write things like
;; (+ (if (gt 1 0) 2 3) 4) — the if returns a number used in addition.

(define (valid-aexpr? expr)
  (cond
    ;; Base case: a number literal
    ((number? expr) #t)

    ;; Recursive case: (op Expr Expr) where op ∈ {+, -, *}
    ((and (pair? expr) (= (length expr) 3))
     (and (arith-op? (car expr))
          (valid-expr? (car (cdr expr)))
          (valid-expr? (car (cdr (cdr expr))))))

    (else #f)))


;; ----------------------------------------------------------------------------
;; valid-bexpr? — boolean literals
;; ----------------------------------------------------------------------------
;; BExpr → true | false
;; Boolean literals are just the symbols 'true and 'false.

(define (valid-bexpr? expr)
  (or (eq? expr 'true)
      (eq? expr 'false)))


;; ----------------------------------------------------------------------------
;; valid-ccond? — compound conditions (used inside if)
;; ----------------------------------------------------------------------------
;; CCond → BCond | (and CCond CCond) | (or CCond CCond) | (not CCond)
;;
;; This is what goes in the condition slot of an if-expression.
;; It can be a basic comparison, or logic combinations of conditions.

(define (valid-ccond? expr)
  (cond
    ;; A basic comparison is a valid condition
    ((valid-bcond? expr) #t)

    ;; (and CCond CCond) or (or CCond CCond) — binary logic on conditions
    ((and (pair? expr) (= (length expr) 3)
          (logic-binary-op? (car expr)))
     (and (valid-ccond? (car (cdr expr)))
          (valid-ccond? (car (cdr (cdr expr))))))

    ;; (not CCond) — unary logic on a condition
    ((and (pair? expr) (= (length expr) 2)
          (eq? (car expr) 'not))
     (valid-ccond? (car (cdr expr))))

    (else #f)))


;; ----------------------------------------------------------------------------
;; valid-bcond? — basic conditions (comparisons)
;; ----------------------------------------------------------------------------
;; BCond → (gt Expr Expr) | (lt Expr Expr) | (eq Expr Expr)
;;
;; These take two Expr operands (which evaluate to numbers) and
;; produce a boolean result. They are the bridge from numbers to booleans.

(define (valid-bcond? expr)
  (and (pair? expr)
       (= (length expr) 3)
       (comp-op? (car expr))
       (valid-expr? (car (cdr expr)))
       (valid-expr? (car (cdr (cdr expr))))))


;; ----------------------------------------------------------------------------
;; valid-ifexpr? — conditional expressions
;; ----------------------------------------------------------------------------
;; IfExpr → (if CCond Expr Expr)
;;
;; Structure: (if <condition> <then-branch> <else-branch>)
;;   - <condition> must be a CCond (compound condition)
;;   - <then-branch> and <else-branch> are Expr (can be anything)
;;   - Only ONE branch is evaluated at runtime (selective evaluation!)

(define (valid-ifexpr? expr)
  (and (pair? expr)
       (= (length expr) 4)
       (eq? (car expr) 'if)
       (valid-ccond? (car (cdr expr)))
       (valid-expr? (car (cdr (cdr expr))))
       (valid-expr? (car (cdr (cdr (cdr expr)))))))


;; ============================================================================
;; PART 2: EVALUATOR
;; ============================================================================
;;
;; The evaluator mirrors the operational semantics from L11.
;; We use the ⇓ ("evaluates to") notation in comments.
;;
;; We build it in layers matching the lecture progression:
;;   Layer 1: Arithmetic  (carried over from Demo 1)
;;   Layer 2: Booleans    (true, false, and, or, not)
;;   Layer 3: Comparisons (gt, lt, eq)
;;   Layer 4: Conditionals (if — with SELECTIVE evaluation)
;;


;; ----------------------------------------------------------------------------
;; eval-expr — main evaluator entry point
;; ----------------------------------------------------------------------------
;; Dispatches to the appropriate sub-evaluator based on expression form.

(define (eval-expr expr)
  (cond
    ;; ---- Layer 1: Arithmetic ----

    ;; n ⇓ n  (a number evaluates to itself)
    ((number? expr)
     expr)

    ;; (+ e1 e2) ⇓ n1 + n2  where e1 ⇓ n1, e2 ⇓ n2
    ((and (pair? expr) (eq? (car expr) '+))
     (+ (eval-expr (car (cdr expr)))
        (eval-expr (car (cdr (cdr expr))))))

    ;; (- e1 e2) ⇓ n1 - n2
    ((and (pair? expr) (eq? (car expr) '-))
     (- (eval-expr (car (cdr expr)))
        (eval-expr (car (cdr (cdr expr))))))

    ;; (* e1 e2) ⇓ n1 * n2
    ((and (pair? expr) (eq? (car expr) '*))
     (* (eval-expr (car (cdr expr)))
        (eval-expr (car (cdr (cdr expr))))))

    ;; ---- Layer 2: Boolean literals ----
    ;; true ⇓ true,  false ⇓ false  (booleans evaluate to themselves)
    ((eq? expr 'true)  'true)
    ((eq? expr 'false) 'false)

    ;; ---- Layer 4: Conditionals ----
    ;; This is placed before comparisons/logic in the cond chain
    ;; because "if" is easy to detect and is a top-level expression form.
    ;;
    ;; SELECTIVE EVALUATION:
    ;;   If c ⇓ true:   (if c e1 e2) ⇓ [[e1]]   — only e1 is evaluated
    ;;   If c ⇓ false:  (if c e1 e2) ⇓ [[e2]]   — only e2 is evaluated
    ((and (pair? expr) (eq? (car expr) 'if))
     (if (eq? (eval-ccond (car (cdr expr))) 'true)
         (eval-expr (car (cdr (cdr expr))))           ;; then-branch
         (eval-expr (car (cdr (cdr (cdr expr)))))))   ;; else-branch

    (else (error "eval-expr: invalid expression" expr))))


;; ----------------------------------------------------------------------------
;; eval-ccond — evaluate compound conditions
;; ----------------------------------------------------------------------------
;; CCond → BCond | (and CCond CCond) | (or CCond CCond) | (not CCond)
;;
;; Semantics (from L11 slides):
;;
;; AND:
;;   b1 ⇓ true, b2 ⇓ true   →  (and b1 b2) ⇓ true
;;   b1 ⇓ false              →  (and b1 b2) ⇓ false
;;   b1 ⇓ true, b2 ⇓ false  →  (and b1 b2) ⇓ false
;;
;; OR:
;;   b1 ⇓ true               →  (or b1 b2) ⇓ true
;;   b2 ⇓ true               →  (or b1 b2) ⇓ true
;;   b1 ⇓ false, b2 ⇓ false  →  (or b1 b2) ⇓ false
;;
;; NOT:
;;   b ⇓ true   →  (not b) ⇓ false
;;   b ⇓ false  →  (not b) ⇓ true

(define (eval-ccond expr)
  (cond
    ;; (and c1 c2): both must be true for result to be true
    ((and (pair? expr) (eq? (car expr) 'and))
     (if (and (eq? (eval-ccond (car (cdr expr))) 'true)
              (eq? (eval-ccond (car (cdr (cdr expr)))) 'true))
         'true
         'false))

    ;; (or c1 c2): at least one must be true
    ((and (pair? expr) (eq? (car expr) 'or))
     (if (or (eq? (eval-ccond (car (cdr expr))) 'true)
             (eq? (eval-ccond (car (cdr (cdr expr)))) 'true))
         'true 
         'false))

    ;; (not c): flip the result
    ((and (pair? expr) (eq? (car expr) 'not))
     (if (eq? (eval-ccond (car (cdr expr))) 'true)
         'false
         'true))

    ;; Fall through to basic comparison
    (else (eval-bcond expr))))


;; ----------------------------------------------------------------------------
;; eval-bcond — evaluate basic conditions (comparisons)
;; ----------------------------------------------------------------------------
;; BCond → (gt Expr Expr) | (lt Expr Expr) | (eq Expr Expr)
;;
;; These take two Exprs, evaluate them to numbers, and compare.
;;   (gt e1 e2) ⇓ true  if [[e1]] > [[e2]]
;;   (lt e1 e2) ⇓ true  if [[e1]] < [[e2]]
;;   (eq e1 e2) ⇓ true  if [[e1]] == [[e2]]

(define (eval-bcond expr)
  (cond
    ;; (gt e1 e2): is the left side greater?
    ((and (pair? expr) (eq? (car expr) 'gt))
     (if (> (eval-expr (car (cdr expr)))
            (eval-expr (car (cdr (cdr expr)))))
         'true
         'false))

    ;; (lt e1 e2): is the left side less?
    ((and (pair? expr) (eq? (car expr) 'lt))
     (if (< (eval-expr (car (cdr expr)))
            (eval-expr (car (cdr (cdr expr)))))
         'true
         'false))

    ;; (eq e1 e2): are they equal?
    ((and (pair? expr) (eq? (car expr) 'eq))
     (if (= (eval-expr (car (cdr expr)))
            (eval-expr (car (cdr (cdr expr)))))
         'true
         'false))

    (else (error "eval-bcond: invalid condition" expr))))


;; ============================================================================
;; PART 3: INTEGRATION - RUN
;; ============================================================================
;;
;; Pipeline: validate → evaluate → display
;;

(define (display-result expr)
  (display "Expression: ")
  (display expr)
  (newline)
  (display "Result: ")
  (display (eval-expr expr))
  (newline))

(define (run expr)
  (cond
    ((valid-expr? expr)
     (display-result expr))
    (else
     (display "Syntax error: invalid expression")
     (newline)
     (display "Check against the grammar:")
     (newline)
     (display "  Expr  → AExpr | BExpr | IfExpr")
     (newline)
     (display "  AExpr → Number | (+ Expr Expr) | (- Expr Expr) | (* Expr Expr)")
     (newline)
     (display "  BExpr → true | false")
     (newline)
     (display "  IfExpr→ (if CCond Expr Expr)")
     (newline)
     (display "  CCond → BCond | (and CCond CCond) | (or CCond CCond) | (not CCond)")
     (newline)
     (display "  BCond → (gt Expr Expr) | (lt Expr Expr) | (eq Expr Expr)")
     (newline))))


;; ============================================================================
;; TEST CASES
;; ============================================================================
;;
;; We test each feature layer incrementally, matching the lecture progression.
;; Run with: racket demo-interpreter2.rkt
;;


;; ============================================================================
;; LAYER 1: ARITHMETIC (carried over from Demo 1 — should still work!)
;; ============================================================================

(display "========== LAYER 1: ARITHMETIC ==========")
(newline)

(display "Test 1.1: (valid-expr? 5)")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-expr? 5))

(display "Test 1.2: (valid-expr? '(+ 1 2))")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-expr? '(+ 1 2)))

(display "Test 1.3: (eval-expr '(+ 2 3))")
(newline)
(display "Expected: 5")
(newline)
(display "Got:      ")
(displayln (eval-expr '(+ 2 3)))

(display "Test 1.4: (eval-expr '(* (+ 1 2) (- 5 3)))")
(newline)
(display "Expected: 6")
(newline)
(display "Got:      ")
(displayln (eval-expr '(* (+ 1 2) (- 5 3))))

(newline)


;; ============================================================================
;; LAYER 2: BOOLEANS
;; ============================================================================
;; New: true, false as literal values
;; These are a second value type in our language (alongside numbers).

(display "========== LAYER 2: BOOLEANS ==========")
(newline)

;; Boolean literals validate
(display "Test 2.1: (valid-expr? 'true)")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-expr? 'true))

(display "Test 2.2: (valid-expr? 'false)")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-expr? 'false))

;; Boolean literals evaluate to themselves
(display "Test 2.3: (eval-expr 'true)")
(newline)
(display "Expected: true")
(newline)
(display "Got:      ")
(displayln (eval-expr 'true))

(display "Test 2.4: (eval-expr 'false)")
(newline)
(display "Expected: false")
(newline)
(display "Got:      ")
(displayln (eval-expr 'false))

;; Symbols that aren't true/false should be invalid
(display "Test 2.5: (valid-expr? 'hello)")
(newline)
(display "Expected: #f")
(newline)
(display "Got:      ")
(displayln (valid-expr? 'hello))

(newline)


;; ============================================================================
;; LAYER 3: COMPARISONS (gt, lt, eq)
;; ============================================================================
;; These BRIDGE arithmetic to booleans.
;; They take two Expr operands and produce a boolean result.
;; They appear inside conditions (CCond), not as standalone Expr.

(display "========== LAYER 3: COMPARISONS ==========")
(newline)

;; Comparisons are valid conditions
(display "Test 3.1: (valid-ccond? '(gt 5 3))")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-ccond? '(gt 5 3)))

(display "Test 3.2: (valid-ccond? '(lt 1 2))")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-ccond? '(lt 1 2)))

(display "Test 3.3: (valid-ccond? '(eq 4 4))")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-ccond? '(eq 4 4)))

;; Evaluating comparisons
(display "Test 3.4: (eval-bcond '(gt 5 3))")
(newline)
(display "Expected: true")
(newline)
(display "Got:      ")
(displayln (eval-bcond '(gt 5 3)))

(display "Test 3.5: (eval-bcond '(gt 1 5))")
(newline)
(display "Expected: false")
(newline)
(display "Got:      ")
(displayln (eval-bcond '(gt 1 5)))

(display "Test 3.6: (eval-bcond '(lt 2 10))")
(newline)
(display "Expected: true")
(newline)
(display "Got:      ")
(displayln (eval-bcond '(lt 2 10)))

(display "Test 3.7: (eval-bcond '(eq 4 4))")
(newline)
(display "Expected: true")
(newline)
(display "Got:      ")
(displayln (eval-bcond '(eq 4 4)))

(display "Test 3.8: (eval-bcond '(eq 3 7))")
(newline)
(display "Expected: false")
(newline)
(display "Got:      ")
(displayln (eval-bcond '(eq 3 7)))

;; Comparisons can use complex arithmetic expressions as operands
(display "Test 3.9: (eval-bcond '(gt (+ 2 3) (* 1 4)))")
(newline)
(display "Expected: true  [because 5 > 4]")
(newline)
(display "Got:      ")
(displayln (eval-bcond '(gt (+ 2 3) (* 1 4))))

(newline)


;; ============================================================================
;; LAYER 3b: COMPOUND CONDITIONS (and, or, not)
;; ============================================================================
;; Logic operators combine conditions into more complex conditions.

(display "========== LAYER 3b: COMPOUND CONDITIONS (and, or, not) ==========")
(newline)

;; Validation
(display "Test 3b.1: (valid-ccond? '(and (gt 5 3) (lt 1 10)))")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-ccond? '(and (gt 5 3) (lt 1 10))))

(display "Test 3b.2: (valid-ccond? '(not (eq 1 2)))")
(newline)
(display "Expected: #t")
(newline)
(display "Got:      ")
(displayln (valid-ccond? '(not (eq 1 2))))

;; Evaluation
;; (and (gt 5 3) (lt 1 10))  →  both true  →  true
(display "Test 3b.3: (eval-ccond '(and (gt 5 3) (lt 1 10)))")
(newline)
(display "Expected: true")
(newline)
(display "Got:      ")
(displayln (eval-ccond '(and (gt 5 3) (lt 1 10))))

;; (and (gt 5 3) (gt 1 10))  →  true AND false  →  false
(display "Test 3b.4: (eval-ccond '(and (gt 5 3) (gt 1 10)))")
(newline)
(display "Expected: false")
(newline)
(display "Got:      ")
(displayln (eval-ccond '(and (gt 5 3) (gt 1 10))))

;; (or (gt 1 10) (eq 3 3))  →  false OR true  →  true
(display "Test 3b.5: (eval-ccond '(or (gt 1 10) (eq 3 3)))")
(newline)
(display "Expected: true")
(newline)
(display "Got:      ")
(displayln (eval-ccond '(or (gt 1 10) (eq 3 3))))

;; (or (gt 1 10) (lt 5 2))  →  false OR false  →  false
(display "Test 3b.6: (eval-ccond '(or (gt 1 10) (lt 5 2)))")
(newline)
(display "Expected: false")
(newline)
(display "Got:      ")
(displayln (eval-ccond '(or (gt 1 10) (lt 5 2))))

;; (not (eq 1 2))  →  not false  →  true
(display "Test 3b.7: (eval-ccond '(not (eq 1 2)))")
(newline)
(display "Expected: true")
(newline)
(display "Got:      ")
(displayln (eval-ccond '(not (eq 1 2))))

;; (not (gt 5 3))  →  not true  →  false
(display "Test 3b.8: (eval-ccond '(not (gt 5 3)))")
(newline)
(display "Expected: false")
(newline)
(display "Got:      ")
(displayln (eval-ccond '(not (gt 5 3))))

(newline)


;; ============================================================================
;; LAYER 4: CONDITIONALS (if)
;; ============================================================================
;; (if <condition> <then> <else>)
;;   - <condition> is a CCond
;;   - Only ONE branch is evaluated (selective evaluation!)

(display "========== LAYER 4: CONDITIONALS ==========")
(newline)

;; Basic if with literal condition (from L11 slide example)
;; (if true (+ 1 2) (* 3 4))  →  condition is true  →  evaluate (+ 1 2) → 3
(display "Test 4.1: (run '(if (eq 1 1) (+ 1 2) (* 3 4)))")
(newline)
(display "Expected result: 3  [condition true, so (+ 1 2)]")
(newline)
(run '(if (eq 1 1) (+ 1 2) (* 3 4)))

;; (if (gt 5 3) (+ 2 3) (* 4 5))  →  5 > 3 is true  →  5
;; This is the example from the L11 slides!
(display "Test 4.2: (run '(if (gt 5 3) (+ 2 3) (* 4 5)))")
(newline)
(display "Expected result: 5  [5 > 3 is true, so (+ 2 3)]")
(newline)
(run '(if (gt 5 3) (+ 2 3) (* 4 5)))

;; (if (eq 1 2) 10 20)  →  1 == 2 is false  →  20
;; Another example from L11 slides
(display "Test 4.3: (run '(if (eq 1 2) 10 20))")
(newline)
(display "Expected result: 20  [1 == 2 is false, so else-branch]")
(newline)
(run '(if (eq 1 2) 10 20))

;; Compound condition in if
;; (if (and (gt 5 3) (lt 1 10)) 100 200)  →  true AND true → 100
(display "Test 4.4: (run '(if (and (gt 5 3) (lt 1 10)) 100 200))")
(newline)
(display "Expected result: 100  [both conditions true]")
(newline)
(run '(if (and (gt 5 3) (lt 1 10)) 100 200))

;; Nested if — if can return values used in arithmetic
;; (+ (if (gt 5 3) 10 20) 5)  →  10 + 5 → 15
(display "Test 4.5: (run '(+ (if (gt 5 3) 10 20) 5))")
(newline)
(display "Expected result: 15  [if returns 10, then 10 + 5]")
(newline)
(run '(+ (if (gt 5 3) 10 20) 5))

;; Nested if with complex branches
;; (if (lt 1 2) (+ 10 (* 2 3)) (- 100 1))  →  true → 10 + 6 → 16
(display "Test 4.6: (run '(if (lt 1 2) (+ 10 (* 2 3)) (- 100 1)))")
(newline)
(display "Expected result: 16  [1 < 2 is true, so (+ 10 (* 2 3)) = 16]")
(newline)
(run '(if (lt 1 2) (+ 10 (* 2 3)) (- 100 1)))

;; Using not in condition
;; (if (not (eq 1 2)) 42 0)  →  not false → true → 42
(display "Test 4.7: (run '(if (not (eq 1 2)) 42 0))")
(newline)
(display "Expected result: 42  [(not (eq 1 2)) is true]")
(newline)
(run '(if (not (eq 1 2)) 42 0))

(newline)


;; ============================================================================
;; LAYER 5: SYNTAX ERROR CASES
;; ============================================================================
;; Demonstrating that invalid expressions are caught by the syntax checker.

(display "========== SYNTAX ERROR CASES ==========")
(newline)

;; Missing else branch
(display "Test 5.1: (run '(if (gt 1 0) 5))  [missing else branch]")
(newline)
(run '(if (gt 1 0) 5))
(newline)

;; Invalid operator
(display "Test 5.2: (run '(/ 10 2))  [/ not in grammar]")
(newline)
(run '(/ 10 2))
(newline)

;; Random symbol
(display "Test 5.3: (run 'hello)  [not a valid expression]")
(newline)
(run 'hello)
(newline)

;; Condition is not a CCond
(display "Test 5.4: (run '(if 5 1 2))  [5 is not a condition]")
(newline)
(run '(if 5 1 2))
(newline)


;; ============================================================================
;; KEY INSIGHT: SELECTIVE EVALUATION
;; ============================================================================
;; This is the most important concept from L11!
;;
;; In arithmetic: ALL subexpressions are always evaluated.
;;   (+ 1 (error)) → tries to evaluate both sides → error
;;
;; In conditionals: ONLY the selected branch is evaluated.
;;   (if true 1 (error)) → only evaluates 1, never touches the else branch
;;
;; We demonstrate this by showing that if the unused branch would cause
;; an error, the program still succeeds.

(display "========== KEY INSIGHT: SELECTIVE EVALUATION ==========")
(newline)
(display "Only the chosen branch of an if-expression is evaluated!")
(newline)
(display "The other branch is NEVER touched, even if it would cause an error.")
(newline)
(newline)

;; The then-branch has an invalid subexpression for our evaluator,
;; but since the condition is false, we never evaluate it.
;; We use (- 0 0) as a safe stand-in. In a real demo you could show
;; how an error in the unused branch doesn't matter.

;; Condition true → evaluate then-branch (safe), skip else-branch
(display "Test 6.1: (if (gt 5 0) 42 999)")
(newline)
(display "Expected result: 42  [else-branch (999) is never evaluated]")
(newline)
(run '(if (gt 5 0) 42 999))

;; Condition false → evaluate else-branch (safe), skip then-branch
(display "Test 6.2: (if (lt 5 0) 999 42)")
(newline)
(display "Expected result: 42  [then-branch (999) is never evaluated]")
(newline)
(run '(if (lt 5 0) 999 42))

;; Nested ifs demonstrating selective evaluation at multiple levels
(display "Test 6.3: (if (gt 10 5) (if (eq 1 1) 100 200) 0)")
(newline)
(display "Expected result: 100  [outer: true→then, inner: true→then]")
(newline)
(run '(if (gt 10 5) (if (eq 1 1) 100 200) 0))

(newline)
(display "========== ALL TESTS COMPLETE ==========")
(newline)
