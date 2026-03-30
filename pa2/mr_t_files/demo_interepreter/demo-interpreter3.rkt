#lang racket

;; ============================================================================
;; DEMO INTERPRETER 3: VARIABLES AND ENVIRONMENTS
;; CSCE 322 - Programming Language Concepts
;;
;; This builds on Demo Interpreter 2 (booleans, comparisons, conditionals).
;; We add VARIABLES using the ENVIRONMENT MODEL from L13.
;;
;; New features:
;;   1. Environment (σ) — a mapping from variable names to values
;;   2. Variable lookup  — a symbol evaluates to its value in the environment
;;   3. Var expressions  — (var (x e1) e2) binds x to e1's value in e2's scope
;;
;; The full grammar:
;;
;;   Program  → Expr
;;   Expr     → AExpr | BExpr | IfExpr | Variable | VarExpr
;;   AExpr    → Number | (+ Expr Expr) | (- Expr Expr) | (* Expr Expr)
;;   BExpr    → true | false
;;   IfExpr   → (if CCond Expr Expr)
;;   CCond    → BCond | (and CCond CCond) | (or CCond CCond) | (not CCond)
;;   BCond    → (gt Expr Expr) | (lt Expr Expr) | (eq Expr Expr)
;;   Variable → symbol (any name like x, y, count, etc.)
;;   VarExpr  → (var VarAssign Expr)
;;   VarAssign→ (Variable Expr)
;;
;; KEY CONCEPT (from L13): The Environment Model
;;   - The environment σ is a list of (name . value) pairs
;;   - Lookup scans left-to-right; the FIRST match wins (most recent binding)
;;   - Extend PREPENDS a new binding — never removes or overwrites old ones
;;   - Shadowing works naturally: new binding for x hides the old one
;;   - Free variables cause an error when lookup finds no match
;;
;; MAJOR CHANGE FROM DEMO 2:
;;   Every evaluator function now takes an extra parameter: env
;;   This threads the environment through the entire evaluation.
;;
;; ============================================================================


;; ============================================================================
;; PART 1: THE ENVIRONMENT
;; ============================================================================
;;
;; The environment is our new data structure. It's an association list:
;; a list of (name . value) pairs, searched front-to-back.
;;
;; Three operations:
;;   (empty-env)            → creates an empty environment: '()
;;   (extend-env env x v)   → adds binding x→v at the front: ((x . v) ...)
;;   (lookup env x)         → finds the value bound to x, or errors if free
;;

;; Create an empty environment — no bindings at all
(define (empty-env) '())

;; Extend the environment by adding a new binding at the FRONT
;; This is how shadowing works: the new binding is found first during lookup
(define (extend-env env name val)
  (cons (cons name val) env))

;; Look up a variable name in the environment
;; Scans left-to-right (front-to-back) — first match wins
;; If the name is not found, it's a FREE VARIABLE → error
(define (lookup env name)
  (cond
    ;; Reached the end without finding it → free variable!
    ((null? env)
     (error "unbound variable" name))

    ;; First pair's name matches → return its value
    ((eq? (car (car env)) name)
     (cdr (car env)))

    ;; Keep searching the rest of the list
    (else
     (lookup (cdr env) name))))


;; ============================================================================
;; PART 2: SYNTAX CHECKER
;; ============================================================================
;;
;; Extended from Demo 2 to handle variables and var expressions.
;; Two new non-terminals:
;;   Variable → any symbol (except reserved words)
;;   VarExpr  → (var (Variable Expr) Expr)
;;
;; Note: The syntax checker does NOT check for free variables.
;; Free variables are valid syntax — they're caught at evaluation time
;; when lookup fails. This is a deliberate design choice.
;;


;; ----------------------------------------------------------------------------
;; Operator and keyword helpers
;; ----------------------------------------------------------------------------

(define (arith-op? op)
  (or (eq? op '+) (eq? op '-) (eq? op '*)))

(define (comp-op? op)
  (or (eq? op 'gt) (eq? op 'lt) (eq? op 'eq)))

(define (logic-binary-op? op)
  (or (eq? op 'and) (eq? op 'or)))

;; Reserved words that cannot be used as variable names
(define (reserved-word? sym)
  (or (eq? sym 'true) (eq? sym 'false)
      (eq? sym 'if) (eq? sym 'var)
      (eq? sym 'and) (eq? sym 'or) (eq? sym 'not)
      (eq? sym 'gt) (eq? sym 'lt) (eq? sym 'eq)
      (arith-op? sym)))


;; ----------------------------------------------------------------------------
;; valid-expr? — top-level expression checker
;; ----------------------------------------------------------------------------
;; Expr → AExpr | BExpr | IfExpr | Variable | VarExpr

(define (valid-expr? expr)
  (or (valid-aexpr? expr)
      (valid-bexpr? expr)
      (valid-ifexpr? expr)
      (valid-variable? expr)
      (valid-varexpr? expr)))


;; ----------------------------------------------------------------------------
;; valid-aexpr? — arithmetic expressions
;; ----------------------------------------------------------------------------

(define (valid-aexpr? expr)
  (cond
    ((number? expr) #t)
    ((and (pair? expr) (= (length expr) 3))
     (and (arith-op? (car expr))
          (valid-expr? (car (cdr expr)))
          (valid-expr? (car (cdr (cdr expr))))))
    (else #f)))


;; ----------------------------------------------------------------------------
;; valid-bexpr? — boolean literals
;; ----------------------------------------------------------------------------

(define (valid-bexpr? expr)
  (or (eq? expr 'true) (eq? expr 'false)))


;; ----------------------------------------------------------------------------
;; valid-ccond? — compound conditions
;; ----------------------------------------------------------------------------

(define (valid-ccond? expr)
  (cond
    ((valid-bcond? expr) #t)
    ((and (pair? expr) (= (length expr) 3)
          (logic-binary-op? (car expr)))
     (and (valid-ccond? (car (cdr expr)))
          (valid-ccond? (car (cdr (cdr expr))))))
    ((and (pair? expr) (= (length expr) 2)
          (eq? (car expr) 'not))
     (valid-ccond? (car (cdr expr))))
    (else #f)))


;; ----------------------------------------------------------------------------
;; valid-bcond? — basic conditions (comparisons)
;; ----------------------------------------------------------------------------

(define (valid-bcond? expr)
  (and (pair? expr)
       (= (length expr) 3)
       (comp-op? (car expr))
       (valid-expr? (car (cdr expr)))
       (valid-expr? (car (cdr (cdr expr))))))


;; ----------------------------------------------------------------------------
;; valid-ifexpr? — conditional expressions
;; ----------------------------------------------------------------------------

(define (valid-ifexpr? expr)
  (and (pair? expr)
       (= (length expr) 4)
       (eq? (car expr) 'if)
       (valid-ccond? (car (cdr expr)))
       (valid-expr? (car (cdr (cdr expr))))
       (valid-expr? (car (cdr (cdr (cdr expr)))))))


;; ----------------------------------------------------------------------------
;; valid-variable? — a symbol that is not a reserved word
;; ----------------------------------------------------------------------------
;; Variable → symbol (like x, y, count, total, etc.)
;; Must not be a reserved word (true, false, if, var, etc.)

(define (valid-variable? expr)
  (and (symbol? expr)
       (not (reserved-word? expr))))


;; ----------------------------------------------------------------------------
;; valid-varexpr? — variable binding expressions
;; ----------------------------------------------------------------------------
;; VarExpr → (var (Variable Expr) Expr)
;;
;; Structure: (var <binding> <body>)
;;   - <binding> is a 2-element list: (name value-expr)
;;   - <body> is any Expr (evaluated in the extended environment)
;;
;; Example: (var (x 5) (+ x 3))
;;   binding = (x 5), where x is the name, 5 is the value expression
;;   body = (+ x 3), evaluated with x bound to 5

(define (valid-varexpr? expr)
  (and (pair? expr)
       (= (length expr) 3)
       (eq? (car expr) 'var)
       ;; The binding must be a 2-element list: (Variable Expr)
       (pair? (car (cdr expr)))
       (= (length (car (cdr expr))) 2)
       (valid-variable? (car (car (cdr expr))))
       (valid-expr? (car (cdr (car (cdr expr)))))
       ;; The body must be a valid Expr
       (valid-expr? (car (cdr (cdr expr))))))


;; ============================================================================
;; PART 3: EVALUATOR
;; ============================================================================
;;
;; MAJOR CHANGE: Every eval function now takes an env parameter.
;;
;; The environment threads through evaluation like this:
;;   - Numbers, booleans: ignore the env (self-evaluating)
;;   - Variables: LOOK UP the name in env
;;   - Arithmetic, comparisons: pass env to recursive eval calls
;;   - Var expressions: EXTEND the env, then eval body in new env
;;   - If expressions: pass env to condition and chosen branch
;;
;; Semantic rules from L13:
;;   E[[n]] σ            = n                                   (number)
;;   E[[x]] σ            = σ(x)                                (lookup)
;;   E[[(var (x e1) e2)]] σ = E[[e2]] (extend(σ, x, E[[e1]] σ)) (var)
;;   E[[(+ e1 e2)]] σ   = E[[e1]] σ  +  E[[e2]] σ            (arith)
;;


;; ----------------------------------------------------------------------------
;; eval-expr — main evaluator (now takes env)
;; ----------------------------------------------------------------------------

(define (eval-expr expr env)
  (cond
    ;; ---- Numbers: E[[n]] σ = n ----
    ((number? expr) expr)

    ;; ---- Arithmetic: E[[(+ e1 e2)]] σ = E[[e1]] σ + E[[e2]] σ ----
    ((and (pair? expr) (eq? (car expr) '+))
     (+ (eval-expr (car (cdr expr)) env)
        (eval-expr (car (cdr (cdr expr))) env)))

    ((and (pair? expr) (eq? (car expr) '-))
     (- (eval-expr (car (cdr expr)) env)
        (eval-expr (car (cdr (cdr expr))) env)))

    ((and (pair? expr) (eq? (car expr) '*))
     (* (eval-expr (car (cdr expr)) env)
        (eval-expr (car (cdr (cdr expr))) env)))

    ;; ---- Booleans: E[[true]] σ = true ----
    ((eq? expr 'true)  'true)
    ((eq? expr 'false) 'false)

    ;; ---- Conditionals: selective evaluation, pass env through ----
    ((and (pair? expr) (eq? (car expr) 'if))
     (if (eq? (eval-ccond (car (cdr expr)) env) 'true)
         (eval-expr (car (cdr (cdr expr))) env)
         (eval-expr (car (cdr (cdr (cdr expr)))) env)))

    ;; ---- Var expressions: E[[(var (x e1) e2)]] σ ----
    ;; 1. Evaluate e1 in the CURRENT environment
    ;; 2. Extend the environment with x → result
    ;; 3. Evaluate e2 (the body) in the EXTENDED environment
    ((and (pair? expr) (eq? (car expr) 'var))
     (eval-expr
      (car (cdr (cdr expr)))                         ;; e2 (body)
      (extend-env env
                  (car (car (cdr expr)))             ;; x (variable name)
                  (eval-expr (car (cdr (car (cdr expr)))) env)))) ;; E[[e1]] σ

    ;; ---- Variables: E[[x]] σ = σ(x) (lookup) ----
    ;; This must come AFTER all keyword checks (var, if, etc.)
    ;; If we reach here with a symbol, it must be a variable reference
    ((symbol? expr) (lookup env expr))

    (else (error "eval-expr: invalid expression" expr))))


;; ----------------------------------------------------------------------------
;; eval-ccond — evaluate compound conditions (now takes env)
;; ----------------------------------------------------------------------------

(define (eval-ccond expr env)
  (cond
    ((and (pair? expr) (eq? (car expr) 'and))
     (if (and (eq? (eval-ccond (car (cdr expr)) env) 'true)
              (eq? (eval-ccond (car (cdr (cdr expr))) env) 'true))
         'true 'false))

    ((and (pair? expr) (eq? (car expr) 'or))
     (if (or (eq? (eval-ccond (car (cdr expr)) env) 'true)
             (eq? (eval-ccond (car (cdr (cdr expr))) env) 'true))
         'true 'false))

    ((and (pair? expr) (eq? (car expr) 'not))
     (if (eq? (eval-ccond (car (cdr expr)) env) 'true)
         'false 'true))

    (else (eval-bcond expr env))))


;; ----------------------------------------------------------------------------
;; eval-bcond — evaluate basic conditions (now takes env)
;; ----------------------------------------------------------------------------

(define (eval-bcond expr env)
  (cond
    ((and (pair? expr) (eq? (car expr) 'gt))
     (if (> (eval-expr (car (cdr expr)) env)
            (eval-expr (car (cdr (cdr expr))) env))
         'true 'false))

    ((and (pair? expr) (eq? (car expr) 'lt))
     (if (< (eval-expr (car (cdr expr)) env)
            (eval-expr (car (cdr (cdr expr))) env))
         'true 'false))

    ((and (pair? expr) (eq? (car expr) 'eq))
     (if (= (eval-expr (car (cdr expr)) env)
            (eval-expr (car (cdr (cdr expr))) env))
         'true 'false))

    (else (error "eval-bcond: invalid condition" expr))))


;; ============================================================================
;; PART 4: INTEGRATION - RUN
;; ============================================================================
;;
;; Pipeline: validate → evaluate (with empty env) → display
;;

(define (display-result expr)
  (display "Expression: ")
  (display expr)
  (newline)
  (display "Result: ")
  (display (eval-expr expr (empty-env)))
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
     (display "  Expr    → AExpr | BExpr | IfExpr | Variable | VarExpr")
     (newline)
     (display "  VarExpr → (var (Variable Expr) Expr)")
     (newline)
     (display "  Variable→ symbol (not a reserved word)")
     (newline))))


;; ============================================================================
;; TEST CASES
;; ============================================================================
;;
;; We test each feature layer, focusing on the new variable functionality.
;; All previous features (arithmetic, booleans, comparisons, conditionals)
;; should still work, just with the environment threaded through.
;;
;; Run with: racket demo-interpreter3.rkt
;;


;; ============================================================================
;; LAYER 1: PREVIOUS FEATURES STILL WORK (with env threaded through)
;; ============================================================================

(display "========== LAYER 1: PREVIOUS FEATURES (still work) ==========")
(newline)

(display "Test 1.1: (run 5)")
(newline)
(display "Expected result: 5")
(newline)
(run 5)

(display "Test 1.2: (run '(+ 2 3))")
(newline)
(display "Expected result: 5")
(newline)
(run '(+ 2 3))

(display "Test 1.3: (run '(if (gt 5 3) (+ 2 3) (* 4 5)))")
(newline)
(display "Expected result: 5")
(newline)
(run '(if (gt 5 3) (+ 2 3) (* 4 5)))

(display "Test 1.4: (run 'true)")
(newline)
(display "Expected result: true")
(newline)
(run 'true)

(newline)


;; ============================================================================
;; LAYER 2: THE ENVIRONMENT DATA STRUCTURE
;; ============================================================================
;; Before we use variables in expressions, let's test the environment
;; functions directly to see how they work.

(display "========== LAYER 2: ENVIRONMENT DATA STRUCTURE ==========")
(newline)

;; Start with an empty environment
(display "Test 2.1: (empty-env)")
(newline)
(display "Expected: ()")
(newline)
(display "Got:      ")
(displayln (empty-env))

;; Extend with one binding
(display "Test 2.2: (extend-env (empty-env) 'x 5)")
(newline)
(display "Expected: ((x . 5))")
(newline)
(display "Got:      ")
(displayln (extend-env (empty-env) 'x 5))

;; Extend with two bindings — notice x→5 is added at the FRONT
(define env1 (extend-env (empty-env) 'x 5))
(define env2 (extend-env env1 'y 10))
(display "Test 2.3: env with x=5, y=10")
(newline)
(display "Expected: ((y . 10) (x . 5))")
(newline)
(display "Got:      ")
(displayln env2)

;; Lookup finds the right values
(display "Test 2.4: (lookup env2 'x)")
(newline)
(display "Expected: 5")
(newline)
(display "Got:      ")
(displayln (lookup env2 'x))

(display "Test 2.5: (lookup env2 'y)")
(newline)
(display "Expected: 10")
(newline)
(display "Got:      ")
(displayln (lookup env2 'y))

;; Shadowing: add a new x binding — lookup finds the NEWEST one
(define env3 (extend-env env2 'x 99))
(display "Test 2.6: Shadowing — (lookup env3 'x) after re-binding x to 99")
(newline)
(display "Expected: 99  [new x=99 shadows old x=5]")
(newline)
(display "Got:      ")
(displayln (lookup env3 'x))

(newline)


;; ============================================================================
;; LAYER 3: SIMPLE VARIABLE BINDING — (var (x e1) e2)
;; ============================================================================
;; (var (x 5) (+ x 3))
;;   → bind x to 5, then evaluate (+ x 3) with x=5
;;   → 5 + 3 = 8

(display "========== LAYER 3: SIMPLE VARIABLE BINDING ==========")
(newline)

;; The basic example from L13 slides
;; (var (x 3) (+ x 2))  →  5
(display "Test 3.1: (var (x 3) (+ x 2))")
(newline)
(display "Expected result: 5")
(newline)
(run '(var (x 3) (+ x 2)))

;; Variable used multiple times
;; (var (x 4) (* x x))  →  16
(display "Test 3.2: (var (x 4) (* x x))")
(newline)
(display "Expected result: 16")
(newline)
(run '(var (x 4) (* x x)))

;; Variable in arithmetic sub-expression
;; (var (y 10) (- y 3))  →  7
(display "Test 3.3: (var (y 10) (- y 3))")
(newline)
(display "Expected result: 7")
(newline)
(run '(var (y 10) (- y 3)))

;; Binding expression is itself a computation
;; (var (x (+ 2 3)) (* x 2))  →  x = 5, then 5 * 2 = 10
(display "Test 3.4: (var (x (+ 2 3)) (* x 2))")
(newline)
(display "Expected result: 10")
(newline)
(run '(var (x (+ 2 3)) (* x 2)))

(newline)


;; ============================================================================
;; LAYER 4: NESTED VAR — MULTIPLE VARIABLES IN SCOPE
;; ============================================================================
;; When var expressions are nested, each inner var can see
;; all the variables from outer scopes.

(display "========== LAYER 4: NESTED VAR (multiple variables) ==========")
(newline)

;; Two variables
;; (var (x 1) (var (y 2) (+ x y)))  →  3
(display "Test 4.1: (var (x 1) (var (y 2) (+ x y)))")
(newline)
(display "Expected result: 3")
(newline)
(run '(var (x 1) (var (y 2) (+ x y))))

;; Inner binding uses outer variable
;; (var (x 1) (var (y (+ x 1)) (+ x y)))  →  x=1, y=2, result=3
(display "Test 4.2: (var (x 1) (var (y (+ x 1)) (+ x y)))")
(newline)
(display "Expected result: 3")
(newline)
(run '(var (x 1) (var (y (+ x 1)) (+ x y))))

;; Exercise 1 from L13 slides!
;; (var (x 1) (var (y (+ x 1)) (var (x (+ y 1)) (+ y x))))
;; x=1 → y=(1+1)=2 → x=(2+1)=3 (shadows old x) → (+ 2 3) = 5
(display "Test 4.3: L13 Exercise 1 — nested vars with shadowing")
(newline)
(display "Expected result: 5")
(newline)
(run '(var (x 1) (var (y (+ x 1)) (var (x (+ y 1)) (+ y x)))))

;; Exercise 3 from L13 slides
;; (var (a 1) (var (b (+ a 2)) (var (c (+ b 3)) (+ a (+ b c)))))
;; a=1 → b=3 → c=6 → (+ 1 (+ 3 6)) = 10
(display "Test 4.4: L13 Exercise 3 — chain of dependent bindings")
(newline)
(display "Expected result: 10")
(newline)
(run '(var (a 1) (var (b (+ a 2)) (var (c (+ b 3)) (+ a (+ b c))))))

(newline)


;; ============================================================================
;; LAYER 5: SHADOWING
;; ============================================================================
;; When a var binds a name that is already bound in an outer scope,
;; the inner binding SHADOWS the outer one. After the inner scope ends,
;; the outer binding is restored (because environments are immutable).

(display "========== LAYER 5: SHADOWING ==========")
(newline)

;; (var (x 2) (var (x 5) (+ x 1)))
;; Outer x=2, inner x=5 shadows it → (+ 5 1) = 6
;; This is the example from L13 slide 17
(display "Test 5.1: (var (x 2) (var (x 5) (+ x 1)))")
(newline)
(display "Expected result: 6  [inner x=5 shadows outer x=2]")
(newline)
(run '(var (x 2) (var (x 5) (+ x 1))))

;; After inner scope, outer x is visible again
;; (var (x 10) (+ (var (x 99) x) x))
;; inner var returns 99, then outer x=10 → 99 + 10 = 109
(display "Test 5.2: (var (x 10) (+ (var (x 99) x) x))")
(newline)
(display "Expected result: 109  [inner x=99 in first operand, outer x=10 in second]")
(newline)
(run '(var (x 10) (+ (var (x 99) x) x)))

(newline)


;; ============================================================================
;; LAYER 6: VARIABLES WITH CONDITIONALS
;; ============================================================================
;; Variables work seamlessly with if expressions.

(display "========== LAYER 6: VARIABLES WITH CONDITIONALS ==========")
(newline)

;; Exercise 2 from L13 slides
;; (var (x 1) (var (y 2) (if (gt y x) (var (x (+ y 1)) (+ x y)) (+ x y))))
;; x=1, y=2, (gt 2 1) → true, then-branch: x=(2+1)=3, (+ 3 2) = 5
(display "Test 6.1: L13 Exercise 2 — variables with if")
(newline)
(display "Expected result: 5")
(newline)
(run '(var (x 1) (var (y 2) (if (gt y x) (var (x (+ y 1)) (+ x y)) (+ x y)))))

;; Variable in condition operands
;; (var (x 10) (var (y 5) (if (gt x y) (+ x y) (- x y))))
;; x=10, y=5, 10>5 → true → 10+5 = 15
(display "Test 6.2: (var (x 10) (var (y 5) (if (gt x y) ...)))")
(newline)
(display "Expected result: 15")
(newline)
(run '(var (x 10) (var (y 5) (if (gt x y) (+ x y) (- x y)))))

;; Var inside a branch
;; (if (gt 5 3) (var (x 42) x) 0)  →  true → x=42 → 42
(display "Test 6.3: (if (gt 5 3) (var (x 42) x) 0)")
(newline)
(display "Expected result: 42")
(newline)
(run '(if (gt 5 3) (var (x 42) x) 0))

(newline)


;; ============================================================================
;; LAYER 7: FREE VARIABLES (errors)
;; ============================================================================
;; A variable that has no binding is FREE. The environment model
;; detects this naturally: lookup reaches the end of the list → error.
;;
;; Note: The syntax checker accepts free variables (they're valid syntax).
;; The error happens at EVALUATION TIME when lookup fails.

(display "========== LAYER 7: FREE VARIABLES (runtime errors) ==========")
(newline)

;; valid-expr? accepts a lone variable (it's syntactically valid)
(display "Test 7.1: (valid-expr? 'x)")
(newline)
(display "Expected: #t  [syntactically valid, but will error at eval time]")
(newline)
(display "Got:      ")
(displayln (valid-expr? 'x))

;; Demonstrate that evaluating a free variable causes an error
;; We catch it to keep the test suite running
(display "Test 7.2: (eval-expr 'z (empty-env))  [z is free → error]")
(newline)
(display "Expected: error — unbound variable z")
(newline)
(display "Got:      ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (eval-expr 'z (empty-env)))

;; x is bound but y is free → error when evaluating y
(display "Test 7.3: (var (x 5) (+ x y))  [y is free → error]")
(newline)
(display "Expected: error — unbound variable y")
(newline)
(display "Got:      ")
(with-handlers ([exn:fail? (lambda (e) (displayln (exn-message e)))])
  (eval-expr '(var (x 5) (+ x y)) (empty-env)))

(newline)


;; ============================================================================
;; LAYER 8: SYNTAX ERROR CASES
;; ============================================================================

(display "========== SYNTAX ERROR CASES ==========")
(newline)

;; var without binding
(display "Test 8.1: (run '(var x))  [wrong structure]")
(newline)
(run '(var x))
(newline)

;; Reserved word as variable name
(display "Test 8.2: (run '(var (true 5) (+ true 1)))  [true is reserved]")
(newline)
(run '(var (true 5) (+ true 1)))
(newline)

;; Missing body
(display "Test 8.3: (run '(var (x 5)))  [missing body]")
(newline)
(run '(var (x 5)))
(newline)


;; ============================================================================
;; LAYER 9: PUTTING IT ALL TOGETHER
;; ============================================================================
;; Complex expressions combining all features.

(display "========== PUTTING IT ALL TOGETHER ==========")
(newline)

;; Exercise 4 from L13: variables + conditionals + free variable in else branch
;; (var (x 10) (var (y 3) (if (gt x y) (var (z (+ x y)) (+ z x)) (+ x z))))
;; x=10, y=3, 10>3 → true → z=13, (+ 13 10) = 23
;; (The else branch has free z, but it's never evaluated!)
(display "Test 9.1: L13 Exercise 4 — true branch taken, else has free z")
(newline)
(display "Expected result: 23")
(newline)
(run '(var (x 10) (var (y 3) (if (gt x y) (var (z (+ x y)) (+ z x)) (+ x y)))))

;; Nested var with arithmetic chain
;; (var (base 100) (var (bonus (+ base 50)) (- bonus base)))
;; base=100, bonus=150, 150-100 = 50
(display "Test 9.2: (var (base 100) (var (bonus (+ base 50)) (- bonus base)))")
(newline)
(display "Expected result: 50")
(newline)
(run '(var (base 100) (var (bonus (+ base 50)) (- bonus base))))

;; Var inside arithmetic operand position
;; (+ (var (x 3) (* x x)) (var (y 4) (* y y)))
;; first operand: x=3, 3*3=9; second operand: y=4, 4*4=16; 9+16=25
(display "Test 9.3: (+ (var (x 3) (* x x)) (var (y 4) (* y y)))")
(newline)
(display "Expected result: 25  [9 + 16]")
(newline)
(run '(+ (var (x 3) (* x x)) (var (y 4) (* y y))))

;; Variable in comparison inside if
;; (var (age 20) (if (gt age 18) (+ age 0) 0))
(display "Test 9.4: (var (age 20) (if (gt age 18) age 0))")
(newline)
(display "Expected result: 20")
(newline)
(run '(var (age 20) (if (gt age 18) age 0)))

(newline)
(display "========== ALL TESTS COMPLETE ==========")
(newline)
