#lang racket

;; ================================================================
;; CSCE 322 - PA3 Auto-Grader
;;
;; Usage:
;;   racket pa3_grader.rkt              (grades ./pa3.rkt)
;;   racket pa3_grader.rkt path/to/pa3.rkt
;;
;; Point breakdown (matches rubric):
;;   Parser Updates (PA1):                  20 pts
;;   Variable Evaluation & Env Lookup:      30 pts
;;   Var Binding & Shadowing:               30 pts
;;   Error Propagation:                     20 pts
;;   Style:                                 10 pts  (manual — not auto-graded)
;; ================================================================

(require racket/string)

;; ================================================================
;; Submission loading
;; ================================================================

(define submission-path
  (path->complete-path
   (if (> (vector-length (current-command-line-arguments)) 0)
       (vector-ref (current-command-line-arguments) 0)
       "pa3.rkt")))

(define load-success? #f)
(define load-error-msg "")

(define student-evaluate-with-env #f)
(define student-validate-program  #f)
(define student-infix->prefix      #f)

(with-handlers ([exn:fail? (lambda (e) (set! load-error-msg (exn-message e)))])
  (parameterize ([current-output-port (open-output-string)]
                 [current-error-port  (open-output-string)])
    (dynamic-require submission-path 0))
  (set! load-success? #t))

(when load-success?
  (set! student-evaluate-with-env
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (dynamic-require submission-path 'evaluate-with-env)))
  (set! student-validate-program
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (dynamic-require submission-path 'validate-program)))
  (set! student-infix->prefix
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (dynamic-require submission-path 'infix->prefix))))

;; ================================================================
;; Test infrastructure
;; ================================================================

(struct test-result
  (category name input expected actual passed? points max-points)
  #:transparent)

(define all-results '())

(define TIMEOUT-SECONDS 5)

(define (safe-call fn arg)
  (if (not fn)
      (list 'EXCEPTION "function not exported or not defined")
      (let ([ch (make-channel)])
        (define thd
          (thread
           (lambda ()
             (channel-put
              ch
              (with-handlers ([exn:fail? (lambda (e)
                                           (list 'EXCEPTION (exn-message e)))])
                (list 'OK (fn arg)))))))
        (define result (sync/timeout TIMEOUT-SECONDS ch))
        (cond
          [(not result)
           (kill-thread thd)
           (list 'EXCEPTION
                 (format "timeout (>~as) — possible infinite loop" TIMEOUT-SECONDS))]
          [(and (list? result) (equal? (car result) 'OK))
           (cadr result)]
          [else result]))))

;; For evaluate-with-env, which takes TWO args
(define (safe-call-2 fn arg1 arg2)
  (if (not fn)
      (list 'EXCEPTION "function not exported or not defined")
      (let ([ch (make-channel)])
        (define thd
          (thread
           (lambda ()
             (channel-put
              ch
              (with-handlers ([exn:fail? (lambda (e)
                                           (list 'EXCEPTION (exn-message e)))])
                (list 'OK (fn arg1 arg2)))))))
        (define result (sync/timeout TIMEOUT-SECONDS ch))
        (cond
          [(not result)
           (kill-thread thd)
           (list 'EXCEPTION
                 (format "timeout (>~as) — possible infinite loop" TIMEOUT-SECONDS))]
          [(and (list? result) (equal? (car result) 'OK))
           (cadr result)]
          [else result]))))

(define (run-test category name fn input expected max-pts)
  (define actual (safe-call fn input))
  (define passed? (equal? actual expected))
  (define pts (if passed? max-pts 0))
  (define r (test-result category name input expected actual passed? pts max-pts))
  (set! all-results (append all-results (list r)))
  r)

(define (run-test-2 category name fn arg1 arg2 expected max-pts)
  (define actual (safe-call-2 fn arg1 arg2))
  (define passed? (equal? actual expected))
  (define pts (if passed? max-pts 0))
  ;; Store arg1 as the "input" for display purposes
  (define r (test-result category name (list arg1 'ENV: arg2) expected actual passed? pts max-pts))
  (set! all-results (append all-results (list r)))
  r)

;; ================================================================
;; Display helpers
;; ================================================================

(define (format-val v)
  (cond
    [(and (list? v) (pair? v) (equal? (car v) 'EXCEPTION))
     (format "EXCEPTION: ~a" (cadr v))]
    [else (format "~v" v)]))

(define (print-test r)
  (if (test-result-passed? r)
      (printf "  [PASS] ~a  (~a/~a)\n"
              (test-result-name r)
              (test-result-points r)
              (test-result-max-points r))
      (begin
        (printf "  [FAIL] ~a  (0/~a)\n"
                (test-result-name r)
                (test-result-max-points r))
        (printf "         input:    ~a\n" (format-val (test-result-input r)))
        (printf "         expected: ~a\n" (format-val (test-result-expected r)))
        (printf "         actual:   ~a\n" (format-val (test-result-actual r))))))

;; ================================================================
;; Test suite
;; ================================================================

(define (run-all-tests)
  (printf "\n================================================================\n")
  (printf "  CSCE 322 — PA3 Auto-Grader\n")
  (printf "  Submission: ~a\n" (path->string submission-path))
  (printf "================================================================\n")

  (cond
    [(not load-success?)
     (printf "\n  FATAL: could not load submission\n")
     (printf "  ~a\n\n" load-error-msg)
     (printf "  Score: 0 / 90  (auto-graded)\n")
     (printf "  Style: __ / 10 (manual)\n\n")]
    [else
     (run-parser-tests)
     (run-variable-eval-tests)
     (run-var-binding-tests)
     (run-error-propagation-tests)
     (print-summary)]))

;; ────────────────────────────────────────
;; Parser tests (20 pts)
;;   validate-program: recognizes variables and var bindings
;;   infix->prefix: correctly translates them
;; ────────────────────────────────────────

(define (run-parser-tests)
  ;; validate-program: variables are valid (5 pts)
  (run-test "Parser" "validate: bare variable x"
            student-validate-program 'x #t 1)
  (run-test "Parser" "validate: bare variable y"
            student-validate-program 'y #t 1)
  (run-test "Parser" "validate: var binding (var (x 1) x)"
            student-validate-program '(var (x 1) x) #t 1)
  (run-test "Parser" "validate: var with expr body"
            student-validate-program '(var (x (1 + 2)) (x * 3)) #t 1)
  (run-test "Parser" "validate: nested var"
            student-validate-program '(var (x 1) (var (y 2) (x + y))) #t 1)

  ;; infix->prefix: variables pass through unchanged (5 pts)
  (run-test "Parser" "infix->prefix: bare variable x"
            student-infix->prefix 'x 'x 1)
  (run-test "Parser" "infix->prefix: bare variable myVar"
            student-infix->prefix 'myVar 'myVar 1)
  (run-test "Parser" "infix->prefix: var binding simple"
            student-infix->prefix '(var (x 5) x) '(var (x 5) x) 1)
  (run-test "Parser" "infix->prefix: var with infix binding expr"
            student-infix->prefix '(var (x (1 + 2)) (x * 3))
            '(var (x (+ 1 2)) (* x 3)) 1)
  (run-test "Parser" "infix->prefix: nested var"
            student-infix->prefix '(var (x (1 + 2)) (var (y (x * 2)) (x + y)))
            '(var (x (+ 1 2)) (var (y (* x 2)) (+ x y))) 1)

  ;; infix->prefix: variables inside arithmetic (5 pts)
  (run-test "Parser" "infix->prefix: variable in arithmetic"
            student-infix->prefix '(x + 1) '(+ x 1) 1)
  (run-test "Parser" "infix->prefix: variable both sides"
            student-infix->prefix '(x + y) '(+ x y) 1)
  (run-test "Parser" "infix->prefix: variable with comparison"
            student-infix->prefix '(x < 10) '(< x 10) 1)
  (run-test "Parser" "infix->prefix: variable with boolean op"
            student-infix->prefix '(x == y) '(== x y) 1)
  (run-test "Parser" "infix->prefix: var binding with nested body"
            student-infix->prefix '(var (x (2 * 3)) ((x + 1) < 10))
            '(var (x (* 2 3)) (< (+ x 1) 10)) 1))

;; ────────────────────────────────────────
;; Variable evaluation & env lookup (30 pts)
;; ────────────────────────────────────────

(define empty-env '())
(define simple-env '((x 5) (y 10) (z 0)))

(define (run-variable-eval-tests)
  ;; Literals still work with env (6 pts)
  (run-test-2 "Variable Eval" "literal 7 with empty env"
              student-evaluate-with-env 7 empty-env 7 2)
  (run-test-2 "Variable Eval" "literal true with env"
              student-evaluate-with-env 'true simple-env 'true 2)
  (run-test-2 "Variable Eval" "literal 0 with env"
              student-evaluate-with-env 0 simple-env 0 2)

  ;; Variable lookup (12 pts)
  (run-test-2 "Variable Eval" "lookup x in ((x 5))"
              student-evaluate-with-env 'x '((x 5)) 5 3)
  (run-test-2 "Variable Eval" "lookup y in simple-env"
              student-evaluate-with-env 'y simple-env 10 3)
  (run-test-2 "Variable Eval" "lookup z in simple-env (value 0)"
              student-evaluate-with-env 'z simple-env 0 3)
  (run-test-2 "Variable Eval" "lookup first binding shadows second"
              student-evaluate-with-env 'x '((x 99) (x 1)) 99 3)

  ;; Variable in expressions (12 pts)
  (run-test-2 "Variable Eval" "x + 1 with x=5"
              student-evaluate-with-env '(+ x 1) simple-env 6 3)
  (run-test-2 "Variable Eval" "x * y with x=5 y=10"
              student-evaluate-with-env '(* x y) simple-env 50 3)
  (run-test-2 "Variable Eval" "x < y with x=5 y=10"
              student-evaluate-with-env '(< x y) simple-env 'true 3)
  (run-test-2 "Variable Eval" "(x + y) * z with z=0"
              student-evaluate-with-env '(* (+ x y) z) simple-env 0 3))

;; ────────────────────────────────────────
;; Var binding & shadowing (30 pts)
;; ────────────────────────────────────────

(define (run-var-binding-tests)
  ;; Simple var bindings (12 pts)
  (run-test-2 "Var Binding" "(var (x 5) x)"
              student-evaluate-with-env '(var (x 5) x) empty-env 5 3)
  (run-test-2 "Var Binding" "(var (x (+ 1 2)) (* x 3))"
              student-evaluate-with-env '(var (x (+ 1 2)) (* x 3)) empty-env 9 3)
  (run-test-2 "Var Binding" "(var (x 4) (< x 10))"
              student-evaluate-with-env '(var (x 4) (< x 10)) empty-env 'true 3)
  (run-test-2 "Var Binding" "var body uses outer env too"
              student-evaluate-with-env '(var (a 3) (+ a y)) simple-env 13 3)

  ;; Nested var (12 pts)
  (run-test-2 "Var Binding" "nested: (var (x 1) (var (y 2) (+ x y)))"
              student-evaluate-with-env
              '(var (x 1) (var (y 2) (+ x y))) empty-env 3 4)
  (run-test-2 "Var Binding" "nested: binding expr uses outer var"
              student-evaluate-with-env
              '(var (x 5) (var (y (* x 2)) y)) empty-env 10 4)
  (run-test-2 "Var Binding" "nested: three levels"
              student-evaluate-with-env
              '(var (a 1) (var (b 2) (var (c 3) (+ a (+ b c))))) empty-env 6 4)

  ;; Shadowing (6 pts)
  (run-test-2 "Var Binding" "shadowing: inner x hides outer x"
              student-evaluate-with-env
              '(var (x 10) (var (x 99) x)) empty-env 99 3)
  (run-test-2 "Var Binding" "shadowing: outer x still accessible after inner scope in body"
              student-evaluate-with-env
              '(var (x 1) (var (y (+ x 1)) x)) empty-env 1 3))

;; ────────────────────────────────────────
;; Error propagation (20 pts)
;; ────────────────────────────────────────

(define (run-error-propagation-tests)
  ;; Free variable errors (6 pts)
  (run-test-2 "Error" "free variable: x not in env"
              student-evaluate-with-env 'x empty-env '(err "free variable") 2)
  (run-test-2 "Error" "free variable: z not in ((x 1))"
              student-evaluate-with-env 'z '((x 1)) '(err "free variable") 2)
  (run-test-2 "Error" "free variable inside expression"
              student-evaluate-with-env '(+ x 1) empty-env '(err "free variable") 2)

  ;; Type errors still work with env (6 pts)
  (run-test-2 "Error" "type error: (+ x true) with x=5"
              student-evaluate-with-env '(+ x true) simple-env '(err "type error") 2)
  (run-test-2 "Error" "type error: (&& 1 2) no env"
              student-evaluate-with-env '(&& 1 2) empty-env '(err "type error") 2)
  (run-test-2 "Error" "type error: (! x) with x=5"
              student-evaluate-with-env '(! x) simple-env '(err "type error") 2)

  ;; Division by zero still works (4 pts)
  (run-test-2 "Error" "div by zero: (/ x z) with x=5 z=0"
              student-evaluate-with-env '(/ x z) simple-env '(err "division by zero") 2)
  (run-test-2 "Error" "div by zero: (/ 1 0) no env"
              student-evaluate-with-env '(/ 1 0) empty-env '(err "division by zero") 2)

  ;; Error propagation through var (4 pts)
  (run-test-2 "Error" "error in binding expr propagates"
              student-evaluate-with-env
              '(var (x (/ 1 0)) (+ x 1)) empty-env '(err "division by zero") 2)
  (run-test-2 "Error" "free variable error in body propagates"
              student-evaluate-with-env
              '(var (x 5) (+ x unbound)) empty-env '(err "free variable") 2))

;; ================================================================
;; Summary
;; ================================================================

(define (print-summary)
  (define categories
    '(("Parser"       "Parser Updates PA1 (20 pts)")
      ("Variable Eval" "Variable Evaluation & Env Lookup (30 pts)")
      ("Var Binding"  "Var Binding & Shadowing (30 pts)")
      ("Error"        "Error Propagation (20 pts)")))

  (define grand-earned 0)
  (define grand-total  0)
  (define grand-passed 0)
  (define grand-tests  0)

  (for ([cat-pair categories])
    (define cat   (car cat-pair))
    (define label (cadr cat-pair))
    (define cat-results
      (filter (lambda (r) (string=? (test-result-category r) cat))
              all-results))
    (define cat-earned (apply + (map test-result-points cat-results)))
    (define cat-total  (apply + (map test-result-max-points cat-results)))
    (define cat-passed (length (filter test-result-passed? cat-results)))
    (define cat-tests  (length cat-results))

    (printf "\n── ~a ── ~a/~a pts  (~a/~a passed)\n"
            label cat-earned cat-total cat-passed cat-tests)
    (for ([r cat-results])
      (print-test r))

    (set! grand-earned (+ grand-earned cat-earned))
    (set! grand-total  (+ grand-total cat-total))
    (set! grand-passed (+ grand-passed cat-passed))
    (set! grand-tests  (+ grand-tests cat-tests)))

  (printf "\n================================================================\n")
  (printf "  Auto-graded:  ~a / ~a pts   (~a/~a tests passed)\n"
          grand-earned grand-total grand-passed grand-tests)
  (printf "  Style/clarity: __ / 10 pts  (grade manually)\n")
  (printf "  ────────────────────────────\n")
  (printf "  TOTAL:         ~a + __ / 110\n" grand-earned)
  (printf "================================================================\n\n"))

;; ================================================================
;; Entry point
;; ================================================================

(run-all-tests)
