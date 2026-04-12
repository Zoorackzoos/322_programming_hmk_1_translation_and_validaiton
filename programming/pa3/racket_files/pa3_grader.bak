#lang racket

;; ================================================================
;; CSCE 322 - PA3 Auto-Grader
;;
;; Usage:
;;   racket pa3_grader.rkt              (grades ./pa1.rkt and ./pa3.rkt)
;;   racket pa3_grader.rkt path/to/dir
;;
;; Point breakdown (matches rubric in pa3.pdf):
;;   Parser updates (PA1):                20 pts
;;   Variable eval & env lookup:          30 pts
;;   Var binding eval & shadowing:        30 pts
;;   Error propagation:                   20 pts
;;   Total:                              100 pts
;; ================================================================

(require racket/string)

;; ================================================================
;; Submission loading
;; ================================================================

(define submission-dir
  (if (> (vector-length (current-command-line-arguments)) 0)
      (vector-ref (current-command-line-arguments) 0)
      "."))

(define pa1-path (build-path submission-dir "pa1.rkt"))
(define pa3-path (build-path submission-dir "pa3.rkt"))

(define load-success? #f)
(define load-error-msg "")

(define student-validate #f)
(define student-translate #f)
(define student-evaluate-with-env #f)

(with-handlers ([exn:fail? (lambda (e) (set! load-error-msg (exn-message e)))])
  (parameterize ([current-output-port (open-output-string)]
                 [current-error-port  (open-output-string)])
    (dynamic-require pa1-path 0)
    (dynamic-require pa3-path 0))
  (set! load-success? #t))

(when load-success?
  (set! student-validate
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (dynamic-require pa1-path 'validate-program)))
  (set! student-translate
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (dynamic-require pa1-path 'infix->prefix)))
  (set! student-evaluate-with-env
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (dynamic-require pa3-path 'evaluate-with-env))))

;; ================================================================
;; Test infrastructure
;; ================================================================

(struct test-result
  (category name input expected actual passed? points max-points)
  #:transparent)

(define all-results '())

(define TIMEOUT-SECONDS 5)

(define (safe-call fn . args)
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
                (list 'OK (apply fn args)))))))
        (define result (sync/timeout TIMEOUT-SECONDS ch))
        (cond
          [(not result)
           (kill-thread thd)
           (list 'EXCEPTION
                 (format "timeout (>~as) — possible infinite loop" TIMEOUT-SECONDS))]
          [(and (list? result) (equal? (car result) 'OK))
           (cadr result)]
          [else result]))))

(define (run-test category name fn args expected max-pts)
  (define actual (apply safe-call fn args))
  (define passed? (equal? actual expected))
  (define pts (if passed? max-pts 0))
  (define r (test-result category name args expected actual passed? pts max-pts))
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
  (printf "  Submission: ~a\n" (path->string (path->complete-path submission-dir)))
  (printf "================================================================\n")

  (cond
    [(not load-success?)
     (printf "\n  FATAL: could not load submission\n")
     (printf "  ~a\n\n" load-error-msg)
     (printf "  Score: 0 / 100  (auto-graded)\n\n")]
    [else
     (run-parser-tests)
     (run-eval-var-tests)
     (run-eval-binding-tests)
     (run-error-tests)
     (print-summary)]))

;; ────────────────────────────────────────
;; Parser updates (PA1) (20 pts)
;; ────────────────────────────────────────

(define (run-parser-tests)
  ;; --- Validation of variables and var forms (9 pts) ---
  (run-test "Parser" "validate variable x"
            student-validate '(x) #t 1)
  (run-test "Parser" "validate variable y"
            student-validate '(y) #t 1)
  (run-test "Parser" "validate (var (x 5) x)"
            student-validate '((var (x 5) x)) #t 2)
  (run-test "Parser" "validate (var (x (1 + 2)) (x * 3))"
            student-validate '((var (x (1 + 2)) (x * 3))) #t 2)
  (run-test "Parser" "validate nested var binding"
            student-validate '((var (x 1) (var (y 2) (x + y)))) #t 2)
  (run-test "Parser" "validate var with boolean body"
            student-validate '((var (x true) (x && false))) #t 1)

  ;; --- Translation of variables and var forms (11 pts) ---
  (run-test "Parser" "translate variable x"
            student-translate '(x) 'x 1)
  (run-test "Parser" "translate variable y"
            student-translate '(y) 'y 1)
  (run-test "Parser" "translate (var (x 5) x)"
            student-translate '((var (x 5) x)) '(var (x 5) x) 1)
  (run-test "Parser" "translate (var (x (1 + 2)) (x * 3))"
            student-translate '((var (x (1 + 2)) (x * 3))) '(var (x (+ 1 2)) (* x 3)) 3)
  (run-test "Parser" "translate nested var binding"
            student-translate '((var (x 1) (var (y 2) (x + y)))) '(var (x 1) (var (y 2) (+ x y))) 3)
  (run-test "Parser" "translate var with boolean body"
            student-translate '((var (x true) (x && false))) '(var (x true) (and x false)) 2))

;; ────────────────────────────────────────
;; Variable eval & env lookup (30 pts)
;; ────────────────────────────────────────

(define (run-eval-var-tests)
  ;; --- Basic env lookup (15 pts) ---
  (run-test "EvalVar" "lookup simple variable x"
            student-evaluate-with-env (list 'x '((x 5))) 5 3)
  (run-test "EvalVar" "lookup variable y past x"
            student-evaluate-with-env (list 'y '((x 5) (y 10))) 10 3)
  (run-test "EvalVar" "front-of-env wins over later binding"
            student-evaluate-with-env (list 'x '((x 5) (x 99))) 5 3)
  (run-test "EvalVar" "lookup boolean-valued variable"
            student-evaluate-with-env (list 'b '((b true))) 'true 3)
  (run-test "EvalVar" "lookup variable bound to 0"
            student-evaluate-with-env (list 'zero '((zero 0))) 0 3)

  ;; --- Variables inside expressions (15 pts) ---
  (run-test "EvalVar" "(x + 1) with x=5"
            student-evaluate-with-env (list '(x + 1) '((x 5))) 6 3)
  (run-test "EvalVar" "(x * y) with x=3 y=4"
            student-evaluate-with-env (list '(x * y) '((x 3) (y 4))) 12 3)
  (run-test "EvalVar" "flat (x + y + 1) with x=2 y=3"
            student-evaluate-with-env (list '(x + y + 1) '((x 2) (y 3))) 6 3)
  (run-test "EvalVar" "(x && y) with x=true y=false"
            student-evaluate-with-env (list '(x && y) '((x true) (y false))) 'false 3)
  (run-test "EvalVar" "nested ((x + y) * z) with x=1 y=2 z=3"
            student-evaluate-with-env (list '((x + y) * z) '((x 1) (y 2) (z 3))) 9 3))

;; ────────────────────────────────────────
;; Var binding eval & shadowing (30 pts)
;; ────────────────────────────────────────

(define (run-eval-binding-tests)
  ;; --- Basic var bindings (12 pts) ---
  (run-test "EvalBinding" "(var (x 5) x)"
            student-evaluate-with-env (list '(var (x 5) x) '()) 5 3)
  (run-test "EvalBinding" "(var (x (1 + 2)) (x * 3))"
            student-evaluate-with-env (list '(var (x (1 + 2)) (x * 3)) '()) 9 3)
  (run-test "EvalBinding" "(var (x 5) (x + 10))"
            student-evaluate-with-env (list '(var (x 5) (x + 10)) '()) 15 3)
  (run-test "EvalBinding" "(var (x true) (x && false))"
            student-evaluate-with-env (list '(var (x true) (x && false)) '()) 'false 3)

  ;; --- Nested bindings & shadowing (18 pts) ---
  (run-test "EvalBinding" "nested no shadow (x 5)(y 10)"
            student-evaluate-with-env (list '(var (x 5) (var (y 10) (x + y))) '()) 15 4)
  (run-test "EvalBinding" "inner shadows outer"
            student-evaluate-with-env (list '(var (x 5) (var (x 10) x)) '()) 10 4)
  (run-test "EvalBinding" "outer restored after inner scope"
            student-evaluate-with-env (list '(var (x 5) ((var (x 10) x) + x)) '()) 15 5)
  (run-test "EvalBinding" "three-deep nested bindings"
            student-evaluate-with-env (list '(var (x 1) (var (y 2) (var (z 3) (x + y + z)))) '()) 6 5))

;; ────────────────────────────────────────
;; Error propagation (20 pts)
;; ────────────────────────────────────────

(define (run-error-tests)
  ;; --- Free variable errors (9 pts) ---
  (run-test "Error" "bare free variable"
            student-evaluate-with-env (list 'x '()) '(err "free variable") 2)
  (run-test "Error" "free variable in expression"
            student-evaluate-with-env (list '(x + 1) '()) '(err "free variable") 2)
  (run-test "Error" "free variable in var body"
            student-evaluate-with-env (list '(var (x 5) (y + 1)) '()) '(err "free variable") 3)
  (run-test "Error" "free variable in binding expr"
            student-evaluate-with-env (list '(var (x (y + 1)) x) '()) '(err "free variable") 2)

  ;; --- Type errors propagated through var (6 pts) ---
  (run-test "Error" "type error (num + bool) in body"
            student-evaluate-with-env (list '(var (x 5) (x + true)) '()) '(err "type error") 3)
  (run-test "Error" "type error (bool + num) in body"
            student-evaluate-with-env (list '(var (b true) (b + 1)) '()) '(err "type error") 3)

  ;; --- Division by zero propagated through var (5 pts) ---
  (run-test "Error" "div by zero in binding expr"
            student-evaluate-with-env (list '(var (x (1 / 0)) x) '()) '(err "division by zero") 3)
  (run-test "Error" "div by zero in body"
            student-evaluate-with-env (list '(var (x 5) (x / 0)) '()) '(err "division by zero") 2))

;; ================================================================
;; Summary
;; ================================================================

(define (print-summary)
  (define categories
    '(("Parser"      "Parser updates (PA1) (20 pts)")
      ("EvalVar"     "Variable eval & env lookup (30 pts)")
      ("EvalBinding" "Var binding eval & shadowing (30 pts)")
      ("Error"       "Error propagation (20 pts)")))

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
  (printf "  ────────────────────────────\n")
  (printf "  TOTAL:         ~a / 100\n" grand-earned)
  (printf "================================================================\n\n"))

;; ================================================================
;; Entry point
;; ================================================================

(run-all-tests)
