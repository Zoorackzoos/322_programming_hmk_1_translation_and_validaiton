#lang racket

;; ================================================================
;; CSCE 322 - PA2 Auto-Grader
;;
;; Usage:
;;   racket pa2_grader.rkt              (grades ./pa2.rkt)
;;   racket pa2_grader.rkt path/to/pa2.rkt
;;
;; Point breakdown (matches rubric):
;;   Evaluation:          50 pts
;;   Type Error:          30 pts
;;   Div Zero:            10 pts
;;   Style:               10 pts  (manual — not auto-graded)
;; ================================================================

(require racket/string)

;; ================================================================
;; Submission loading
;; ================================================================

(define submission-path
  (path->complete-path
   (if (> (vector-length (current-command-line-arguments)) 0)
       (vector-ref (current-command-line-arguments) 0)
       "pa2.rkt")))

(define load-success? #f)
(define load-error-msg "")

(define student-evaluate #f)

(with-handlers ([exn:fail? (lambda (e) (set! load-error-msg (exn-message e)))])
  (parameterize ([current-output-port (open-output-string)]
                 [current-error-port  (open-output-string)])
    (dynamic-require submission-path 0))
  (set! load-success? #t))

(when load-success?
  (set! student-evaluate
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (dynamic-require submission-path 'evaluate-program))))

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

(define (run-test category name fn input expected max-pts)
  (define actual (safe-call fn input))
  (define passed? (equal? actual expected))
  (define pts (if passed? max-pts 0))
  (define r (test-result category name input expected actual passed? pts max-pts))
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
  (printf "  CSCE 322 — PA2 Auto-Grader\n")
  (printf "  Submission: ~a\n" (path->string submission-path))
  (printf "================================================================\n")

  (cond
    [(not load-success?)
     (printf "\n  FATAL: could not load submission\n")
     (printf "  ~a\n\n" load-error-msg)
     (printf "  Score: 0 / 90  (auto-graded)\n")
     (printf "  Style: __ / 10 (manual)\n\n")]
    [else
     (run-evaluation-tests)
     (run-type-error-tests)
     (run-div-zero-tests)
     (print-summary)]))

;; ────────────────────────────────────────
;; Evaluation tests (50 pts)
;; ────────────────────────────────────────

(define (run-evaluation-tests)
  ;; Literals (4 pts)
  (run-test "Evaluation" "literal 5" student-evaluate 5 5 1)
  (run-test "Evaluation" "literal true" student-evaluate 'true 'true 1)
  (run-test "Evaluation" "literal false" student-evaluate 'false 'false 1)
  (run-test "Evaluation" "literal 0" student-evaluate 0 0 1)

  ;; Arithmetic (10 pts)
  (run-test "Evaluation" "(1 + 2)" student-evaluate '(1 + 2) 3 2)
  (run-test "Evaluation" "(5 - 2)" student-evaluate '(5 - 2) 3 2)
  (run-test "Evaluation" "(3 * 4)" student-evaluate '(3 * 4) 12 2)
  (run-test "Evaluation" "(8 / 2)" student-evaluate '(8 / 2) 4 2)
  (run-test "Evaluation" "(- 5)" student-evaluate '(- 5) -5 2)

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

  ;; Complex Expressions (16 pts)
  (run-test "Evaluation" "(1 + 2 * 3)" student-evaluate '(1 + 2 * 3) 7 4)
  (run-test "Evaluation" "((1 + 2) * 3)" student-evaluate '((1 + 2) * 3) 9 4)
  (run-test "Evaluation" "(false || (! false))" student-evaluate '(false || (! false)) 'true 4)
  (run-test "Evaluation" "((2 * 3) < 7)" student-evaluate '((2 * 3) < 7) 'true 4))

;; ────────────────────────────────────────
;; Type Error tests (30 pts)
;; ────────────────────────────────────────

(define (run-type-error-tests)
  (run-test "Type Error" "(1 + true)" student-evaluate '(1 + true) '(err "type error") 3)
  (run-test "Type Error" "(false - 2)" student-evaluate '(false - 2) '(err "type error") 3)
  (run-test "Type Error" "(1 && 2)" student-evaluate '(1 && 2) '(err "type error") 3)
  (run-test "Type Error" "(true < false)" student-evaluate '(true < false) '(err "type error") 3)
  (run-test "Type Error" "(! 5)" student-evaluate '(! 5) '(err "type error") 3)
  (run-test "Type Error" "(- true)" student-evaluate '(- true) '(err "type error") 3)
  (run-test "Type Error" "(1 == true)" student-evaluate '(1 == true) '(err "type error") 3)
  (run-test "Type Error" "(false != 0)" student-evaluate '(false != 0) '(err "type error") 3)
  (run-test "Type Error" "propagated (1 + (2 && 3))" student-evaluate '(1 + (2 && 3)) '(err "type error") 3)
  (run-test "Type Error" "propagated (! (1 + true))" student-evaluate '(! (1 + true)) '(err "type error") 3))

;; ────────────────────────────────────────
;; Division by Zero tests (10 pts)
;; ────────────────────────────────────────

(define (run-div-zero-tests)
  (run-test "Div Zero" "(1 / 0)" student-evaluate '(1 / 0) '(err "division by zero") 3)
  (run-test "Div Zero" "(5 / (2 - 2))" student-evaluate '(5 / (2 - 2)) '(err "division by zero") 4)
  (run-test "Div Zero" "propagated (1 + (2 / 0))" student-evaluate '(1 + (2 / 0)) '(err "division by zero") 3))

;; ================================================================
;; Summary
;; ================================================================

(define (print-summary)
  (define categories
    '(("Evaluation"  "Correct Evaluation (50 pts)")
      ("Type Error"  "Type Checking (30 pts)")
      ("Div Zero"    "Division by Zero (10 pts)")))

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
  (printf "  TOTAL:         ~a + __ / 100\n" grand-earned)
  (printf "================================================================\n\n"))

;; ================================================================
;; Entry point
;; ================================================================

(run-all-tests)
