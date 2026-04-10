#lang racket

;; ================================================================
;; CSCE 322 - PA1 Auto-Grader
;;
;; Usage:
;;   racket pa1_grader.rkt              (grades ./pa1.rkt)
;;   racket pa1_grader.rkt path/to/pa1.rkt
;;
;; Point breakdown (matches rubric):
;;   Validation:          35 pts
;;   Translation:         45 pts
;;   Error localization:  10 pts
;;   Style:               10 pts  (manual — not auto-graded)
;;
;; Input constraint: unary operators (- ! not) are always provided
;; as a parenthesized sublist, never flat in a longer expression.
;;   Valid inputs:   (- 5)  (! true)  (false || (! false))
;;   NOT tested:     (false || ! false)  (1 + - 2)
;; ================================================================

(require racket/string)

;; ================================================================
;; Submission loading
;; ================================================================

(define submission-path
  (path->complete-path
   (if (> (vector-length (current-command-line-arguments)) 0)
       (vector-ref (current-command-line-arguments) 0)
       "pa1.rkt")))

(define load-success? #f)
(define load-error-msg "")

(define student-validate #f)
(define student-translate #f)

(with-handlers ([exn:fail? (lambda (e) (set! load-error-msg (exn-message e)))])
  (parameterize ([current-output-port (open-output-string)]
                 [current-error-port  (open-output-string)])
    (dynamic-require submission-path 0))
  (set! load-success? #t))

(when load-success?
  (set! student-validate
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (dynamic-require submission-path 'validate-program)))
  (set! student-translate
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
  (printf "  CSCE 322 — PA1 Auto-Grader\n")
  (printf "  Submission: ~a\n" (path->string submission-path))
  (printf "================================================================\n")

  (cond
    [(not load-success?)
     (printf "\n  FATAL: could not load submission\n")
     (printf "  ~a\n\n" load-error-msg)
     (printf "  Score: 0 / 90  (auto-graded)\n")
     (printf "  Style: __ / 10 (manual)\n\n")]
    [else
     (run-validation-tests)
     (run-translation-tests)
     (run-error-tests)
     (print-summary)]))

;; ────────────────────────────────────────
;; Validation tests  (35 pts)
;; ────────────────────────────────────────

(define (run-validation-tests)
  ;; --- Literals (5 pts) ---
  (run-test "Validation" "number literal 5"
            student-validate 5 #t 1)
  (run-test "Validation" "number literal 0"
            student-validate 0 #t 1)
  (run-test "Validation" "negative number -3"
            student-validate -3 #t 1)
  (run-test "Validation" "boolean true"
            student-validate 'true #t 1)
  (run-test "Validation" "boolean false"
            student-validate 'false #t 1)

  ;; --- Simple binary (6 pts) ---
  (run-test "Validation" "(1 + 2)"
            student-validate '(1 + 2) #t 1)
  (run-test "Validation" "(3 - 1)"
            student-validate '(3 - 1) #t 1)
  (run-test "Validation" "(2 * 4)"
            student-validate '(2 * 4) #t 1)
  (run-test "Validation" "(8 / 2)"
            student-validate '(8 / 2) #t 1)
  (run-test "Validation" "(true && false)"
            student-validate '(true && false) #t 1)
  (run-test "Validation" "(true || false)"
            student-validate '(true || false) #t 1)

  ;; --- Comparison operators (3 pts) ---
  (run-test "Validation" "(1 < 2)"
            student-validate '(1 < 2) #t 1)
  (run-test "Validation" "(1 == 1)"
            student-validate '(1 == 1) #t 1)
  (run-test "Validation" "(5 >= 3)"
            student-validate '(5 >= 3) #t 1)

  ;; --- Unary expressions (3 pts) ---
  (run-test "Validation" "unary (- 5)"
            student-validate '(- 5) #t 1)
  (run-test "Validation" "unary (! true)"
            student-validate '(! true) #t 1)
  (run-test "Validation" "unary (not false)"
            student-validate '(not false) #t 1)

  ;; --- Nested expressions (4 pts) ---
  (run-test "Validation" "nested ((1 + 2) * 3)"
            student-validate '((1 + 2) * 3) #t 1)
  (run-test "Validation" "nested (1 + (2 * 3))"
            student-validate '(1 + (2 * 3)) #t 1)
  (run-test "Validation" "nested ((2 * 3) < 7)"
            student-validate '((2 * 3) < 7) #t 1)
  (run-test "Validation" "nested (false || (! false))"
            student-validate '(false || (! false)) #t 1)

  ;; --- Flat precedence (5 pts) ---
  (run-test "Validation" "flat (1 + 2 * 3)"
            student-validate '(1 + 2 * 3) #t 1)
  (run-test "Validation" "flat (1 * 2 + 3)"
            student-validate '(1 * 2 + 3) #t 1)
  (run-test "Validation" "flat (true && false || true)"
            student-validate '(true && false || true) #t 1)
  (run-test "Validation" "flat (1 + 2 + 3)"
            student-validate '(1 + 2 + 3) #t 1)
  (run-test "Validation" "flat (1 * 2 * 3)"
            student-validate '(1 * 2 * 3) #t 1)

  ;; --- Invalid expressions (9 pts) ---
  (run-test "Validation" "invalid: (1 + * 3) => '*"
            student-validate '(1 + * 3) '* 2)
  (run-test "Validation" "invalid: non-assoc (1 < 2 > 3) => '>"
            student-validate '(1 < 2 > 3) '> 2)
  (run-test "Validation" "invalid: (true && && false) => '&&"
            student-validate '(true && && false) '&& 2)
  (run-test "Validation" "invalid: non-assoc (1 == 2 != 3) => '!="
            student-validate '(1 == 2 != 3) '!= 2)
  (run-test "Validation" "invalid: bare symbol hello"
            student-validate 'hello 'hello 1))

;; ────────────────────────────────────────
;; Translation tests  (45 pts)
;; ────────────────────────────────────────

(define (run-translation-tests)
  ;; --- Literal pass-through (4 pts) ---
  (run-test "Translation" "literal 5"
            student-translate 5 5 1)
  (run-test "Translation" "literal 0"
            student-translate 0 0 1)
  (run-test "Translation" "literal true"
            student-translate 'true 'true 1)
  (run-test "Translation" "literal false"
            student-translate 'false 'false 1)

  ;; --- Simple binary (6 pts) ---
  (run-test "Translation" "(1 + 2) => (+ 1 2)"
            student-translate '(1 + 2) '(+ 1 2) 1)
  (run-test "Translation" "(3 - 1) => (- 3 1)"
            student-translate '(3 - 1) '(- 3 1) 1)
  (run-test "Translation" "(2 * 4) => (* 2 4)"
            student-translate '(2 * 4) '(* 2 4) 1)
  (run-test "Translation" "(8 / 2) => (/ 8 2)"
            student-translate '(8 / 2) '(/ 8 2) 1)
  (run-test "Translation" "(1 < 2) => (< 1 2)"
            student-translate '(1 < 2) '(< 1 2) 1)
  (run-test "Translation" "(1 == 1) => (== 1 1)"
            student-translate '(1 == 1) '(== 1 1) 1)

  ;; --- Operator normalization (5 pts) ---
  (run-test "Translation" "(true && false) => (and true false)"
            student-translate '(true && false) '(and true false) 2)
  (run-test "Translation" "(true || false) => (or true false)"
            student-translate '(true || false) '(or true false) 2)
  (run-test "Translation" "(! true) => (not true)"
            student-translate '(! true) '(not true) 1)

  ;; --- Unary translation (4 pts) ---
  (run-test "Translation" "(- 5) => (- 5)"
            student-translate '(- 5) '(- 5) 2)
  (run-test "Translation" "(not false) => (not false)"
            student-translate '(not false) '(not false) 2)

  ;; --- Nested expressions (6 pts) ---
  (run-test "Translation" "((1 + 2) * 3) => (* (+ 1 2) 3)"
            student-translate '((1 + 2) * 3) '(* (+ 1 2) 3) 1)
  (run-test "Translation" "(1 + (2 * 3)) => (+ 1 (* 2 3))"
            student-translate '(1 + (2 * 3)) '(+ 1 (* 2 3)) 1)
  (run-test "Translation" "((2 * 3) < 7) => (< (* 2 3) 7)"
            student-translate '((2 * 3) < 7) '(< (* 2 3) 7) 1)
  (run-test "Translation" "(false || (! false)) => (or false (not false))"
            student-translate '(false || (! false)) '(or false (not false)) 1)
  (run-test "Translation" "((1 + 2) * (3 - 4)) => (* (+ 1 2) (- 3 4))"
            student-translate '((1 + 2) * (3 - 4)) '(* (+ 1 2) (- 3 4)) 1)
  (run-test "Translation" "(1 + (- 2)) => (+ 1 (- 2))"
            student-translate '(1 + (- 2)) '(+ 1 (- 2)) 1)

  ;; --- Flat precedence (10 pts) ---
  (run-test "Translation" "(1 + 2 * 3) => (+ 1 (* 2 3))"
            student-translate '(1 + 2 * 3) '(+ 1 (* 2 3)) 2)
  (run-test "Translation" "(1 * 2 + 3) => (+ (* 1 2) 3)"
            student-translate '(1 * 2 + 3) '(+ (* 1 2) 3) 2)
  (run-test "Translation" "(2 + 3 * 4 - 1) => (- (+ 2 (* 3 4)) 1)"
            student-translate '(2 + 3 * 4 - 1) '(- (+ 2 (* 3 4)) 1) 2)
  (run-test "Translation" "(true && false || true) => (or (and true false) true)"
            student-translate '(true && false || true) '(or (and true false) true) 2)
  (run-test "Translation" "(true || false && true) => (or true (and false true))"
            student-translate '(true || false && true) '(or true (and false true)) 2)

  ;; --- Associativity (10 pts) ---
  (run-test "Translation" "left-assoc (1 + 2 + 3) => (+ (+ 1 2) 3)"
            student-translate '(1 + 2 + 3) '(+ (+ 1 2) 3) 2)
  (run-test "Translation" "left-assoc (1 - 2 - 3) => (- (- 1 2) 3)"
            student-translate '(1 - 2 - 3) '(- (- 1 2) 3) 2)
  (run-test "Translation" "left-assoc (1 * 2 * 3) => (* (* 1 2) 3)"
            student-translate '(1 * 2 * 3) '(* (* 1 2) 3) 2)
  (run-test "Translation" "left-assoc (2 * 3 * 4 + 1) => (+ (* (* 2 3) 4) 1)"
            student-translate '(2 * 3 * 4 + 1) '(+ (* (* 2 3) 4) 1) 2)
  (run-test "Translation" "left-assoc (true && false && true) => (and (and true false) true)"
            student-translate '(true && false && true) '(and (and true false) true) 2))

;; ────────────────────────────────────────
;; Error localization tests  (10 pts)
;; ────────────────────────────────────────

(define (run-error-tests)
  (run-test "Error" "(1 + * 3) => (err *)"
            student-translate '(1 + * 3) '(err *) 2)
  (run-test "Error" "(1 < 2 > 3) => (err >)"
            student-translate '(1 < 2 > 3) '(err >) 2)
  (run-test "Error" "(true && && false) => (err &&)"
            student-translate '(true && && false) '(err &&) 2)
  (run-test "Error" "(1 == 2 != 3) => (err !=)"
            student-translate '(1 == 2 != 3) '(err !=) 2)
  (run-test "Error" "bare symbol => (err hello)"
            student-translate 'hello '(err hello) 2))

;; ================================================================
;; Summary
;; ================================================================

(define (print-summary)
  (define categories
    '(("Validation"  "Correct Validation (35 pts)")
      ("Translation" "Correct Prefix Translation (45 pts)")
      ("Error"       "Error Localization (10 pts)")))

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
