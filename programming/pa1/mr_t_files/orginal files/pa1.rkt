#lang racket
(provide (all-defined-out))

;; ============================================================
;; CSCE 322 - PA1 Starter File
;;
;; In this assignment you will:
;;   1. validate expressions in the infix-list language
;;   2. translate valid expressions into prefix form
;;
;; You are NOT evaluating expressions.
;;
;; Allowed style:
;;   - top-level define only
;;   - recursion
;;   - if / cond
;;   - basic list operations
;;
;; Not allowed:
;;   - let / let*
;;   - lambda
;;   - nested define
;;   - mutation
;;   - loops
;; ============================================================

;; ============================================================
;; Name: TODO
;; ============================================================


;; ============================================================
;; Basic list selector helpers
;; Build your own selectors using car/cdr only
;; ============================================================

(define (my-first lst)
  (car lst))

(define (my-second lst)
  (car (cdr lst)))

(define (my-third lst)
  (car (cdr (cdr lst))))

(define (my-fourth lst)
  (car (cdr (cdr (cdr lst)))))

(define (my-fifth lst)
  (car (cdr (cdr (cdr (cdr lst))))))


;; ============================================================
;; Literal checks
;; ============================================================

(define (boolean-literal? x)
  (or (equal? x 'true)
      (equal? x 'false)))

(define (literal? x)
  (or (number? x)
      (boolean-literal? x)))


;; ============================================================
;; Operator checks
;; ============================================================

(define (arithmetic-op? x)
  (or (equal? x '+)
      (equal? x '-)
      (equal? x '*)
      (equal? x '/)))

(define (boolean-op? x)
  (or (equal? x '&&)
      (equal? x '||)))

(define (comparison-op? x)
  (or (equal? x '==)
      (equal? x '!=)
      (equal? x '<)
      (equal? x '<=)
      (equal? x '>)
      (equal? x '>=)))

(define (unary-op? x)
  (or (equal? x '-)
      (equal? x '!)
      (equal? x 'not)))

(define (binary-op? x)
  (or (arithmetic-op? x)
      (boolean-op? x)
      (comparison-op? x)))


;; ============================================================
;; Operator normalization
;;
;; Convert infix operator symbols into canonical prefix names.
;; Examples:
;;   && -> and
;;   || -> or
;;   !  -> not
;; Everything else stays the same.
;; ============================================================

(define (normalize-op op)
  (cond
    [(equal? op '&&) 'and]
    [(equal? op '||) 'or]
    [(equal? op '!)  'not]
    [else op]))


;; ============================================================
;; Precedence
;;
;; Larger number = tighter binding
;;
;; unary:            highest
;; * /
;; + -
;; < <= > >=
;; == !=
;; &&
;; ||               lowest
;;
;; You may use this helper however you want.
;; ============================================================

(define (precedence op)
  (cond
    [(or (equal? op '-)
         (equal? op '!)
         (equal? op 'not))
     7]
    [(or (equal? op '*)
         (equal? op '/))
     6]
    [(or (equal? op '+)
         (equal? op '-))
     5]
    [(or (equal? op '<)
         (equal? op '<=)
         (equal? op '>)
         (equal? op '>=))
     4]
    [(or (equal? op '==)
         (equal? op '!=))
     3]
    [(equal? op '&&)
     2]
    [(equal? op '||)
     1]
    [else
     0]))


;; ============================================================
;; Associativity helpers
;; ============================================================

(define (non-associative-op? op)
  (or (equal? op '==)
      (equal? op '!=)
      (equal? op '<)
      (equal? op '<=)
      (equal? op '>)
      (equal? op '>=)))

(define (left-associative-op? op)
  (or (equal? op '+)
      (equal? op '-)
      (equal? op '*)
      (equal? op '/)
      (equal? op '&&)
      (equal? op '||)))

(define (right-associative-op? op)
  (or (equal? op '!)
      (equal? op 'not)))


;; ============================================================
;; Shape helpers
;;
;; These helpers do NOT solve the whole problem.
;; They only help you identify common shapes.
;; ============================================================

(define (unary-shape? e)
  (and (list? e)
       (= (length e) 2)
       (unary-op? (my-first e))))

(define (three-part-shape? e)
  (and (list? e)
       (= (length e) 3)))

(define (binary-shape? e)
  (and (three-part-shape? e)
       (binary-op? (my-second e))))


;; ============================================================
;; TODO helper ideas
;;
;; You do not have to use all of these, but may find them useful.
;;
;; Possible helpers to implement:
;;
;; (valid-literal? e)
;; (valid-unary? e)
;; (valid-binary? e)
;; (smallest-error e)
;; (translate-unary e)
;; (translate-binary e)
;;
;; If you want to support expressions like:
;;   '(1 + 2 * 3)
;; without requiring nested parentheses,
;; you will likely need helpers that find the correct top-level
;; operator to split on according to precedence/associativity.
;;
;; Example helper names:
;;   (top-level-op-index e)
;;   (split-left e n)
;;   (split-right e n)
;;
;; ============================================================


;; ============================================================
;; validate-program
;;
;; Input:
;;   any Racket value
;;
;; Output:
;;   #t if valid
;;   otherwise the smallest offending symbol or subexpression
;;
;; Examples:
;;   (validate-program '(1 + 2))          => #t
;;   (validate-program '(1 + * 3))        => '*
;;   (validate-program '(1 < 2 > 3))      => '>
;;
;; TODO:
;;   implement this function
;; ============================================================

(define (validate-program e)
  (cond
    [(literal? e)
     #t]

    ;; TODO: handle unary expressions
    ;; TODO: handle binary expressions
    ;; TODO: handle longer infix expressions with precedence
    ;; TODO: return smallest offending piece on failure

    [else
     e]))


;; ============================================================
;; infix->prefix
;;
;; Input:
;;   any Racket value
;;
;; Output:
;;   canonical prefix expression if valid
;;   otherwise '(err offending-part)
;;
;; Examples:
;;   (infix->prefix '(1 + 2))         => '(+ 1 2)
;;   (infix->prefix '(false || !false))
;;                                    => '(or false (not false))
;;
;; TODO:
;;   implement this function
;; ============================================================

(define (infix->prefix e)
  (cond
    [(equal? (validate-program e) #t)
     ;; TODO:
     ;; Replace this placeholder with your translation logic.
     e]

    [else
     (list 'err (validate-program e))]))


;; ============================================================
;; Public test cases
;;
;; You may add more tests as you work.
;; ============================================================

;; validation tests
(validate-program 5)
(validate-program 'true)
(validate-program '(1 + 2))
(validate-program '(1 + 2 * 3))
(validate-program '((1 + 2) * 3))
(validate-program '(false || !false))
(validate-program '(1 + * 3))
(validate-program '(1 < 2 > 3))
(validate-program '(true && && false))

;; translation tests
(infix->prefix 5)
(infix->prefix 'true)
(infix->prefix '(1 + 2))
(infix->prefix '(1 + 2 * 3))
(infix->prefix '((1 + 2) * 3))
(infix->prefix '(false || !false))
(infix->prefix '((2 * 3) < 7))