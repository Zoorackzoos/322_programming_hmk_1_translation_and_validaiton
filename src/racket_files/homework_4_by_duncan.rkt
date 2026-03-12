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
;; Name: Duncan Holmes
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

;; goofy ahh funcitons man. 
;; if this is a true or a true or a false, then true.
;; otherwise false. 
(define (boolean-literal? x)
  (or (equal? x 'true)
      (equal? x 'false)))

;; returns true if the input is a number or boolean 
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

;; returns true if: - , !. returns false if antyhging else
(define (unary-op? x)
  (or (equal? x '-)
      (equal? x '!)
      (equal? x 'not)))

;; true if either of these:
;;   +, -, *, /
;;   &&, ||
;;   ==, !=, <, <=, >, >= 
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

;; this is a word nobody uses.
;;     https://docs.google.com/document/d/1HOnXb8v4rdqWKvQWRcCS--AHOB0hQhTt20cyo58_x1s/edit?usp=sharing
;; if e is a list, and has length 2, and the 1st element is either ! or -.
;;    like: - x, ! x
;;    not: + + x, - - x, ~ x, -x, !x, !!x, --x
(define (unary-shape? e)
  (and (list? e)
       (= (length e) 2)
       (unary-op? (my-first e))))

;; if the list is a list, and is the length 3
;; why is this called a shape?
(define (three-part-shape? e)
  (and (list? e)
       (= (length e) 3)))

;; has to be a list of length 3.
;; the 2nd piece of the list has to be a "binary operation":
;;   +, -, *, /
;;   &&, ||
;;   ==, !=, <, <=, >, >= 
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

;; (define (valid-unary? e))

;; ============================================================
;; validate-program
;;\
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
;; duncan esamples:
;;   unary 
;;       (validate-program '(! 1) )      => #t
;;       (validate-program '(- 1) )      => #t
;;
;; TODO:
;;   implement this function
;; ============================================================

(define (validate-program e)
  (cond
    [(literal? e) ;;checks if it's a number or a -,+,/,*,
     #t]          ;;so if you put in a number it's just true, or if ture tures

    ;; TODO: handle unary expressions
    ;;   if it's `(- x) or ! x then yay
    ;;   if it's not you have to predict the bullshit the user put in.
    ;;   ! ! x and - - x means return '-' and '!'
    [(unary-shape? e) #t ]
    ;;    idk how to clarify the "you messed up" string on a unary.
    
    ;; TODO: handle binary expressions
    ;;   aceptable:
    ;;     '(1 + 2) <-- correct --> #t
    ;;     '(1 + * 3) <-- not proper use of binary operation --> `*
    ;;     '(1 < 2 > 3) <-- not a binary operation --> `>
    ;;     '(1 + 1 *) <-- not correct --> `*
    ;;     '(1 + 1 + * 1) <-- "undefined behavoir" --> error
    ;;     '(1 + (1 + 1)) <-- good --> #t
    ;;     ` ' <-- left does something idk, right does good
    ;;
    ;;  1. check length. either == or length > wahtever
    ;;  2. identify operatator
    ;;  3. binary -> l
    ;;  `(1 +) <-- invalid because poor length --> '+
    ;;  make length > 4 && length < 2 like algorithm
    ;;  
    [(binary-shape? e) #t ]
    [
      (and
        (or
         ( =(length e) 4)
         ( >(length e) 4))
        ( binary-op? (my-third e))
      )
      (my-third e) ;; return value 
    ]
    
    ;; TODO: handle longer infix expressions with precedence
    ;; (1 + (1 + 1)) <-- what be yapping about
    ;; (1 + (2 * 3)) <-- other one use for
    ;; use precidence here
    
    ;; TODO: return smallest offending piece on failure
    ;; 
    
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
`mr.t_validation_tests
(validate-program 5)
(validate-program 'true)
(validate-program '(1 + 2))
(validate-program '(1 + 2 * 3))
(validate-program '((1 + 2) * 3))
(validate-program '(false || !false))
`meant_to_fail_tests
;;(binary-shape? '(1 + * 3))
(validate-program '(1 + * 3)) ;; must return `*
(validate-program '(1 < 2 > 3)) ;; must return `>
(validate-program '(true && && false))

`_
`_
`_

;; translation tests
`mr.t_translation_tests
(infix->prefix 5)
(infix->prefix 'true)
(infix->prefix '(1 + 2))
(infix->prefix '(1 + 2 * 3))
(infix->prefix '((1 + 2) * 3))
(infix->prefix '(false || !false))
(infix->prefix '((2 * 3) < 7))

`_
`_
`_

`duncan_validate_program_tests_unary
(length `(-x + -x) ) ;; 3
(length `(-x+-x) ) ;; 1
(validate-program `(- 1))
(validate-program `(! 1))
(validate-program `(- -1))
(validate-program `(! !1))
;;(validate-program `- x) ;; x: unbound identifier in: x
(validate-program `(- 1 -))
(validate-program `(! 1 -))
(length'(1 < 2 > 3))
(> (length'(1 < 2 > 3)) 4)
( binary-op? (my-third '(1 < 2 > 3)))