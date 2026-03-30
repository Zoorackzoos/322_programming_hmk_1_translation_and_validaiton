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
  (or
   (equal? x 'true)
   (equal? x 'false)
  )
)

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

(define (weird-boolean-literal? x)
  (or
   (equal? x '!true)
   (equal? x '!false)
   (equal? x '!!true)
   (equal? x '!!false)
  )
)

(define (duncan-is-literal? x)
  (or
   (weird-boolean-literal? x)
   (literal? x)
  )
)

(define (lowest-precedence-position list iterator index_of_lowest_precedent)
 ;(println list)
 ;(println iterator)
 ;(println index_of_lowest_precedent)
 ;;(println (list-ref list index_of_lowest_precedent))
 (if
  (= iterator (length list)) ;; conditional
  index_of_lowest_precedent ;; if yes
  (if ;; if no
   (and
     (> (precedence (list-ref list iterator)) 0)
     (or
      (= index_of_lowest_precedent -1)
      (<
       (precedence (list-ref list iterator))
       (precedence (list-ref list index_of_lowest_precedent))
      )
     )
    )
   (lowest-precedence-position list (+ iterator 1) iterator) ;; if yes 
   (lowest-precedence-position list (+ iterator 1) index_of_lowest_precedent) ;; if no
  )
 )
)

(define (translate-op op)
 (cond
  [(equal? op '||) 'or]
  [(equal? op '&&) 'and]
  [else op]
 )
)

(define (translate-weird-bools bool)
 (cond
  [(equal? bool '!false) '(not false)]
  [(equal? bool '!true) '(not true)]
  [else bool]
 )
)

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
  (
   cond
    [
     (null? e)
      #t
    ]
    [
      (duncan-is-literal? e)
      #t
    ]
    [
     (binary-op? e)
      e
    ]
    [
     (list? e)
      (
       cond
        [
         (unary-shape? e)
          #t
        ]
        [
         (= (length e) 1);; single element left, check it's a valid value
          (if (or
               (duncan-is-literal? (car e))
               (list? (car e))
              )
            #t      ;; yes_condition
            (car e) ;; no_condtion
          )
        ]
        [
         (< (length e) 3) ;; length 2 is always malformed
          (car e)
        ]
        [
         (not (binary-op? (my-second e)))
          (my-second e)
        ]
        [
         (and (> (length e) 3)
              (non-associative-op? (my-second e))
         )
          (my-second (cddr e))  ;; return the NEXT operator as the offender
        ]
        [
         else
          (validate-program (cddr e))
        ]
      )
    ]
    [
     else
      #t
    ]
  )
)

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

  ;; literal values
  [
   (duncan-is-literal? e)
   (translate-weird-bools e)
  ]

  ;; unary expressions
  [
   (unary-shape? e)
   (list
    (my-first e)
    (translate-weird-bools (infix->prefix (my-second e)))
   )
  ]

  ;; unwrap parentheses
  [
   (= (lowest-precedence-position e 0 -1) -1)
   (infix->prefix (my-first e))
  ]

  ;; normal infix expression
  [
   else

   (list
    (translate-op (list-ref e (lowest-precedence-position e 0 -1)))
    (translate-weird-bools (infix->prefix (take e (lowest-precedence-position e 0 -1))))
    (translate-weird-bools (infix->prefix (drop e (+ (lowest-precedence-position e 0 -1) 1))))
   )
  ]
 )
)





;; ============================================================
;; Public test cases
;;
;; You may add more tests as you work.
;; ============================================================

;; validation tests. the correct return statment is rightwards of the test.
`mr.t_validation_tests
(validate-program 5) ;; #t
(validate-program 'true) ;; #t
(validate-program '(1 + 2)) ;; #t
(validate-program '(1 + 2 * 3)) ;; #t
(validate-program '((1 + 2) * 3)) ;; #t
(validate-program '(false || !false)) ;; #t
'meant_to_fail_tests
(validate-program '(1 + * 3)) ;; `*
(validate-program '(1 < 2 > 3)) ;; `>
(validate-program '(true && && false)) ;; `&&

`_
`_
`_

;; translation tests
`mr.t_translation_tests
(infix->prefix 5) ;; 5
(infix->prefix 'true) ;; %#t
(infix->prefix '(1 + 2)) ;; '(+ 1 2)
(infix->prefix '(1 + 2 * 3)) ;; '(+ 1 (* 2 3) )
(infix->prefix '((1 + 2) * 3)) ;; '(* (+ 1 2) 3)
(infix->prefix '(false || !false)) ;; '(or false (not false) )
(infix->prefix '((2 * 3) < 7)) ;; '( < (* 2 3) 7)

`_
`_
`_

'Barbismo_tests
(infix->prefix '(2 * 3)) ;; '(* 2 3)
(infix->prefix '(2 * 3 + 4)) ;; '(+ (* 2 3) 4)
(infix->prefix '(2 + 3 * 4 + 5)) ;; '(+ (+ 2 (* 3 4)) 5)
(infix->prefix '((2 + 3))) ;; '(+ 2 3)
(infix->prefix '((2 + 3) * (4 + 5))) ;; '(* (+ 2 3) (+ 4 5))
(infix->prefix '(!true)) ;; '(not true)
(infix->prefix '(!false)) ;; '(not false)
(infix->prefix '(true && false)) ;; '(and true false)
(infix->prefix '(true || false)) ;; '(or true false)
(infix->prefix '(1 < 2)) ;; '(< 1 2)