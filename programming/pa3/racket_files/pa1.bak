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
  [
   (or (equal? op '!) (equal? op 'not))  ;; removed - from here
   7
  ]
  [
   (or (equal? op '*) (equal? op '/))
   6
  ]
  [
   (or (equal? op '+) (equal? op '-))    ;; - now correctly gets 5
   5
  ]
  [
   (or (equal? op '<) (equal? op '<=) (equal? op '>) (equal? op '>=))
   4
  ]
  [
   (or (equal? op '==) (equal? op '!=))
   3
  ]
  [
   (equal? op '&&)
   2
  ]
  [
   (equal? op '||)
   1
  ]
  [
   else
   0
  ]
 )
)


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

(define (lowest-precedence-position list iterator index_of_lowest_precedent)
 ;;(println list)
 ;;(println iterator)
 ;;(println index_of_lowest_precedent)
 ;;(println (list-ref list index_of_lowest_precedent))
 (if
  (= iterator (length list)) ;; conditional
  index_of_lowest_precedent ;; if yes
  (if ;; if no
   (and
     (> (precedence (list-ref list iterator)) 0)
     (or
      (= index_of_lowest_precedent -1)
      (<=
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

(define (is-string? e)
  (and
   (not (binary-op? e))
   (not (unary-op? e))
   (not (list? e))
  )
)

(define (contains-string? e index e-length)
  ;;technically you could do (length e) every time instead of e-length.
  ;;but having it be a parameter makes less method calls, thuss effecincy++
  (cond
    [ (= index e-length) #f ]
    [ (not (list? e)) e ]
    [ (is-string? (list-ref e index)) (list-ref e index) ]
    [
     else
      (contains-string? e (+ index 1) e-length)
    ]
  )
)

(define (translate-unary-op e)
  (cond
   [ (equal? e '!) 'not ]
   [ else e ]
  )
)

;; ============================================================
;; pa3 helper funcitons
;; ============================================================

(define (reserved-keyword? s)
  (cond
    [(equal? s '+)   #t]
    [(equal? s '-)   #t]
    [(equal? s '*)   #t]
    [(equal? s '/)   #t]
    [(equal? s 'true)  #t]
    [(equal? s 'false) #t]
    [(equal? s 'var)   #t]
    [(equal? s 'and)   #t]
    [(equal? s 'or)    #t]
    [(equal? s 'not)   #t]
    [(equal? s '<)   #t]
    [(equal? s '<=)  #t]
    [(equal? s '>)   #t]
    [(equal? s '>=)  #t]
    [(equal? s '==)  #t]
    [(equal? s '!=)  #t]
    [(equal? s '&&)  #t]
    [(equal? s '||)  #t]
    [(equal? s '!)   #t]
    [else #f]
  )
)

(define (variable? s)
  (and
   (symbol? s)
   (not (reserved-keyword? s))
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
  ;;display 
  (println "validate-program")
  (print "    ")
  (println e)

  ;;work
  (cond
    [
     (and
      (null? e)
      (println "    e is null")
      (println "    valid")
      #t
     )
    ]

    [
     (and
      (binary-op? e)
      (println "    this is a binary operation.")
      (println "    not valid")
      e
     )
    ]

    ;; pa3: valid variable
    [
     (variable? e)
     (and
      (println "    valid variable")
      #t
     )
    ]

    ;; pa3: unknown bare symbol (reserved keyword used as value, or typo)
    [
     (symbol? e)
     (and
      (println "    invalid bare symbol")
      e
     )
    ]

    ;; pa3: var binding shape
    [
     (and
      ;;(println "    var in binding shape ?")
      (list? e)
      (= (length e) 3)
      (equal? (car e) 'var)
      (list? (my-second e))
      (= (length (my-second e)) 2)
      (variable? (car (my-second e)))
     )
     (and
      (println "    var binding confirmed")
      (eq? #t (validate-program (my-second (my-second e))))
      (eq? #t (validate-program (my-third e)))
     )
    ]

    [
     (and
      (println "    unknowable bools activated")
      (list? e)
      (cond
        [
         (and
          (println "        unary shape in recursion")
          (unary-shape? e)
         )
        ]
        [
         (and
          (println "        binary shape & both elements aren't a list")
          (binary-shape? e)
          (not (list? (my-first e)))
          (not (list? (my-third e)))
         )
        ]
        [
         (and
          (println "        single element left ?")
          (= (length e) 1)
          (if
           (or;;conditional
            (literal? (car e))
            (list? (car e))
           )
            #t;;if yes
            (car e);;if no
          )
         )
        ]
        [
         (and
          (println "        e.size > 3 ?")
          (< (length e) 3)
          (car e)
         )
        ]
        [
         (and
          (println "        (not (binary-op? (my-second e)))")
          (not (binary-op? (my-second e)))
          (my-second e)
         )
        ]
        [
         (and
          (println "        big e and non-associativty")
          (and
           (> (length e) 3)
           (non-associative-op? (my-second e))
          )
          (my-second (cddr e))
         )
        ]
        [
         else
         (and
          (println "        recursion in e")
          (validate-program (cddr e))
         )
        ]
      )
     )
    ]

    [else
     (and
      (println "    else function reached")
      #t
     )
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

  ;;not valid program
  [
   (not (equal? (validate-program e) #t)) (list 'err (validate-program e))
  ]

  ;; pa3: variable passes through unchanged
  [(variable? e) e]

  ;; pa3: var binding
  [(and (list? e)
        (= (length e) 3)
        (equal? (car e) 'var))
   (list 'var
         (list (car (my-second e))
               (infix->prefix (my-second (my-second e))))
         (infix->prefix (my-third e)))
  ]
   
  ;; literal values
  [
   (literal? e)
   (translate-weird-bools e)
  ]

  ;; unary expressions
  [
   (unary-shape? e)
   (list
    (translate-unary-op (my-first e))
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

;;test cases
(infix->prefix '(var (x (1 + 2)) (x * 3)))












