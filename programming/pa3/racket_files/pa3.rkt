#lang racket
(provide (all-defined-out))
(require "pa1.rkt")
(require "pa2.rkt")

;; ============================================================
;; CSCE 322 - PA3 Starter File
;;
;; In this assignment you will:
;;   1. evaluate variables using an environment
;;   2. evaluate `var` bindings and extend the environment
;;   3. handle variable shadowing and free variable errors
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
;; Environment Helpers
;; ============================================================

;; ============================================================
;; lookup-env
;;
;; Input:
;;   var: a variable symbol (e.g., 'x)
;;   env: an environment list (e.g., '((x 5) (y 10)))
;;
;; Output:
;;   The bound value if found, or #f if not found.
;;
;; Examples:
;;   (lookup-env 'x '((x 5) (y 10))) => 5
;;   (lookup-env 'z '((x 5) (y 10))) => #f
;;
;; TODO:
;;   implement this function
;; ============================================================
(define (lookup-env var env)
  ;;not fucking with the signature in case mr.t has a secret grader
  
  ;;display 
  (println "lookup-env")
  (print "    ")
  (println var)
  (println "    ")
  (println env)
  
  ;;work
  (prefixed-eval-with-env var env 0)
)

(define (duncan-lookup-env var env iterator)
  ;;display
  (println "        duncan-lookup-env")
  (print "            ")
  (println var)
  (print "            ")
  (println env)
  (print "            ")
  (println iterator)
  
  (if
   (equal? (my-first (list-ref env iterator)) var);;condtional
   (my-second (list-ref env iterator));;if yes
   (prefixed-eval-with-env var env (+ iterator 1));;if no
  )
)


;; ============================================================
;; extend-env
;;
;; Input:
;;   var: a variable symbol (e.g., 'x)
;;   val: the evaluated value to bind to the variable
;;   env: the current environment list
;;
;; Output:
;;   A new environment list with `(var val)` added to the front.
;;
;; Examples:
;;   (extend-env 'x 5 '()) => '((x 5))
;;   (extend-env 'z 20 '((x 5))) => '((z 20) (x 5))
;;
;; TODO:
;;   implement this function
;; ============================================================
(define (extend-env var val env)
  'todo
)


;; ============================================================
;; Evaluator
;; ============================================================

;; helper functions for evaluator
;;   just put the infix->prefix version of e into this.
(define (prefixed-eval-with-env e env)
  ;;display
  (println "    prefixed-eval-with-env")
  (print "        ")
  (println e)
  (print "        ")
  (println env)

  ;;work
  (cond
    [
     (variable? (my-second e))
     (prefixed-eval-with-env (list (my-first e) (duncan-lookup-env (my-second e) env 0) (my-third e)) env)
    ]
    [
     (variable? (my-third e))
     (prefixed-eval-with-env (list (my-first e) (my-second e) (duncan-lookup-env (my-third e) env 0)) env)
    ]
    [
     else
     (println "        TODO: evaluate the fucking thing lol")
    ]
  )
)

;; ============================================================
;; evaluate-with-env
;;
;; Input:
;;   e:   an infix expression (same as PA1/PA2, but with variables)
;;   env: the current environment list
;;
;; Output:
;;   The evaluated value, or an error list like '(err "free variable")
;;
;; Examples:
;;   (evaluate-with-env '(x + 1) '((x 5))) => 6
;;   (evaluate-with-env '(var (x 5) (x * 2)) '()) => 10
;;   (evaluate-with-env 'y '()) => '(err "free variable")
;;
;; TODO:
;;   implement this function. You should convert the infix
;;   expression `e` to a prefix AST using `infix->prefix` from PA1,
;;   then recursively evaluate the AST.
;;   You may reuse evaluation helpers from PA2.
;; ============================================================
(define (evaluate-with-env e env)
  ;;display
  (println "evaluate-with-env")
  (print "    ")
  (println e)
  (print "    ")
  (println env)

  ;;work
  (prefixed-eval-with-env (infix->prefix e) env)
)


;; ============================================================
;; Public test cases
;;
;; You may add more tests as you work.
;; ============================================================

;; intended output =
;; 5
;;(evaluate-with-env 'x '((x 5)) )

;; 6
(evaluate-with-env '(x + 1) '((x 5)))














