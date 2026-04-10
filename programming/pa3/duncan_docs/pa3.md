CSCE 322 — Programming Assignment 3 

Variables and Environments (Racket Fundamentals) 

Overview   
In this assignment, you will extend your parser from PA1 and your evaluator from PA2 to support variables and static scoping. You will implement an environment as a list to keep track of variable bindings, following the concepts discussed in class. The goal of this assignment is to help you: 

 understand how variables and scoping are implemented in programming languages  implement an environment model for variable binding 

 practice writing larger programs by combining modules and reusing existing code 

You will incrementally update your pa1.rkt parser to recognize variable bindings, and create a new evaluator in pa3.rkt that uses an environment. 

Allowed Racket Subset 

All work must be done using only the following Racket primitives: 

define, if, cond, and, or, not, car, cdr, cons, list, null?, pair?, number?, boolean?, symbol?, arithmetic operators, and recursion. 

The following are not allowed: 

let, let\*, lambda, begin, match, struct, loops, map, mutation (set\!), nested defines. All helper functions must be defined at the top level. 

1\. Parser Updates (Extending PA1) 

You must update your validate-program and infix-\>prefix functions in pa1.rkt to recognize variables and variable bindings. 

Variables 

A variable is any symbol that is not a reserved keyword (\+, \-, \*, /, true, false, var, and, or, not, \<, \<=, \>, \>=, \==, \!=, &&, ||, \!). 

1  
Variable Bindings 

The syntax for a variable binding is: 

(var (variable\_name binding\_expr) body\_expr) 

The var expression is already in a prefix-like form. Your infix-\>prefix function should recursively translate the binding expr and body expr while preserving the var structure. 

For example: 

| Input (infix):  ’(var (x (1 \+ 2)) (x \* 3))  Output (prefix):  ’(var (x (+ 1 2)) (\* x 3)) |
| :---- |

2\. Evaluator Updates (PA3) 

You must create a new evaluator in pa3.rkt to handle the new AST nodes and manage the environment, while reusing base logic from pa2.rkt. 

New Signature 

Create a new main entry point in pa3.rkt named (evaluate-with-env e env), which takes an expression e and an initial environment env. 

Environment Structure 

The environment must be implemented as a list of bindings, where each binding is a 2-element list (variable value). For example: ’((x 5\) (y 10)). 

Evaluating Variables 

When evaluating a variable: 

1\. Look up the variable name in the environment list, starting from the front (head). 2\. If found, return its bound value. 

3\. If not found, return a new runtime error: ’(err "free variable"). 2  
Evaluating var Expressions 

When evaluating a (var (variable name binding expr) body expr) expression: 1\. Evaluate the binding expr in the current environment using evaluate-with-env. 

2\. Extend the current environment by adding the new binding (variable name evaluated value) to the front of the list. This naturally handles variable shadow ing. 

3\. Evaluate the body expr in this new extended environment. 

4\. Return the result of the body expr. 

Reusing PA2 

For standard arithmetic, boolean, and comparison operations, evaluate-with-env should recursively evaluate the operands and can reuse helper functions from pa2.rkt to perform the actual computation and type checking. 

3\. Error Handling 

 Continue to enforce all PA2 runtime type checks and division by zero errors.  Add the new ’(err "free variable") error for unbound variables. 

 Ensure that errors in the binding expr or body expr of a var expression are properly propagated up to the top level. 

Deliverables 

Submit a single zip file containing pa1.rkt, pa2.rkt, and pa3.rkt. 

 pa1.rkt should contain the updated validate-program and infix-\>prefix func tions. 

 pa2.rkt should contain the original evaluate-program and its helpers (reused by PA3). 

 pa3.rkt should contain the new evaluate-with-env function and helper functions for environment lookup and management (e.g., lookup-env, extend-env). 

Your pa3.rkt file must begin with: 

\#lang racket 

(provide (all-defined-out)) 

(require "pa1.rkt") 

(require "pa2.rkt") 

3  
Grading 

Correct parser updates (PA1) 20 pts Correct variable evaluation and environment lookup 30 pts Correct var binding evaluation and shadowing 30 pts Correct error propagation (free variables, type errors) 20 pts Total 100 pts 

4