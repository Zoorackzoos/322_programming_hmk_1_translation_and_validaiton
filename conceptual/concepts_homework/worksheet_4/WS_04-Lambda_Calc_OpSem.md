CSCE 322 — Worksheet 4 

Lambda Calculus & Operational Semantics 

Instructions: 

 For Part 1, explicitly show your steps for *β*\-reduction and *α*\-renaming. 

 For Part 2, use the big-step operational semantics rules from class. Write your derivations using the fraction style (premises over consequent). 

 Assume the standard rules for numbers, booleans, variables, arithmetic, comparisons, condi tionals, and var. 

Part 1: Lambda Calculus 

Evaluate the following lambda calculus expressions to normal form. Be careful with free and bound variables, and apply *α*\-renaming where necessary to avoid variable capture. 

1\. Multiple Free Variables in Argument 

(*λx. λy. x* (*λz. x y z*)) (*λy. λx. y z x*) 

2\. Shadowing and Scope 

((*λx. λy. x* (*λy. x y*)) (*λz. y z*)) *x* 

3\. Function Composition with Free Variables 

(*λf. λg. λx. f* (*g x*)) (*λx. y x*) (*λy. x y*) 

1  
Part 2: Operational Semantics 

1\. Deep Shadowing & Nested Evaluation 

Give a full big-step derivation for the following expression. Assume an arbitrary initial envi ronment *ρ*. 

*⟨*(*var* (*x* 5\) (*var* (*y* (*if* (*gt x* 3\) (+ *x* 2\) 0)) (*var* (*x* (+ *y* 1)) (+ *x y*))))*, ρ⟩* 

2\. Late Stuck State 

Give a big-step derivation showing exactly where the following expression gets stuck. Explain why the derivation cannot continue. 

*⟨*(*if* (*eq* 1 1\) (*var* (*x true*) (*if x* (+ *x* 1\) 5)) 0\)*, ρ⟩* 

3\. Complex Conditionals & Initial Environment 

Let *ρ*0 \= *{a 7→* 10*, b 7→* 5*}*. Evaluate the following expression: 

*⟨*(*if* (*and* (*lt b a*) (*eq* (*− a b*) 5)) (*var* (*a b*) (+ *a b*)) 0\)*, ρ*0*⟩* 

2