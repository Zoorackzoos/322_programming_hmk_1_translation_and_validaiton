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

use a IDE to view this because seeing the "()" without highlighting is difficult. 
rephrase into a duncan formatted lambda calculus question
( \lx.( \ly.( x ( \lz. ( x y z ) ) ) ) ) ( \ly. ( \lx. ( y z x ) ) )
do \a renaming
( \lx2.( \ly.( x2 ( \lz1. ( x2 y z1 ) ) ) ) ) ( \ly1. ( \lx1. ( y1 z x1 ) ) )
the reason these lambda calculus problems are in here, is becuase that left part has 2 x'es attached to it's \lx.   
they're like variables, i guess :-/  
in the previous problems the \l functions were either overwritten or duplicates didn't exist in the first place.  
    not like I remember -_-
plug in the right \ly1. and it's buddies for x2
( \ly.( ( \ly1.( \lx1. ( y1 z x1 ) ) ) ( \lz1. ( ( \ly1. ( \lx1. ( y1 z x1 ) ) ) y z1 ) ) ) )
plug in the rightward piece for y1
( \ly.( \lx1. ( ( \lz1. ( ( \ly1. ( \lx1. ( y1 z x1 ) ) ) y z1 ) ) z x1 ) ) )
can't plug into y because it's out of scope
plug in y for x1
( \ly.( ( \lz1. ( ( \ly1. ( \lx1. ( y1 z y ) ) ) z1 ) ) z x1 ) )
plug in z for y
( ( \lz1. ( ( \ly1. ( \lx1. ( y1 z z ) ) ) z1 ) ) x1 )
plug in x1 for z1
( ( \ly1. ( \lx1. ( y1 z z ) ) ) x1 )
plug in x1 for y1
( \lx1. ( x1 z z ) )


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