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
plug in right side for x2
  _____        __                             _______________________________
( \lx2.( \ly.( x2 ( \lz1. ( x2 y z1 ) ) ) ) ) ( \ly1. ( \lx1. ( y1 z x1 ) ) )
-->
( \ly.( ( \ly1. ( \lx1. ( y1 z x1 ) ) ) ( \lz1.( ( \ly1.( \lx1.( y1 z x1 ) ) ) y z1 ) ) ) )
cannot plug var into \ly. no var present. go to \ly1.
plug in \lz1 for y1
( \ly. ( \lx1. ( ( \lz1. ( ( \ly1.( \lx1.( y1 z x1 ) ) ) y z1 ) ) z x1 ) ) )
cannot plug var into \lx1. no var present go to \lz1.
plug in z for z1
                   _____                                   __     _
( \ly. ( \lx1. ( ( \lz1. ( ( \ly1.( \lx1.( y1 z x1 ) ) ) y z1 ) ) z x1 ) ) )
-->
( \ly. ( \lx1. ( ( ( \ly1. ( \lx1. ( y1 z x1 ) ) ) y z ) x1 ) ) )
plug in y for y1
                     _____           __            _
( \ly. ( \lx1. ( ( ( \ly1. ( \lx1. ( y1 z x1 ) ) ) y z ) x1 ) ) )
-->
( \ly. ( \lx1. ( ( ( \lx1. ( y z x1 ) ) z ) x1 ) ) )
plug in z for \lx1.
                     _____       __     _
( \ly. ( \lx1. ( ( ( \lx1. ( y z x1 ) ) z ) x1 ) ) )
-->
( \ly. ( \lx1. ( ( y z z ) x1 ) ) )
this problem has more memes that make it harder than traditional plug and play lambda calculus problems
for here, you have to see if substitution is possible.

2\. Shadowing and Scope 

((*λx. λy. x* (*λy. x y*)) (*λz. y z*)) *x* 
convert this into a duncan lambda calculus question
( ( \lx. ( \ly. ( x ( \ly. ( x y ) ) ) ) ) ( \lz. ( y z ) ) ) x
\a renaming time :-DDD
( ( \lx1. ( \ly. ( x1 ( \ly1. ( x1 y1 ) ) ) ) ) ( \lz. ( y z ) ) ) x
plug in \lz. and it's buddies for x1
    _____          __           __              __________________
( ( \lx1. ( \ly. ( x1 ( \ly1. ( x1 y1 ) ) ) ) ) ( \lz. ( y z ) ) ) x
-->
( \ly. ( ( \lz. ( y z ) ) ( \ly1. ( ( \lz. ( y z ) ) y1 ) ) ) ) x
plug in x for y
  ____            _                          _                  _
( \ly. ( ( \lz. ( y z ) ) ( \ly1. ( ( \lz. ( y z ) ) y1 ) ) ) ) x
-->
( \lz. ( x z ) ) ( \ly1. ( ( \lz. ( x z ) ) y1 ) )
plug in \ly1 and it's buddies for z
  ____     _     _________________________________
( \lz. ( x z ) ) ( \ly1. ( ( \lz. ( x z ) ) y1 ) )
-->
( x ( \ly1. ( ( \lz. ( x z ) ) y1 ) ) )
cannot plug var into x, not a \l function.
cannot plug var into y1, no var available.
plug in y1 for z
                ____     _     __
( x ( \ly1. ( ( \lz. ( x z ) ) y1 ) ) )
-->
( x ( \ly1. ( x y1 ) ) )
cannot plug var in for y1, no var available.
seems like that's the end.


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