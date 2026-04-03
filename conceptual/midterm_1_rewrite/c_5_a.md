# Problem 5 (20 points)
We define:  
pair ≡ λx. λy. λf. ( ( f x ) y )  
fst ≡ λp. p ( λx. λy. x )  
snd ≡ λp. p ( λx. λy. y )  
Show all β-reducitoin steps. Use α-conversions if necessary.

(a) (10 pts) Reduce:  
fst ( pair a ( pair b c ) )  

plug in fst  
λp. ( p ( λx. ( λy. ( x ) ) ) ) ( pair a ( pair b c ) )  
plug in the right part for p
( ( pair a ( pair b c ) ) ( λx. ( λy. ( x ) ) ) )
plug in "pair values" for pair
( ( ( λx. ( λy. ( λf. ( ( f x ) y ) ) ) ) a ( pair b c ) ) ( λx. ( λy. ( x ) ) ) )
plug in a for x
( ( ( λy. ( λf. ( ( f a ) y ) ) ) ( pair b c ) ) ( λx. ( λy. ( x ) ) ) )
plug in middle pair for y
( ( λf. ( ( f a ) ( pair b c ) ) ) ( λx. ( λy. ( x ) ) ) )
plug in λx. on the right for f
( ( ( λx. ( λy. ( x ) ) ) a ) ( pair b c ) )
plug in a for x
( ( ( λy. ( x ) ) a ) ( pair b c ) )
plug in pair b c for x. there's no y for λy. , and we're plugging into x. so it just goes away. like in derivative rules.
( ( a ) )
a
that's the answer

