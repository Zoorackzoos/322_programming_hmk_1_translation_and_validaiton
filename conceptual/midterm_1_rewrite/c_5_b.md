We define:  
pair ≡ λx. λy. λf. ( ( f x ) y )  
fst ≡ λp. p ( λx. λy. x )  
snd ≡ λp. p ( λx. λy. y )  
Show all β-reduction steps. Use α-conversions if necessary.

(b) (10 pts) Reduce:  
snd ( pair ( fst ( pair x y ) ) z )  

plug in snd values for snd
λp. ( p ( λx. ( λy. ( y ) ) ) ) ( pair ( fst ( pair x y ) ) z )  
plug in right pair for p
( ( pair ( fst ( pair x y ) ) z ) ( λx. ( λy. ( y ) ) ) ) 
plug in pair values for pair
( ( ( λx. ( λy. ( λf. ( ( f x ) y ) ) ) ) ( fst ( pair x y ) ) z ) ( λx. ( λy. ( y ) ) ) ) 
plug in fst on the right for x
( ( ( λy. ( λf. ( ( f ( fst ( pair x y ) ) ) y ) ) ) z ) ( λx. ( λy. ( y ) ) ) ) 
plug in z for outer y
( ( λf. ( ( f ( fst ( pair x y ) ) ) z ) ) ( λx. ( λy. ( y ) ) ) )
plug in ( λx. ( λy. ( y ) ) ) for f
( ( ( λx. ( λy. ( y ) ) ) ( fst ( pair x y ) ) ) z )
plug in ( fst ( pair x y ) ) for x. but there's no x so it just goes away.
( ( λy. ( y ) ) z )
plug in z for y
( ( z ) )
z
:-) 