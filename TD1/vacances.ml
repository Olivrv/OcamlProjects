(*TD1 Exo 5*)

let f = function
|0 -> true
|x when ((x - 2) mod 4) = 0 || ((x - 1) mod 2) = 0 -> true
|_ -> false 

(*Exo 8*)

let curry f x y = f (x,y)
let uncurry f (x,y) = f x y

(*Exo 10*)
type ent = Zero | N of ent * ent

let successeur n = N(Zero, n)

let rec addition n = function
|Zero -> n
|N(Zero, x) -> successeur (addition n x)
|_ -> failwith "whattheheck" 
