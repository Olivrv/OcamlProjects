(*Exo 1*)
let impl statement = function
|x when not statement -> true
|x when statement && x -> true
|_ -> false;;

let a = 10 and b = 5;;
impl (a=20) (b+a = 30);;
impl (a=10) (b+a=20);;
impl (a=10) (b+a = 15);;

(*Exo 2*)
let rec f m n = match (m, n) with (*Explicite*)
|(x, y) when x = 0 -> n
|_ -> f (m-1) (n+1);;

let rec f m = function (*Implicite*)
|x when m = 0 -> x
|y -> f (m-1) (y+1);;

let rec g m n = match (m, n) with (*Explicite*)
|(x, y) when x = 0 -> y
|_ -> (g (m-1) (n)) + 1;;

(*Exo 3*)
let rec d n = match n with
|x when x<10 && x>(-10) -> 1
|y -> 1 + d (y/10);;

(*Exo 4*)
let rec hamming n = function
|0 -> true
|x when (x mod 2 = 0) -> hamming (x mod 2)
|x when (x mod 3 = 0) -> hamming (x mod 3)
|x when (x mod 5 = 0) -> hamming (x mod 5) 
|_ -> false
