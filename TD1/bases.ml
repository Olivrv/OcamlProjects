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
let hamming x = (((x mod 5) mod 3) mod 2) = 0;;

(*Exo 6*)
let delta u = function x -> u (x+1) - u (x);;

(*Exo 7*)
let th x = let exp2 x = exp (-2.*.x) in (1.-.exp2 x)/.(1.+.exp2 x);;

(*Exo 9*)
type nombre = Flottant of float | Entier of int;;

let add a b = match a, b with
|Entier a, Entier b -> Entier (a + b)
|Flottant a, Flottant b -> Flottant (a +. b)
|Flottant a, Entier b -> Flottant (a +. (float_of_int b))
|Entier a, Flottant b -> Flottant ((float_of_int a) +. b);;
 
