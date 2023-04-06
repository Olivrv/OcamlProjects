(*Exercice 1*)
let rec foo f x n = match n with
|0 -> x
|n -> foo f (f x) (n-1);;

(*Exercice 2*)
let rec foo x n = match n with
|0 -> 1
|n -> x * (foo x (n-1));;

let footerm x n = match n with
|0 -> 1
|1 -> x
|n when n mod 2 = 0 -> foo (x*x) (n/2)
|n -> foo (x*x) (n/2);;
