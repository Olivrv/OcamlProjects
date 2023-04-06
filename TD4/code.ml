(*Exercice 1*)
let rec foo f x n = match n with
|0 -> x
|n -> foo f (f x) (n-1);;

(*Exercice 2*)
let rec foo x n = match n with
|0 -> 1
|n -> x * (foo x (n-1));;

let footerm x n = 
  let rec aux x n acc = match n with
  |0 -> 1
  |1 -> x * acc
  |n -> aux x (n-1) (acc * x) 
in aux x n 1;;
