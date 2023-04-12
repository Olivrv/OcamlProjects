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

(*Exercice 6*)
let rec compte n liste = match liste with
|[] -> 0
|[a] -> if a <= n then 1 else 0
|t::q -> if t <= n then 
            compte n q + compte (n-t) (liste) 
         else 
            compte n q;;
compte 50 [1;2;5;10;20];;

(*Exo 7*)
let rec solve ti ts te= function
|0 -> print_string "\n"
|n -> solve ti te ts (n-1);
      print_string "\nDéplacement d'un disque de ";
      print_string ti;
      print_string " vers ";
      print_string te;
      solve ts ti te (n-1);;
solve "Tige 1" "Tige 2" "Tige 3" 3;;