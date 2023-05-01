(*Exo 1*)
type 'a abs = Feuille of 'a | Noeud of 'a * 'a abs * 'a abs;;
let sosaStradonitz arbre =
  let rec aux n = function
  |Noeud(a, arb1, arb2) -> Noeud(n, (aux (2*n) arb1), (aux (2*n + 1) arb2))
  |Feuille _ -> Feuille n
  in aux 1 arbre;;

(*Exo 2*)
type arbre = Feuille | Noeud of arbre * arbre;;
let rec arbFibo = function
|0 -> Feuille
|1 -> Feuille
|n -> Noeud(arbFibo (n-1), arbFibo (n-2));;

(*TD Rab Exo 2*)
type 'a arbreb = Nil | Noeud of 'a *'a arbreb *'a arbreb ;;
let rec parcoursPrefixe = function
|Nil -> print_newline
|Noeud(a, arbg, arbd) -> (print_int a); (parcoursPrefixe arbg); (parcoursPrefixe arbd);;