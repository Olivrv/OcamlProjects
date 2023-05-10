type 'a arbre = Noeud of 'a * ('a arbre list);;  
type 'a arbreBino = Noeud of ('a * int) * ('a arbre list);;
let rang = function
|Noeud(_, r) -> r;;
let priorite = function
|Noeud(p, _) -> p;;
