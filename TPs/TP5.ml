type 'a arbre = 
  | Feuille 
  | Noeud of 'a arbre * 'a * 'a arbre;;
let arbre_simple valeur = Noeud(Feuille, valeur, Feuille)

let rec hauteur arbre = match arbre with
|Feuille -> 0
|Noeud(bebe1, valeur, bebe2) -> 
  let max i j = 
    if i > j 
      then i 
    else j 
  in 1 + max (hauteur bebe1) (hauteur bebe2);;

hauteur (arbre_simple 1)

let rec noeuds arbre = match arbre with
|Feuille -> 0
|Noeud(bebe1, valeur, bebe2) -> 1 + noeuds bebe1 + noeuds bebe2;;

let cheminement arbre = 
  let rec aux arbre acc = match arbre with
  |Feuille -> acc
  |Noeud(bebe1, valeur, bebe2) -> aux bebe1 (acc + 1) + aux bebe2 (acc + 1) 
in aux arbre 0;;
