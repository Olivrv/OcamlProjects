type 'a arbre = 
  | Feuille 
  | Noeud of 'a arbre * 'a * 'a arbre;;
let arbre_simple valeur = Noeud(Feuille, valeur, Feuille)

let rec hauteur arbre = match arbre with
|Feuille -> 1
|Noeud(bebe1, _, bebe2) -> 
  let max i j = 
    if i > j 
      then i 
    else j 
  in 1 + max (hauteur bebe1) (hauteur bebe2);;

hauteur (arbre_simple 1)

let rec noeuds arbre = match arbre with
|Feuille -> 0
|Noeud(bebe1, _, bebe2) -> 1 + noeuds bebe1 + noeuds bebe2;;

let cheminement arbre = 
  let rec aux arbre acc = match arbre with
  |Feuille -> acc
  |Noeud(bebe1, _, bebe2) -> aux bebe1 (acc + 1) + aux bebe2 (acc + 1) 
in aux arbre 0;;

let exemple = Noeud(Noeud(arbre_simple 2, 5, Noeud(arbre_simple 6, 7, arbre_simple 8)), 9, Noeud(Feuille, 11, Noeud(arbre_simple 13, 15, Feuille)));;
hauteur exemple;;
noeuds exemple;;
cheminement exemple;;

let rec contenu arbre = match arbre with
|Feuille -> []
|Noeud(Feuille, noeud, Feuille) -> [noeud]
|Noeud(bebe1, valeur, bebe2) -> (contenu bebe1)@[valeur]@(contenu bebe2);;

let rec minimum arbre = match arbre with
|Feuille -> failwith "nope"
|Noeud(Feuille, noeud, _) -> noeud
|Noeud(arbreg, _, arbred) -> minimum arbreg;;

let rec maximum arbre = match arbre with
|Feuille -> failwith "nope"
|Noeud(_, noeud, Feuille) -> noeud
|Noeud(arbreg, _, arbred) -> maximum arbred;;

maximum exemple;;

let rec recherche x = function
|Feuille -> false
|Noeud(arbreg, valeur, arbred) -> 
  if x = valeur then true 
  else (
    if valeur < x then recherche x arbred 
    else recherche x arbreg 
  );;

let rec insere x arbre = match arbre with
|Feuille -> Noeud(Feuille, x, Feuille)
|Noeud(ag, v, ad) -> 
  if x > v then Noeud(ag, v, (insere x ad)) 
  else Noeud((insere x ag), v, ad);;

let rec cree liste = match liste with
|[] -> Feuille
|t::q -> insere t (cree q);;

