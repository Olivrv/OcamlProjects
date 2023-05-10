type 'a arbreBino = Noeud of 'a * ('a arbreBino list) * int;;
let rang (Noeud(_, _, r)) = r;;
let priorite (Noeud(p, _, _)) = p;;
let creerArbre c = Noeud(c, [], 0);;
let rec fusionArbre arb1 arb2 = match arb1, arb2 with 
|Noeud(cle1, liste1, r), Noeud(cle2, liste2, _) when cle1 <= cle2 -> Noeud(cle2, arb1::liste2, r+1)
|Noeud(cle1, liste1, r), Noeud(cle2, liste2, _) -> Noeud(cle1, arb2::liste1, r+1);;

type 'a tasBino = 'a arbreBino list;;
let rec insererArbre tas arbre = match tas with
|[] -> [arbre]
|t::q when rang(t) < rang(arbre) -> arbre::t::q
|t::q when rang(t) = rang(arbre) -> insererArbre tas (fusionArbre t arbre)
|t::q when rang(t) > rang(arbre) -> insererArbre q arbre;;

let rec insererElement tas n = match tas with
|