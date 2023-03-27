let rec decoupe l p = match l with
|[] -> ([],[])
|t::q -> let (linf, lsup) = decoupe q p in
          if t < p then (t::linf, lsup) else (linf, t::lsup);;

let decoupe_strict l p = l, l, p;;
let rec concat = function
|[] -> []
|t::q -> t@ (concat q);;
concat [[1;2];[3];[4;2;5]];;

let rec ieme l i = match l with
  |[] -> 0
  |t::q -> let (linf, lsup, ni) = decoupe_strict l t in
  let n = List.length(linf) in if i <= n then ieme linf i else if i <= n + ni then t else ieme lsup (i-n-ni);;

type pol_d = P of int list
let x = P [1]
let rec image_d n p = match p with
|[] -> 0
|t::q -> t + n*(image_d n q);;

let rec somme_d p1 p2 = match p1, p2 with
|(p, []) -> p
|([],p) -> p
|(t::q, t2::q2) -> (t+t2)::(somme_d q q2)    

let rec prode_mc a d pol =
  if d > 0 then 0::(prode_mc a (d-1) pol) else match pol with
  |[]->[]
  |t::q-> (a*t)::(prode_mc a d q)

let prode_d pol1 pol2 =
  let rec aux pol1 pol2 d = match pol1 with
  |[] -> []
  |t::q -> somme_d (prode_mc t d pol2) (aux q pol2 (d+1)) in
  aux pol1 pol2 0;;

let rec div pol1 pol2 = match pol1, pol2 with
|([],[]) -> [1],[]
|([],p) -> [],p
|(p,[]) -> failwith "nope" 
|(t1::q1,t2::q2) when snd(t1) < snd(t2) -> ([],t1::q1);;

let div pol1 pol2 = 
  let (coef_2, deg_2) = domin pol2 in 
  let rec aux q r =
    let (coef_r, deg_r) = domin r in
    print_string "P";
    print_int deg_r;
    print_int (coef_r/coef_2);
    if coef_r >= deg_2 then
      let newP = [(coef_r/coef_2, coef_r - coef_2)]  in aux (somme_d q newP) (somme_d r (sub_c produit (pol2 newP)))
    else (q, r) in aux [] pol1
(*sub change le signe du polynôme, domin donne le coefficient dominant et le degre *)