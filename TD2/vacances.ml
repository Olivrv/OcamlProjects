(*Exo 1*)
let rec somme liste = match liste with
|[] -> 0
|t::q -> t + somme q

(*Exo 2*)
let rec mini l = match l with
|[] -> failwith "none"
|[a] -> a
|t::q -> let m = mini q in if t < m then t else m
let l = [8; 6; 7; 4; 2; 0; -2]
let m = mini l;;

(*Exo5*)
let rec reverse liste = match liste with
|[] -> []
|t::[a] -> a::[t]
|t::q -> (reverse q)@[t]

let betterReverse liste =
  let rec transfer reversed = function
  |[] -> reversed
  |t::q -> transfer (t::reversed) q
  in transfer [] liste


(*Exo 3*)
let rec a_der = function
|[] -> failwith "none"
|t::[a] -> t
|t::q -> a_der q;;

(*Exo 4*)
let rec prefixes = function 
| [] -> []
| t::q -> [t]::(List.map (fun a -> t::a) (prefixes q));;

(*Exo 6*)
let c_gauche = function
|[] -> []
|t::q -> q@[t];;
let rec c_gauche2 = function
|[] -> []
|[a] -> [a]
|a::b::q -> b::c_gauche2 (a::q);;
c_gauche2 [1;2;3;4;5];;
let c_droite l = reverse (c_gauche (reverse l))

(*Exo 7*)
(*a)*)
let inter li1 li2 = 
  let rec aux l1 l2 l3 = match l1 with
  |[] -> l3
  |t::q -> if List.mem t l2 then aux q l2 (t::l3) else aux q l2 l3 in
  aux li1 li2 [];;
inter [1;2;3;4;5] [2;3;8];;

(*b)*)
let rec union l2 = function
|[] -> l2
|t::q -> if List.mem t l2 then union l2 q else union (t::l2) q;;

(*Exo 8*)
(*a)*)
let clean_last l = 
  let rec aux l_i l_f = match l_i with
  |[] -> l_f
  |t::q -> if List.mem t q then aux q l_f else aux q (t::l_f) in
  aux l [];;

(*b)*)
let rec clean_first l lf = match l with
|[] -> lf
|t::q -> if List.mem t lf then clean_first q lf else clean_first q (t::lf);;
