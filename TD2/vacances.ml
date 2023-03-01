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

