(*Exo 1*)
let rec somme liste = match liste with
|[] -> 0
|t::q -> t + somme q

(*Exo 2*)
let rec mini l = match l with
|[] -> failwith "none"
|[a] -> a
|t::q -> min t (mini q)
let l = [8, 6, 7, 4, 2, 0]
let m = mini l;;