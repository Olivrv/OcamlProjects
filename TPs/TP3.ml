(*Exo 1*)
let reverse l = 
  let rec aux l1 l2 = match l1 with
  |[] -> l2
  |t::q -> aux q (t::l2)
in aux l [];;

(*Exo 2*)
let rec foldr f z l = match l with

|[] -> failwith "           ė̸͇͉̊͋̄̃ͅr̸̩͚͓̾̐̈́͒͠r̴̠͖̙͓͗̋͠ó̸͇̝̦̘̒̈͒ͅṛ̵̢̖̦͛"
|[a] -> f(a,z)
|t::q -> f(t, (foldr f z q))

let foldl g z l =
  let l = reverse l in
  let rec aux g z l = match l with

  |[] -> failwith "         ė̸͇͉̊͋̄̃ͅr̸̩͚͓̾̐̈́͒͠r̴̠͖̙͓͗̋͠ó̸͇̝̦̘̒̈͒ͅṛ̵̢̖̦͛"
  |[a] -> g(a,z)
  |t::q -> g(aux g z q ,t) in aux g z l;;

(*Exo 3*)
let minimax l =
  let rec aux l min max = match l with
  |[] -> (min, max)
  |t::q ->  if t < min then aux q t max else 
    if t > max then 
      aux q min t 
    else
      aux q min max 
  in let a::_ = l in aux l a a ;;
minimax [1;2;2;3;4;7;8;6;7];;

let rec minimax = function
|[] -> failwith "       ė̸͇͉̊͋̄̃ͅr̸̩͚͓̾̐̈́͒͠r̴̠͖̙͓͗̋͠ó̸͇̝̦̘̒̈͒ͅṛ̵̢̖̦͛"
|[a] -> (a,a)
|t::q -> let (u,v) = minimax q in
        if t > v then (u,t) else 
          if t < u then (t,v) 
          else (u,v)
(*Exo 4*)
let rec concat = function
|[] -> []
|t::q -> t@(concat q);;
concat [[1;2;3;4];[5;6;7;8;9]];;

(*Exo 5*)
let ajoute l a = a::l;;
let supprime l = 
  if l = [] then failwith "ė̸͇͉̊͋̄̃ͅr̸̩͚͓̾̐̈́͒͠r̴̠͖̙͓͗̋͠ó̸͇̝̦̘̒̈͒ͅṛ̵̢̖̦͛" 
  else 
    let t::q = reverse l in (t, reverse q);;

