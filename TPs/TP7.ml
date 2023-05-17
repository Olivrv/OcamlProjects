(*Ecrire une fonction prenant en paramètre un entier p>=1
    et un tableau carré de côté p d'entiers t et renvoyant un 
    booléen disant si ce tableau est un carré latin ie contenant dans 
    chaque ligne et chaque colonne exactement tous les entiers de 1 à p*)

let sum_zero t = 
  let s = ref 0 in
  let zero = ref false in
  for i = 0 to (Array.length(t)-1) do
    if t.(i) <= 0 then zero := true 
    else s := !s + t.(i)
  done;
  if !zero then 0 else !s;; 
let transpose t =
  let p = Array.length(t) in
    for i = 0 to p-1
    do for j = 0 to p-1
    do let a = t.(i).(j) in
    t.(i).(j) <- t.(j).(i);
    t.(j).(i) <- a
  done;
  done;
  t;;
let nom_de_fonction_cool t p = 
  let s = ((p+1)*p)/2 in
  let bob = ref true in
  for k = 0 to (p-1) do
    print_int (sum_zero t.(k));
    if (sum_zero t.(k)) <> s then bob := false
  done;
  let tr = transpose t in
  for k = 0 to (p-1) do
    if (sum_zero tr.(k)) <> s then bob := false
  done;
  !bob;;
let c2 = [|[|1;2|];[|2;1|]|];;
let c3 = [|[|3;1;2|];[|1;2;3|];[|2;3;1|]|];;
let c4 = [|[|4;1;2;3|];[|1;2;3;4|];[|2;3;4;1|];[|3;4;1;2|]|];;
let c42 = [|[|4;1;2;3|];[|1;4;3;2|];[|2;3;4;1|];[|3;2;1;4|]|];;
nom_de_fonction_cool c42 4;;
