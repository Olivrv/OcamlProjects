let numero (i,j) = 
  i*9 + j;;

let coordonnes n = 
  let i = n / 9 and j = n mod (9) in (i,j);;

let meme_bloc (i, j) (k, l) = 
  (j / 3 ) = (l / 3) && (i / 3) = (k / 3);;

let condition m n = match (n,m) with
|(x,y) when x=y -> false
|_ -> let (i,j) = coordonnes m and (k,l) = coordonnes n in
    (i=k) || (j=l) || meme_bloc (i,j) (k,l);;

let verification grille m v = 
  let ok = ref true in
  for k = 0 to 80 do
    if k <> m then 
      if condition m k then
        let (i,j) = coordonnes k in
        let valeur = grille.(i).(j) in
        ok := ((!ok)||(valeur=v))
   done;
   !ok;;

let case_vide grille = 
  let k = ref 0 and zero = ref false in 
  while !k <> 81 && not !zero do
    let (i,j) = coordonnes !k in
    if grille.(i).(j) = 0 then zero := true
    else k := !k + 1
  done;
  !k;;

let sudoku grille =
  let rec exploration grille = match (case_vide grille) with
  |81 -> true
  |n -> let (i,j) = coordonnes n in let b = ref true in
    for v = 1 to 9 do
      if (verification grille n v) && (!b) then 
        grille.(i).(j) <- v;
        if exploration grille then b := false 
    done;
    b := false; 
    !b;
  in exploration grille;
  if (case_vide grille) = 81 then raise Not_found;;

let grille = 
  [|
  [|2;5;0;0;3;0;9;0;1|];
  [|0;1;0;0;0;4;0;0;0|];
  [|4;0;7;0;0;0;2;0;8|];
  [|0;0;5;2;0;0;0;0;0|];
  [|0;0;0;0;9;8;1;0;0|];
  [|0;4;0;0;0;3;0;0;0|];
  [|0;0;0;3;6;0;0;7;2|];
  [|0;7;0;0;0;0;0;0;3|];
  [|9;0;3;0;0;0;6;0;4|]
  |];;

