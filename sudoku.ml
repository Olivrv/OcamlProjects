let numero (i,j) = 
  i*9 + j;;
let coordonnes n = 
  let i = n / 9 and j = n mod (9) in (i,j);;
let meme_bloc (i, j) (k, l) = 
  (j / 3 ) = (l / 3) && (i / 3) = (k / 3);;
let condition m n = match (n,m) with
|(n,n) -> false
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
  while k <> 81 && not zero do
    let (i,j) = coordonnes k in
    if grille.(i).(j) = 0 then zero := true
    else k := !k + 1
  done;
  !k;;
let rec sudoku grille =
  let vide = case_vide grille in
  let (i,j) = coordonnes vide in
  if vide = 81 then 
    failwith "NotFound"
  else 
    let vtest = ref 1 in
    let notfound = ref true in
    while vtest <= 9 && notfound do
      if verification vide vtest then
        grille.(i).(j) <- !vtest;
        notfound := false;
        sudoku grille
      else vtest := !vtest + 1
    done;
    if notfound then 
