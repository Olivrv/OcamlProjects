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