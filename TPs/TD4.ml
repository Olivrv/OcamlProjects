(*0: Up Left
  1: Up Right
  2: Right Up
  3: Right down
  etc
*)
let deplace (i,j) = 
  let arrives = [] in (i+2, j-1)::(i+2, j+1)::(i-2, j-1)::(i-2, j+1)::(i-1, j-2)::(i-1, j+2)::(i+1, j-2)::(i+1, j+2)::arrives;;
let valid (n, p) (x,y) = 
  if 0<=x && x<n && 0<=y && y<p then true
  else false;;
