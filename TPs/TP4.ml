(*0: Up Left
  1: Up Right
  2: Right Up
  3: Right down
  etc
*)
let deplace (i,j) = 
  let arrives = [] in (i+2, j-1)::(i+2, j+1)::(i-2, j-1)::(i-2, j+1)::(i-1, j-2)::(i-1, j+2)::(i+1, j-2)::(i+1, j+2)::arrives;;
let valide (n, p) (x,y) = 
  0<=x && x<n && 0<=y && y<p;;

let rec filter p li = match li with
|[] -> []
|t::q when p t -> t::(filter p q)
|t::q -> filter p q;;

let deplacements_valides (n,p) (x,y) = 
  filter (valide (n,p)) (deplace (x,y));;
let set matrix pos value =
  let x = fst pos and y = snd pos in
  matrix.(x).(y) <- value;;
let get matrix pos = 
  let x = fst pos and y = snd pos in
  matrix.(x).(y);;
let resoudre dimensions = 
  let (n,p) = dimensions in
  let echequier = Array.make_matrix n p true in
  let successeur = Array.make_matrix n p (-1,-1) in
  let rec parcours nb_parcourues prpos pos =
    set successeur prpos pos;
    if (nb_parcourues = (n*p) - 1) then true
    else 
      begin
      set echequier pos false;
      let d = deplacements_valides (n,p) pos in
      if (List.exists (parcours (nb_parcourues +1) pos) (filter (fun coord -> get echequier coord) d)) 
        then true
      else begin
          set echequier pos true;
          false 
          end
      end
  in if parcours 0 (0,0) (0,0) then chemin(successeur) else [];;

let chemin successeur pos = 
  




