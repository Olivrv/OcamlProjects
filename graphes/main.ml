let kruskal g w = 
  let n = Array.length g in 
  let t = Array.make n in [] in 
  let rec aux l b = match 
    |[] -> []
    |(u,v)::q -> if not chemin t u v then ajout_arete t u v; aux q in
    aux(tri_arete q w)

//TODO chemin, tri_arete
  
let rec chemin t u v = 
  (*Parcours en profondeur*)
  let n = Array.length t in
	let visited = Array.make n false in
	let p = Stack.create () in
	stack.push r p;
	while not in stack.is_empty p do
		let u = stack.pop p in
		if not visited.(u) then (
			visited.(u) <- true;
			List.iter (fun v -> stack.pool v p ) g.(u))
	done;
;;
  