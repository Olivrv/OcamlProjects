let kruskal g w = 
  let n = Array.length g in 
  let t = Array.make n in [] in 
  let rec aux l b = match
    |[] -> []
    |(u,v)::q -> if not chemin t u v then ajout_arete t u v; aux q in
    aux(tri_arete q w)

//TODO chemin, tri_arete