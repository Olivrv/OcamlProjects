let rec delta_etoile m etat u = match m with
|[] -> etat
|[t::q] -> let d = m.delta in delta_etoile m (d etat t) q;;

let est_synchronisant m u = 
  let ok = ref true in
  let q0 = delta_etoile m 0 u in 
  for k = 1 to m.n_etats do
    if delta_etoile m k u <> q0 then 
      ok := false;
  done;
  !ok;;

  
 
