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

let ajoute f x =
  let n = Array.length f.tab in
  if (f.fin + 1) mod n = f.deb then failwith "File pleine"
  else 
    f.fin <- (f.fin + 1) mod n;
    f.tab.(f.fin) <- x;;

let retire f = 
  let n = Array.length f.tab in
  if f.vide = true then failwith "No way"
  else 
    let a = f.tab.(f.deb) in 
    f.deb <- (f.deb - 1) mod n;
    if f.deb = f.fin then (f.vide <- true;)
    a;;
     
