(*Exo 1*)
let f x y =   
  let c = !x in
  (x:=!y); 
  (y:=c);;
(*Exo 2*)
let fib n =
  let a = ref 1 in
  let b = ref 1 in
  for k = 3 to n do
    b := !a + !b;
    a := !b - !a;
  done;
  !b;;

(*Exo 3*)
let nb_occurrence tab x =
  let s = ref 0 in
  for i = 0 to Array.length tab - 1 do 
    if tab.(i) = x then s := !s + 1
  done;
  !s;;

(*Exo 4*)
let map f tab =
  let l = Array.length tab - 1 in
  for i = 0 to l do
    f tab.(i)
  done;;

(*Exo 5*)
let est_palindrome tab = 
  let length = Array.length tab in
  let rec aux i = if i = -1 then true else (aux(i-1) && tab.(i) = tab.(length - i - 1)) in
  aux ((length/2)-1);;


(*Exo 6*)
let maxi tab =
  let indices = ref [0] in
  let max = ref tab.(0) in
  let n = Array.length tab in 
  for i = 1 to (n-1) do 
    if tab.(i) > !max then 
      (
      max := tab.(i);
      indices := [i]
      )
    else (
      if tab.(i) = !max then
        indices := i::!indices
    )
  done;
  !indices;;

maxi [|1;2;3;2;2;3|]
