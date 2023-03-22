(*Exo 1*)
let est_palindrome arr =
  let b = ref true in
  let l = Array.length arr in
  for k = 0 to (l/2) do
    if arr.(k) <> arr.(l-k) then b := false 
  done;
  !b;;

let est_palindrome arr = 
  let rev = 
    let reverse liste =
      let rec transfer reversed = function
      |[] -> reversed
      |t::q -> transfer (t::reversed) q
      in transfer [] liste
     in reverse arr
    in
  arr = rev;;

est_palindrome [1, 1, 1];;
est_palindrome [1, 0, 1];;
est_palindrome [1, 1, 0];;

let est_palindrome tab = 
  let length = Array.length tab in
  let rec aux i = if i = -1 then true else ((tab.(i) = tab.(length - i - 1) && aux(i-1))) in
  aux ((length/2)-1);;

(*Exo 2*)
let ordre_N2 a b = (fst a < fst b) || (fst a = fst b && snd a <= snd b)
let ordre_lex a b =
  let length = Array.length a in
  let rec aux i = i = length || a.(i) < b.(i) || a.(i) = b.(i) && aux(i+1) in (length = Array.length b) && (length = 0 || aux 0);;
ordre_lex [|2, 3, 4, 5|] [|1, 2, 3, 4|]

(*Exo 4*)
let cribble_simple n =
  let tab = Array.make n 0 in tab

let rec begaie l = match l with
|[] -> []
|t::q -> t::t::(begaie q);;

begaie [1; 2; 3] 

let rec max l = match l with
|[] -> failwith "nope"
|[a] -> a
|t::q -> let m = max q in if t > m then t else m;;
max [1;2;3;4;5;6;7; -1; 9; -8]

let decoupe l p = 
  let rec aux li lp = function
  |[] -> li, lp
  |t::q -> if t < p then aux (t::li) lp q else aux li (t::lp) q in
  aux [] [] l

let decoupe_strict l p = 
  let rec aux li lp np = function
  |[] -> li, lp
  |t::q -> if t < p then aux (t::li) lp np q else if t > p then aux li (t::lp) np q else aux li lp (np+1) q in
  aux [] [] 0 l

let ieme l i = 
  let aux x = x in l;;
let cube x = x*x*x
let armstrong n = let a = n mod 10 in let b = (n-a)/10 mod 10 in let c = (n - 10*b - a)/100 in n = (cube a) + (cube b) + (cube c)