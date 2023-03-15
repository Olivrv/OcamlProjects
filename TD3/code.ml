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



