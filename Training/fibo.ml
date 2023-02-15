let fib n = 
  let a = ref 1 in
  let b = ref 1 in
  let i = ref 0 in
  while !i <= !n - 3 do
    a := !a + !b;
    b := !a - !b;
    i := !i + 1
  done; 
  !a;;
let fibo n =
  let i = ref 0 in
  while !i <= !n do
    print_int (fib i);
    print_string "\n";
    i := !i + 1
  done;;
let n = ref 10 in
fibo n;;

  


