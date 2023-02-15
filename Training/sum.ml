let sum n = 
  let s = ref 0 in
  let i = ref 1 in
  while !i <= !n do
    s := !i + !s;
    i := !i + 1
  done;
  !s;;
let n = ref 10 in
print_endline (string_of_int (sum n))

let sum_ar l = 
  let s = ref 0 in
  let len = Array.length l in
  for k = 0 to len - 1 do
    s := !s + l.(k);
  done;
  !s;;
let l = [|1; 2; 3|] in
print_endline (string_of_int (sum_ar l))


