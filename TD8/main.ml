let pile_to_liste pile = 
  let p = Stack.copy pile in 
  let li = ref [] in
  while (not (Stack.is_empty p)) do
    let x = Stack.pop p in
    li := x::!li
  done;
  !li;;

let rec piletl pile =
  let p = Stack.copy pile in
  if Stack.is_empty p then []
  else let x = Stack.pop p in x::(piletl p);;

let s = Stack.create() in
Stack.push 1 s;
Stack.push 2 s;
Stack.push 3 s;
piletl s;
