let z = 9.5;;
let f x = x +. z;;
let z =
  let z = z +. 2.3 in let x = f z in z +. x in z *. z;;

(*
# #trace z;;
z is not a function.
# #trace f;;
f is now traced.
# let z =
  let z = z +. 2.3 in let x = f z in z +. x in z *. z;;
  f <-- 11.8
f --> 21.3
- : float = 1095.61000000000013
# #untrace f;;
f is no longer traced.
*)

let rec mystere x y =
  x := !x - 5;
  print_int !x;
  print_newline ();
  y := max !x !y;
  print_int !y;
  print_newline ();
  !x - !y;;

let x = ref 10 in
mystere x x;;

let x = ref 10 in
let y = ref 10 in
mystere x y;;

let x = ref 10 in
let y = x in
mystere x y;;
