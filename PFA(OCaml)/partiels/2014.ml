let g z b =
  match b with
  | [] -> z = 0
  | x :: l ->
     z =
       try 6/x
       with Division_by_zero -> failwith "division par zero"

let f x =
  let y = 4 + x in g (z+y);;

let h x y =
  if true then
    begin
      y := 1;
      x := !y + 2;
     end
  else ();;
let x = ref 10;;
let y = ref 99;;
h x y;;
!x;;
!y;;

let inc x = x + 1 in
let mystere y = f2 y + y

let toto () =
  let u = ref 0 in
  for i = 0 to 10 do
    Printf.printf "i = %d" i;
    print_newline ();
    u := !u + i;
    Printf.printf "u = %d" !u;
    print_newline ();
  done;
  !u;;
toto ();;
    
let rec mystere a b c =
  match a with
  | [] -> b c
  | x::l -> b (mystere l b (c+1));;

mystere [1;2;3] (fun x -> x+1) 0;;
