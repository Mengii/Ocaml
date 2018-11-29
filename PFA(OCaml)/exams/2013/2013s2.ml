let f1 g (x, y) = (g x, y);;
let f2 x y = try x/y with Division_by_zero -> Print_string "erreur";;
let rec f3 x y =
  match (x,y) with
  | ([], 0) -> true
  | (u::l, z) -> f3 l (z+1)
  | _ -> false;;
let f4 x y = if true then [x] else y;;
let rec f5 (x,y) = (f5 (y, x - 1)) +. 1.5;;

let rec mystere1 x =
  if !x <= 1 then ()
  else
    let y = !x in
    print_int y;
    print_newline ();
    begin
      decr x;
      print_int !x;
      print_newline ();
      mystere1 x;
      x := y + !x;
      print_int !x;
      print_newline ()
     end;;
let x = ref 5 in (mystere1 x; !x);;

type t = A of int list | B of int;;
let f l =
  List.fold_left
    (fun acc x -> match x with
    | B i -> i*10 + acc
    | A v ->
       List.fold_left (fun u v -> u + v) acc v) 0 l;;
f [A ([2;3]); B 2; A ([4;5]); B 3]
