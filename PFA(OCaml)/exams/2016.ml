let f1 g (x,y) = (g x, y);;
let f2 x y = try x/y with Division_by_zero -> print_string "erreur";;
let f3 x y = if !x < y then x := y;;
let rec f4 (x,y) = (f4 (y, x-1)) +. 1.;;

type t = {mutable a : int list; b : bool};;
let rec f x =
  match x.a with
  | [] -> raise Not_found
  | _::l -> x.a <- l; f x ;;
let rec g x =
  match x.a with
  | [] -> raise Not_found
  | _::l -> g {x with a = l};;

let v1 =
  let x = {a=[1;2]; b=false} in
  try f x with Not_found -> x.a;;

let v2 =
  let x = {a=[1;2]; b=false} in
  try g x with Not_found -> x.a;;
  
