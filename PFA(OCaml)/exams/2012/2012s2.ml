let f1 a b = if a=b then 1 else 2;;
let f2 g x y = (g x, g y);;
let f3 x = (x+1, x);;
let rec f4 x y = f4 y ([]::x);;
let f5 x y = if x() then y else x y;;
let rec f6 x y = match (x, y) with
    ([], v) -> v
  | (a::l, b) -> f6 l true;;

let rec f7 x = f7 (x || f7 (x && f7 x));;

let rec mystere x = function
  | [] -> 0::x
  | y::l -> mystere (y::x) l;;
mystere [0] [0;0];;
mystere [0] [1;2];;
mystere [1] [2;3];;

let rec f b =
  if !b > 0 then
  begin
    b := !b - 2;
    f b;
    print_int !b;
    print_newline ();
    b := !b + 4;
    print_int !b;
    print_newline ();
  end;;

let a = ref 9 in
  f a;
!a+20;;  
