let f1 x y z =
  if x>y then z x else z y;;
let f2 a b = a::(b a);;
let rec f3 x y =
  if x<10.5 then 0 else f3 (y-. 1.) x;;
let rec f4 x = f4 x;;

let rec f x y =
  match x with
  | [] -> y
  | _::r -> f y r;;

f [3;2;1] [1;2];;
f [] [1;2;3];;
f [] [];;
f [3;4;5] [];;
