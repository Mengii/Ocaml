let f1 (x, y) z = if z then x::[y] else [];;
let f2 x y = 4 + ((y x) 1);;
let rec f3 x = not (f3 x);;

let g (x,y) = x;;
g ('e','b');;
g (g ((1,2),3));;
(g (2.5,4)) + 4;;

let rec f4 x =
  match x with
  | [] -> []
  | z::_ -> f4 z;;

let rec foo f x =
  if x=0 then
    f x
  else
    foo (fun y -> f (x+y)) (x-1);;
foo (fun x->x) 5;;
