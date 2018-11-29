let rec f x =
  let y = x + 3 in
  if y = 0 then 100
  else y + f (x - 1)
    

let g x y =
  let v =
    if x < y then (Printf.printf "%d" x; x) else y
  in
  v+10;;
    
g 3 4;;

let f3 x y =
  if x = [] then y else y::x;;

let rec mystere x l1 l2 =
  match l1 with
  | [] -> x::l2
  | y::s -> x::y::(mystere x l2 s);;

mystere 100 [1;2;3] [4;5]
