let l = "ab" :: ['a';'b'];;
let f2 x =
  let g y = y+1 in g x;;

let rec mystere x y =
  if not x then 0
  else if y then 2 + mystere y (not x)
  else 1 + mystere y (not x);;
mystere true true;;

let rec mystere1 x = function
  | [] -> [x]
  | y :: l -> mystere1 ([y]::x) l;;

mystere1 [] [1;2;3];;
