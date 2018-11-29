let rec f x y =
  if x < y then print_int x; y + 2;;

f 1 2;;

let y = fun x -> x;;

let f3 x = x x
let rec f4 x y = f4 y ([]::x)
