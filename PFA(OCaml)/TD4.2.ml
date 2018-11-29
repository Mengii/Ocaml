exception Zero;;
let rec mult =
  function
  | [] -> 1
  | 0::_ -> raise Zero
  | x::s -> x*mult s;;
let l = [1;2;0;4;5];;
mult l;;

let rec mult2 l =
  try
    match l with
    | [] -> 1
    | 0::_ -> raise Zero
    | x::s -> x*mult s
  with
    Zero -> 0;;

mult2 l;;
