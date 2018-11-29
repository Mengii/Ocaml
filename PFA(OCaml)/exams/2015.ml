let f1 h x y = h y && x/0 > y;;
let f2 = function Some c -> c | _ -> exit 0;;
let f3 x y = if x>y then 1 else -1;;

type ('a, 'b) t = A of 'a | B of 'b;;
let f4 x y = function
  | ([],_) -> A x
  | (x,[]) -> B (List.length x/y)
  | _ -> A "42";;

type line = {id:int; mutable org:int; mutable lth:int ref};;
let create =
  let i = ref 0 in
  fun o l -> incr i; {id= !i; org=o; lth=ref l};;
let a = create 0 10;;
let b = {a with org=2};;
let c = a;;
a;;
a.org <- 7;;
a;;
c.lth <- ref 20;;
c;;
a;;
incr b.lth;;
b;;
