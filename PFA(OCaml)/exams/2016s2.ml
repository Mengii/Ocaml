let f1 x f = if x>0 then f x else 5.2;;
let rec f2 a b = if a<0.5 then a else (f2 b a)+.10.;;
let f3 x y = if x<y then y::[x] else [];;
let rec f4 f x =
  match f x with
  | (true, (y,_)) -> f4 f y
  | (_, (_, z)) -> f4 f z;;

let f g l =
  List.map (fun u -> List.fold_left g 0 u) l;;
let v1 = f (fun x y -> x+y) [[1;2;3];[];[1;4];[2]];;
let v2 = f (fun x y -> x+1) [[3];[1;2];[];[2;2;3]]
