let rec f x y =
  match x with
  | [] -> true
  | (0,_) :: _ -> false
  | (a,_) :: _ when a=y -> false
  | _ :: t -> f t y;;

(* (int * 'a) list -> int -> bool = <fun> *)

let rec somme_list l =
  match l with
  | [] -> 0
  | x::s -> x + (somme_list s);;

somme_list [3;6;9;12];;

let somme l =
  let rec somme_fold f l acc =
    match l with
    | [] -> acc
    | x::s -> f x (somme_fold f s acc)
  in
  somme_fold (+) l 0;;

somme [6;2;5];;
