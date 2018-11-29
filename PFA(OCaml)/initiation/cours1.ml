(*Ocaml est un langage dit fonctionnel, donc tous est une fonction.*)

let x = 2;;
let y = 3;;

x+y;;  (*car tout est fonctionnel => int = 5*)

let x = 2 in x+2;; (*x vaut 2 pour ce calcul la => int = 4*)

let x = 1 in let y = 5 in x+y;; (*int = 6*)

x+y;;(*ici int=5 encore*)

let x = 2 in let y = x+1 in let x=3 in x+y;; (*j'ai redefini x=3 au lieu de 2 =>int = 6*)

let x=2 and y=5 in x+y;; (*int = 7*)

let x = 2.5;; (*type float*)
let s = "salut";; (*type string*)
let c = 'a';; (*type char*)

[1;2;3;4;5];; (*type int list*)
['x'];;       (*type char list*)

not(false);;  (*bool = true*)
2=3=false;;   (*bool = true*)
false=2=3;;   (*Error*)

let double x = 2*x;;
double 5;;
let x = double 3;;
double x;; (*=>12*)

let rec fact x =
  match x with
    0 -> 1
  | e -> e * fact(e-1);;
fact 5;;
(*
  # #trace fact;;
  # fact 3;;
  fact <-- 3
  fact <-- 2
  fact <-- 1
  fact <-- 0
  fact --> 1
  fact --> 1
  fact --> 2
  fact --> 6
  - : int = 6
  
  # #untrace fact;;
*)

let rec somme1 l = match l with
    [] -> 0
  | [e] -> e
  | e :: reste -> e + somme1(reste);;
somme1 [1;2;3;4];;

type couple = int*char;;
let rec somme2 l = match l with
    [] -> 0
  | [e] -> fst(e)
  | e :: reste -> fst(e) + somme2 (reste);;
somme2 [(1,'a');(2,'c');(4,'e')];;  (*=> 7*)

let somme3 = fun l -> List.fold_left(fun x y -> x+y) 0 l;;
somme3 [1;2;3;4;5;6;7];;             (* int list -> int = <fun> *)

let nbElem = fun l -> List.fold_left (fun acc x -> 1+acc) 0 l;;
nbElem [1;2;3;4;5;6;7];;             (* 'a list -> int = <fun> *)

let plusUn = fun l -> List.map (fun x -> x+1) l;;
plusUn [1;2;3;4;5;6;7];;             (* int list -> int list = <fun> *)

let boolListToInt = fun l -> List.fold_left (fun x y -> (2*x)+(if y then 1 else 0)) 0 l;;
boolListToInt [true;false;true];; (*2*2+1 = 5*)
