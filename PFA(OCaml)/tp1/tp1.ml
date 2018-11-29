(* CONTROL+X+S pour enregister *)
(* CONTROL+X+E pour exécuter *)
(*1 ere etape: faire dans le terminal emacs docname.ml*)
(*2 eme etape: ocamlc docname.ml -o docname*)
(*3 eme etape: ./docname*)

(* exoA *)
let a = 5;;
let b = a;;


let trois_ou_plus l = match l with
  |x1::x2::x3::l -> true (* _::_::_::_ -> true *)
  |_ -> false;;
trois_ou_plus [1;2;3];;

let rec dernier_element l = match l with
  |[]->0 (* assert false *)
  |[x]->x
  |x1::l1 -> dernier_element l1;;
dernier_element [1;2;3];;

let rec somme l = match l with
  |[] -> 0
  |[x] -> x (* pa besoin *)
  |x1::l1 -> x1 + somme l1;;
somme [1;2];;

let rec est_croissante l = match l with
  |[] -> true
  |x1::x2::l1 -> if x1 <= x2 then est_croissante (x2::l1) else false
  |x3 -> true;;
est_croissante [2;1;2];;

let rec nb_occ e l = match l with
  |[] -> 0
  |x1::l1 -> if x1 = e then 1 + nb_occ e l1 else nb_occ e l1;;
nb_occ 2 [5;3;3;3];;

let rec nieme n l = match l with
  |[] -> 0
  |x1::l1 -> if n = 1 then x1 else nieme (n-1) l1;;
nieme 3 [1;2;3;4];; (* manque cas d'un elt *)

let rec max l = match l with
  |[] -> 0
  |[x] -> x
  |x1::x2::l1 -> if x1 >= x2 then x1 else max (x2::l1);;
max [4;2;1;9];;  (* faux *)

(* exoB *)

(* let decaler_1 c = char_of_int (int_of_char c +1);;
   decaler_1 'a';; *)

let rec cesar k l = match l with
  |[]->[]
  |x::l1-> (char_of_int(int_of_char x +k))::cesar k l1;;  
cesar 9 ['a';'b'];;

let rec list_of_string s = let rec fct s1 acc l =
			     if acc = String.length s1 then l
			     else fct s1 (acc+1) (s1.[(String.length s1-acc-a)]::l)
			   in fct s 0 [];;
list_of_string "abcd";;


