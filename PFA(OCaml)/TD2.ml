let rec mystere x = function
	|[] -> [x]
	|y::l -> mystere ([y]::x) l
;;

mystere [] [1;2;3];;

(* Parite 2 Iterateur*)
(*Exercice 1*)
(*1*)
let b2 = [1;2;3;'abcd'];;

let rec affiche_liste_entiers l = match l with
	| [] -> () 
	| h::t -> Printf.printf "%d \n" h; affiche_liste_entiers t;;

affiche_liste_entiers b2;;
(*2*)
let affiche_liste_entiers_bis l =
	List.iter (fun x -> Printf.printf "%d \n" x) l;;

affiche_liste_entiers_bis b2;;

(*Exercice 2*)
(*1*)
let count x l = 
	let rec aux x l acc = match l with
		| [] -> acc
		| h::t -> if x = h then aux x t (acc+1) else aux x t acc
	in
	aux x l 0
;;

(*2*)
let count_bis x l =
	List.fold_left (fun a b -> if a = b then a + 1 else a) 0 l
;;

(*Exercice 3*)
(*1*)
(*let flatten l = 
	let rec flt ls s = 
		match ls with
			| [[]] -> []
			| [x]::[[y]] -> flt ([[y]]) (x::s)
	in
	flt l []
;;*)
let rec flatten l = match l with
	| [] -> []
	| h::t -> List.append h (flatten t)
;;

(*2*)
let flatten_bis l = 
	List.fold_right ( fun a b -> List.append a b) l []
;;

let flatten_ter l =
	List.fold_left ( fun a b -> List.append a b) [] l
;;

(*Exercice 4*)
(*1*)
let fst_list_1 l =
	List.fold_right ( fun a b -> let (x,y) = a in x :: b) l [];;

let fst_list_2 l = 
	List.fold_left ( fun b a -> let (x,y) = a in x :: b) [] l;;

(*Here must be some problem because it can run on another computer, even its fonctionnement is printing out the result instead of returning a function.
let fst_list l = 
	 List.iter ( fun -> Printf.printf "%d \n" (fst x)) l;;  (*just print it, not return a function like we did in the two methods above.*)	
*)

(*Exercice 5*)
type t = B|N|R;;
