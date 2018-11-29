

(* trois_ou_plus l qui teste si une liste l a au moins 3 éléments *)
let trois_ou_plus l =
  match l with
  | _ :: _ :: _ :: _ -> true
  | _ -> false

(* dernier_element l qui renvoie le dernier élément d'une liste non vide l *)
let rec dernier_element l =
  match l with
  | [x] -> x
  | x :: s -> dernier_element s
  | [] -> assert false
	   
(* somme l qui renvoie la somme des éléments d'une liste l *)
let rec somme l =
  match l with
  | [] -> 0
  | x :: s -> x + somme s
			
(* est_croissante l qui teste si une liste lest croissante *)
let rec est_croissante l =
  match l with
  | [] | [_] -> true
  | x :: y :: s -> x <= y && est_croissante (y :: s)

(* nb_occ e l qui compte le nombre d'occurrences de e dans l*)
let rec nb_occ e l =
  match l with
  | [] -> 0
  | x :: s -> (if x = e then 1 else 0) + (nb_occ e s)

(* nieme n l qui renvoie le n-ième élément de l	*)
let rec nieme n l =
  match n, l with
  | 0, x :: _ -> x
  | _, [] -> assert false
  | _, x :: s -> nieme (n-1) s 

(* max l qui renvoie le maximum d'une liste non vide l *)		       
let max l =
  let rec max_rec m l =
    match l with
    | [] ->  m
    | x :: s -> if m<x then max_rec x s else max_rec m s
  in
  match l with
  | [] -> assert false
  | m :: s -> max_rec m s
