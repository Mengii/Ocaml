let couper l = 
  let rec couper_rec (l1, l2) l = 
    match l with
      | [] -> (l1, l2)
      | [x] -> (x :: l1, l2)
      | x :: y :: s ->
	couper_rec (x :: l1, y :: l2) s
  in
  couper_rec ([], []) l;;

couper [2;2;3;3;4;5;5];; (*[5;4;3;2],[5;3;2]*)

let couper l = 
  let rec couper_rec (l1, l2) l = 
    match l with
      | [] -> (l1, l2)
      | x :: s ->
	couper_rec (l2, x :: l1) s
  in
  couper_rec ([], []) l;;

couper [2;2;3;3;4;5;5];;  (*[5;3;2],[5;4;3;2]*)

let couper l = List.fold_left (fun (l1, l2) x -> (l2, x::l1)) ([],[]) l;;

couper [2;2;3;3;4;5;5];;  (*[5;3;2],[5;4;3;2]*)

let couper l = 
  let n = List.length l in
  let rec couper_rec acc l n = 
    if n = 0 then List.rev acc, l
    else couper_rec (List.hd l :: acc) (List.tl l) (n - 1)
  in
  couper_rec [] l (n / 2);;

couper [2;2;3;3;4;5;5];;  (*[2;2;3],[3;4;5;5]*)

let fusion comp l1 l2 = 
  let rec fusion_rec acc l1 l2 = 
    match l1, l2 with
      | [], l | l, [] -> List.rev_append acc l
      | x :: s1, y :: s2 -> 
	if comp x y <= 0 then fusion_rec (x :: acc) s1 l2
	else fusion_rec (y :: acc) l1 s2
  in
  fusion_rec [] l1 l2;;

let rec trier comp l = 
  match l with
    | [] | [ _ ] -> l
    | _ -> 
      let (l1, l2) = couper l in
      fusion comp (trier comp l1) (trier comp l2)

let make_list n = 
  let rec make_list_rec acc m = 
    if m = 0 then acc
    else
      let i = Random.int n in
      make_list_rec (i::acc) (m - 1)
  in
  make_list_rec [] n;;

make_list 5;; (*au moins un doublon car valeur de la liste entre 0 et n-1*)

let numerotation l = 
  let s, _ = 
    List.fold_left (fun (acc, n) x -> (x, n) :: acc, n + 1) ([], 0) l 
  in
  List.rev s;;

numerotation [10;20;30;5];; (*[(10,0);(20,1);(30,2);(5,3)]*)

let comp (x, _) (y, _) = x - y;;

comp (1,4) (2,1);;

let affiche_liste l = List.iter (fun (x, d) -> Printf.printf "%d(%d) " x d) l;;

affiche_liste [(1,14);(3,43);(2,4);(4,5)];;  (*1(14) 3(43) 2(4) 4(5)*)

let () = 
  let l = numerotation (make_list 10) in
  affiche_liste l;
  Printf.printf "\n";
  let l' = trier comp l in
  affiche_liste l';
  Printf.printf "\n";;



    
