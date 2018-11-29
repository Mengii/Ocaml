let couper l = 
  let rec couper_rec (l1, l2) l = 
    match l with
      | [] -> (l1, l2)
      | [x] -> (x :: l1, l2)
      | x :: y :: s ->
	couper_rec (x :: l1, y :: l2) s
  in
  couper_rec ([], []) l;;

(*couper [10;5;6;7;3;2;8];;  -> [8;3;6;10],[2;7;5]*)

let couper l = 
  let rec couper_rec (l1, l2) l = 
    match l with
      | [] -> (l1, l2)
      | x :: s ->
	couper_rec (l2, x :: l1) s
  in
  couper_rec ([], []) l;;

(*couper [10;5;6;7;3;2;8];; ->  [2;7;5],[8;3;6;10]*)

let couper l = List.fold_left (fun (l1, l2) x -> (l2, x::l1)) ([],[]) l;;

(*couper [10;5;6;7;3;2;8];; -> [2;7;5],[8;3;6;10]*)

let couper l = 
  let n = List.length l in
  let rec couper_rec acc l n = 
    if n = 0 then List.rev acc, l
    else couper_rec (List.hd l :: acc) (List.tl l) (n - 1)
  in
  couper_rec [] l (n / 2);;

(*couper [10;5;6;7;3;2;8];; -> [10;5;6],[7;3;2;8]*)

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
      fusion comp (trier comp l1) (trier comp l2);;

let make_list n = 
  let rec make_list_rec acc m = 
    if m = 0 then acc
    else
      let i = Random.int n in
      make_list_rec (i::acc) (m - 1)
  in
  make_list_rec [] n;;

(*make_list 3;; -> [1;1;0] --> au moins un doublon car valeur de liste entre 0 et n-1*)

let numerotation l = 
  let s, _ = 
    List.fold_left (fun (acc, n) x -> (x, n) :: acc, n + 1) ([], 0) l 
  in
  List.rev s ;;

(*numerotation [2;3;5;6;8;9];; -> [(2,0);(3,1);(5,2);(6,3);(8,4);(9,5)]*)

let comp (x, _) (y, _) = x - y;;

let print_list l = List.iter (fun (x, d) -> Printf.printf "%d(%d) " x d) l;;

(*print_list [(2,58);(3,33);(1,12)];; -> 2(58) 3(33) 1(12)*)

let () = 
  let l = numerotation (make_list 10) in
  print_list l;
  Printf.printf "\n";
  let l' = trier comp l in
  print_list l';
  Printf.printf "\n";;

(*
    0(0) 1(1) 0(2) 4(3) 0(4) 9(5) 1(6) 2(7) 5(8) 4(9)
    0(0) 0(2) 0(4) 1(1) 1(6) 2(7) 4(3) 4(9) 5(8) 9(5)
 
  ==> stable
*)


    
