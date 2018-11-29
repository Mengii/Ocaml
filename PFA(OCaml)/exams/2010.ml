(* jeudi le 14 decembre 2017 *)
(* Examen 2010 *)

(* exo 1.1*)
let rec f x y = match (y, x) with
  | ([], 0) -> true
  | (y::l, z) -> (y = z mod 2) && (f (z/2) l)
  | _ -> false;;

(* exo 1.1*)
let f l = List.map (fun (g, x) -> (g x) + 1) l;;

(* exo 3.1 *)
let rec ack x = match x with
	| (0,n) -> n+1
	| (m,0) -> ack (m-1, 1)
	| (m,n) -> ack (m-1, ack (m,n-1)) ;;
(* val ack : int * int -> int = <fun> *)
ack(4,0) ;; (* - : int = 13 *)
ack(5,0) ;; ne marche pas 

(* exo 3.2
au final sans memorisation, on fait un nombre exponentiel d'appel à File 
avec memorisation, on fait n appel à File *)

let ack_memo x = 
	(* let ackhash = Hashtbl.create 1000000000 in *)
	let ackhash = Hashtbl.create 100 in 
	let rec ackbis y = 
		try
			Hashtbl.find ackhash y 
		with Not_found -> match y with 
			| (0,n) -> let tmp = n+1 in (Hashtbl.add ackhash y tmp; tmp;) 
			| (m,0) -> let tmp = ackbis (m-1,1) in (Hashtbl.add ackhash y tmp; tmp;)
			| (m,n) -> let tmp = ackbis (m-1,ackbis (m,n-1)) in (Hashtbl.add ackhash y tmp; tmp)
	in ackbis x ;;
(* val ack_memo : int * int -> int = <fun> *)


ack_memo(3,0) ;;  
(* - : int = 5 *)
ack_memo(4,0) ;;
(* - : int = 13 *)
ack_memo(5,0) ;;
(* - : int = 65533 *)
ack_memo(6,0) ;;
(* Stack overflow during evaluation (looping recursion?). *)


let dim = 10 ;;

let uf = create (dim * dim) ;;

let t = random_bool_matrix () ;;

calcul_parties_connexes uf t ;;

uf ;; 




(* exo 4 *)
let dim = 2 ;;
let inc_color = 83 ;;
let pas = 50 ;;
let draw_pixel i j c = 
	set_color c ;
	fill_rect (pas*i) (pas*j) pas pas ;;

(* exo4.1 renvoir une nouvelle matrice de dimension dim x dim cintnant un nombre aleatoire de cases noires *)
let random_bool_matrix () = 
	Array.init dim (fun x -> Array.init dim (fun y -> Random.bool())) ;;
(* val random_bool_matrix : unit -> bool array array = <fun> *)


(* exo4.2 en utilisant la fonction draw_pixel cette fonction affihe une image en noir et blanc. *)
let draw_BW_image mtrx = 
	for i = 0 to dim -1 do 
		for j = 0 to dim - 1 do 
			if mtrx.(i).(j) then 
				draw_pixel i j balck
			else draw_pixel i j white 
			  ;
		done ;
	done ;;


(* exo4.3 *)
type uf = int array ;; 

let create n = Array.init n (fun i -> i) ;;

let find t i = t.(i) ;; 

let union t i j = 
	let ri = find t i in 
	let rj = find t j in 
	for k = 0 to Array.length t - 1 do 
		if t.(k) = rj then t.(k) <- ri 
	done ;;


let int_of_pixel (i,j) = i * dim + j ;;


let calcul_parties_connexes uf t = 
	let p = Array.length t in 
	for i = 0 to p - 1 do 
		for j = 0 to p -1 do 
			if i < p-1 then 
				if t.(i).(j) = t.(i+1).(j) then 
					union uf (int_of_pixel (i,j)) (int_of_pixel ((i+1),j)) ;
			if j < p - 1 then 
				if t.(i).(j) = t.(i).(j+1) then 
					union uf (int_of_pixel (i,j)) (int_of_pixel (i,(j+1))) ;
		done 
	done ;;
(* val calcul_parties_connexes : 'a array -> 'b array array -> unit = <fun> *)



let new_color = 
	let r = ref 10 in 
	let g = ref 10 in 
	let b = ref 10 in 
	fun () -> 
		let b' = !b + inc_color in 
		let g' = !g + (b' / 255 * inc_color) in 
		let r' = !r + (g' / 255 * inc_color) in 
		b := b' mod 256;
		g := g' mod 256;
		r := r' mod 256;
		rgb !r !g !b
;;


let uf = create (dim * dim) ;;

let t = random_bool_matrix () ;;

calcul_parties_connexes uf t ;;


let colors = ref [] ;;
let hash (i,j) = find uf (i * dim + j) ;;
let get_color p = List.assoc (hash p) !colors ;;
let add_color p c = colors := (hash p, c) :: !colors ;;

let color_of_pixel (i, j) = 
	try 
		get_color (i, j)
	with Not_found -> let c = new_color () in add_color (i, j) c; c ;;


let draw_COLOR_image t = 
	let p = Array.length t in 
	for i = 0 to p - 1 do 
		for j = 0 to p - 1 do 
			let c = color_of_pixel (i,j) in 
			draw_pixel i j c 
		done 
	done ;;


let () = 
	open_graph (Printf.sprintf " %dx%d" (pas*dim + 50) (pas*dim + 50)) ;
	draw_BW_image t ;
	ignore (read_key()) ;
	(* draw_COLOR_image uf; *) 
	draw_COLOR_image t ;
	ignore (read_key());





