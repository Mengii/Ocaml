let f1 x y = (x, not y);;

let rec f2 a b =
  match a with
  | [] -> not b
  | x::l -> f2 l x;;

let rec f3 f g h = f g && g h;;

let f4 x y =
  if x y then [x] else [y];;

type 'a t = G of string | D of 'a;;
let f x = if x = 0.0 then G("erreur") else D(1.0 /. x);;
let g h x = h (D x);;

let f l = List.map (fun x -> incr x;print_int !x; if !x>2 then (ref 10) else x) l;;
f (let r = ref 1 in [r;r;r;r]);;

let rec f b =
  if !b <= 0 then raise Exit;
  begin
    b := !b - 2;
    f b;
    b := !b + 4
  end
;;
let a = ref 9 in try f a; !a+20 with Exit -> !a;;

let mystere a =
  let r = ref 0 in
  let l = ref a in
  while !l<>[] do
    let x = List.hd !l in
    r := !r + x;
    print_int !r;
    print_newline ();
    l := List.tl !l
  done;
  !r;;
mystere [4;1;5;3];;
