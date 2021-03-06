(* représentation des arbres quaternaires *)
#load "graphics.cma";;
open Graphics

let black = Graphics.black
let white = Graphics.white
  
type 'a t = F of 'a | N of 'a t * 'a t * 'a t * 'a t;;
type image_tree = int t;;


let a0  =
  N (
    N (
      F (white),
      F (black),
      F (black),
      F (black)),
    F (black),
    F (white),
    F (black));;

(* décompression *)
let rec get_pixel x y l a = match a with
  | F(c) -> c
  | N (so, se, no, ne) ->
     let ml = l / 2 in
     if x >= ml then
       if y >= ml then
	 get_pixel (x - ml) (y - ml) ml ne
       else
	 get_pixel (x - ml) y ml se
     else
       if y >= ml then
	 get_pixel x (y - ml) ml no
       else
	 get_pixel x y ml so ;;

get_pixel 2 1 4 a0;;


let image_matrix_of_tree l a =
  let m = Array.make_matrix l l 0 in
  for i = 0 to l-1 do
      for j = 0 to l-1 do
	m.(i).(j) <- get_pixel i j l a
      done
  done
    ;m;;


image_matrix_of_tree 4 a0;;
