(* chiffrage de Cesar *)

let ca = Char.code 'a'
let cz = Char.code 'z'
let cA = Char.code 'A'
let cZ = Char.code 'Z'

let rec cesar cle l =
  match l with
  | [] -> []
  | x :: s ->
     let nx =
       let cx = Char.code x in
       if cA <= cx && cx <= cZ then
	 let k = cx - cA in
	 ((k + cle) mod 26) + cA
       else if ca <= cx && cx <= cz then
	 let k = cx - ca in
	 ((k + cle) mod 26) + ca
       else cx
     in
     (Char.chr nx) :: (cesar cle s);;

(*cesar 3 ['a';'b';'c'];; -> ['d';'e';'f']*)
       
let list_of_string s =
  let le = String.length s in
  let rec lstring i =
    if i = le then []
    else s.[i] :: lstring (i+1)
  in
  lstring 0;;

(*list_of_string "Bonjour";; -> char list = ['B';'o';'n';'j';'o';'u';'r']*)

let rec output_char_list cout l =
  match l with
  | [] -> ()
  | x :: s -> output_char cout x ; output_char_list cout s
					       
let chiffrement_canal cle cin cout =
  let rec chiffre_rec () =
    let s = input_line cin in
    let l = list_of_string s in
    let sl = cesar cle l in
    output_char_list cout sl; output_char cout '\n';
    chiffre_rec ()
  in
  try chiffre_rec () with End_of_file -> close_in cin; close_out cout
					
let () =
  let book = Sys.argv.(1) in
  let cypherbook = Sys.argv.(2) in
  let cle = int_of_string Sys.argv.(3) in
  let cin = open_in book in
  let cout = open_out cypherbook in
  chiffrement_canal cle cin cout
  
		
