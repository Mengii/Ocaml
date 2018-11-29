
let rec syracuse u_n =
  Printf.printf "%d\n" u_n;
  if u_n > 1 then
    if u_n mod 2 = 0 then 
      syracuse (u_n / 2) else syracuse (3 * u_n + 1)
;;

let () = 
  let u_0 = int_of_string (Sys.argv.(1)) in
  syracuse u_0
;;




let lancer_de () = Random.int 6 + 1
;;

let jouer () =

  let rec jouer_aux score =
    let n = lancer_de () in
    let new_score = n + score in
    Printf.printf "De : %d (score = %d)\n" n new_score;
    if n < 5 then new_score else jouer_aux new_score
  in
  jouer_aux 0
;;


let () =
  Random.self_init ();
  Printf.printf "Score: %d\n" (jouer ())
;;



