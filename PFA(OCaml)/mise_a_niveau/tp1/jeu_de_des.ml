let lancer_de () = Random.int 6 + 1 ;;

let jouer () =
  let rec jouer_aux score =
    let n = lancer_de ()  in
    let new_score = n + score in
    Printf.printf "De : %d (score = %d)\n" n new_score;
    if n < 5 then new_score else jouer_aux new_score
  in
  jouer_aux 0 ;;

let () =
  Random.self_init ();
  Printf.printf "Score : %d\n" (jouer ()) ;;
