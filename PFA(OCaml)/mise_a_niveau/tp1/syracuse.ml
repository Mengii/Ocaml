let rec syracuse u_n =
  Printf.printf "%d\n" u_n;
  if u_n > 1 then
    if u_n mod 2 = 0 then
      syracuse (u_n / 2) else syracuse (3 * u_n + 1);;

let () =
  let u_0 = int_of_string (Sys.argv.(1)) in
  syracuse u_0 ;;

(* exemple
   ./syracuse 20
  
   20
   10
   5
   16
   8
   4
   2
   1
*)
