
let rec decompte n =
  match n with
  | 0 -> ()
  | _ -> print_int n; decompte (n - 1);;

decompte 10;;


let compte n =
  let rec compte_rec i =
    print_int i;
    if i<n then compte_rec (i+1)
  in
  compte_rec 1;;

compte 101;;

let mystere n =
  if n > 100 then
    begin
      failwith "Trop grand";
      Printf.printf "fantome"
    end
  else if n < 0 then
    begin
      failwith "Trop petit";
      Printf.printf "fantome"
    end
  else
    compte n;;
