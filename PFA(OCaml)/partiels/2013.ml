let x = 10;;

let x =
  let x =
    let y =
      let x = 10 + x in
      let x = float_of_int x in
      x +. x
    in y = float x
  in not x;;

let rec mystere1 a b =
  if a < b then (0, a) else
    let (x, y) = mystere1 (a - b) b in (x + 1, y);;

mystere1 10 2;;
