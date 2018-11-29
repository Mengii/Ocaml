let f1 x e = if x=0 then raise Exit; e (x+1);;
let f2 g (x,y) = g y (g x y);;
let f3 x = if x > 'a' then "oui" else "non";;
let f4 g x = g (g (x +. 0.3) +. 2.);;
let rec f5 g x = f5 g (g x);;

let x = 10;;
let f y = y + x;;
let x = 100;;
f 3;;

let x = 10;;
let f y =
  let x = y + x in
  let y = y * x in
  y + x + 100;;

f 10;;
