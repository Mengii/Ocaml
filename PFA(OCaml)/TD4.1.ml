type humain = {prenom : string; mutable age : int};;
let san = { prenom = " Sandrine"; age = 10};;
san.age <- 29;;

let annif h = h.age <- h.age + 1;;
annif san;;
san.age;;


let rt = [|1;2|];;
let rt' = Array.copy rt;;
let _ = rt.(0) <- 10;;
rt;;
rt';;

let t = Array.make 3 0;;
let m = Array.make 3 t;;
let () = m.(0).(0) <- 10;;
m;;
m.(1).(0);;
t.(0);;

type humain2 = {prenom : string; mutable age : int ref};;
let san = { prenom = " Sandrine"; age = ref 10};;
san.age :=  26;;

let mic = {san with prenom="Michel"};;
let annif2 h2 = h2.age := !(h2.age) + 1;;
annif2 mic;;
san.age;;
mic.age;;
