type note = Do | Re | Mi | Fa | Sol | La | Si ;;
type hauteur = { note : note; octave: int } ;;
type duree = Blanche | Noire ;;
type signe = Note of hauteur * duree | Silence of duree ;;
type partition = { signe : signe list; tempo : int };;

let frequence { note = n; octave = o } =
  let f0 =
    match n with
    | Do -> 33
    | Re -> 37
    | Mi -> 41
    | Fa -> 44
    | Sol -> 49
    | La -> 55
    | Si -> 62
  in
  f0 * truncate (2. ** float o);;

let millisecondes d t =
  let quarter = 60000/t in
  match d with
  | Half -> quarter * 2
  | Quarter -> quarter ;;

let son t s =
  match s with
  | Note (p,d) ->
     let f = frequence p in
     Graphics.son f (millisecondes d t)
  | Silence r ->
     Graphics.son 0 (millisecondes r t);;

let jouer_partition {signe = l;tempo = t }=
  List.iter (son t) l ;;
