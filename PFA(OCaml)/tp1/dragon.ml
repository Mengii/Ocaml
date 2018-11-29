
open Graphics

let () = open_graph " 400x400"
       
let rec draw n x y z t =
  if n = 1 then
    begin
      let x, y = int_of_float x, int_of_float y in
      let z, t = int_of_float z, int_of_float t in
      moveto x y; lineto z t
    end
  else
    begin
      let u = (x +. z +. t -. y) /. 2. in
      let v = (y +. t -. z +. x) /. 2. in
      draw (n-1) x y u v;
      draw (n-1) z t u v
    end

let () =
  draw 20 20. 20. 220. 220.;
  ignore(read_key())
    
  
