open Printf;;
open Lazy;;


let pyth a b = sqrt ((a *. a) +. (b *. b));;

let sum a b c = (a +. b +. c);;

let result a b c = (a * b * c);;

let rec find a b =
  let c = pyth a b in
  let s = sum a b c in
  match (s = 1000.0, s > 1000.0) with
  | (true, _) -> (a,b,c)
  | (false, true) -> find (a +. 1.0) a
  | (false, false) -> find a (b +. 1.0);;

let a,b,c = find 1.0 1.0;;
printf "%f %f %f\n" a b c;;
printf "%f\n" (a *. b *. c);;
