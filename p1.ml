open Printf;;


let isMultOf n m = (n mod m) = 0;;
let addIf n b = match b with
  | true -> n
  | false -> 0;;

(* Sums all multiples of 3 or 5 below 1000 *)
let rec p1 n =
  let isMultOf3Or5 = (isMultOf n 3) or (isMultOf n 5) in
  match (n > 0) with
  | true -> (addIf n isMultOf3Or5) + (p1 (n - 1))
  | false -> 0;;

printf "%d" (p1 999);;
