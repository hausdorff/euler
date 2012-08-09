open Printf;;
open List;;


let isDivisible n =
  let rec iterate i = match (i = 1) with
  | true -> (n mod i) = 0
  | false -> (n mod i) = 0 && (iterate (i - 1))
  in
  iterate 20;;

let rec findSmallest i = match (isDivisible i) with
| true -> i
| false -> findSmallest (i + 2);;

printf "%d" (findSmallest 20);;
