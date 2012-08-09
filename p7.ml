open Printf;;


let isPrime n =
  let rec iterate i = match (i = 2) with
  | true -> ((n mod i) <> 0)
  | false -> ((n mod i) <> 0) && (iterate (i - 1))
  in
  iterate (n/2 + 1);;

let condInc f i t = match (f i) with
| true -> t + 1
| false -> t;;

let nthPrime n =
  let rec iterate i t = match (t = n && isPrime i) with
  | true -> i
  | false -> (*printf "%d\n" t;*) iterate (i + 1) (condInc isPrime i t)
  in
  iterate 3 2;;

printf "%d\n" (nthPrime 10001);;
