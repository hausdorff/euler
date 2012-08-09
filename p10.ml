open Printf;;
open Num;;
open Sieve;;


let rec printList li = match li with
| [] -> ()
| x::xs ->
    printf "%d\n" (int_of_num x);
    printList xs;;

let rec sumNums li = match li with
| [] -> (num_of_int 0)
| x::xs -> (num_of_int x) +/ (sumNums xs);;

let _ =
  let primes = Sieve.sieve 2000000 in
  printf "%s\n" (string_of_num (sumNums primes));;

