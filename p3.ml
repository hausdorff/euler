open Num;;
open Pprint;;
open Sieve;;

let x = num_of_string "600851475143";;

let primes = Sieve.sieve 10;;
let lzprimes = Sieve.lzsieve (num_of_int 10);;

let _ =
  Pprint.plist "%d" primes;;
