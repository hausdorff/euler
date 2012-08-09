open Printf;;
open Num;;


let rec siter f n = match (eq_num n (num_of_int 1)) with
| true -> f (num_of_int 1)
| false -> (f n) +/ (siter f (n -/ (num_of_int 1)));;

let rec sum n = siter (function x -> x) n;;

let rec sumOfSq n = siter (function x -> x*/x) n;;

let rec sqOfSum n =
  let s = sum n in
  s */ s;;


let p3 =
  let n = (num_of_int 100) in
  let sq = sqOfSum n in
  let su = sumOfSq n in
  sq -/ su;;


(* Small *)

let rec range x y =
  if x > y then []
  else x :: range (succ x) y;;

let nums = range 1 100;;

let sumOfSquares = List.fold_left (+) 0 (List.map (fun x -> x*x) nums);;

let squareOfSums =
  let sum = (List.fold_left (+) 0 nums) in
  sum * sum;;

let _ =
  print_int (squareOfSums - sumOfSquares);
  print_newline ();;

(* Small *)


printf "%d\n" (int_of_num p3);;
