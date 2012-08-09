open Num;;
open Printf;;
open Array;;


let num = 4000000;;

let basis = Array.make (num + 1) (num_of_int 0);;
basis.(0) <- (num_of_int 0);;
basis.(1) <- (num_of_int 1);;
basis.(2) <- (num_of_int 1);;

let populateCache n =
  let a1 = basis.(n-1) in
  let a2 = basis.(n-2) in
  let result = a1 +/ a2 in
  basis.(n) <- result;
  n;;

let cachefib n =
  let rec fibHelper n acc = match ((int_of_num basis.(acc)) <= n) with
  | true -> fibHelper n (populateCache (acc + 1))
  | false -> populateCache n
  in
  fibHelper n 2;;

let target = cachefib num;;

(*
let sumEvenFib =
  let rec helper curr = match ((int_of_num basis.(curr)) <= num) with
  | true -> printf "%d %d %d\n" (int_of_num basis.(curr)) curr num; (int_of_num (basis.(curr))) + (helper (curr + 2))
  | false -> 0
  in
  helper 0;;
*)

let sumEvenFib =
  let rec helper curr =
    let tmp = int_of_num basis.(curr) in
    let test = (tmp <= num),((tmp mod 2) = 0) in
    match test with
    | (false,_) -> 0
    | (true,false) -> helper (curr + 1)
    | (true,true) -> tmp + helper (curr + 1)
  in
  helper 0;;

(*
let sumEvenFib n acc =
  let x = ref 0 in
  let acc = ref (num_of_int 0) in

  while (int_of_num basis.(!x)) <= n do
    printf "%d %d %d\n" !x (int_of_num !acc) (int_of_num basis.(!x));
    acc := !acc +/ basis.(!x);
    x := !x + 2;
  done;

  !acc;;
*)

let fibSum n =
  let rec helper curr =
    let tmp = (int_of_num basis.(curr)) in
    let cond = (tmp <= n),((tmp mod 2) = 0) in
    match cond with
    | (false,_) -> 0
    | (true,false) -> (helper (curr + 1))
    | (true,true) -> tmp + (helper (curr + 1))
  in
  helper 0;;

(*printf "%d %d\n" (int_of_num (sumEvenFib target 0)) target;;*)
(*printf "%d %d\n" sumEvenFib target;;*)
printf "%d %d\n" (fibSum target) target;;
