open String;;
open Num;;
open Printf;;


exception CharException;;

let nstr =
  "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450";;

(* Slices a string *)
let slice n m str =
  let len = String.length str in
  let payload = match m >= len with
  | true -> String.make (len - n) 'a'
  | false -> String.make (m - n) 'a'
  in
  let rec slide n m i t =
    match (i >= m, i >= n) with
    | (true,_) -> payload
    | (false,true) ->
        payload.[t] <- str.[i];
        slide n m (i + 1) (t + 1)
    | (false,false) -> slide n m (i + 1) t
  in
  match m >= len with
  | true -> slide n len 0 0
  | false -> slide n m 0 0;;

let charToInt c =
  let i = (int_of_char c) - 48 in
  match (i < 0 || i > 9) with
  | true -> raise CharException
  | false -> i;;

let productChars str =
  let len = String.length str in
  let rec strToIntList i = match (i < len) with
  | true -> (charToInt str.[i])::(strToIntList (i + 1))
  | false -> []
  in
  List.fold_left ( * ) 1 (strToIntList 0);;

let slidingMaxProduct n str =
  let max = ref 0 in
  let len = String.length str in
  let adjustMax num = match (!max > num) with
  | true -> ()
  | false -> max := num
  in
  let rec iterate i = match (i <= len) with
  | true ->
      (let s = slice (i - n) i str in
      adjustMax (productChars s);
      iterate (i + 1))
  | false -> !max
  in
  iterate n;;

(*printf "%d\n" (slidingMaxProduct 5 nstr);;*)



(* Smaller code *)

let comp n m =
  if n = m then 0
  else if n < m then 1
  else -1;;

let rec toIntList i str =
  if i >= (String.length str) then []
  else int_of_string (String.sub str i 1) :: toIntList (succ i) str;;

let rec product li = match li with
| a::b::c::d::e::tl -> printf "%d %d %d %d %d %d\n" a b c d e (a*b*c*d*e); a * b * c * d * e :: product (b::c::d::e::tl)
| _ -> [];;

let tmp = (product (toIntList 0 nstr));;
let mx = List.hd (List.sort comp tmp);;

let _ = printf "%d\n" mx;;

(* Smaller code *)
