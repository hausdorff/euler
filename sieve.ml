open Num;;
open LazyList;;

module Sieve =
struct
  (*
   * Sieve of Eratosthenes, conceptually pulled directly from the Wikipedia
   * article[1]. Takes an integer, outputs a list of prime integers [2..n].
   *
   * [1] http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
   *)

  (* Sieve of Eratosthenes that uses bignums *)
  let lzsieve n =
    Printf.printf "%s\n" (string_of_num n);;

  (* Sieve of Eratosthenes that uses regular ints *)
  let sieve n =
    let arr = Array.make n false in
    (* Step 3 from #Algorithm description section of the wiki article *)
    let rec step3 p factor =
      let np = p * factor in
      match (np >= n) with
	| true -> step4 (succ p)
	| false ->
          arr.(np) <- true;
          step3 p (succ factor)
    and
        (* Step 4 from Algorithm description section of the wiki article *)
	step4 p = match (p >= n) with
	  | true -> []
	  | false ->
	    (match arr.(p) with
	      | true -> step4 (succ p)
	      | false -> p :: (step3 p 1))
    in step4 2;;

end;;
