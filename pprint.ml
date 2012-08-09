module Pprint = struct
  open Printf;;

  let rec loop s del li = match li with
    | [] -> ()
    | (x::xs) ->
      Printf.printf s x;
      Printf.printf del;
      loop s del xs;;

  let parr s li =
    Printf.printf "[|";
    loop s "; " li;
    Printf.printf "|]\n";;

  let plist s li =
    Printf.printf "[";
    loop s "; " li;
    Printf.printf "\b]\n";;

end;;
