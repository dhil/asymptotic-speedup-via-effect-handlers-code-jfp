open Generic_search
open Queens

let main () =
  let usage appname =
    Printf.printf "usage: %s <naive|berger|pruned|eff|bespoke> <one|all> <n> [no-repeat]\n%!" appname
  in
  let string_of_result = function
    | `One None -> "None"
    | `One (Some xs) -> Printf.sprintf "Some [%s]" (String.concat "; " (List.map string_of_int xs))
    | `All xss -> Printf.sprintf "%d" (List.length xss)
  in
  let mode = match String.lowercase_ascii (Array.get Sys.argv 2) with
    | "one" -> `One
    | "all" -> `All
    | _         -> usage (Array.get Sys.argv 0); exit 1
    | exception _ -> usage (Array.get Sys.argv 0); exit 1
  in
  let n =
    try int_of_string (Array.get Sys.argv 3)
    with _ -> usage (Array.get Sys.argv 0); exit 1
  in
  let (module Searcher : GENERIC_SEARCH) =
    match String.lowercase_ascii (Array.get Sys.argv 1) with
    | "naive"     -> (module Naive_Search)
    | "berger"    -> (module Fun_Search)
    | "pruned"    -> (module Mod_Search)
    | "eff"       -> (module Eff_Search)
    | "bespoke"   ->
       let f () = match mode with
         | `One -> `One (Bespoke_Queens.find_one n)
         | `All -> `All (Bespoke_Queens.find_all n)
       in
       let (result, elapsed) = Time2.run f in
       print_endline (string_of_result result);
       Printf.printf "%f\n%!" elapsed;
       exit 0
    | _           -> usage (Array.get Sys.argv 0); exit 1
    | exception _ -> usage (Array.get Sys.argv 0); exit 1
  in
  let nqueens =
    match String.lowercase_ascii (Array.get Sys.argv 4) with
    | "no-repeat" -> n_queens'
    | _ -> n_queens
    | exception _ -> n_queens'
  in
  let f () =
    match mode with
    | `One -> `One (Searcher.find_one (nqueens n))
    | `All -> `All (Searcher.find_all (nqueens n))
  in
  let (result, elapsed) = Time2.run f in
  print_endline (string_of_result result);
  Printf.printf "%f\n%!" elapsed

let _ = main ()
