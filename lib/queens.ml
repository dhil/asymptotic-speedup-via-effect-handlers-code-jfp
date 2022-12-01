(* Comparing solutions to the n queens problem via various
   generic search algorithms and via a bespoke implementation. *)

(* Daniel Hillerström, November 2018, heavily based on John Longley's
   implementation in SML. *)

open Generic_search
(* All our generic search operations take an input of type SearchProblem.
   We here give two ways of presenting n queens as a SearchProblem,
   parametric in n. *)

(* First way: nQueens : int -> SearchProblem *)

(* This doesn't refrain from querying the proposed solution p
   repeatedly on the same argument i.
   E.g. (p 0) is evaluated afresh for each comparison with some (p i), i>0.
   All our generic search implementations are robust enough to cope with this. *)

let test_queen_pair : (int -> int) -> int -> int -> bool
  = fun p i j ->
  let (jpos, ipos) =
    (* The let-bindings may appear to be irrelevant, but they are
       absolutely crucial to ensure left-to-right evaluation order.
       Writing `(p j, p i)` is equivalent to `let ipos = p i in let
       jpos = p j in ...` in OCaml. The exact evaluation order is
       crucial to guarantee that the search procedures yield the
       results in the same order. *)
    let jpos = p j in
    let ipos = p i in
    jpos, ipos
  in
  not (ipos = jpos || ipos - jpos = i - j || ipos - jpos = j - i)

let rec test_queen_pairs_from : (int -> int) -> int -> int -> bool
  = fun p i j ->
  i = j || (test_queen_pair p i j && test_queen_pairs_from p i (j + 1))

let rec test_queens_from : int -> (int -> int) -> int -> bool
  = fun n p i ->
  i = n || (test_queen_pairs_from p i 0 && test_queens_from n p (i + 1))

let n_queens : int -> search_problem
  = fun n ->
  { dimensions = List.tabulate n (fun _ -> n);
    property = fun p -> test_queens_from n p 0 }

(* Second way: nQueens : int -> SearchProblem *)

(* This remembers results of earlier queries to p, and in fact asks for
   each of p 0, p 1, ... once only, in this order. *)
let rec test_against : int -> int -> int list -> bool
  = fun y j zs ->
  match zs with
  | [] -> true
  | z :: zs ->
     z <> y && abs (z - y) <> j && test_against y (j + 1) zs

let rec test_from : int -> (int -> int) -> int -> int list -> bool
  = fun n p i zs ->
  if i = n then true
  else let y = p i in
       test_against y 1 zs && test_from n p (i + 1) (y :: zs)

let n_queens' : int -> search_problem
  = fun n ->
  { dimensions = List.tabulate n (fun _ -> n);
    property = (fun p -> test_from n p 0 []) }

(* Efficiency comparisons between nQueens and nQueens' are interesting
   (see below for some typical tests).  In general, nQueens' is
   faster, though more dramatically so for some generic search
   algorithms than others.  In particular, pruned search (without
   backup) is much more competitive for nQueens' than for nQueens.
   Advantage of backup is more strikingly visible for nQueens. *)

(* BESPOKE implementation of n queens problem, for comparison *)

module Bespoke_Queens = struct
  let test_queen_pair : int array -> int -> int -> bool
    = fun a i j ->
    let (jpos, ipos) = a.(j), a.(i) in
    not (ipos = jpos || ipos - jpos = i - j || ipos - jpos = j - i)

  let rec test_queen_pairs_from : int array -> int -> int -> bool
    = fun a i j ->
    j = i || (test_queen_pair a i j && test_queen_pairs_from a i (j + 1))

  exception Found of int array

  let rec search_all_extensions : int -> int array -> int -> unit
    = fun n a i ->
    if i = n then raise (Found a)
    else search_all_extensions_from n a i 0
  and search_all_extensions_from : int -> int array -> int -> int -> unit
    = fun n a i j ->
    if j = n then ()
    else (a.(i) <- j;
          ignore(if test_queen_pairs_from a i 0
                 then search_all_extensions n a (i + 1) else ());
          search_all_extensions_from n a i (j + 1))

  let rec search_all_extensions' : int list list -> int -> int array -> int -> int list list
    = fun acc n a i ->
    if i = n then Array.to_list a :: acc
    else search_all_extensions_from' acc n a i 0
  and search_all_extensions_from' : int list list -> int -> int array -> int -> int -> int list list
    = fun acc n a i j ->
    if j = n then acc
    else (a.(i) <- j;
          let acc' =
            if test_queen_pairs_from a i 0
            then search_all_extensions' acc n a (i + 1)
            else acc
          in
          search_all_extensions_from' acc' n a i (j + 1))

  let find_one : int -> int list option
    = fun n ->
    try
      search_all_extensions n (Array.make n 0) 0;
      None
    with Found a -> Some (Array.to_list a)

  let find_all : int -> int list list
    = fun n -> List.rev (search_all_extensions' [] n (Array.make n 0) 0)
end
