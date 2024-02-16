(* OCaml implementations of some generic search algorithms,
   including one using 'effect handlers'. *)

(* Daniel Hillerström, November 2018, based heavily on John Longley's
   implementation in SML. *)

(* We give four implementations of generic search using different methods
   and language features, all cast as instances of a signature GENERIC_SEARCH.
   Search spaces are hypercuboids of any dimension: e.g. the list [3,4,5,6]
   specifies the space of points (x0,x1,x2,x3) where the xi are natural numbers
   and x0<3, x1<4, x2<5, x3<6 (a space of size 3x4x5x6).
   Points themselves are represented by functions p : int->int
   (e.g. p i = xi for i=0,1,2,3).
*)

module List = struct
  include List

  exception Size
  let tabulate : int -> (int -> 'a) -> 'a list
    = fun n f ->
    if n < 0 then raise Size
    else
      let rec aux i n f =
        if i = n then []
        else f i :: aux (i + 1) n f
      in aux 0 n f
end

type property = (int -> int) -> bool

type search_problem = {
    dimensions: int list;
    property: property
  }

type 'a operation = property -> int array -> int array -> 'a

module type GENERIC_SEARCH = sig
  val name : string
  val find_one : search_problem -> int list option
  val find_all : search_problem -> int list list
end

(* Naive exhaustive search done generically.
   Not remotely competitive, but included here for completeness. *)

module Naive_Search : GENERIC_SEARCH = struct
  let name = "naive"

  exception Finished

  let rec next_point' : int array -> int array -> int -> int array
    = fun maxs coords i ->
    if coords.(i) < maxs.(i)
    then (coords.(i) <- coords.(i) + 1; coords)
    else if i > 0 then (coords.(i) <- 0; next_point' maxs coords (i - 1))
    else raise Finished

  let next_point : int array -> int array -> int array
    = fun maxs coords ->
    next_point' maxs coords (Array.length coords - 1)

  let next_point1 : int array -> int array -> int array option
    = fun maxs coords ->
    try
      Some (next_point maxs coords)
    with
    | Finished -> None

  let rec find_one' : (int list option) operation
    = fun property maxs coords ->
    if property (fun i -> coords.(i))
    then Some (Array.to_list coords)
    else let point = next_point maxs coords in
         find_one' property maxs point

  let rec find_all' : (int list list) operation
    = fun property maxs coords ->
    if property (fun i -> coords.(i))
    then
      let xs = Array.to_list coords in
      let point = next_point maxs coords in
      xs :: find_all' property maxs point
    else
      let point = next_point1 maxs coords in
      find_all'' property maxs point
  and find_all'' : property -> int array -> int array option -> int list list
    = fun property maxs coords' ->
    match coords' with
    | Some coords -> find_all' property maxs coords
    | None -> []

  exception Empty_Space

  let decrement : int -> int = function
    | n when n > 0 -> n - 1
    | _ -> raise Empty_Space

  let start_search : 'a operation -> 'a -> search_problem -> 'a
    = fun search_op default prob ->
    let property = prob.property in
    let dimensions = prob.dimensions in
    try
      let maxs = Array.of_list (List.map decrement dimensions) in
      let coords = Array.make (List.length dimensions) 0 in
      search_op property maxs coords
    with
    | Empty_Space
      | Finished -> default

  let find_one : search_problem -> int list option
    = fun prob -> start_search find_one' None prob

  let find_all : search_problem -> int list list
    = fun prob -> start_search find_all' [] prob
end

(* 'Pure functional' implementation of findOne using Berger's algorithm. *)
(* Version with vectors: no in-place update. *)

module Fun_Search : GENERIC_SEARCH = struct
  let name = "fun"

  let memoize : (unit -> int array) -> unit -> int array
    = fun thunk ->
    let cache : 'a option ref = ref None in
    fun () ->
    match !cache with
    | Some x -> x
    | None   -> let x = thunk () in
                cache := Some x; x

  (* (\* Berger's algorithm: selection function for a property P *\)
   * let select : property -> int list -> int array
   *   = fun property all_dimensions ->
   *   let rec best_shot solution i = function
   *     | [] -> solution
   *     | d :: dims ->
   *        let partial_solution = Array.concat [solution; [|i|]] in
   *        if i = d - 1
   *        then best_shot partial_solution 0 dims
   *        else let f = best_fun partial_solution dims in
   *             if property f
   *             then Array.init (List.length all_dimensions) f
   *             else best_shot solution (i + 1) (d :: dims)
   *   and best_fun partial_solution dims =
   *     let fallback = memoize (fun () -> best_shot partial_solution 0 dims) in
   *     fun i -> if i < Array.length partial_solution then partial_solution.(i)
   *              else let result = fallback() in
   *                   result.(i)
   *   in
   *   best_shot [||] 0 all_dimensions *)

  (* Select function now restructured to expose best_fun (called by find_all) *)
  let rec best_shot : ((int -> int) -> bool) -> int -> int array -> int list -> int -> int array
    = fun p len sol dims i ->
    match dims with
    | [] -> sol
    | (d :: dims) ->
       let sol' = Array.concat [sol; [|i|]] in
       if Int.equal i (d-1)
       then best_shot p len sol' dims 0
       else let f = best_fun p len sol' dims in
            if p f then Array.init len f
            else best_shot p len sol (d::dims) (i+1)
  and best_fun : ((int -> int) -> bool) -> int -> int array -> int list -> (int -> int)
    = fun p len sol dims ->
    let fallback = memoize (fun () -> best_shot p len sol dims 0) in
    (fun i -> if i < Array.length sol
              then Array.get sol i
              else Array.get (fallback ()) i)

  let select : property -> int list -> int array
    = fun p dims ->
    let len = List.length dims in
    best_shot p len [||] dims 0

  let find_one : search_problem -> int list option
    = fun prob ->
    let property = prob.property in
    let dimensions = prob.dimensions in
    let best = select property dimensions in
    if property (fun i -> best.(i))
    then Some (Array.to_list best)
    else None

  let find_all : search_problem -> int list list
    = fun prob ->
    let p = prob.property in
    let dims = prob.dimensions in
    let len = List.length dims in
    (* Find all solutions in the fresh subtree determined by sol *)
    let rec find_all' sol pos rem_dims acc =
      match dims with
      | [] -> if p (fun i -> Array.get sol i)
              then Array.to_list sol :: acc
              else acc
      | _  -> let f = best_fun p len sol rem_dims in
              if p f
              then find_all'' sol pos rem_dims (Array.init len f) acc
              else acc
    and find_all'' partial_sol pos dims full_sol acc =
      match dims with
      | [] -> Array.to_list full_sol :: acc
      | d :: dims ->
         let i = Array.get full_sol pos in
         let acc' = find_all'' (Array.concat [partial_sol; [|i|]]) (pos + 1) dims full_sol acc in
         let rec find_in_subtree j acc'' =
           if j = d then acc''
           else find_in_subtree (j+1)
                  (find_all' (Array.concat [partial_sol; [|j|]])
                     (pos + 1) dims acc'')
         in
         find_in_subtree (i+1) acc'
    in
    find_all' [||] 0 dims []
end

(* Generic pruned search using a modulus functional *)

module Mod_Search : GENERIC_SEARCH = struct
  let name = "mod"
  (* Modulus operation for properties of 'points' (i.e. potential solutions) *)
  (* Example of a sequentially realizable functional: uses local state *)
  (* This is the only 'essential' use of an impure (imperative) feature in
     this approach, although we do allow ourselves the use of mutable arrays
     below for a bit more efficiency. *)

  (* returns value of (property point), along with list of the
     distinct indices at which point was inspected, most recent first,
     and their values at these indices *)
  let property_with_modulus : property -> int array -> (bool * (int * int) list)
    = fun property point ->
    let inspected = Array.make (Array.length point) false in
    let log : (int * int) list ref = ref [] in
    let result =
      property (fun i ->
          (if inspected.(i) then ()
           else (inspected.(i) <- true;
                log := (i, point.(i)) :: !log));
          point.(i))
    in
    (result, !log)

  (* E.g. propertyWithModulus (fn g =>g(0)+g(2)+g(0)+g(1)=7)
     (tabulate(4,fn i=>i+1)) ; returns (true,[(1,2),(2,3),(0,1)]) *)

  exception Finished

  let rec next_point' : int array -> (int * int) list -> (int * int) list
    = fun maxs log ->
    match log with
    | [] -> raise Finished
    | (i, j) :: log' when j < maxs.(i) ->
       (i, j + 1) :: log'
    | _ :: log' ->
       next_point' maxs log'

  let rec update_array_with : 'a array -> (int * 'a) list -> 'a array
    = fun arr xs ->
    match xs with
    | [] -> arr
    | (i, x) :: xs ->
       arr.(i) <- x; update_array_with arr xs

  let next_point : int array -> (int * int) list -> int array
    = fun maxs log ->
    let initial = Array.make (Array.length maxs) 0 in
    let point = next_point' maxs log in
    update_array_with initial point

  let next_point1 : int array -> (int * int) list -> int array option
    = fun maxs log ->
    try
      Some (next_point maxs log)
    with
    | Finished -> None

  let rec find_one' : property -> int array -> int array -> int list option
    = fun property maxs coords ->
    match property_with_modulus property coords with
    | (true, _) -> Some (Array.to_list coords)
    | (false, log) -> find_one' property maxs (next_point maxs log)

  let rec find_all' : property -> int array -> int array -> int list list
    = fun property maxs coords ->
    match property_with_modulus property coords with
    | (true, log) ->
       let result =
         let initial = Array.make (Array.length maxs) (-1) in
         Array.to_list (update_array_with initial log)
       in
       result :: find_all'' property maxs (next_point1 maxs log)
    | (false, log) ->
       find_all'' property maxs (next_point1 maxs log)
  and find_all'' : property -> int array -> int array option -> int list list
    = fun property maxs coords' ->
    match coords' with
    | None -> []
    | Some coords -> find_all' property maxs coords

  exception Empty_Space

  let decrement : int -> int = function
    | n when n > 0 -> n - 1
    | _ -> raise Empty_Space

  let start_search : 'a operation -> 'a -> search_problem -> 'a
    = fun search_op default prob ->
    let property = prob.property in
    let dimensions = prob.dimensions in
    let maxs =
      Array.of_list (List.map decrement dimensions)
    in
    let coords =
      Array.make (List.length dimensions) 0
    in
    try
      search_op property maxs coords
    with
    | Empty_Space
    | Finished -> default

  let find_one : search_problem -> int list option
    = fun prob -> start_search find_one' None prob

  let find_all : search_problem -> int list list
    = fun prob -> start_search find_all' [] prob
end

(* Generic search using effect handlers.  This provides the effect of
   both pruning and backup. *)
module Eff_Search : GENERIC_SEARCH = struct
  let name = "eff"

  type _ Effect.t += Branch : int -> int Effect.t
  let branch i = Effect.perform (Branch i)

  let find : (int array -> unit) -> int array -> property -> unit
    = fun register dims property ->
    let path =
      Array.make (Array.length dims) (-1)
    in
    let override p i =
      if path.(i) = (-1) then p i
      else path.(i)
    in
    let rec search_from resume i j =
      if j < dims.(i)
      then (path.(i) <- j;
            let () = resume j in
            search_from resume i (j + 1))
      else path.(i) <- (-1)
    in
    let hsearch : (bool, unit) Effect.Deep.handler
      = let open Effect.Deep in
        { retc = (fun x -> if x then register path)
        ; exnc = raise
        ; effc = (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Branch i ->
             Some (fun (k : (a, _) Effect.Deep.continuation) ->
                 let open Multicont.Deep in
                 let r = promote k in
                 search_from (resume r) i 0)
          | _ -> None) }
    in
    Effect.Deep.match_with property (override branch) hsearch

  exception Found of int array
  let find_one : search_problem -> int list option
    = fun prob ->
    let found path = raise (Found path) in
    let dims = Array.of_list prob.dimensions in
    try
      find found dims prob.property;
      None
    with Found path -> Some (Array.to_list path)

  let find_all : search_problem -> int list list
    = fun prob ->
    let solutions : int list list ref = ref [] in
    let push solution =
      let solution' = Array.to_list solution in
      solutions := solution' :: !solutions
    in
    let dims = Array.of_list prob.dimensions in
    find push dims prob.property;
    List.rev !solutions
end
