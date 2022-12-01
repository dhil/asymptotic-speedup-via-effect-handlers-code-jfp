(* Efficiency comparisons for various exact real integration operators,
   including one using catchcont. *)

(* Daniel Hillerström, October 2018 (heavily based on John Longley's
   implementation in SML) *)

(* We use int streams with digits 1,0,~1 to represent reals in [-1,1].
   So (int stream -> int stream) represents math functions [-1,1] ->
   [-1,1].

   We give four integration functions of the form: ...Integrate01 :
   int -> (int stream -> int stream) -> dyadic which perform definite
   integration from 0 to 1 with any specified precision.  More
   precisely, if F : int stream -> int stream represents f : [-1,1] ->
   [-1,1], then ...Integrate01 k F returns some dyadic within 2^-k of
   the true value of the integral of f from 0 to 1.

   In fact, for any given k and F, all four of our integrators will
   return the *same* dyadic approximation - they are all accessing the
   same information about F - so that efficiency comparisons between
   them are indeed meaningful. *)

module Z = struct
  include Z

  let two = Z.of_int 2
  let four = Z.of_int 4
end

(* Memoized powers of two *)
module PowersOf2 = struct
  let max_exponent = 80

  let powers_array : Z.t array
    = Array.init max_exponent (fun i -> Z.pow Z.two i)

  let get : int -> Z.t
    = fun i -> Array.get powers_array i
end

(* Basic dyadic arithmetic *)
module Dyadic = struct
  type t = Z.t * int (* (a, k) represents a*2^k *)

  let zero : t = (Z.zero, -1)
  let one  : t = (Z.one, 0)
  let half : t = (Z.one, -1)
  let minus_half : t = (Z.minus_one, -1)

  let to_string (a, k) = Printf.sprintf "(%s, %d)" (Z.to_string a) k

  let equal : t -> t -> bool
    = fun (a, k) (a', k') -> Int.equal k k' && Z.equal a a'

  let rec simp : t -> t
  = fun ((a, k) as d) ->
    let res = Z.erem a Z.four in
    if Z.equal res Z.zero
    then if Z.equal a Z.zero
         then zero
         else simp (Z.div a Z.four, k + 2)
    else if Z.equal res Z.two
    then (Z.div a Z.two, k + 1)
    else d

  let add : t -> t -> t
    = fun (a, k) (a', k') ->
    if k >= k'
    then let r = PowersOf2.get (k - k') in
         (Z.add (Z.mul r a) a', k')
    else let r = PowersOf2.get (k' - k) in
         (Z.add a (Z.mul r a'), k)

  let neg : t -> t
    = fun (a, k) -> (Z.neg a, k)

  let sub : t -> t -> t
    = fun d d' -> add d (neg d')

  let mult : t -> t -> t
    = fun (a, k) (a', k') -> (Z.mul a a', k + k')

  let leq : t -> t -> bool
    = fun (a, k) (a', k') ->
    if k >= k'
    then let r = PowersOf2.get (k - k') in
         Z.leq (Z.mul r a) a'
    else let r = PowersOf2.get (k' - k) in
         Z.leq a (Z.mul r a')

  let rescale : t -> int -> t
    = fun (a, k) k' -> (a, k + k')

  let average : t -> t -> t
    = fun d d' -> rescale (add d d') (-1)

  let (left_average, right_average) : ((t -> t -> t) * (t -> t -> t))
    = let thrice : t -> t
        = fun (a, k) -> (Z.add (Z.add a a) a, k)
      in
      (fun d d' -> rescale (add (thrice d) d') (-2)), (* (3d + d') / 4 *)
      (fun d d' -> rescale (add d (thrice d')) (-2))  (* (d + 3d') / 4 *)
end

(* Signed binary streams, representing reals in [~1,1]. Digits are 1,0,~1. *)
module Stream = struct
  type 'a t = Cons of 'a * (unit -> 'a t)

  let cons : 'a -> (unit -> 'a t) -> 'a t
    = fun d u -> Cons (d, u)

  let tail : 'a t -> 'a t = function
    | Cons (_, u) -> u()

  let rec const : 'a -> 'a t
    = fun d -> cons d (fun () -> const d)

  let rec append : 'a list -> 'a t -> 'a t
    = fun ds s ->
    match ds with
    | [] -> s
    | d :: ds -> cons d (fun () -> append ds s)

  (* Produces stream rep of integer m in [-2^j,2^j), relative to
     [-2^j,2^j].  Will consist of just -1 and 1, ending in an infinite
     tail of -1s.  First j+1 digits will be the binary rep of m+2^j,
     writing -1 for 0. *)
  let rec of_big : Z.t -> int -> int t
    = fun m j ->
    if j = 0
    then if Z.equal m Z.minus_one then const (-1)
         else cons 1 (fun () -> const (-1))
    else let j' = j - 1 in
         if Z.lt m Z.zero then cons (-1) (fun () -> of_big (Z.add m (PowersOf2.get j')) j')
         else cons 1 (fun () -> of_big (Z.sub m (PowersOf2.get j')) j')

  (* Produces signed binary stream for dyadic in range [-1,1),
     consisting of just -1 and 1 and ending in tail of -1.  This choice
     of representation facilitates comparison with the catchcont
     implementation of integration. *)
  let of_dyadic : Dyadic.t -> int t
    = fun (a, k) -> of_big a (-k)

  let rec take : int -> 'a t -> 'a list
    = fun i (Cons (x, u)) ->
    if i = 0 then []
    else if i = 1 then [x]
    else let x' = u() in
         x :: take (i - 1) x'
end

(* returns centre of interval represented by a finite digit list *)
let rec dyadic_of_list : int -> int list -> Dyadic.t
  = fun i ds ->
  match ds with
  | [] -> Dyadic.zero
  | d :: ds -> Dyadic.add (Z.of_int d, i) (dyadic_of_list (i - 1) ds)

(* returns a dyadic within 2^-k of the true value of stream *)
let dyadic_of_stream : int -> int Stream.t -> Dyadic.t
  = fun k stream ->
  dyadic_of_list (-1) (Stream.take k stream)

let apply_to_precision : int -> ('a -> int Stream.t) -> 'a -> Dyadic.t
  = fun k f stream -> dyadic_of_stream k (f stream)

(* returns right end of interval represented by a finite digit list *)
let rec max_dyadic_of_list : int -> int list -> Dyadic.t
  = fun i ds ->
  match ds with
  | [] -> (Z.one, i + 1)
  | d :: ds ->
     let d' = max_dyadic_of_list (i - 1) ds in
     Dyadic.add (Z.of_int d, i) d'

(* INTEGRATION OPERATIONS *)

(* Purely functional integration (Berger/Simpson) *)

(* We allow stream memoization to achieve the typical effect of
   a Haskell-style call-by-need implementation - we consider this
   to be within the spirit of 'pure functional' programming.
 *)

let rec memo_stream' : int -> ('a Array.t * (unit -> 'a Stream.t)) ref -> 'a Stream.t
  = fun i memo ->
  let (ds, u) = !memo in
  if i < Array.length ds
  then Stream.cons (Array.get ds i) (fun () -> memo_stream' (i + 1) memo)
  else let (Stream.Cons (d, u')) = u() in
       (memo := (Array.append ds [|d|], u');
        Stream.cons d (fun () -> memo_stream' (i + 1) memo))

let memo_stream : 'a Stream.t -> 'a Stream.t
  = fun (Stream.Cons (d, u)) ->
  let memo = ref ([|d|], u) in
  memo_stream' 0 memo

let rec critical_stream : (int Stream.t -> Dyadic.t) -> Dyadic.t -> int list -> int Stream.t
  = fun g y ds ->
  let stream =
    memo_stream (Stream.cons (-1) (fun () -> critical_stream g y (ds @ [(-1)])))
  in
  let r = g (Stream.append ds stream) in
  if not (Dyadic.equal r y) then stream
  else Stream.cons 1 (fun () -> critical_stream g y (ds @ [1]))

let rec fun_integrate' : (int Stream.t -> Dyadic.t) -> int list -> Dyadic.t option -> Dyadic.t
  = fun g ds z ->
  let y = match z with
    | Some y' -> y'
    | None -> g (Stream.append ds (Stream.const (-1)))
  in
  let r = g (Stream.append ds (critical_stream g y ds)) in
  if Dyadic.equal r y then Dyadic.rescale y (1 - List.length ds)
  else Dyadic.add
         (fun_integrate' g (ds @ [(-1)]) (Some y)) (* passing [y] here saves recomputing G (ds @ -1,-1,-1,...) *)
         (fun_integrate' g (ds @ [1]) None)

let fun_integrate01 : int -> (int Stream.t -> int Stream.t) -> Dyadic.t
  = fun k f ->
  Dyadic.simp (fun_integrate' (apply_to_precision k f) [1] None)

(* Slower version omitting memoization, for comparison: *)
let rec slow_critical_stream : (int Stream.t -> Dyadic.t) -> Dyadic.t -> int list -> int Stream.t
  = fun g y ds ->
  let stream =
    Stream.cons (-1) (fun () -> slow_critical_stream g y (ds @ [(-1)]))
  in
  let r = g (Stream.append ds stream) in
  if not (Dyadic.equal r y) then stream
  else Stream.cons 1 (fun () -> slow_critical_stream g y (ds @ [1]))

let rec slow_fun_integrate' : (int Stream.t -> Dyadic.t) -> int list -> Dyadic.t option -> Dyadic.t
  = fun g ds z ->
  let y = match z with
    | Some y' -> y'
    | None -> g (Stream.append ds (Stream.const (-1)))
  in
  let r =
    let stream' =
      slow_critical_stream g y ds
    in
    g (Stream.append ds stream')
  in
  if Dyadic.equal r y then Dyadic.rescale y (1 - List.length ds)
  else Dyadic.add
         (slow_fun_integrate' g (ds @ [(-1)]) (Some y))
         (slow_fun_integrate' g (ds @ [1]) None)

let slow_fun_integrate01 : int -> (int Stream.t -> int Stream.t) -> Dyadic.t
  = fun k f ->
  Dyadic.simp (slow_fun_integrate' (apply_to_precision k f) [1] None)

(* Stream modulus functional, implemented using local state.
   Example of a sequentially realizable functional. *)
let result_with_modulus : (int Stream.t -> 'a) -> int Stream.t -> 'a * int list
  = fun f str ->
  let log : int list ref = ref [] in
  let rec log_stream : int Stream.t -> int Stream.t
    = fun (Stream.Cons (x, u)) ->
    log := x :: !log;
    Stream.cons x (fun () -> log_stream (u()))
  in
  let result =
    f (log_stream str)
  in
  (result, List.rev !log)


(* Integration using modulus functional *)
let eq_one : Dyadic.t -> bool
  = fun (a, k) -> (Z.equal a (PowersOf2.get (-k)))

let string_of_list f xs = "[" ^ String.concat ", " (List.map f xs) ^ "]"

(* Integrates [f] from [start] up to 1, with precision [k] *)
let mod_integrate : int -> (int Stream.t -> int Stream.t) -> Dyadic.t -> Dyadic.t
  = fun k f start ->
  let rec sweep x total =
    if eq_one x then total
    else let (y, modulus) =
           result_with_modulus (apply_to_precision k f)
                               (Stream.of_dyadic x)
         in
         (* let () =
          *   Printf.printf "%s %s\n%!" (string_of_dyadic y) (string_of_list string_of_int modulus)
          * in *)
         let new_x = max_dyadic_of_list (-1) modulus in
         sweep new_x Dyadic.(add total (mult y (sub new_x x)))
         (* let () = Printf.printf "%s %s\n%!" (string_of_dyadic sweep') (string_of_dyadic (simp sweep')) in *)
         (* sweep' *)
  in
  sweep start Dyadic.zero

let mod_integrate01 : int -> (int Stream.t -> int Stream.t) -> Dyadic.t
  = fun k f -> Dyadic.simp (mod_integrate k f Dyadic.zero)

(* Integration using effect handlers. *)
type _ Effect.t += Branch : int Stream.t Effect.t

let branch : unit -> int Stream.t
  = fun () -> Effect.perform Branch

let eff_integrate' : int -> ((unit -> int Stream.t) -> Dyadic.t) -> Dyadic.t
  = fun i g ->
  let hintegrate : (Dyadic.t, (int -> Dyadic.t)) Effect.Deep.handler
    = let open Effect.Deep in
      { retc = (fun v ->
          (fun i -> Dyadic.rescale v i))
      ; exnc = (fun e ->
        (fun _i -> raise e))
      ; effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Branch ->
           Some (fun (k : (a, _) continuation) ->
               (fun i ->
                 let open Multicont.Deep in
                 let r = promote k in
                 let lhs =
                   resume r (Stream.cons (-1) branch) (i - 1)
                 in
                 let rhs =
                   resume r (Stream.cons 1 branch) (i - 1)
                 in
                 Dyadic.add lhs rhs))
        | _ -> None) }
  in
  let integrate' g =
    Effect.Deep.match_with g branch hintegrate
  in
  integrate' g i

let eff_integrate01 : int -> (int Stream.t -> int Stream.t) -> Dyadic.t
  = fun k f ->
  Dyadic.simp (eff_integrate' 0
          (fun g -> apply_to_precision k f (Stream.cons 1 g)))

(* Example: Squaring *)
exception SquareError

let rec square_stream' : int -> int -> Dyadic.t -> Dyadic.t -> (unit -> int Stream.t) -> int Stream.t
  = fun in_wt out_wt sq_min sq_max u ->
  if Dyadic.(leq zero sq_min) then
    Stream.cons 1 (fun () ->
        let sq_min' = Dyadic.(sub (rescale sq_min 1) one) in
        let sq_max' = Dyadic.(sub (rescale sq_max 1) one) in
        square_stream' in_wt (out_wt - 1) sq_min' sq_max' u)
  else if Dyadic.(leq sq_max zero) then
    Stream.cons (-1) (fun () ->
        let sq_min' = Dyadic.(add (rescale sq_min 1) one) in
        let sq_max' = Dyadic.(add (rescale sq_max 1) one) in
        square_stream' in_wt (out_wt - 1) sq_min' sq_max' u)
  else if Dyadic.(leq minus_half sq_min && leq sq_max half) then
    Stream.cons 0 (fun () ->
        let sq_min' = Dyadic.rescale sq_min 1 in
        let sq_max' = Dyadic.rescale sq_max 1 in
        square_stream' in_wt (out_wt - 1) sq_min' sq_max' u)
  else match u() with
       | Stream.Cons (1, u') ->
          let sq_min' =
            Dyadic.(sub (average sq_min sq_max))
                   (Z.one, 2 * in_wt - out_wt)
          in
          square_stream' (in_wt - 1) out_wt sq_min' sq_max u'
       | Stream.Cons ((-1), u') ->
          let sq_max' =
            Dyadic.(sub (average sq_min sq_max))
                   (Z.one, 2 * in_wt - out_wt)
          in
          square_stream' (in_wt - 1) out_wt sq_min sq_max' u'
       | Stream.Cons (0, u') ->
          let sq_min' =
            Dyadic.(sub (left_average sq_min sq_max))
                   (Z.of_int 3, 2 * in_wt - 2 - out_wt)
          in
          let sq_max' =
            Dyadic.(sub (right_average sq_min sq_max))
                   (Z.of_int 3, 2 * in_wt - 2 - out_wt)
          in
          square_stream' (in_wt - 1) out_wt sq_min' sq_max' u'
       | _ -> raise SquareError

let rec neg_stream : int Stream.t -> int Stream.t
  = fun (Stream.Cons (d, u)) -> Stream.cons (-d) (fun () -> neg_stream (u()))

let rec square : int Stream.t -> int Stream.t = function
  | Stream.Cons (1, u)    -> square_stream' (-1) 0 Dyadic.zero Dyadic.one u
  | Stream.Cons ((-1), u) -> square_stream' (-1) 0 Dyadic.zero Dyadic.one (fun () -> neg_stream (u()))
  | Stream.Cons (0, u)    -> Stream.cons 0 (fun () -> Stream.cons 0 (fun () -> square (u())))
  | _ -> raise SquareError

(* Logistic map: x -> 1 - 2x^2. Iterations of this behave chaotically. *)
let logistic : int Stream.t -> int Stream.t
  = fun stream ->
  let (Stream.Cons (_, u)) = square stream in
  let x = u() in
  neg_stream x

let rec iter : int -> ('a -> 'a) -> 'a -> 'a
  = fun n f x ->
  if n = 0 then x
  else let n' = n - 1 in
       let x' = f x in
       iter n' f x'

let id : 'a -> 'a
  = fun x -> x
