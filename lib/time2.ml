let run : (unit -> 'a) -> ('a * float)
  = fun f ->
  let t0 = Unix.gettimeofday () in
  let result = f () in
  let t1 = Unix.gettimeofday () in
  (result, t1 -. t0)
