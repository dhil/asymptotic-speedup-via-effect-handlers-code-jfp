open Integration

let main () =
  let usage appname =
    Printf.printf "usage: %s <naive|berger|pruned|eff> <id|square|logistic> <m> <n>\n%!" appname
  in
  let integrator =
    match String.lowercase_ascii (Array.get Sys.argv 1) with
    | "naive"   -> slow_fun_integrate01
    | "berger"  -> fun_integrate01
    | "pruned"  -> mod_integrate01
    | "eff"     -> eff_integrate01
    | _         -> usage (Array.get Sys.argv 0); exit 1
    | exception _ -> usage (Array.get Sys.argv 0); exit 1
  in
  let f = match String.lowercase_ascii (Array.get Sys.argv 2) with
    | "id" ->
       let m =
         try int_of_string (Array.get Sys.argv 3)
         with _ -> usage (Array.get Sys.argv 0); exit 1
       in
       (fun () -> integrator m id)
    | "square" ->
       let m =
         try int_of_string (Array.get Sys.argv 3)
         with _ -> usage (Array.get Sys.argv 0); exit 1
       in
       (fun () -> integrator m square)
    | "logistic" ->
       let m, n =
         try (int_of_string (Array.get Sys.argv 3), int_of_string (Array.get Sys.argv 4))
         with _ -> usage (Array.get Sys.argv 0); exit 1
       in
       (fun () -> integrator m (iter n logistic))
    | _         -> usage (Array.get Sys.argv 0); exit 1
    | exception _ -> usage (Array.get Sys.argv 0); exit 1
  in
  let (result, elapsed) = Time2.run f in
  print_endline (Dyadic.to_string result);
  Printf.printf "%f\n%!" elapsed

let _ = main()
