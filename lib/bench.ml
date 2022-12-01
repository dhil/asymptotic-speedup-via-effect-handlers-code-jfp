(* #use "topfind";;
 * #require "unix";; *)

type 'a t = {
    m: int;
    n: int;
    niter: int;
    label: string;
    f: unit -> 'a;
    validate: 'a -> bool;
  }

let run : out_channel -> 'a t -> unit
  = fun oc { m; n; niter; label; f; validate } ->
      try
      for i = 1 to niter do
        Printf.printf "  (%d/%d) Running %s...%!" i niter label;
        let t0 = Unix.gettimeofday () in
        let result = f () in
        let t1 = Unix.gettimeofday () in
        let valid = validate result in
        let elapsed = t1 -. t0 in
        ignore (if valid then
                  Printf.printf " SUCCESS! [elapsed %f]\n%!" elapsed
                else
                  Printf.printf " FAILED! [elapsed %f]\n%!" elapsed);
        output_string oc (Printf.sprintf "%s,%d,%d,%d,%f,%s\n%!" label i m n elapsed (match valid with true -> "true" | _ -> "false"));
        flush oc;
      done;
    with e -> Printf.eprintf "error: %s\n%!" (Printexc.to_string e)


let run_suite : string -> 'a t list -> unit
  = fun logfile items ->
  let fh : out_channel option ref = ref None in
  let _ =
    try
      let oc = open_out logfile in
      fh := Some oc;
      output_string oc (Printf.sprintf "label,iteration,m,n,time,valid\n%!");
      flush oc;
      let n = List.length items  - 1 in
      let items = Array.of_list items in
      for i = 0 to n do
        Printf.printf "Starting benchmark [%d/%d]...\n%!" (i + 1) (n + 1);
        run oc items.(i)
      done
    with e -> Printf.eprintf "error: %s\n%!" (Printexc.to_string e)
  in
  match !fh with
  | None -> ()
  | Some fh -> close_out fh

module Bench = struct
  type nonrec 'a t = 'a t

  let run = run
  let run_suite = run_suite
end
