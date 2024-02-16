(* Experiments runner *)

let use_parallel_runner : bool ref = ref true
let nparallel_jobs : int ref = ref 6
let nrepetitions : int ref = ref 11

module List = struct
  include List

  let cartesian xs ys =
    List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)

  let replicate_elem n xs =
    List.(concat (map (fun x -> init n (fun _ -> x)) xs))
end

module Task = struct
  type queen_task = { input : int
                    ; searcher : string
                    ; f : (int -> (string * float))
                    ; mode : string
                    ; repeat : bool }
  type integration_task = { precision: int
                          ; iterations: int
                          ; g : (int -> (int -> (string * float)))
                          ; integrator : string }


  type t = QueensTask of queen_task
         | IntegrationTask of integration_task
end

module Result = struct
  type 'a result = { task : 'a
                   ; result : string
                   ; elapsed : float }
  type t = QueensResult of Task.queen_task result
         | IntegrationResult of Task.integration_task result

  let to_csv_string = function
    | QueensResult { task = Task.{ searcher; mode; repeat; input; _ }
                   ; result; elapsed } ->
       Printf.sprintf "%s,%s,%s,%d,%f,%s" searcher mode (if repeat then "true" else "false") input elapsed result
    | IntegrationResult { task = Task.{ precision; iterations; integrator; _ }
                        ; result; elapsed } ->
       Printf.sprintf "%s,%d,%d,%f,%s" integrator precision iterations elapsed result
end

module Queens_Experiments = struct
  type mode = [`One | `All]

  type searcher = Naive | Berger | Pruned | Eff | Bespoke

  let select_searcher : searcher -> (module Generic_search.GENERIC_SEARCH) = function
    | Naive  -> (module Generic_search.Naive_Search)
    | Berger -> (module Generic_search.Fun_Search)
    | Pruned -> (module Generic_search.Mod_Search)
    | Eff    -> (module Generic_search.Eff_Search)
    | _ -> raise Not_found

  let string_of_result = function
    | `One None -> "None"
    | `One (Some xs) -> Printf.sprintf "Some [%s]" (String.concat "; " (List.map string_of_int xs))
    | `All xss -> Printf.sprintf "%d" (List.length xss)

  let prepare : searcher list -> mode -> bool -> int list -> Task.t list
    = fun searchers mode repeat inputs ->
    let nqueens =
      if repeat then Queens.n_queens
      else Queens.n_queens'
    in
    let rec configure tasks searchers =
      let open Task in
      match searchers with
      | Bespoke :: searchers ->
         let task = match mode with
           | `One ->
              let task n =
                let f () = `One (Queens.Bespoke_Queens.find_one n) in
                let (result, elapsed) = Time2.run f in
                (string_of_result result, elapsed)
              in (fun n ->
                  { input = n
                  ; f = task
                  ; searcher = "bespoke"
                  ; mode = "one"
                  ; repeat })
           | `All ->
              let task n =
                let f () = `All (Queens.Bespoke_Queens.find_all n) in
                let (result, elapsed) = Time2.run f in
                (string_of_result result, elapsed)
              in (fun n ->
                  { input = n
                  ; f = task
                  ; searcher = "bespoke"
                  ; mode = "all"
                  ; repeat })
         in
         configure (task :: tasks) searchers
        | searcher :: searchers ->
           let (module Searcher : Generic_search.GENERIC_SEARCH) = select_searcher searcher in
           let task n =
             let f () =
               match mode with
               | `One -> `One (if Searcher.name = "naive" && n > 10
                               then None
                               else Searcher.find_one (nqueens n))
               | `All -> `All (if Searcher.name = "naive" && n > 10
                               then []
                               else Searcher.find_all (nqueens n))
             in
             let (result, elapsed) = Time2.run f in
             (string_of_result result, elapsed)
           in
           let task =
             (fun n ->
                  { input = n
                  ; f = task
                  ; searcher = Searcher.name
                  ; mode = (match mode with `One -> "one" | `All -> "all")
                  ; repeat })
           in
           configure (task :: tasks) searchers
        | [] -> List.rev tasks
    in
    let tasks = configure [] searchers in
    List.map (fun (inp, make_task) -> Task.QueensTask (make_task inp)) (List.cartesian inputs tasks)
end

module Integration_Experiments = struct
  type integrator = Naive | Berger | Pruned | Eff

  let string_of_integrator = function
    | Naive -> "naive" | Berger -> "berger" | Pruned -> "pruned" | Eff -> "eff"

  let select_integrator = let open Integration in function
    | Naive   -> slow_fun_integrate01
    | Berger  -> fun_integrate01
    | Pruned  -> mod_integrate01
    | Eff     -> eff_integrate01

  type fn = (int -> (int Integration.Stream.t -> int Integration.Stream.t))

  let prepare : integrator list -> fn -> (int * int) list -> Task.t list
    = fun integrators g inputs ->
    let rec configure tasks integrators =
      match integrators with
      | [] -> List.rev tasks
      | integrator :: integrators ->
         let open Integration in
         let integrator_name = string_of_integrator integrator in
         let integrator = select_integrator integrator in
         let task m n =
           let f () =
             integrator m (g n)
           in
           let (result, elapsed) = Time2.run f in
           (Dyadic.to_string result, elapsed)
         in
         let task m n =
           let task =
             if integrator_name = "naive" && m = 15 && n > 3
             then (fun _m _n -> (Dyadic.to_string Dyadic.zero, 0.0))
             else task
           in
           Task.({ precision = m
                 ; iterations = n
                 ; g = task
                 ; integrator = integrator_name })
         in
         configure (task :: tasks) integrators
    in
    let tasks = configure [] integrators in
    List.map (fun ((m, n), make_task) -> Task.IntegrationTask (make_task m n)) (List.cartesian inputs tasks)
end

module Runner = struct
  module T = Domainslib.Task

  let sequential_run : Task.t list -> Result.t array
    = fun tasks ->
    let tasks = Array.of_list tasks in
    let results : Result.t option array = Array.make (Array.length tasks) None in
    for i = 0 to (Array.length tasks - 1) do
      let task = Array.get tasks i in
      match task with
      | QueensTask ({ input; f; _ } as task) ->
         let (result, elapsed) = f input in
         Array.set results i (Some Result.(QueensResult { result; elapsed; task }))
      | IntegrationTask ({ precision; iterations; g; _ } as task) ->
         let (result, elapsed) = g precision iterations in
         Array.set results i (Some Result.(IntegrationResult { result; elapsed; task }))
    done;
    Array.map Option.get results

  let parallel_run : int -> Task.t list -> Result.t array
    = fun njobs tasks ->
    let tasks = Array.of_list tasks in
    let results : Result.t option array = Array.make (Array.length tasks) None in
    let pool = T.setup_pool ~num_domains:(njobs - 1) () in
    let f _ =
      T.parallel_for ~start:0 ~finish:(Array.length tasks - 1)
        ~body:(fun i ->
          let task = Array.get tasks i in
          match task with
          | QueensTask ({ input; f; _ } as task) ->
             let (result, elapsed) = f input in
             Array.set results i (Some Result.(QueensResult { result; elapsed; task }))
          | IntegrationTask ({ precision; iterations; g; _ } as task) ->
             let (result, elapsed) = g precision iterations in
             Array.set results i (Some Result.(IntegrationResult { result; elapsed; task })))
        pool
    in
    T.run pool f;
    T.teardown_pool pool; Array.map Option.get results
end

let rec run : (string * Task.t list) list -> int -> unit
  = fun ts repetitions ->
  match ts with
  | [] -> ()
  | (fname, tasks) :: ts ->
     let oc = open_out fname in
     let tasks = List.replicate_elem repetitions tasks in
     let results =
       if !use_parallel_runner
       then Runner.parallel_run !nparallel_jobs tasks
       else Runner.sequential_run tasks
     in
     Array.iter (fun result -> output_string oc (Printf.sprintf "%s\n" (Result.to_csv_string result))) results;
     flush oc; close_out oc; run ts repetitions

let main () =
  let repetitions = !nrepetitions in
  let (queens_one, queens_all) =
    let open Queens_Experiments in
    let procedures = [Naive; Berger; Pruned; Eff; Bespoke] in
    ( prepare procedures `One false [20; 24; 28]
    , prepare procedures `All false [8; 10; 12])
  in
  let (integration_id, integration_square, integration_logistic) =
    let open Integration_Experiments in
    let integrators = [Naive; Berger; Pruned; Eff] in
    ( prepare integrators (fun _ -> Integration.id) [(20, 1)]
    , prepare integrators (fun _ -> Integration.square) [(14, 1); (17, 1); (20, 1)]
    , prepare integrators (fun n -> Integration.(iter n logistic)) [(15, 1); (15, 2); (15, 3); (15, 4); (15, 5)])
  in
  run [ ("data/queens.one.csv", queens_one)
      ; ("data/queens.all.csv", queens_all)
      ; ("data/integration.id.csv", integration_id)
      ; ("data/integration.square.csv", integration_square)
      ; ("data/integration.logistic.csv", integration_logistic) ]
    repetitions

let _ =
  let usage_msg =
    "./runner [--sequential | --parallel [ --njobs <num> ] ] [ --repetitions <num> ]"
  in
  let unknown_arg arg =
    Printf.fprintf stderr "error: unknown argument %s\n%!" arg
  in
  let speclist =
    [ ("--sequential", Arg.Clear use_parallel_runner, "Use a single thread to run the experiments (default: false)")
    ; ("--parallel", Arg.Set use_parallel_runner, "Use multiple threads to run the experiments (default: true)")
    ; ("--njobs", Arg.Set_int nparallel_jobs, "The number of threads to use with the parallel runner (default: 6)")
    ; ("--repetitions", Arg.Set_int nrepetitions, "The number of times to repeat each experiment") ]
  in
  Arg.parse speclist unknown_arg usage_msg;
  main ()
