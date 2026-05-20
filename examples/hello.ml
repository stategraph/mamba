open Mamba

(* Persistent flag: defined on root, visible to every subcommand. *)
let verbose = Flag.count ~name:"verbose" ~short:'v' ~doc:"verbosity level" ()

(* Local flags: scoped to the subcommand that declares them. *)
let count = Flag.int  ~name:"count" ~short:'n' ~default:1     ~doc:"how many times" ()
let upper = Flag.bool ~name:"upper"             ~default:false ~doc:"uppercase" ()

let say =
  Command.make ~name:"say"
    ~short:"print a greeting"
    ~long:"Print the greeting one or more times, optionally in uppercase."
    ~example:"  $ hello say world\n  $ hello say world -n 3 --upper\n  $ hello -v say world"
    ~args:(Arg.exactly 1)
    ~flags:[ Flag.pack count; Flag.pack upper ]
    ~run:(fun args ->
      let n   = Args.get args count in
      let u   = Args.get args upper in
      let v   = Args.get args verbose in        (* inherited from root *)
      let who = Args.positional_1 args in
      if v > 0 then prerr_endline "say: running";
      for _ = 1 to n do
        print_endline (if u then String.uppercase_ascii who else who)
      done;
      0)
    ()

let root =
  Command.make ~name:"hello"
    ~short:"a tiny greeting CLI"
    ~long:"A minimal mamba example. Run [hello say <name>] to greet someone."
    ~persistent_flags:[ Flag.pack verbose ]
    ~subcommands:[ say ]
    ()

let () = Program.make ~name:"hello" ~version:"0.1.0" ~root () |> Program.run_exn
