(* A multi-level subcommand tree with persistent flags and lifecycle hooks.
   Inspired by `git remote add origin URL`. *)

open Mamba

let verbose = Flag.count ~name:"verbose" ~short:'v' ~doc:"increase verbosity" ()
let config  = Flag.string ~name:"config" ~short:'C' ~default:"."
                ~doc:"working directory" ()

let remote_add =
  let url_required = Flag.bool ~name:"force" ~short:'f' ~doc:"replace if it exists" () in
  Command.make ~name:"add"
    ~short:"add a remote"
    ~args:(Arg.exactly 2)
    ~flags:[ Flag.pack url_required ]
    ~run:(fun args ->
      match Args.positional args with
      | [ name; url ] ->
        let dir = Args.get args config in
        let v   = Args.get args verbose in
        let f   = Args.get args url_required in
        Printf.printf
          "adding remote %s -> %s (in %s, verbose=%d, force=%b)\n"
          name url dir v f;
        0
      | _ -> 2)
    ()

let remote_rm =
  Command.make ~name:"rm"
    ~aliases:[ "remove" ]
    ~short:"remove a remote"
    ~args:(Arg.exactly 1)
    ~run:(fun args ->
      let name = List.hd (Args.positional args) in
      Printf.printf "removing remote %s\n" name;
      0)
    ()

let remote =
  Command.group ~name:"remote"
    ~short:"manage remote refs"
    ~subcommands:[ remote_add; remote_rm ]
    ()

let status =
  Command.make ~name:"status"
    ~short:"show working tree status"
    ~run:(fun args ->
      Printf.printf "status of %s (verbose=%d)\n"
        (Args.get args config) (Args.get args verbose);
      0)
    ()

let root =
  Command.make ~name:"gitlike"
    ~short:"a git-shaped sample CLI"
    ~persistent_flags:[ Flag.pack verbose; Flag.pack config ]
    ~persistent_pre_run:(fun args ->
      if Args.get args verbose > 0 then
        Printf.eprintf "[debug] verbose=%d, config=%s\n"
          (Args.get args verbose) (Args.get args config);
      None)
    ~subcommands:[ remote; status ]
    ()

let () =
  Program.make ~name:"gitlike" ~version:"0.1.0" ~root ()
  |> Program.run_exn
