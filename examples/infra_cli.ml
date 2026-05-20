(** Mini infra CLI built to dogfood mamba — not a real tool.
    See examples/infra_cli/FRICTION.md for the build-time friction log. *)

open Mamba

(* --- persistent flags --- *)

let config_flag =
  Flag.string ~name:"config" ~default:"~/.infra.yaml"
    ~doc:"config file path" ()

let log_level =
  Flag.enum ~name:"log-level"
    ~values:[ ("debug", `Debug); ("info", `Info);
              ("warn", `Warn); ("error", `Error) ]
    ~default:`Info
    ~doc:"log verbosity" ()

(* --- shared flag --- *)

let workspace =
  Flag.string ~name:"workspace" ~short:'w' ~default:"default"
    ~doc:"workspace to operate on" ()

(* --- plan --- *)

let plan_cmd =
  Command.make ~name:"plan"
    ~short:"Show what would change"
    ~flags:[ Flag.pack workspace ]
    ~run:(fun args ->
      Printf.printf "plan ws=%s\n" (Args.get args workspace);
      0)
    ()

(* --- apply --- *)

let auto_approve =
  Flag.bool ~name:"auto-approve"
    ~doc:"skip the interactive confirmation prompt" ()

let apply_cmd =
  Command.make ~name:"apply"
    ~short:"Apply pending changes to a workspace"
    ~flags:[ Flag.pack workspace; Flag.pack auto_approve ]
    ~run:(fun args ->
      Printf.printf "apply ws=%s auto=%b\n"
        (Args.get args workspace)
        (Args.get args auto_approve);
      0)
    ()

(* --- state group --- *)

let state_list =
  Command.make ~name:"list"
    ~short:"List resources tracked in state"
    ~run:(fun _ -> print_endline "state.list"; 0)
    ()

let state_rm =
  Command.make ~name:"rm"
    ~short:"Remove a resource from state"
    ~args:(Arg.exactly 1)
    ~run:(fun args ->
      Printf.printf "state.rm addr=%s\n" (Args.positional_1 args);
      0)
    ()

let state_mv =
  Command.make ~name:"mv"
    ~short:"Move a resource within state"
    ~args:(Arg.exactly 2)
    ~run:(fun args ->
      let (src, dst) = Args.positional_2 args in
      Printf.printf "state.mv src=%s dst=%s\n" src dst;
      0)
    ()

let state_cmd =
  Command.make ~name:"state"
    ~short:"Inspect or mutate workspace state"
    ~subcommands:[ state_list; state_rm; state_mv ]
    ()

(* --- mtx (multi-workspace) --- *)

let depth =
  Flag.int ~name:"depth" ~default:1
    ~doc:"max subdirectory depth to scan" ()

let mtx_cmd =
  Command.make ~name:"mtx"
    ~short:"Discover and operate on a tree of workspaces"
    ~args:(Arg.exactly 1)
    ~flags:[ Flag.pack depth ]
    ~run:(fun args ->
      Printf.printf "mtx dir=%s depth=%d\n"
        (Args.positional_1 args) (Args.get args depth);
      0)
    ()

(* --- root --- *)

let root =
  Command.make ~name:"infra"
    ~short:"mini infra CLI for dogfooding mamba"
    ~persistent_flags:[ Flag.pack config_flag; Flag.pack log_level ]
    ~subcommands:[ plan_cmd; apply_cmd; state_cmd; mtx_cmd ]
    ()

let () =
  Program.make ~name:"infra" ~version:"0.1.0" ~root () |> Program.run_exn
