(* Demo: regenerate man pages for the git_like example into /tmp/mamba-man.
   Run with [dune exec examples/gen_man.exe]. *)

open Mamba

(* Reuse the git_like tree by re-declaring it minimally. *)
let url = Flag.string ~name:"url" ~doc:"URL" ()
let sub_add =
  Command.make ~name:"add" ~short:"add a remote"
    ~flags:[ Flag.pack url ]
    ~run:(fun _ -> 0) ()

let sub_rm =
  Command.make ~name:"rm" ~aliases:[ "remove" ] ~short:"remove a remote"
    ~run:(fun _ -> 0) ()

let remote =
  Command.group ~name:"remote" ~short:"manage remote refs"
    ~subcommands:[ sub_add; sub_rm ] ()

let root =
  Command.make ~name:"demo" ~short:"demo CLI" ~subcommands:[ remote ] ()

let () =
  let dir = try Sys.argv.(1) with _ -> "/tmp/mamba-man" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let files = Man.write_all ~dir ~program_name:"demo" ~program_version:"0.1.0" ~root in
  List.iter print_endline files
