(* mamba-cli: scaffolds a new mamba-based CLI project.

   Built with mamba itself; if you want to see a real example, read this
   file. *)

open Mamba

(* --- Templates --- *)

let dune_project_template ~name =
  Printf.sprintf
    "(lang dune 3.16)\n\
     \n\
     (name %s)\n\
     \n\
     (generate_opam_files true)\n\
     \n\
     (package\n\
     \ (name %s)\n\
     \ (synopsis \"A %s CLI built with mamba\")\n\
     \ (depends\n\
     \  (ocaml (>= 5.1))\n\
     \  (dune  (>= 3.16))\n\
     \  mamba))\n"
    name name name

let bin_dune_template ~name =
  Printf.sprintf
    "(executable\n\
     \ (name main)\n\
     \ (public_name %s)\n\
     \ (libraries mamba))\n"
    name

let main_ml_template ~name =
  Printf.sprintf
    {|open Mamba

let upper = Flag.bool ~name:"upper" ~doc:"uppercase the greeting" ()

let say_cmd =
  Command.make ~name:"say"
    ~short:"print a greeting"
    ~args:(Arg.exactly 1)
    ~flags:[ Flag.pack upper ]
    ~run:(fun args ->
      let who = List.hd (Args.positional args) in
      let u = Args.get args upper in
      print_endline (if u then String.uppercase_ascii who else who);
      0)
    ()

let root =
  Command.make ~name:%S
    ~short:"a freshly scaffolded mamba CLI"
    ~subcommands:[ say_cmd ]
    ()

let () =
  Program.make ~name:%S ~version:"0.1.0" ~root ()
  |> Program.run_exn
|}
    name name

let readme_template ~name =
  Printf.sprintf
    "# %s\n\
     \n\
     A CLI built with [mamba](https://github.com/josh/mamba).\n\
     \n\
     ## Build\n\
     \n\
     ```\n\
     dune build\n\
     dune exec %s -- --help\n\
     dune exec %s -- say world --upper\n\
     ```\n"
    name name name

(* --- File I/O --- *)

let mkdir_p path =
  let prefix = if String.length path > 0 && path.[0] = '/' then "/" else "" in
  let comps = String.split_on_char '/' path |> List.filter ((<>) "") in
  let rec walk acc = function
    | [] -> ()
    | comp :: rest ->
      let p = if acc = "" then comp else Filename.concat acc comp in
      (try Unix.mkdir p 0o755
       with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
      walk p rest
  in
  walk prefix comps

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

(* --- new <name> --- *)

let target_dir_flag =
  Flag.string ~name:"dir" ~short:'d' ~default:"."
    ~doc:"parent directory where the new project goes" ()

let new_cmd =
  let run args =
    let name = List.hd (Args.positional args) in
    let dir  = Args.get args target_dir_flag in
    let root_dir = Filename.concat dir name in
    let bin_dir  = Filename.concat root_dir "bin" in
    if Sys.file_exists root_dir then begin
      Printf.eprintf "error: %s already exists\n" root_dir;
      Error.parse_error
    end
    else begin
      mkdir_p bin_dir;
      write_file (Filename.concat root_dir "dune-project")
        (dune_project_template ~name);
      write_file (Filename.concat root_dir "README.md")
        (readme_template ~name);
      write_file (Filename.concat bin_dir "dune")
        (bin_dune_template ~name);
      write_file (Filename.concat bin_dir "main.ml")
        (main_ml_template ~name);
      Printf.printf "Scaffolded %s at %s\n\nNext steps:\n  cd %s\n  dune build\n  dune exec %s -- --help\n"
        name root_dir root_dir name;
      Error.success
    end
  in
  Command.make ~name:"new"
    ~short:"scaffold a new mamba CLI project"
    ~long:"Create a new directory with a runnable hello-world mamba CLI."
    ~example:"  $ mamba-cli new myapp\n  $ cd myapp && dune build && dune exec myapp -- say world"
    ~args:(Arg.exactly 1)
    ~flags:[ Flag.pack target_dir_flag ]
    ~run
    ()

let root =
  Command.make ~name:"mamba-cli"
    ~short:"scaffolding for mamba CLI projects"
    ~long:"mamba-cli scaffolds new projects that use the mamba CLI library.\n\
           Eventually it will also add subcommands and regenerate man pages."
    ~subcommands:[ new_cmd ]
    ()

let () =
  Program.make ~name:"mamba-cli" ~version:"0.1.0" ~root ()
  |> Program.run_exn
