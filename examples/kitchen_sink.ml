(** Kitchen-sink CLI exercising every documented mamba feature.
    Companion file: [examples/kitchen_sink/FEATURES.md].

    The shape is contrived — the goal is FEATURE COVERAGE, not realism.
    Every entry in FEATURES.md should be traceable to a line here or to
    a scenario in [tests/test_kitchen_sink.ml]. *)

open Mamba

(* -------------------------------------------------------------------- *)
(* Persistent flags on root                                             *)
(* -------------------------------------------------------------------- *)

(* Flag.path with must_exist (exercised by running on an existing file) *)
let p_config =
  Flag.path ~name:"config" ~short:'c'
    ~env:"KIT_CONFIG" ~default:"/etc/hosts"
    ~must_exist:false  (* set true and we'd fail in CI for missing default *)
    ~doc:"config file path" ()

(* Flag.count: -v, -vv, -vvv *)
let p_verbose = Flag.count ~name:"verbose" ~short:'v' ~doc:"verbosity (-v, -vv, -vvv)" ()

(* Flag.bool with aliases *)
let p_quiet =
  Flag.bool ~name:"quiet" ~short:'q' ~aliases:[ "silent" ]
    ~default:false ~doc:"suppress informational output" ()

(* Flag.enum at root, inherited by everything *)
let p_log_level =
  Flag.enum ~name:"log-level"
    ~values:[ "debug", `Debug; "info", `Info; "warn", `Warn; "error", `Error ]
    ~default:`Info ~doc:"log level" ()

(* Hidden persistent flag *)
let p_debug_internal =
  Flag.bool ~name:"debug-internal" ~default:false ~hidden:true
    ~doc:"internal debug toggle (not in --help)" ()

(* Deprecated persistent flag *)
let p_old_color =
  Flag.bool ~name:"color-output" ~default:false
    ~deprecated:"use --log-level for log formatting instead"
    ~doc:"legacy color toggle" ()

(* -------------------------------------------------------------------- *)
(* Hooks                                                                 *)
(* -------------------------------------------------------------------- *)

let hook_log = ref []
let log_hook tag _args = hook_log := tag :: !hook_log; None

(* Short-circuit hook: returns Some n to skip the run *)
let short_circuit_hook args =
  if Args.get args p_debug_internal
  then Some 99  (* internal debug short-circuits with exit 99 *)
  else None

(* -------------------------------------------------------------------- *)
(* "build" group                                                         *)
(* -------------------------------------------------------------------- *)

(* Flag.bool (release) + Flag.string with required + Flag.enum *)
let build_release = Flag.bool ~name:"release" ~default:false ~doc:"optimized build" ()
let build_target  = Flag.string ~name:"target" ~short:'t'
                      ~default:"native" ~doc:"build target" ()
let build_jobs    = Flag.int ~name:"jobs" ~short:'j'
                      ~default:1 ~doc:"parallel jobs" ()

let build_cmd =
  Command.make ~name:"build"
    ~short:"build the project"
    ~long:"Compile sources into the target's binary format."
    ~example:"  $ kit build --release -j8\n  $ kit build --target wasm"
    ~group_id:"dev"
    ~flags:[ Flag.pack build_release; Flag.pack build_target; Flag.pack build_jobs ]
    ~run:(fun args ->
      Printf.printf "build release=%b target=%s jobs=%d\n"
        (Args.get args build_release) (Args.get args build_target)
        (Args.get args build_jobs);
      Error.success)
    ()

(* Flag.float exercise *)
let test_timeout = Flag.float ~name:"timeout" ~default:60.0 ~doc:"timeout in seconds" ()

(* Required flag exercise: --pattern has no default *)
let test_pattern =
  Flag.string ~name:"pattern" ~short:'p' ~required:true
    ~doc:"test name regex (required)" ()

let test_cmd =
  Command.make ~name:"test"
    ~short:"run tests"
    ~group_id:"dev"
    ~flags:[ Flag.pack test_timeout; Flag.pack test_pattern ]
    ~pre_run:(log_hook "test.pre_run")
    ~post_run:(log_hook "test.post_run")
    ~run:(fun args ->
      hook_log := "test.run" :: !hook_log;
      Printf.printf "test pattern=%s timeout=%.1f\n"
        (Args.get args test_pattern) (Args.get args test_timeout);
      Error.success)
    ()

(* "run" — exercises Args.cmd_path *)
let run_watch = Flag.bool ~name:"watch" ~short:'w' ~default:false ~doc:"re-run on change" ()
let run_cmd =
  Command.make ~name:"run"
    ~short:"run the project"
    ~aliases:[ "r" ]
    ~suggest_for:[ "exec" ]    (* "kit exec" → suggest "run" *)
    ~group_id:"dev"
    ~flags:[ Flag.pack run_watch ]
    ~run:(fun args ->
      Printf.printf "run cmd_path=[%s] watch=%b\n"
        (String.concat ";" (Args.cmd_path args))
        (Args.get args run_watch);
      Error.success)
    ()

(* -------------------------------------------------------------------- *)
(* "deploy" group                                                        *)
(* -------------------------------------------------------------------- *)

(* required-together flag group: --aws-key + --aws-secret *)
let deploy_key    = Flag.string ~name:"aws-key" ~default:"" ~doc:"AWS access key" ()
let deploy_secret = Flag.string ~name:"aws-secret" ~default:"" ~doc:"AWS secret key" ()
let deploy_dry    = Flag.bool   ~name:"dry-run" ~default:false ~doc:"preview only" ()
let deploy_force  = Flag.bool   ~name:"force"   ~short:'f' ~default:false ~doc:"skip confirms" ()

let deploy_cmd =
  Command.make ~name:"deploy"
    ~short:"deploy the project to an environment"
    ~aliases:[ "d" ]
    ~usage:"<env>"   (* override Usage line *)
    ~args:(Arg.only_valid_of [ "dev"; "staging"; "prod" ])
    ~group_id:"ops"
    ~flags:[ Flag.pack deploy_key; Flag.pack deploy_secret;
             Flag.pack deploy_dry; Flag.pack deploy_force ]
    ~flag_groups:[
      Flag_group.required_together [ Flag.pack deploy_key; Flag.pack deploy_secret ];
      Flag_group.mutually_exclusive [ Flag.pack deploy_dry; Flag.pack deploy_force ];
    ]
    ~run:(fun args ->
      Printf.printf "deploy env=%s dry=%b force=%b\n"
        (Args.positional_1 args) (Args.get args deploy_dry) (Args.get args deploy_force);
      Error.success)
    ()

(* mutually_exclusive_by_name + one_required *)
let user_flag  = Flag.string ~name:"user"  ~default:"" ~doc:"user id" ()
let email_flag = Flag.string ~name:"email" ~default:"" ~doc:"user email" ()

let grant_cmd =
  Command.make ~name:"grant"
    ~short:"grant a role to a user"
    ~args:(Arg.named1 "role" |> fst)
    ~group_id:"ops"
    ~flags:[ Flag.pack user_flag; Flag.pack email_flag ]
    ~flag_groups:[
      Flag_group.one_required_by_name [ "user"; "email" ];
    ]
    ~run:(fun args ->
      Printf.printf "grant role=%s user=%s email=%s\n"
        (Args.positional_1 args) (Args.get args user_flag) (Args.get args email_flag);
      Error.success)
    ()

(* Deprecated command *)
let rollback_cmd =
  Command.make ~name:"rollback"
    ~short:"undo the last deploy"
    ~deprecated:"use `kit deploy <prev-env>` instead"
    ~args:(Arg.exactly 1)
    ~group_id:"ops"
    ~run:(fun args ->
      Printf.printf "rollback to %s\n" (Args.positional_1 args);
      Error.success)
    ()

(* -------------------------------------------------------------------- *)
(* "db" group + nested "db migrate"                                      *)
(* -------------------------------------------------------------------- *)

let migrate_steps = Flag.int ~name:"steps" ~short:'n' ~default:1 ~doc:"how many" ()

(* Custom Arg validator + Arg.all_of composing two specs *)
let valid_migration_name s =
  let r = Str.regexp "^[0-9]+_[a-z_]+$" in
  if Str.string_match r s 0 then Ok () else Error "name must look like '0042_foo_bar'"

(* Use Arg.custom in one place; combine with exactly 1 via Arg.all_of *)
let migrate_create_args =
  Arg.all_of [
    Arg.exactly 1;
    Arg.custom (fun args ->
      match args with
      | [ name ] -> valid_migration_name name
      | _ -> Ok ())   (* exactly 1 handles the count check *)
  ]

let migrate_up =
  Command.make ~name:"up"
    ~short:"apply pending migrations"
    ~flags:[ Flag.pack migrate_steps ]
    ~run:(fun args ->
      Printf.printf "migrate up steps=%d\n" (Args.get args migrate_steps);
      Error.success)
    ()

let migrate_down =
  Command.make ~name:"down"
    ~short:"roll back applied migrations"
    ~flags:[ Flag.pack migrate_steps ]
    ~run:(fun args ->
      Printf.printf "migrate down steps=%d\n" (Args.get args migrate_steps);
      Error.success)
    ()

let migrate_create =
  Command.make ~name:"create"
    ~short:"scaffold a new migration"
    ~usage:"<name>"
    ~args:migrate_create_args
    ~run:(fun args ->
      Printf.printf "migrate create name=%s\n" (Args.positional_1 args);
      Error.success)
    ()

let migrate_cmd =
  Command.make ~name:"migrate"
    ~short:"schema migrations"
    ~subcommands:[ migrate_up; migrate_down; migrate_create ]
    ()  (* group: no ~run -> auto-shows help *)

(* Flag.list ~sep:',' AND Flag.repeated together *)
let seed_files =
  Flag.list ~sep:',' (Flag.string ~name:"file" ~short:'f' ~doc:"seed file" ())
let seed_tag   =
  Flag.repeated (Flag.string ~name:"tag" ~short:'t' ~doc:"tag filter" ())

let seed_cmd =
  Command.make ~name:"seed"
    ~short:"insert seed data"
    ~flags:[ Flag.pack seed_files; Flag.pack seed_tag ]
    ~run:(fun args ->
      Printf.printf "seed files=[%s] tags=[%s]\n"
        (String.concat "," (Args.get args seed_files))
        (String.concat "," (Args.get args seed_tag));
      Error.success)
    ()

let db_cmd =
  Command.make ~name:"db"
    ~short:"database operations"
    ~group_id:"ops"
    ~subcommands:[ migrate_cmd; seed_cmd ]
    ()

(* -------------------------------------------------------------------- *)
(* "config" group                                                        *)
(* -------------------------------------------------------------------- *)

(* Arg.none, Arg.named2 (typed), Arg.named1 *)
let config_list =
  Command.make ~name:"list"
    ~short:"list all config keys"
    ~args:Arg.none
    ~run:(fun _ -> print_endline "config list"; Error.success)
    ()

let config_get =
  let (spec, get_key) = Arg.named1 "key" in
  Command.make ~name:"get"
    ~short:"read a config value"
    ~args:spec
    ~run:(fun args ->
      Printf.printf "config get key=%s\n" (get_key args);
      Error.success)
    ()

let config_set =
  let (spec, get_kv) = Arg.named2 "key" "value" in
  Command.make ~name:"set"
    ~short:"write a config value"
    ~args:spec
    ~run:(fun args ->
      let (k, v) = get_kv args in
      Printf.printf "config set key=%s value=%s\n" k v;
      Error.success)
    ()

(* Arg.named3 (typed) *)
let config_alias =
  let (spec, get_kvc) = Arg.named3 "key" "value" "comment" in
  Command.make ~name:"alias"
    ~short:"alias a key with a comment"
    ~args:spec
    ~run:(fun args ->
      let (k, v, c) = get_kvc args in
      Printf.printf "config alias %s=%s (%s)\n" k v c;
      Error.success)
    ()

(* Arg.variadic with min:0 — "0 or more" *)
let config_import =
  Command.make ~name:"import"
    ~short:"import config files (or none for defaults)"
    ~args:(Arg.variadic ~min:0 "file")
    ~run:(fun args ->
      Printf.printf "config import files=[%s]\n"
        (String.concat "," (Args.positional args));
      Error.success)
    ()

let config_cmd =
  Command.make ~name:"config"
    ~short:"manage configuration"
    ~aliases:[ "cfg" ]
    ~group_id:"meta"
    ~subcommands:[ config_list; config_get; config_set; config_alias; config_import ]
    ()

(* -------------------------------------------------------------------- *)
(* "plugin" group: variadic install + mutex flag group on list           *)
(* -------------------------------------------------------------------- *)

(* Args.was_set probe + Args.get_opt for "truly optional" flag *)
let plugin_optional_tag = Flag.string ~name:"only-tag" ~doc:"filter to one tag" ()
(* no ~default, no ~required -> Args.get_opt returns None|Some *)

let plugin_install =
  Command.make ~name:"install"
    ~short:"install one or more plugins"
    ~aliases:[ "i" ]
    ~args:(Arg.variadic "name")   (* default min=1 *)
    ~flags:[ Flag.pack plugin_optional_tag ]
    ~run:(fun args ->
      let names = Args.positional args in
      let tag =
        match Args.get_opt args plugin_optional_tag with
        | Some t -> Printf.sprintf "tag=%s" t
        | None   -> "tag=<any>"
      in
      let was_tag = Args.was_set args "only-tag" in
      Printf.printf "plugin install %s names=[%s] (--only-tag was_set=%b)\n"
        tag (String.concat "," names) was_tag;
      Error.success)
    ()

let plugin_installed = Flag.bool ~name:"installed" ~default:false ~doc:"only installed" ()
let plugin_available = Flag.bool ~name:"available" ~default:false ~doc:"only available" ()

let plugin_list =
  Command.make ~name:"list"
    ~short:"list plugins"
    ~aliases:[ "ls" ]
    ~flags:[ Flag.pack plugin_installed; Flag.pack plugin_available ]
    ~flag_groups:[
      Flag_group.mutually_exclusive [ Flag.pack plugin_installed; Flag.pack plugin_available ];
    ]
    ~run:(fun args ->
      Printf.printf "plugin list installed=%b available=%b\n"
        (Args.get args plugin_installed) (Args.get args plugin_available);
      Error.success)
    ()

let plugin_cmd =
  Command.make ~name:"plugin"
    ~short:"manage plugins"
    ~group_id:"meta"
    ~subcommands:[ plugin_install; plugin_list ]
    ()

(* -------------------------------------------------------------------- *)
(* Misc commands exercising less-common features                         *)
(* -------------------------------------------------------------------- *)

(* Arg.minimum / Arg.at_least / Arg.maximum / Arg.at_most / Arg.range *)
let take_at_least_two =
  Command.make ~name:"take2plus"
    ~short:"takes at least 2 positionals"
    ~group_id:"misc"
    ~args:(Arg.at_least 2)
    ~run:(fun args ->
      Printf.printf "take2plus: %s\n"
        (String.concat "," (Args.positional args));
      Error.success)
    ()

let take_at_most_three =
  Command.make ~name:"take3max"
    ~short:"takes at most 3 positionals"
    ~group_id:"misc"
    ~args:(Arg.at_most 3)
    ~run:(fun args ->
      Printf.printf "take3max: %s\n"
        (String.concat "," (Args.positional args));
      Error.success)
    ()

let take_range =
  Command.make ~name:"take2to4"
    ~short:"takes 2 to 4 positionals"
    ~group_id:"misc"
    ~args:(Arg.range ~min:2 ~max:4)
    ~run:(fun args ->
      Printf.printf "take2to4: %s\n"
        (String.concat "," (Args.positional args));
      Error.success)
    ()

(* Arg.named ["a";"b";"c"] (non-typed variant) + Args.positional_at + Args.positional_3 *)
let named_listed =
  Command.make ~name:"triple"
    ~short:"takes named triple"
    ~group_id:"misc"
    ~args:(Arg.named [ "alpha"; "beta"; "gamma" ])
    ~run:(fun args ->
      let a = Args.positional_at args 0 in
      let (b, c) = (Args.positional_at args 1, Args.positional_at args 2) in
      Printf.printf "triple a=%s b=%s c=%s\n" a b c;
      let (x, y, z) = Args.positional_3 args in
      Printf.printf "        (also via _3) %s %s %s\n" x y z;
      Error.success)
    ()

(* Arg.any explicit, with raw passthrough exercise *)
let shell_cmd =
  Command.make ~name:"shell"
    ~short:"run an arbitrary shell command"
    ~group_id:"misc"
    ~args:Arg.any
    ~run:(fun args ->
      Printf.printf "shell positional=[%s] raw=[%s]\n"
        (String.concat "," (Args.positional args))
        (String.concat "," (Args.raw args));
      Error.success)
    ()

(* Flag.make (raw constructor for a custom type) *)
type date = { y : int; m : int; d : int }
let parse_date s =
  try Scanf.sscanf s "%d-%d-%d" (fun y m d -> Ok { y; m; d })
  with _ -> Error (Printf.sprintf "%S is not YYYY-MM-DD" s)
let print_date d = Printf.sprintf "%04d-%02d-%02d" d.y d.m d.d

let since_flag =
  Flag.make ~name:"since" ~doc:"start date (YYYY-MM-DD)"
    ~placeholder:"<YYYY-MM-DD>"
    ~parser:parse_date ~printer:print_date ()
    (* Note: no ~default so unset reads as Missing_flag via Args.get,
       OR None via Args.get_opt *)

let report_cmd =
  Command.make ~name:"report"
    ~short:"generate a report from <date>"
    ~group_id:"misc"
    ~flags:[ Flag.pack since_flag ]
    ~run:(fun args ->
      let d = Args.get args since_flag in   (* will raise Missing_flag if unset *)
      Printf.printf "report since %s\n" (print_date d);
      Error.success)
    ()

(* Hidden command (Command-level ~hidden) *)
let hidden_dump =
  Command.make ~name:"__dump"
    ~short:"dump internal state"
    ~hidden:true
    ~run:(fun _ -> print_endline "internal dump"; Error.success)
    ()

(* Hidden experimental command (per-command ~version was removed as dead). *)
let plugin_v2_cmd =
  Command.make ~name:"plugin-v2-preview"
    ~short:"experimental v2 plugin runner"
    ~hidden:true
    ~run:(fun _ -> print_endline "v2 stub"; Error.success)
    ()

(* -------------------------------------------------------------------- *)
(* Root command                                                          *)
(* -------------------------------------------------------------------- *)

let root =
  Command.make ~name:"kit"
    ~short:"the everything-bagel CLI for exercising mamba"
    ~long:"kit is a contrived dev-ops tool whose only purpose is to use\n\
           every documented mamba feature in one program."
    ~example:"  $ kit build --release\n  $ kit deploy prod -f\n  $ kit db migrate up"
    ~aliases:[ "kitchen" ]
    ~suggest_for:[ "kits"; "kt" ]
    ~persistent_flags:[
      Flag.pack p_config;
      Flag.pack p_verbose;
      Flag.pack p_quiet;
      Flag.pack p_log_level;
      Flag.pack p_debug_internal;
      Flag.pack p_old_color;
    ]
    ~groups:[
      "dev",  "Development";
      "ops",  "Operations";
      "meta", "Configuration & Plugins";
      "misc", "Miscellaneous (feature exercise)";
    ]
    ~persistent_pre_run:short_circuit_hook  (* short-circuits when --debug-internal *)
    ~persistent_post_run:(log_hook "root.persistent_post_run")
    ~subcommands:[
      build_cmd; test_cmd; run_cmd;
      deploy_cmd; grant_cmd; rollback_cmd;
      db_cmd;
      config_cmd; plugin_cmd;
      take_at_least_two; take_at_most_three; take_range;
      named_listed; shell_cmd; report_cmd;
      hidden_dump; plugin_v2_cmd;
    ]
    ()

(* -------------------------------------------------------------------- *)
(* Program                                                               *)
(* -------------------------------------------------------------------- *)

let () =
  Program.make
    ~name:"kit"
    ~version:"1.0.0"
    ~description:"every-feature exercise"
    ~author:"mamba dev"
    ~case_insensitive:true       (* exercise this knob *)
    ~color:`Auto
    ~root
    ()
  |> Program.run_exn
