(** Integration test for examples/kitchen_sink.exe.

    Every public feature must show up here AND in examples/kitchen_sink.ml.
    See examples/kitchen_sink/FEATURES.md for the inventory. *)

open Test_util

let kitchen_sink_exe () =
  let here = Filename.dirname Sys.executable_name in
  Filename.concat here "../examples/kitchen_sink.exe"

let run ?(env = [||]) argv =
  let exe = kitchen_sink_exe () in
  let full_argv = Array.of_list (exe :: argv) in
  let merged_env = Array.append (Unix.environment ()) env in
  let (out_ic, stdin_oc, err_ic) =
    Unix.open_process_args_full exe full_argv merged_env
  in
  let read_all ic =
    let buf = Buffer.create 256 in
    let chunk = Bytes.create 4096 in
    let rec loop () =
      let n = input ic chunk 0 4096 in
      if n = 0 then ()
      else begin
        Buffer.add_subbytes buf chunk 0 n;
        loop ()
      end
    in
    loop ();
    Buffer.contents buf
  in
  let out = read_all out_ic in
  let err = read_all err_ic in
  let code =
    match Unix.close_process_full (out_ic, stdin_oc, err_ic) with
    | Unix.WEXITED n -> n
    | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> -1
  in
  (code, out, err)

let check_exit ~label expected (code, _, _) =
  Alcotest.(check int) (label ^ " exit code") expected code

let check_stdout ~label needle (_, out, _) =
  must_contain (label ^ ": stdout") out needle

let check_stderr ~label needle (_, _, err) =
  must_contain (label ^ ": stderr") err needle

let omit_in_stdout ~label needle (_, out, _) =
  must_omit (label ^ ": stdout") out needle

let flag_types () =
  let r = run [ "build"; "--release"; "-t"; "wasm"; "-j"; "8" ] in
  check_exit ~label:"build" 0 r;
  check_stdout ~label:"build" "release=true target=wasm jobs=8" r

let flag_enum () =
  let r_ok = run [ "--log-level"; "debug"; "build" ] in
  check_exit ~label:"enum debug" 0 r_ok;
  let r_bad = run [ "--log-level"; "garbage"; "build" ] in
  check_exit ~label:"enum invalid" 2 r_bad;
  check_stderr ~label:"enum invalid" "not one of" r_bad

let flag_env_fallback () =
  let r = run ~env:[| "KIT_CONFIG=/tmp/x" |] [ "config"; "list" ] in
  check_exit ~label:"env" 0 r

let flag_required () =
  let r_missing = run [ "test" ] in
  check_exit ~label:"required missing" 2 r_missing;
  check_stderr ~label:"required missing" "required flag" r_missing;
  let r_given = run [ "test"; "--pattern"; "foo" ] in
  check_exit ~label:"required given" 0 r_given

let flag_hidden_and_deprecated () =
  let r_help = run [ "--help" ] in
  omit_in_stdout ~label:"hidden flag" "--debug-internal" r_help;
  omit_in_stdout ~label:"deprecated flag" "--color-output" r_help;
  let r_use = run [ "--color-output"; "build" ] in
  check_exit ~label:"deprecated still works" 0 r_use;
  check_stderr ~label:"deprecated warns" "deprecated" r_use

let flag_short_circuit_hook () =
  let r = run [ "--debug-internal"; "build" ] in
  check_exit ~label:"hook short-circuit" 99 r

let arg_specs () =
  check_exit ~label:"none (config list)"  0 (run [ "config"; "list" ]);
  check_exit ~label:"any (shell)"         0 (run [ "shell"; "anything" ]);
  check_exit ~label:"exactly (rollback)"  0 (run [ "rollback"; "v1" ]);
  check_exit ~label:"named (triple ok)"   0 (run [ "triple"; "a"; "b"; "c" ]);
  check_exit ~label:"named (triple too few)" 2 (run [ "triple"; "a"; "b" ]);
  check_exit ~label:"variadic min=1 ok"   0 (run [ "plugin"; "install"; "a" ]);
  check_exit ~label:"variadic min=1 fail" 2 (run [ "plugin"; "install" ]);
  check_exit ~label:"variadic min=0 ok zero" 0 (run [ "config"; "import" ]);
  check_exit ~label:"at_least 2 ok"       0 (run [ "take2plus"; "a"; "b" ]);
  check_exit ~label:"at_least 2 fail"     2 (run [ "take2plus"; "a" ]);
  check_exit ~label:"at_most 3 ok"        0 (run [ "take3max"; "a"; "b" ]);
  check_exit ~label:"at_most 3 fail"      2 (run [ "take3max"; "a"; "b"; "c"; "d" ]);
  check_exit ~label:"range 2-4 ok"        0 (run [ "take2to4"; "a"; "b"; "c" ]);
  check_exit ~label:"range 2-4 fail (1)"  2 (run [ "take2to4"; "a" ]);
  check_exit ~label:"only_valid_of ok"    0 (run [ "deploy"; "dev" ]);
  check_exit ~label:"only_valid_of fail"  2 (run [ "deploy"; "bogus" ]);
  check_exit ~label:"custom validator ok"
    0 (run [ "db"; "migrate"; "create"; "0001_init" ]);
  check_exit ~label:"custom validator fail"
    2 (run [ "db"; "migrate"; "create"; "bad name" ])

let typed_named_helpers () =
  check_stdout ~label:"named1"
    "key=foo" (run [ "config"; "get"; "foo" ]);
  check_stdout ~label:"named2"
    "key=k value=v" (run [ "config"; "set"; "k"; "v" ]);
  check_stdout ~label:"named3"
    "k=v (note)" (run [ "config"; "alias"; "k"; "v"; "note" ])

let args_accessors () =
  let r = run [ "shell"; "a"; "b"; "--"; "--c"; "d" ] in
  check_exit ~label:"shell ok" 0 r;
  check_stdout ~label:"positional has all"
    "positional=[a,b,--c,d]" r;
  check_stdout ~label:"raw has post --" "raw=[--c,d]" r

let args_was_set () =
  let r_set = run [ "plugin"; "install"; "x"; "--only-tag"; "dev" ] in
  check_stdout ~label:"was_set=true" "was_set=true" r_set;
  let r_unset = run [ "plugin"; "install"; "x" ] in
  check_stdout ~label:"was_set=false" "was_set=false" r_unset

let missing_flag () =
  let r = run [ "report" ] in
  check_exit ~label:"missing flag" 2 r;
  check_stderr ~label:"missing flag msg" "required flag --since not set" r;
  check_stderr ~label:"missing flag footer" "Run \"kit report --help\"" r

let aliases_and_suggest_for () =
  check_exit ~label:"alias r" 0 (run [ "r" ]);
  let r = run [ "exec" ] in
  check_exit ~label:"unknown" 2 r;
  check_stderr ~label:"suggest_for" "Did you mean \"run\"" r

let did_you_mean () =
  let r_cmd = run [ "bulid" ] in
  check_stderr ~label:"cmd typo" "Did you mean \"build\"" r_cmd;
  let r_flag = run [ "build"; "--rlease" ] in
  check_stderr ~label:"flag typo" "Did you mean --release" r_flag

let prescan_flag_before_command () =
  let r = run [ "-j8"; "build" ] in
  check_exit ~label:"-j8 build" 0 r;
  check_stdout ~label:"-j8 picked up" "jobs=8" r

let case_insensitive () =
  let r_top = run [ "BUILD"; "--release" ] in
  check_exit ~label:"BUILD" 0 r_top;
  check_stdout ~label:"BUILD ran" "release=true" r_top;
  let r_deep = run [ "Db"; "Migrate"; "Up"; "--steps"; "3" ] in
  check_exit ~label:"Db Migrate Up" 0 r_deep;
  check_stdout ~label:"deep ran" "steps=3" r_deep

let flag_groups () =
  let r_half = run [ "deploy"; "prod"; "--aws-key"; "K" ] in
  check_exit ~label:"req-together half" 2 r_half;
  check_stderr ~label:"req-together msg" "requires --aws-secret" r_half;
  let r_both = run [ "deploy"; "prod"; "--dry-run"; "--force" ] in
  check_exit ~label:"mutex both" 2 r_both;
  check_stderr ~label:"mutex msg" "mutually exclusive" r_both;
  let r_none = run [ "grant"; "admin" ] in
  check_exit ~label:"one-required none" 2 r_none;
  check_stderr ~label:"one-required msg" "at least one of" r_none;
  let r_user = run [ "grant"; "admin"; "--user"; "alice" ] in
  check_exit ~label:"one-required ok" 0 r_user

let command_groups_in_help () =
  let r = run [ "--help" ] in
  check_stdout ~label:"Development group"   "Development:"   r;
  check_stdout ~label:"Operations group"    "Operations:"    r;
  check_stdout ~label:"Meta group"          "Configuration & Plugins:" r;
  check_stdout ~label:"Misc group"          "Miscellaneous"  r;
  check_stdout ~label:"Additional Commands" "Additional Commands:" r

let hidden_and_deprecated_commands () =
  let r_help = run [ "--help" ] in
  omit_in_stdout ~label:"hidden cmd" "__dump" r_help;
  check_exit ~label:"hidden cmd still runnable" 0 (run [ "__dump" ]);
  let r_dep = run [ "rollback"; "v1" ] in
  check_exit ~label:"deprecated cmd runs" 0 r_dep;
  check_stderr ~label:"deprecated cmd warns" "deprecated" r_dep

let auto_subcommands () =
  check_exit ~label:"help"       0 (run [ "help" ]);
  check_exit ~label:"version"    0 (run [ "version" ]);
  check_stdout ~label:"version output" "kit version 1.0.0" (run [ "version" ]);
  check_stdout ~label:"--version output" "kit version 1.0.0" (run [ "--version" ]);
  let r_bash = run [ "completion"; "bash" ] in
  check_exit ~label:"completion bash" 0 r_bash;
  let (_, bash_out, _) = r_bash in
  Alcotest.(check bool) "bash script non-empty" true (String.length bash_out > 100)

let flag_list_and_repeated () =
  let r = run [ "db"; "seed"; "-f"; "a,b"; "-f"; "c"; "--tag"; "x"; "--tag"; "y" ] in
  check_exit ~label:"db seed" 0 r;
  check_stdout ~label:"list accumulates" "files=[a,b,c]" r;
  check_stdout ~label:"repeated accumulates" "tags=[x,y]" r

let flag_count () =
  check_exit ~label:"-vvv build" 0 (run [ "-vvv"; "build" ])

let flag_make_custom () =
  let r_ok = run [ "report"; "--since"; "2024-12-31" ] in
  check_exit ~label:"custom date" 0 r_ok;
  check_stdout ~label:"date parsed" "2024-12-31" r_ok;
  let r_bad = run [ "report"; "--since"; "notadate" ] in
  check_exit ~label:"custom date bad" 2 r_bad;
  check_stderr ~label:"custom date msg" "not YYYY-MM-DD" r_bad

let flag_path () =
  check_exit ~label:"path default" 0 (run [ "build" ]);
  check_exit ~label:"path explicit" 0
    (run [ "-c"; "/etc/hostname"; "build" ])

let tc name f = Alcotest.test_case name `Quick f

let () =
  Alcotest.run "kitchen_sink"
    [
      "Flag types", [
        tc "string/int/bool/float on build" flag_types;
        tc "enum"                            flag_enum;
        tc "list + repeated"                 flag_list_and_repeated;
        tc "count"                           flag_count;
        tc "make (custom date)"              flag_make_custom;
        tc "path"                            flag_path;
      ];
      "Flag attributes", [
        tc "env fallback"            flag_env_fallback;
        tc "required"                flag_required;
        tc "hidden + deprecated"     flag_hidden_and_deprecated;
      ];
      "Hooks", [
        tc "short-circuit hook"      flag_short_circuit_hook;
      ];
      "Args / positional", [
        tc "every Arg.spec shape"    arg_specs;
        tc "typed named helpers"     typed_named_helpers;
      ];
      "Args accessors", [
        tc "positional + raw"        args_accessors;
        tc "was_set"                 args_was_set;
        tc "Missing_flag friendly"   missing_flag;
      ];
      "Command attributes", [
        tc "aliases + suggest_for"   aliases_and_suggest_for;
        tc "hidden + deprecated cmd" hidden_and_deprecated_commands;
        tc "groups appear in help"   command_groups_in_help;
      ];
      "Flag groups", [
        tc "all three rule shapes"   flag_groups;
      ];
      "Program features", [
        tc "did-you-mean cmd + flag" did_you_mean;
        tc "case-insensitive"        case_insensitive;
        tc "pre-scan flag-first"     prescan_flag_before_command;
        tc "auto subcommands"        auto_subcommands;
      ];
    ]
