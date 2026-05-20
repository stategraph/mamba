open Mamba
open Test_util

let str_list = Alcotest.(list string)

(* --- Suggest --- *)

let test_suggest_distance () =
  Alcotest.(check int) "identical = 0"     0 (Suggest.distance "abc" "abc");
  Alcotest.(check int) "insertion = 1"     1 (Suggest.distance "ab"  "abc");
  Alcotest.(check int) "deletion = 1"      1 (Suggest.distance "abc" "ab");
  Alcotest.(check int) "substitution = 1"  1 (Suggest.distance "abc" "abd");
  Alcotest.(check int) "transposition = 1" 1 (Suggest.distance "abc" "acb")

let test_suggest_closest () =
  let candidates = [ "say"; "shout"; "whisper" ] in
  Alcotest.(check str_list) "typo of say"
    [ "say" ] (Suggest.closest ~max_distance:2 "sya" candidates);
  Alcotest.(check str_list) "too far"
    [] (Suggest.closest ~max_distance:1 "completely-different" candidates)

(* --- Flag + Args (typed lookup) --- *)

let test_flag_typed_lookup () =
  let count = Flag.int  ~name:"count" ~default:1 ~doc:"" () in
  let name  = Flag.string ~name:"name"  ~default:"x" ~doc:"" () in
  let upper = Flag.bool ~name:"upper" ~doc:"" () in
  let sub =
    Command.make ~name:"sub"
      ~flags:[ Flag.pack count; Flag.pack name; Flag.pack upper ]
      ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = Program.make ~name:"app" ~version:"0" ~root ~help_command:false
               ~completion_command:false () in
  match Program.dispatch prog ~argv:[| "app"; "sub"; "--count"; "5"; "--upper" |] with
  | Run { args; _ } ->
    Alcotest.(check int)    "count" 5    (Args.get args count);
    Alcotest.(check string) "name"  "x"  (Args.get args name);  (* default *)
    Alcotest.(check bool)   "upper" true (Args.get args upper)
  | _ -> Alcotest.fail "expected Run"

(* --- Parser: short flag clustering --- *)

let test_short_cluster () =
  let v = Flag.bool  ~name:"verbose" ~short:'v' ~doc:"" () in
  let f = Flag.bool  ~name:"force"   ~short:'f' ~doc:"" () in
  let n = Flag.int   ~name:"num"     ~short:'n' ~doc:"" () in
  let sub =
    Command.make ~name:"go" ~flags:[ Flag.pack v; Flag.pack f; Flag.pack n ]
      ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = Program.make ~name:"app" ~version:"0" ~root
               ~help_command:false ~completion_command:false () in
  (match Program.dispatch prog ~argv:[| "app"; "go"; "-vf"; "-n7" |] with
   | Run { args; _ } ->
     Alcotest.(check bool) "v" true (Args.get args v);
     Alcotest.(check bool) "f" true (Args.get args f);
     Alcotest.(check int)  "n" 7    (Args.get args n)
   | _ -> Alcotest.fail "expected Run")

(* --- Parser: count flag --- *)

let test_count_flag () =
  let vv = Flag.count ~name:"verbose" ~short:'v' ~doc:"" () in
  let sub = Command.make ~name:"go" ~flags:[ Flag.pack vv ] ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = Program.make ~name:"app" ~version:"0" ~root
               ~help_command:false ~completion_command:false () in
  match Program.dispatch prog ~argv:[| "app"; "go"; "-vvv" |] with
  | Run { args; _ } -> Alcotest.(check int) "count" 3 (Args.get args vv)
  | _ -> Alcotest.fail "expected Run"

(* --- Parser: env fallback --- *)

let test_env_fallback () =
  Unix.putenv "MAMBA_TEST_PORT" "9000";
  let port = Flag.int ~name:"port" ~env:"MAMBA_TEST_PORT" ~doc:"" () in
  let sub = Command.make ~name:"go" ~flags:[ Flag.pack port ] ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = Program.make ~name:"app" ~version:"0" ~root
               ~help_command:false ~completion_command:false () in
  (match Program.dispatch prog ~argv:[| "app"; "go" |] with
   | Run { args; _ } -> Alcotest.(check int) "port" 9000 (Args.get args port)
   | _ -> Alcotest.fail "expected Run")

(* --- Parser: required flag missing --- *)

let test_required_flag_missing () =
  let port = Flag.int ~name:"port" ~required:true ~doc:"" () in
  let sub = Command.make ~name:"go" ~flags:[ Flag.pack port ] ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = Program.make ~name:"app" ~version:"0" ~root
               ~help_command:false ~completion_command:false () in
  match Program.dispatch prog ~argv:[| "app"; "go" |] with
  | Error { code; _ } -> Alcotest.(check int) "exit code" 2 code
  | _ -> Alcotest.fail "expected Error"

(* --- Parser: -- passthrough --- *)

let test_raw_passthrough () =
  let sub = Command.make ~name:"sh" ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = Program.make ~name:"app" ~version:"0" ~root
               ~help_command:false ~completion_command:false () in
  match Program.dispatch prog
          ~argv:[| "app"; "sh"; "--"; "--unknown"; "value" |] with
  | Run { args; _ } ->
    (* POSIX: tokens after "--" land in positional too. raw mirrors for
       wrapper-tool transparency. *)
    Alcotest.(check str_list) "raw"
      [ "--unknown"; "value" ] (Args.raw args);
    Alcotest.(check str_list) "positional includes post-`--`"
      [ "--unknown"; "value" ] (Args.positional args)
  | _ -> Alcotest.fail "expected Run"

(* Regression: `cmd -- 5` should satisfy Arg.exactly 1 (the post-`--` token
   is a positional, not hidden in raw). Was broken before the POSIX fix. *)
let test_dash_dash_satisfies_arg_validator () =
  let sub =
    Command.make ~name:"done" ~args:(Arg.exactly 1)
      ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer (Buffer.create 16))
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let code = Program.run prog ~argv:[| "app"; "done"; "--"; "5" |] in
  Alcotest.(check int) "exit 0 (Arg.exactly 1 satisfied)" 0 code

(* Regression: a Flag.list with default [] should NOT render "(default: )"
   in help output. *)
let test_empty_list_default_not_rendered () =
  let tags = Flag.list ~sep:',' (Flag.string ~name:"tag" ~doc:"" ()) in
  let sub =
    Command.make ~name:"ls" ~flags:[ Flag.pack tags ] ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let out_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let _ = Program.run prog ~argv:[| "app"; "ls"; "--help" |] in
  let out = Buffer.contents out_buf in
  Alcotest.(check bool) "no '(default: )' artifact" false
    (contains out "(default: )")

(* --- Multi flags: list (sep-based) and repeated (occurrence-based) --- *)

let mk_prog ~root =
  Program.make ~name:"app" ~version:"0" ~root
    ~help_command:false ~completion_command:false ()

let test_list_single_occurrence () =
  let tags = Flag.list ~sep:','
      (Flag.string ~name:"tags" ~doc:"" ()) in
  let sub = Command.make ~name:"go" ~flags:[ Flag.pack tags ] ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  match Program.dispatch prog ~argv:[| "app"; "go"; "--tags=a,b,c" |] with
  | Run { args; _ } ->
    Alcotest.(check str_list) "list one occurrence"
      [ "a"; "b"; "c" ] (Args.get args tags)
  | _ -> Alcotest.fail "expected Run"

let test_list_multiple_occurrences () =
  let tags = Flag.list ~sep:','
      (Flag.string ~name:"tags" ~doc:"" ()) in
  let sub = Command.make ~name:"go" ~flags:[ Flag.pack tags ] ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  match Program.dispatch prog
          ~argv:[| "app"; "go"; "--tags=a,b"; "--tags=c"; "--tags=d,e" |] with
  | Run { args; _ } ->
    Alcotest.(check str_list) "list accumulates across occurrences"
      [ "a"; "b"; "c"; "d"; "e" ] (Args.get args tags)
  | _ -> Alcotest.fail "expected Run"

let test_repeated_single () =
  let filename = Flag.repeated
      (Flag.string ~name:"filename" ~short:'f' ~doc:"" ()) in
  let sub =
    Command.make ~name:"go" ~flags:[ Flag.pack filename ] ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  match Program.dispatch prog ~argv:[| "app"; "go"; "-f"; "a.yaml" |] with
  | Run { args; _ } ->
    Alcotest.(check str_list) "single -f" [ "a.yaml" ] (Args.get args filename)
  | _ -> Alcotest.fail "expected Run"

let test_repeated_multiple () =
  let filename = Flag.repeated
      (Flag.string ~name:"filename" ~short:'f' ~doc:"" ()) in
  let sub =
    Command.make ~name:"go" ~flags:[ Flag.pack filename ] ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  match Program.dispatch prog
          ~argv:[| "app"; "go"; "-f"; "a.yaml"; "-f"; "b.yaml"; "--filename=c.yaml" |]
  with
  | Run { args; _ } ->
    Alcotest.(check str_list) "all -f accumulate (mixed short/long)"
      [ "a.yaml"; "b.yaml"; "c.yaml" ] (Args.get args filename)
  | _ -> Alcotest.fail "expected Run"

let test_repeated_default_when_absent () =
  let filename = Flag.repeated
      (Flag.string ~name:"filename" ~short:'f' ~doc:"" ()) in
  let sub =
    Command.make ~name:"go" ~flags:[ Flag.pack filename ] ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  match Program.dispatch prog ~argv:[| "app"; "go" |] with
  | Run { args; _ } ->
    Alcotest.(check str_list) "default empty when no occurrence"
      [] (Args.get args filename)
  | _ -> Alcotest.fail "expected Run"

let test_repeated_of_int () =
  let port = Flag.repeated
      (Flag.int ~name:"port" ~short:'p' ~doc:"" ()) in
  let sub =
    Command.make ~name:"go" ~flags:[ Flag.pack port ] ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  match Program.dispatch prog
          ~argv:[| "app"; "go"; "-p"; "80"; "-p"; "443"; "-p"; "8080" |] with
  | Run { args; _ } ->
    Alcotest.(check (list int)) "int repeated"
      [ 80; 443; 8080 ] (Args.get args port)
  | _ -> Alcotest.fail "expected Run"

(* --- Hidden flag: parses but doesn't appear in help/completion --- *)

let test_hidden_flag_parses () =
  let secret = Flag.string ~name:"secret" ~hidden:true ~default:"" ~doc:"" () in
  let captured = ref "" in
  let sub =
    Command.make ~name:"go" ~flags:[ Flag.pack secret ]
      ~run:(fun args -> captured := Args.get args secret; 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  let code = Program.run prog ~argv:[| "app"; "go"; "--secret=abc" |] in
  Alcotest.(check int) "exit 0" 0 code;
  Alcotest.(check string) "value parsed" "abc" !captured

let test_hidden_flag_omitted_from_help () =
  let secret = Flag.string ~name:"secret" ~hidden:true ~default:"" ~doc:"" () in
  let visible = Flag.string ~name:"visible" ~default:"" ~doc:"" () in
  let sub =
    Command.make ~name:"go"
      ~flags:[ Flag.pack secret; Flag.pack visible ] ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let out_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let _ = Program.run prog ~argv:[| "app"; "go"; "--help" |] in
  let out = Buffer.contents out_buf in
  Alcotest.(check bool) "visible flag present"  true  (contains out "--visible");
  Alcotest.(check bool) "hidden flag suppressed" false (contains out "--secret")

let test_hidden_flag_omitted_from_completion () =
  let secret = Flag.string ~name:"secret" ~hidden:true ~default:"" ~doc:"" () in
  let visible = Flag.string ~name:"visible" ~default:"" ~doc:"" () in
  let sub =
    Command.make ~name:"go"
      ~flags:[ Flag.pack secret; Flag.pack visible ] ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  Completion.emit ~out:fmt ~shell:Bash ~program_name:"app" ~root;
  Format.pp_print_flush fmt ();
  let s = Buffer.contents buf in
  Alcotest.(check bool) "visible in bash"  true  (contains s "--visible");
  Alcotest.(check bool) "hidden suppressed" false (contains s "--secret")

(* --- -h/--help and --version visible in help output --- *)

let test_help_flag_listed_in_help () =
  let root = Command.make ~name:"app" ~run:(fun _ -> 0) () in
  let out_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"app" ~version:"1.2.3" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let _ = Program.run prog ~argv:[| "app"; "--help" |] in
  let out = Buffer.contents out_buf in
  Alcotest.(check bool) "-h, --help shown" true (contains out "-h, --help");
  Alcotest.(check bool) "--version shown"  true (contains out "--version")

let test_version_flag_hidden_when_version_unset () =
  let root = Command.make ~name:"app" ~run:(fun _ -> 0) () in
  let out_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"app" ~version:"" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let _ = Program.run prog ~argv:[| "app"; "--help" |] in
  let out = Buffer.contents out_buf in
  Alcotest.(check bool) "-h still shown"      true  (contains out "-h, --help");
  Alcotest.(check bool) "--version suppressed" false (contains out "--version")

(* --- auto-injected version subcommand --- *)

let test_version_subcommand_auto_injected () =
  let root = Command.make ~name:"app" ~run:(fun _ -> 0) () in
  let out_buf = Buffer.create 64 in
  let prog =
    Program.make ~name:"app" ~version:"1.2.3" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let _ = Program.run prog ~argv:[| "app"; "version" |] in
  Alcotest.(check bool) "version output" true
    (contains (Buffer.contents out_buf) "app version 1.2.3")

let test_version_subcommand_skipped_when_user_declared () =
  let user_ran = ref false in
  let user_version =
    Command.make ~name:"version"
      ~run:(fun _ -> user_ran := true; 0) ()
  in
  let root =
    Command.make ~name:"app" ~run:(fun _ -> 0)
      ~subcommands:[ user_version ] ()
  in
  (* No Invalid_argument from duplicate-name check expected: auto-inject
     yields to the user-declared command. *)
  let prog =
    Program.make ~name:"app" ~version:"1.0.0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer (Buffer.create 16))
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let _ = Program.run prog ~argv:[| "app"; "version" |] in
  Alcotest.(check bool) "user's run fired" true !user_ran

(* --- Arg.variadic: named variadic positionals --- *)

let test_arg_variadic_default_min_1 () =
  let captured = ref [] in
  let sub =
    Command.make ~name:"cat" ~args:(Arg.variadic "file")
      ~run:(fun args -> captured := Args.positional args; 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let out_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let _ = Program.run prog ~argv:[| "app"; "cat"; "--help" |] in
  Alcotest.(check bool) "<file>... in usage" true
    (contains (Buffer.contents out_buf) "<file>...");
  let code = Program.run prog ~argv:[| "app"; "cat"; "a"; "b"; "c" |] in
  Alcotest.(check int) "exit 0" 0 code;
  Alcotest.(check (list string)) "all positionals" [ "a"; "b"; "c" ] !captured;
  (* zero positionals rejected with min=1 default *)
  let code_empty = Program.run prog ~argv:[| "app"; "cat" |] in
  Alcotest.(check int) "exit 2 on zero" Error.parse_error code_empty

let test_arg_variadic_min_zero () =
  let sub =
    Command.make ~name:"ls" ~args:(Arg.variadic ~min:0 "dir")
      ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  let code = Program.run prog ~argv:[| "app"; "ls" |] in
  Alcotest.(check int) "min=0 accepts zero" 0 code

(* --- Arg.named2: typed pairing of spec + accessor --- *)

let test_arg_named2_typed_pairing () =
  let (set_spec, get_kv) = Arg.named2 "key" "value" in
  let captured = ref ("", "") in
  let sub =
    Command.make ~name:"set" ~args:set_spec
      ~run:(fun args -> captured := get_kv args; 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  let code = Program.run prog ~argv:[| "app"; "set"; "foo"; "bar" |] in
  Alcotest.(check int) "exit 0" 0 code;
  Alcotest.(check (pair string string)) "tuple" ("foo", "bar") !captured

(* --- Smart default: leaf + flags + run -> Arg.none rejects stray --- *)

let test_smart_default_rejects_strays () =
  let intf = Flag.int ~name:"intf" ~default:0 ~doc:"" () in
  let sub =
    Command.make ~name:"go" ~flags:[ Flag.pack intf ] ~run:(fun _ -> 0) ()
    (* No explicit ~args: leaf + named flag + run -> Arg.none *)
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer (Buffer.create 16))
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  (* No stray positionals -> works *)
  let code_ok = Program.run prog ~argv:[| "app"; "go"; "--intf=7" |] in
  Alcotest.(check int) "no positionals exit 0" 0 code_ok;
  (* Stray positional -> rejected by smart-default Arg.none *)
  let code_err = Program.run prog ~argv:[| "app"; "go"; "stray" |] in
  Alcotest.(check int) "stray exit 2" Error.parse_error code_err

(* --- Arg.named: names show in Usage; arity = list length --- *)

let test_arg_named_describe_and_arity () =
  let sub =
    Command.make ~name:"set"
      ~args:(Arg.named [ "key"; "value" ])
      ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let out_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  (* Help shows named positionals in usage *)
  let _ = Program.run prog ~argv:[| "app"; "set"; "--help" |] in
  let out = Buffer.contents out_buf in
  Alcotest.(check bool) "<key> in usage"   true (contains out "<key>");
  Alcotest.(check bool) "<value> in usage" true (contains out "<value>");
  (* Arity is enforced *)
  let bad_buf = Buffer.create 64 in
  let prog2 =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer (Buffer.create 16))
      ~err:(Format.formatter_of_buffer bad_buf)
      ()
  in
  let code = Program.run prog2 ~argv:[| "app"; "set"; "only-one" |] in
  Alcotest.(check int) "wrong arity exits 2" Error.parse_error code;
  Alcotest.(check bool) "names 2 positional" true
    (contains (Buffer.contents bad_buf) "2 positional arguments")

(* --- positional_n helpers --- *)

let test_positional_2 () =
  let captured = ref ("", "") in
  let sub =
    Command.make ~name:"mv" ~args:(Arg.exactly 2)
      ~run:(fun args -> captured := Args.positional_2 args; 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  let code = Program.run prog ~argv:[| "app"; "mv"; "src"; "dst" |] in
  Alcotest.(check int) "exit 0" 0 code;
  Alcotest.(check (pair string string)) "tuple" ("src", "dst") !captured

let test_positional_at () =
  let captured = ref "" in
  let sub =
    Command.make ~name:"pick" ~args:(Arg.minimum 3)
      ~run:(fun args -> captured := Args.positional_at args 1; 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  let code =
    Program.run prog ~argv:[| "app"; "pick"; "a"; "b"; "c"; "d" |]
  in
  Alcotest.(check int) "exit 0" 0 code;
  Alcotest.(check string) "index 1" "b" !captured

(* --- Flag_group: help renders constraint annotations --- *)

let test_flag_group_annotations_in_help () =
  let installed = Flag.bool ~name:"installed" ~doc:"" () in
  let available = Flag.bool ~name:"available" ~doc:"" () in
  let sub =
    Command.make ~name:"list"
      ~flags:[ Flag.pack installed; Flag.pack available ]
      ~flag_groups:[
        Flag_group.mutually_exclusive [ Flag.pack installed; Flag.pack available ]
      ]
      ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let out_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let _ = Program.run prog ~argv:[| "app"; "list"; "--help" |] in
  let out = Buffer.contents out_buf in
  Alcotest.(check bool) "mutex annotation on --installed" true
    (contains out "(mutually exclusive with --available)");
  Alcotest.(check bool) "mutex annotation on --available" true
    (contains out "(mutually exclusive with --installed)")

(* --- Flag_group: typo in _by_name caught at Program.make --- *)

let test_flag_group_typo_rejected () =
  let alpha = Flag.bool ~name:"alpha" ~doc:"" () in
  let sub =
    Command.make ~name:"s" ~flags:[ Flag.pack alpha ]
      ~flag_groups:[
        Flag_group.required_together_by_name [ "alpha"; "betta" ]
        (* "betta" is a typo -- only "alpha" exists on this command *)
      ]
      ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  try
    let _ =
      Program.make ~name:"app" ~version:"0" ~root
        ~help_command:false ~completion_command:false
        ~out:(Format.formatter_of_buffer (Buffer.create 16))
        ~err:(Format.formatter_of_buffer (Buffer.create 16))
        ()
    in
    Alcotest.fail "expected Invalid_argument for typo'd flag-group ref"
  with Invalid_argument _ -> ()

(* The type-safe constructor closes this off at compile-time, so a runtime
   test verifies the same passes when the flags really exist. *)
let test_flag_group_typed_accepts () =
  let alpha = Flag.bool ~name:"alpha" ~doc:"" () in
  let beta  = Flag.bool ~name:"beta"  ~doc:"" () in
  let sub =
    Command.make ~name:"s"
      ~flags:[ Flag.pack alpha; Flag.pack beta ]
      ~flag_groups:[
        Flag_group.required_together [ Flag.pack alpha; Flag.pack beta ]
      ]
      ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let _ =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer (Buffer.create 16))
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  (* No exception = pass. *)
  ()

(* --- Missing_flag: get on no-default no-env unset flag --- *)

let test_missing_flag_friendly_error () =
  let no_default =
    Flag.string ~name:"workspace" ~short:'w' ~doc:"" ()
    (* deliberate: no ~default, no ~required, no ~env *)
  in
  let sub =
    Command.make ~name:"go" ~flags:[ Flag.pack no_default ]
      ~run:(fun args ->
        let _ : string = Args.get args no_default in
        0)
      ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let err_buf = Buffer.create 64 in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer (Buffer.create 16))
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  let code = Program.run prog ~argv:[| "app"; "go" |] in
  Alcotest.(check int) "exit 2 (parse_error, not runtime)" Error.parse_error code;
  let err = Buffer.contents err_buf in
  Alcotest.(check bool) "names the flag"           true  (contains err "--workspace");
  Alcotest.(check bool) "no raw Not_found leak"    false (contains err "Not_found");
  Alcotest.(check bool) "says 'required ... not set'" true
    (contains err "required flag --workspace not set")

(* --- Flag deprecation: parses, warns when set, hidden from help --- *)

let test_deprecated_flag_parses () =
  let old =
    Flag.string ~name:"old-name" ~deprecated:"use --new-name instead"
      ~default:"" ~doc:"" ()
  in
  let captured = ref "" in
  let sub =
    Command.make ~name:"go" ~flags:[ Flag.pack old ]
      ~run:(fun args -> captured := Args.get args old; 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  let code = Program.run prog ~argv:[| "app"; "go"; "--old-name=value" |] in
  Alcotest.(check int) "exit 0" 0 code;
  Alcotest.(check string) "still parses" "value" !captured

let test_deprecated_flag_warns_when_set () =
  let old =
    Flag.string ~name:"old-name" ~deprecated:"use --new-name instead"
      ~default:"" ~doc:"" ()
  in
  let sub = Command.make ~name:"go" ~flags:[ Flag.pack old ] ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let err_buf = Buffer.create 64 in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer (Buffer.create 16))
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  let _ = Program.run prog ~argv:[| "app"; "go"; "--old-name=value" |] in
  let err = Buffer.contents err_buf in
  Alcotest.(check bool) "warning present"      true (contains err "deprecated");
  Alcotest.(check bool) "names the flag"       true (contains err "--old-name");
  Alcotest.(check bool) "names the suggestion" true (contains err "use --new-name instead")

let test_deprecated_flag_silent_when_unset () =
  let old =
    Flag.string ~name:"old-name" ~deprecated:"use --new-name instead"
      ~default:"" ~doc:"" ()
  in
  let sub = Command.make ~name:"go" ~flags:[ Flag.pack old ] ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let err_buf = Buffer.create 64 in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer (Buffer.create 16))
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  let _ = Program.run prog ~argv:[| "app"; "go" |] in
  let err = Buffer.contents err_buf in
  Alcotest.(check bool) "no warning when unset" false (contains err "deprecated")

let test_deprecated_flag_hidden_from_help () =
  let old =
    Flag.string ~name:"old-name" ~deprecated:"x" ~default:"" ~doc:"" ()
  in
  let new_ = Flag.string ~name:"new-name" ~default:"" ~doc:"" () in
  let sub =
    Command.make ~name:"go"
      ~flags:[ Flag.pack old; Flag.pack new_ ] ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let out_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let _ = Program.run prog ~argv:[| "app"; "go"; "--help" |] in
  let out = Buffer.contents out_buf in
  Alcotest.(check bool) "new-name shown" true  (contains out "--new-name");
  Alcotest.(check bool) "old-name hidden" false (contains out "--old-name")

(* --- Flag.enum: accepts valid, rejects invalid --- *)

let test_enum_accepts_valid () =
  let level =
    Flag.enum ~name:"level"
      ~values:[ ("debug", `Debug); ("info", `Info); ("warn", `Warn) ]
      ~doc:"" ()
  in
  let captured = ref `Debug in
  let sub =
    Command.make ~name:"go" ~flags:[ Flag.pack level ]
      ~run:(fun args -> captured := Args.get args level; 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  let code = Program.run prog ~argv:[| "app"; "go"; "--level=warn" |] in
  Alcotest.(check int) "exit 0" 0 code;
  Alcotest.(check bool) "warn selected" true (!captured = `Warn)

let test_enum_rejects_invalid () =
  let level =
    Flag.enum ~name:"level"
      ~values:[ ("debug", `Debug); ("info", `Info) ]
      ~doc:"" ()
  in
  let sub = Command.make ~name:"go" ~flags:[ Flag.pack level ] ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  let code = Program.run prog ~argv:[| "app"; "go"; "--level=trace" |] in
  Alcotest.(check int) "exit 2" Error.parse_error code

(* --- Flag.path: must_exist=true errors on missing file --- *)

let test_path_must_exist_passes_when_present () =
  let cfg = Flag.path ~name:"cfg" ~must_exist:true ~doc:"" () in
  let sub = Command.make ~name:"go" ~flags:[ Flag.pack cfg ] ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  (* /etc/hostname is reliably present on Linux test hosts. *)
  let code =
    Program.run prog ~argv:[| "app"; "go"; "--cfg=/etc/hostname" |]
  in
  Alcotest.(check int) "exit 0 when file exists" 0 code

let test_path_must_exist_fails_when_missing () =
  let cfg = Flag.path ~name:"cfg" ~must_exist:true ~doc:"" () in
  let sub = Command.make ~name:"go" ~flags:[ Flag.pack cfg ] ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = mk_prog ~root in
  let code =
    Program.run prog ~argv:[| "app"; "go"; "--cfg=/nope/never/exists.yaml" |]
  in
  Alcotest.(check int) "exit 2 when missing" Error.parse_error code

(* --- Arg.spec validator --- *)

let test_arg_validator () =
  let sub =
    Command.make ~name:"need2" ~args:(Arg.exactly 2)
      ~run:(fun _ -> 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ sub ] () in
  let prog = Program.make ~name:"app" ~version:"0" ~root
               ~help_command:false ~completion_command:false () in
  (match Program.dispatch prog ~argv:[| "app"; "need2"; "a" |] with
   | Error { code; _ } -> Alcotest.(check int) "too few" 2 code
   | _ -> Alcotest.fail "expected Error");
  (match Program.dispatch prog ~argv:[| "app"; "need2"; "a"; "b" |] with
   | Run _ -> ()
   | _ -> Alcotest.fail "expected Run")

(* --- Lifecycle hook order --- *)

let test_lifecycle_order () =
  let log = ref [] in
  let push s = log := s :: !log in
  let mk_hook tag args =
    push tag;
    ignore args;
    None
  in
  let leaf =
    Command.make ~name:"leaf"
      ~persistent_pre_run:(mk_hook "ppre-leaf")
      ~pre_run:(mk_hook "pre-leaf")
      ~run:(fun _ -> push "run-leaf"; 0)
      ~post_run:(mk_hook "post-leaf")
      ~persistent_post_run:(mk_hook "ppost-leaf")
      ()
  in
  let mid =
    Command.make ~name:"mid"
      ~persistent_pre_run:(mk_hook "ppre-mid")
      ~persistent_post_run:(mk_hook "ppost-mid")
      ~subcommands:[ leaf ] ()
  in
  let root =
    Command.make ~name:"root"
      ~persistent_pre_run:(mk_hook "ppre-root")
      ~persistent_post_run:(mk_hook "ppost-root")
      ~subcommands:[ mid ] ()
  in
  let prog = Program.make ~name:"root" ~version:"0" ~root
               ~help_command:false ~completion_command:false () in
  let code = Program.run prog ~argv:[| "root"; "mid"; "leaf" |] in
  Alcotest.(check int) "exit 0" 0 code;
  Alcotest.(check str_list)
    "hook order"
    [
      "ppre-root"; "ppre-mid"; "ppre-leaf";
      "pre-leaf"; "run-leaf"; "post-leaf";
      "ppost-leaf"; "ppost-mid"; "ppost-root";
    ]
    (List.rev !log)

(* --- Lifecycle short-circuit --- *)

let test_lifecycle_short_circuit () =
  let log = ref [] in
  let push s = log := s :: !log in
  let leaf =
    Command.make ~name:"leaf"
      ~pre_run:(fun _ -> push "pre"; Some 42)  (* short-circuit *)
      ~run:(fun _ -> push "run"; 0)
      ~post_run:(fun _ -> push "post"; None)
      ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ leaf ] () in
  let prog = Program.make ~name:"app" ~version:"0" ~root
               ~help_command:false ~completion_command:false () in
  let code = Program.run prog ~argv:[| "app"; "leaf" |] in
  Alcotest.(check int)    "exit code from hook" 42 code;
  Alcotest.(check str_list)
    "only pre fires" [ "pre" ] (List.rev !log)

(* --- Program: did-you-mean --- *)

let test_did_you_mean () =
  let say = Command.make ~name:"say" ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ say ] () in
  let buf = Buffer.create 256 in
  let err_fmt = Format.formatter_of_buffer buf in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root ~err:err_fmt
      ~help_command:false ~completion_command:false ()
  in
  let _ = Program.run prog ~argv:[| "app"; "sya" |] in
  Format.pp_print_flush err_fmt ();
  let out = Buffer.contents buf in
  Alcotest.(check bool) "contains suggestion"
    true (contains out "Did you mean \"say\"")

(* --- Completion: smoke --- *)

let test_completion_smoke () =
  let go = Command.make ~name:"go" ~short:"go" ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ go ] () in
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  Completion.emit ~out:fmt ~shell:Bash ~program_name:"app" ~root;
  Format.pp_print_flush fmt ();
  let s = Buffer.contents buf in
  Alcotest.(check bool) "bash header" true (contains s "_app_complete")

(* --- Man: smoke --- *)

let test_man_write_all () =
  let dir = Filename.temp_dir "mamba_man_test" ".d" in
  let leaf =
    Command.make ~name:"leaf" ~short:"a leaf" ~run:(fun _ -> 0) ()
  in
  let mid =
    Command.make ~name:"mid" ~short:"a mid" ~subcommands:[ leaf ] ()
  in
  let root =
    Command.make ~name:"demo" ~short:"demo"
      ~subcommands:[ mid ] ()
  in
  let written =
    Man.write_all ~dir ~program_name:"demo" ~program_version:"1.0" ~root
  in
  Alcotest.(check bool) "wrote at least 3 files (root + mid + leaf)"
    true (List.length written >= 3);
  List.iter (fun p ->
    let size = (Unix.stat p).st_size in
    Alcotest.(check bool) (p ^ " non-empty") true (size > 0))
    written;
  List.iter Sys.remove written;
  Unix.rmdir dir

let test_man_smoke () =
  let cmd = Command.make ~name:"go" ~short:"go doc" ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~short:"app doc" ~subcommands:[ cmd ] () in
  let buf = Buffer.create 256 in
  let fmt = Format.formatter_of_buffer buf in
  Man.emit ~out:fmt ~program_version:"1.0" ~command_path:[ "app" ] ~command:root;
  Format.pp_print_flush fmt ();
  let s = Buffer.contents buf in
  Alcotest.(check bool) "TH header" true (contains s ".TH \"APP\"")

(* --- runner --- *)

let () =
  Alcotest.run "mamba"
    [
      "suggest", [
        Alcotest.test_case "distance" `Quick test_suggest_distance;
        Alcotest.test_case "closest"  `Quick test_suggest_closest;
      ];
      "flag-args", [
        Alcotest.test_case "typed lookup"     `Quick test_flag_typed_lookup;
        Alcotest.test_case "short cluster"    `Quick test_short_cluster;
        Alcotest.test_case "count flag"       `Quick test_count_flag;
        Alcotest.test_case "env fallback"     `Quick test_env_fallback;
        Alcotest.test_case "required missing" `Quick test_required_flag_missing;
        Alcotest.test_case "raw passthrough"  `Quick test_raw_passthrough;
        Alcotest.test_case "-- satisfies Arg.exactly N"
          `Quick test_dash_dash_satisfies_arg_validator;
        Alcotest.test_case "empty list default not rendered"
          `Quick test_empty_list_default_not_rendered;
        Alcotest.test_case "arg validator"    `Quick test_arg_validator;
      ];
      "multi-flags", [
        Alcotest.test_case "list: one occurrence"    `Quick test_list_single_occurrence;
        Alcotest.test_case "list: many occurrences"  `Quick test_list_multiple_occurrences;
        Alcotest.test_case "repeated: single"        `Quick test_repeated_single;
        Alcotest.test_case "repeated: multiple"      `Quick test_repeated_multiple;
        Alcotest.test_case "repeated: default empty" `Quick test_repeated_default_when_absent;
        Alcotest.test_case "repeated: int"           `Quick test_repeated_of_int;
      ];
      "hidden-flags", [
        Alcotest.test_case "parses normally"          `Quick test_hidden_flag_parses;
        Alcotest.test_case "omitted from help"        `Quick test_hidden_flag_omitted_from_help;
        Alcotest.test_case "omitted from completion"  `Quick test_hidden_flag_omitted_from_completion;
      ];
      "help-visibility", [
        Alcotest.test_case "-h/--help + --version listed"
          `Quick test_help_flag_listed_in_help;
        Alcotest.test_case "--version hidden when version unset"
          `Quick test_version_flag_hidden_when_version_unset;
      ];
      "version-subcommand", [
        Alcotest.test_case "auto-injected when version is set"
          `Quick test_version_subcommand_auto_injected;
        Alcotest.test_case "skipped when user declares one"
          `Quick test_version_subcommand_skipped_when_user_declared;
      ];
      "arg-named", [
        Alcotest.test_case "describe + arity"
          `Quick test_arg_named_describe_and_arity;
        Alcotest.test_case "named2 typed pairing"
          `Quick test_arg_named2_typed_pairing;
      ];
      "arg-variadic", [
        Alcotest.test_case "default min=1, usage and validation"
          `Quick test_arg_variadic_default_min_1;
        Alcotest.test_case "min=0 accepts zero"
          `Quick test_arg_variadic_min_zero;
      ];
      "smart-default", [
        Alcotest.test_case "leaf + flags + run rejects strays"
          `Quick test_smart_default_rejects_strays;
      ];
      "positional-helpers", [
        Alcotest.test_case "positional_2 tuple"     `Quick test_positional_2;
        Alcotest.test_case "positional_at index 1"  `Quick test_positional_at;
      ];
      "flag-group-validation", [
        Alcotest.test_case "_by_name typo rejected at Program.make"
          `Quick test_flag_group_typo_rejected;
        Alcotest.test_case "typed constructor accepts real flags"
          `Quick test_flag_group_typed_accepts;
        Alcotest.test_case "constraint annotation appears in --help"
          `Quick test_flag_group_annotations_in_help;
      ];
      "missing-flag", [
        Alcotest.test_case "friendly error, not Not_found"
          `Quick test_missing_flag_friendly_error;
      ];
      "deprecated-flags", [
        Alcotest.test_case "parses normally"
          `Quick test_deprecated_flag_parses;
        Alcotest.test_case "warns when set"
          `Quick test_deprecated_flag_warns_when_set;
        Alcotest.test_case "silent when unset"
          `Quick test_deprecated_flag_silent_when_unset;
        Alcotest.test_case "hidden from help"
          `Quick test_deprecated_flag_hidden_from_help;
      ];
      "enum-path", [
        Alcotest.test_case "enum: accepts valid"
          `Quick test_enum_accepts_valid;
        Alcotest.test_case "enum: rejects invalid"
          `Quick test_enum_rejects_invalid;
        Alcotest.test_case "path: must_exist when present"
          `Quick test_path_must_exist_passes_when_present;
        Alcotest.test_case "path: must_exist when missing"
          `Quick test_path_must_exist_fails_when_missing;
      ];
      "lifecycle", [
        Alcotest.test_case "hook order"     `Quick test_lifecycle_order;
        Alcotest.test_case "short-circuit"  `Quick test_lifecycle_short_circuit;
      ];
      "program", [
        Alcotest.test_case "did you mean" `Quick test_did_you_mean;
      ];
      "completion", [
        Alcotest.test_case "bash smoke" `Quick test_completion_smoke;
      ];
      "man", [
        Alcotest.test_case "groff smoke" `Quick test_man_smoke;
        Alcotest.test_case "write_all to dir"
          `Quick test_man_write_all;
      ];
    ]
