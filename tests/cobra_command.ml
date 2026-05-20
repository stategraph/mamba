(** Port of selected tests from Cobra's command_test.go (spf13/cobra v1.10.2).

    Scope (~35 of 125): dispatch, aliases, flags, persistent flags, required
    flags, help/--help, --version, deprecated, hidden, hooks, Find-equivalent
    dispatch, unknown-flag positions.

    Tests intentionally omitted (Cobra-only features / divergent semantics):
      - TestExecuteContext{,C} / TestExecute_NoContext / TestSetContext*
          Go context.Context; not modeled in mamba.
      - TestEnablePrefixMatching / TestAliasPrefixMatching
          Cobra-only feature.
      - TestPlugin{,WithSubCommands} / TestVersionFlagExecutedDiplayName /
        TestVersionFlagExecutedWithNoName
          Annotations / CommandDisplayNameAnnotation not in mamba.
      - TestStripFlags
          Tests Cobra's internal stripFlags helper.
      - TestDisableFlagParsing / TestPersistentRequiredFlagsWithDisableFlagParsing
          mamba has no DisableFlagParsing toggle.
      - TestChildFlagShadowsParentPersistentFlag
          Tests InheritedFlags/LocalFlags inspection API (not in mamba).
      - TestInitHelpFlagMergesFlags / TestSetHelpCommand / TestSetHelpTemplate /
        TestSetUsageTemplate / TestVersionTemplate / TestShorthandVersionTemplate /
        TestHelpFuncExecuted
          Template / SetHelp* APIs not in mamba (fixed renderer).
      - TestHelpCommandExecutedOnChildWithFlagThatShadowsParentFlag /
        TestHelpFlagInHelp / TestFlagsInUsage
          Exact help-text byte comparison; mamba's renderer phrases differently.
      - TestRootErrPrefixExecutedOnSubcommand / TestRootAndSubErrPrefix
          SetErrPrefix not in mamba.
      - TestVersionFlagOnlyAddedToRoot / TestShortVersionFlagOnlyAddedToRoot /
        TestShorthandVersionFlag*
          --version is handled at any level in mamba (intentional simplification).
          -v as version shorthand is not bound in mamba.
      - TestUsageIsNotPrintedTwice / TestUsageStringRedirected /
        TestCommandPrintRedirection / TestSetOut{,put} / TestSetErr / TestSetIn
          Internal printer plumbing not exposed by mamba.
      - TestVisitParents / TestUpdateName / TestCalledAs / TestRemoveCommand /
        TestReplaceCommandWithRemove
          mamba's Command.t is immutable; no mutator API.
      - TestSuggestions
          Existing test_did_you_mean covers the basic case;
          SuggestFor / DisableSuggestions not in mamba.
      - TestCaseInsensitive / TestCaseSensitivityBackwardCompatibility
          EnableCaseInsensitive not in mamba.
      - TestGlobalNormFuncPropagation / TestNormPassedOn* /
        TestConsistentNormalizedName / TestFlagOnPflagCommandLine
          pflag normalization APIs not in mamba.
      - TestCommandsAreSorted / TestEnableCommandSortingIsDisabled
          Internal sort behavior; mamba preserves insertion order.
      - TestUsageWithGroup / TestUsageHelpGroup / TestUsageCompletionGroup /
        TestUngroupedCommand / TestAddGroup / TestWrongGroup*
          Cobra Group / GroupID feature not in mamba.
      - TestFlagErrorFunc{,Help} / TestSortedFlags / TestMergeCommandLineToFlags /
        TestUseDeprecatedFlags
          pflag-level features not in mamba.
      - TestTraverse* (5)
          Cobra Traverse() method; not in mamba.
      - TestFParseErrWhitelist*
          FParseErrWhitelist not in mamba.
      - TestNoRootRunCommandExecuted{With,Without}VersionSet /
        TestHelpCommandExecuted{With,Without}VersionSet /
        TestHelpflagCommandExecuted{With,Without}VersionSet
          Permutations of help+version that depend on exact help text content.
      (TestFlagBeforeCommand is ported below as Misc tests. Bundled AND
       unbundled forms are supported via parser.ml:prescan -- the prescan
       consults a whole-tree fallback table to know whether -i takes a
       value when -i is declared on a not-yet-descended descendant.)
      - TestRootUnknownCommandSilenced
          SilenceErrors/SilenceUsage not in mamba.

    Assertions are behavioral: exit code 0/2 and run-callback side effects.
    Error-message wording is not asserted. *)

open Mamba
open Test_util

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let exec ?(version = "0") ?(help_command = false) ?(completion_command = false)
    (root : Command.t) (args : string list) =
  let out_buf = Buffer.create 64 in
  let err_buf = Buffer.create 64 in
  let prog =
    Program.make ~name:root.name ~version ~root
      ~help_command ~completion_command
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  let argv = Array.of_list (root.name :: args) in
  let code = Program.run prog ~argv in
  let () = Format.pp_print_flush (Format.formatter_of_buffer out_buf) () in
  let () = Format.pp_print_flush (Format.formatter_of_buffer err_buf) () in
  (code, Buffer.contents out_buf, Buffer.contents err_buf)

let dispatch_one (root : Command.t) (args : string list) =
  let prog =
    Program.make ~name:root.name ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer (Buffer.create 16))
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  Program.dispatch prog ~argv:(Array.of_list (root.name :: args))

let capture_run () =
  let captured = ref [] in
  let run args = captured := Args.positional args; Error.success in
  (captured, run)

let success_code code = Alcotest.(check int) "exit 0" Error.success code
let failure_code code = Alcotest.(check int) "exit 2" Error.parse_error code
let check_args expected actual =
  Alcotest.(check (list string)) "captured args" expected actual
let check_contains label out needle =
  Alcotest.(check bool) (label ^ ": contains " ^ needle) true (contains out needle)

(* ------------------------------------------------------------------ *)
(* Dispatch basics                                                     *)
(* ------------------------------------------------------------------ *)

let single_command () =
  let captured, run = capture_run () in
  let a = Command.make ~name:"a" ~args:Arg.none ~run:(fun _ -> 0) () in
  let b = Command.make ~name:"b" ~args:Arg.none ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"root" ~args:(Arg.exactly 2) ~run
      ~subcommands:[ a; b ] ()
  in
  let code, _, _ = exec root [ "one"; "two" ] in
  success_code code;
  check_args [ "one"; "two" ] !captured

let child_command () =
  let captured, run = capture_run () in
  let child1 =
    Command.make ~name:"child1" ~args:(Arg.exactly 2) ~run ()
  in
  let child2 = Command.make ~name:"child2" ~args:Arg.none ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"root" ~args:Arg.none ~run:(fun _ -> 0)
      ~subcommands:[ child1; child2 ] ()
  in
  let code, _, _ = exec root [ "child1"; "one"; "two" ] in
  success_code code;
  check_args [ "one"; "two" ] !captured

let call_command_without_subcommands () =
  let root = Command.make ~name:"root" ~args:Arg.none ~run:(fun _ -> 0) () in
  let code, _, _ = exec root [] in
  success_code code

(* Mamba's default Args is [Arg.any] (Cobra's default legacyArgs would reject
   here). We use [Arg.none] to preserve the assertion intent. *)
let root_execute_unknown_command () =
  let child = Command.make ~name:"child" ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"root" ~args:Arg.none ~run:(fun _ -> 0)
      ~subcommands:[ child ] ()
  in
  let code, _, _ = exec root [ "unknown" ] in
  failure_code code

let empty_inputs () =
  let captured, run = capture_run () in
  let intf = Flag.int ~name:"intf" ~short:'i' ~default:(-1) ~doc:"" () in
  (* This test deliberately passes empty-string positionals; the command
     must accept them, so we override the smart default. *)
  let root =
    Command.make ~name:"c" ~args:Arg.any ~flags:[ Flag.pack intf ] ~run ()
  in
  let code, _, _ = exec root [ ""; "-i7"; "" ] in
  success_code code;
  (* mamba treats "" as a positional Word; Cobra does too. *)
  ignore captured

(* ------------------------------------------------------------------ *)
(* Aliases / same-name                                                 *)
(* ------------------------------------------------------------------ *)

let command_alias () =
  let captured, run = capture_run () in
  let times = Command.make ~name:"times" ~args:(Arg.exactly 2) ~run () in
  let echo =
    Command.make ~name:"echo" ~aliases:[ "say"; "tell" ]
      ~args:Arg.none ~run:(fun _ -> 0) ~subcommands:[ times ] ()
  in
  let root =
    Command.make ~name:"root" ~args:Arg.none ~run:(fun _ -> 0)
      ~subcommands:[ echo ] ()
  in
  let code, _, _ = exec root [ "tell"; "times"; "one"; "two" ] in
  success_code code;
  check_args [ "one"; "two" ] !captured

let child_same_name () =
  let captured, run = capture_run () in
  let foo_child = Command.make ~name:"foo" ~args:(Arg.exactly 2) ~run () in
  let bar = Command.make ~name:"bar" ~args:Arg.none ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"foo" ~args:Arg.none ~run:(fun _ -> 0)
      ~subcommands:[ foo_child; bar ] ()
  in
  let code, _, _ = exec root [ "foo"; "one"; "two" ] in
  success_code code;
  check_args [ "one"; "two" ] !captured

let grandchild_same_name () =
  let captured, run = capture_run () in
  let foo_grand = Command.make ~name:"foo" ~args:(Arg.exactly 2) ~run () in
  let bar =
    Command.make ~name:"bar" ~args:Arg.none ~run:(fun _ -> 0)
      ~subcommands:[ foo_grand ] ()
  in
  let root =
    Command.make ~name:"foo" ~args:Arg.none ~run:(fun _ -> 0)
      ~subcommands:[ bar ] ()
  in
  let code, _, _ = exec root [ "bar"; "foo"; "one"; "two" ] in
  success_code code;
  check_args [ "one"; "two" ] !captured

(* ------------------------------------------------------------------ *)
(* Flags                                                               *)
(* ------------------------------------------------------------------ *)

let flag_long () =
  let captured = ref [] in
  let raw_capt = ref [] in
  let intf = Flag.int ~name:"intf" ~default:(-1) ~doc:"" () in
  let sf   = Flag.string ~name:"sf" ~default:"" ~doc:"" () in
  let int_val = ref 0 and str_val = ref "" in
  let run args =
    int_val := Args.get args intf;
    str_val := Args.get args sf;
    captured := Args.positional args;
    raw_capt := Args.raw args;
    Error.success
  in
  let root =
    Command.make ~name:"c" ~args:Arg.any
      ~flags:[ Flag.pack intf; Flag.pack sf ] ~run ()
  in
  let code, _, _ = exec root [ "--intf=7"; "--sf=abc"; "one"; "--"; "two" ] in
  success_code code;
  Alcotest.(check int) "intf" 7 !int_val;
  Alcotest.(check string) "sf" "abc" !str_val;
  (* POSIX: tokens after "--" are positional. Match Cobra's behaviour
     (which the original port had asserted incorrectly against mamba's
     pre-fix raw-only behaviour). *)
  check_args [ "one"; "two" ] !captured;
  Alcotest.(check (list string)) "raw" [ "two" ] !raw_capt

let flag_short () =
  let captured = ref [] in
  let intf = Flag.int ~name:"intf" ~short:'i' ~default:(-1) ~doc:"" () in
  let sf   = Flag.string ~name:"sf" ~short:'s' ~default:"" ~doc:"" () in
  let int_val = ref 0 and str_val = ref "" in
  let run args =
    int_val := Args.get args intf;
    str_val := Args.get args sf;
    captured := Args.positional args;
    Error.success
  in
  let root =
    Command.make ~name:"c" ~args:Arg.any
      ~flags:[ Flag.pack intf; Flag.pack sf ] ~run ()
  in
  let code, _, _ = exec root [ "-i"; "7"; "-sabc"; "one"; "two" ] in
  success_code code;
  Alcotest.(check int) "intf" 7 !int_val;
  Alcotest.(check string) "sf" "abc" !str_val;
  check_args [ "one"; "two" ] !captured

let child_flag () =
  let intf = Flag.int ~name:"intf" ~short:'i' ~default:(-1) ~doc:"" () in
  let int_val = ref 0 in
  let run args = int_val := Args.get args intf; Error.success in
  let child =
    Command.make ~name:"child" ~flags:[ Flag.pack intf ] ~run ()
  in
  let root =
    Command.make ~name:"root" ~run:(fun _ -> 0) ~subcommands:[ child ] ()
  in
  let code, _, _ = exec root [ "child"; "-i7" ] in
  success_code code;
  Alcotest.(check int) "intf" 7 !int_val

(* Parent's *local* flag must NOT inherit to child. Calling child with parent's
   -s should fail. *)
let child_flag_with_parent_local_flag () =
  let intf = Flag.int ~name:"intf" ~short:'i' ~default:(-1) ~doc:"" () in
  let sf   = Flag.string ~name:"sf" ~short:'s' ~default:"" ~doc:"" () in
  let child = Command.make ~name:"child" ~flags:[ Flag.pack intf ] ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"root" ~flags:[ Flag.pack sf ] ~run:(fun _ -> 0)
      ~subcommands:[ child ] ()
  in
  let code, _, _ = exec root [ "child"; "-i7"; "-sabc" ] in
  failure_code code

let flag_invalid_input () =
  let intf = Flag.int ~name:"intf" ~short:'i' ~default:(-1) ~doc:"" () in
  let root = Command.make ~name:"root" ~flags:[ Flag.pack intf ] ~run:(fun _ -> 0) () in
  let code, _, _ = exec root [ "-iabc" ] in
  failure_code code

(* ------------------------------------------------------------------ *)
(* Persistent flags                                                    *)
(* ------------------------------------------------------------------ *)

let persistent_flags_on_same_command () =
  let captured = ref [] in
  let intf = Flag.int ~name:"intf" ~short:'i' ~default:(-1) ~doc:"" () in
  let int_val = ref 0 in
  let run args =
    int_val := Args.get args intf;
    captured := Args.positional args;
    Error.success
  in
  let root =
    Command.make ~name:"root" ~args:Arg.any
      ~persistent_flags:[ Flag.pack intf ] ~run ()
  in
  let code, _, _ = exec root [ "-i7"; "one"; "two" ] in
  success_code code;
  Alcotest.(check int) "intf" 7 !int_val;
  check_args [ "one"; "two" ] !captured

let persistent_flags_on_child () =
  let captured = ref [] in
  let parentf = Flag.int ~name:"parentf" ~short:'p' ~default:(-1) ~doc:"" () in
  let childf  = Flag.int ~name:"childf"  ~short:'c' ~default:(-1) ~doc:"" () in
  let p_val = ref 0 and c_val = ref 0 in
  let run args =
    p_val := Args.get args parentf;
    c_val := Args.get args childf;
    captured := Args.positional args;
    Error.success
  in
  let child =
    Command.make ~name:"child" ~args:Arg.any
      ~flags:[ Flag.pack childf ] ~run ()
  in
  let root =
    Command.make ~name:"root" ~run:(fun _ -> 0)
      ~persistent_flags:[ Flag.pack parentf ]
      ~subcommands:[ child ] ()
  in
  let code, _, _ = exec root [ "child"; "-c7"; "-p8"; "one"; "two" ] in
  success_code code;
  Alcotest.(check int) "parentf" 8 !p_val;
  Alcotest.(check int) "childf"  7 !c_val;
  check_args [ "one"; "two" ] !captured

(* ------------------------------------------------------------------ *)
(* Required flags                                                      *)
(* ------------------------------------------------------------------ *)

let required_flags () =
  let foo1 = Flag.string ~name:"foo1" ~required:true ~doc:"" () in
  let foo2 = Flag.string ~name:"foo2" ~required:true ~doc:"" () in
  let bar  = Flag.string ~name:"bar"  ~doc:"" () in
  let root =
    Command.make ~name:"c" ~run:(fun _ -> 0)
      ~flags:[ Flag.pack foo1; Flag.pack foo2; Flag.pack bar ] ()
  in
  let code, _, _ = exec root [] in
  failure_code code

let persistent_required_flags () =
  let foo1 = Flag.string ~name:"foo1" ~required:true ~doc:"" () in
  let foo2 = Flag.string ~name:"foo2" ~required:true ~doc:"" () in
  let foo3 = Flag.string ~name:"foo3" ~doc:"" () in
  let bar1 = Flag.string ~name:"bar1" ~required:true ~doc:"" () in
  let bar2 = Flag.string ~name:"bar2" ~required:true ~doc:"" () in
  let bar3 = Flag.string ~name:"bar3" ~doc:"" () in
  let child =
    Command.make ~name:"child" ~run:(fun _ -> 0)
      ~flags:[ Flag.pack bar1; Flag.pack bar2; Flag.pack bar3 ] ()
  in
  let root =
    Command.make ~name:"parent" ~run:(fun _ -> 0)
      ~persistent_flags:[ Flag.pack foo1; Flag.pack foo2 ]
      ~flags:[ Flag.pack foo3 ]
      ~subcommands:[ child ] ()
  in
  let code, _, _ = exec root [ "child" ] in
  failure_code code

(* ------------------------------------------------------------------ *)
(* Help, version                                                       *)
(* ------------------------------------------------------------------ *)

let help_command_executed () =
  let child = Command.make ~name:"child" ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"root" ~long:"Long description" ~run:(fun _ -> 0)
      ~subcommands:[ child ] ()
  in
  let code, out, _ = exec ~help_command:true root [ "help" ] in
  success_code code;
  check_contains "help output" out "Long description"

let help_command_executed_on_child () =
  let child =
    Command.make ~name:"child" ~long:"Long description" ~run:(fun _ -> 0) ()
  in
  let root =
    Command.make ~name:"root" ~run:(fun _ -> 0) ~subcommands:[ child ] ()
  in
  let code, out, _ = exec ~help_command:true root [ "help"; "child" ] in
  success_code code;
  check_contains "child help" out "Long description"

let help_flag_executed () =
  let root =
    Command.make ~name:"root" ~long:"Long description" ~run:(fun _ -> 0) ()
  in
  let code, out, _ = exec root [ "--help" ] in
  success_code code;
  check_contains "--help output" out "Long description"

let help_flag_executed_on_child () =
  let child =
    Command.make ~name:"child" ~long:"Long description" ~run:(fun _ -> 0) ()
  in
  let root =
    Command.make ~name:"root" ~run:(fun _ -> 0) ~subcommands:[ child ] ()
  in
  let code, out, _ = exec root [ "child"; "--help" ] in
  success_code code;
  check_contains "child --help" out "Long description"

let help_executed_on_non_runnable_child () =
  let child = Command.make ~name:"child" ~long:"Long description" () in
  let root =
    Command.make ~name:"root" ~run:(fun _ -> 0) ~subcommands:[ child ] ()
  in
  let code, out, _ = exec root [ "child" ] in
  success_code code;
  check_contains "non-runnable child" out "Long description"

let version_flag_executed () =
  let root = Command.make ~name:"root" ~run:(fun _ -> 0) () in
  let code, out, _ = exec ~version:"1.0.0" root [ "--version"; "arg1" ] in
  success_code code;
  check_contains "version output" out "root version 1.0.0"

(* ------------------------------------------------------------------ *)
(* Deprecated, hidden                                                  *)
(* ------------------------------------------------------------------ *)

let deprecated_command () =
  let dep =
    Command.make ~name:"deprecated"
      ~deprecated:"This command is deprecated" ~run:(fun _ -> 0) ()
  in
  let root =
    Command.make ~name:"root" ~run:(fun _ -> 0) ~subcommands:[ dep ] ()
  in
  let code, _, err = exec root [ "deprecated" ] in
  success_code code;
  check_contains "deprecation notice" err "deprecated"

let hidden_command_executes () =
  let executed = ref false in
  let root =
    Command.make ~name:"c" ~hidden:true ~run:(fun _ -> executed := true; 0) ()
  in
  let code, _, _ = exec root [] in
  success_code code;
  Alcotest.(check bool) "ran" true !executed

(* ------------------------------------------------------------------ *)
(* Hooks                                                               *)
(* ------------------------------------------------------------------ *)

let hooks_all_five () =
  let log = ref [] in
  let push tag args =
    log := (tag, Args.positional args) :: !log;
    None
  in
  let run args =
    log := ("run", Args.positional args) :: !log;
    Error.success
  in
  let root =
    Command.make ~name:"c" ~args:Arg.any
      ~persistent_pre_run:(push "pers_pre")
      ~pre_run:(push "pre")
      ~run
      ~post_run:(push "post")
      ~persistent_post_run:(push "pers_post")
      ()
  in
  let code, _, _ = exec root [ "one"; "two" ] in
  success_code code;
  let seq = List.rev !log in
  let tags = List.map fst seq in
  let argvs = List.map snd seq in
  Alcotest.(check (list string)) "hook order"
    [ "pers_pre"; "pre"; "run"; "post"; "pers_post" ] tags;
  List.iter
    (fun a -> Alcotest.(check (list string)) "each hook sees args"
        [ "one"; "two" ] a)
    argvs

(* Cobra testPersistentHooks: when invoking a child, parent's persistent
   hooks bracket the call but parent's pre/run/post do NOT fire.
   mamba's lifecycle implements this directly (no Cobra-style
   EnableTraverseRunHooks toggle). *)
let persistent_hooks_traversal () =
  let log = ref [] in
  let push tag _args = log := tag :: !log; None in
  let push_run tag _args = log := tag :: !log; Error.success in
  let child =
    Command.make ~name:"child" ~args:Arg.any
      ~persistent_pre_run:(push "child PersistentPreRun")
      ~pre_run:(push "child PreRun")
      ~run:(push_run "child Run")
      ~post_run:(push "child PostRun")
      ~persistent_post_run:(push "child PersistentPostRun")
      ()
  in
  let parent =
    Command.make ~name:"parent" ~args:Arg.any
      ~persistent_pre_run:(push "parent PersistentPreRun")
      ~pre_run:(push "parent PreRun")
      ~run:(push_run "parent Run")
      ~post_run:(push "parent PostRun")
      ~persistent_post_run:(push "parent PersistentPostRun")
      ~subcommands:[ child ] ()
  in
  let code, _, _ = exec parent [ "child"; "one"; "two" ] in
  success_code code;
  Alcotest.(check (list string)) "hook order"
    [ "parent PersistentPreRun"
    ; "child PersistentPreRun"
    ; "child PreRun"
    ; "child Run"
    ; "child PostRun"
    ; "child PersistentPostRun"
    ; "parent PersistentPostRun"
    ]
    (List.rev !log)

(* Cobra TestSubcommandExecuteC: ExecuteC returns the dispatched command.
   mamba's Program.dispatch returns Run { command; ... }. *)
let subcommand_execute_c () =
  let child = Command.make ~name:"child" ~run:(fun _ -> 0) () in
  let root  =
    Command.make ~name:"root" ~run:(fun _ -> 0) ~subcommands:[ child ] ()
  in
  match dispatch_one root [ "child" ] with
  | Run { command; _ } ->
    Alcotest.(check string) "dispatched cmd" "child" command.name
  | _ -> Alcotest.fail "expected Run"

(* Cobra TestFlagBeforeCommand: a child's flag placed before the child's
   name in argv is accepted because Cobra pre-scans argv to find the
   subcommand path before attributing flags. Mamba now does the same
   (parser.ml:prescan) for the bundled forms ("-i7" / "--intf=8") that
   Cobra tests; the unbundled "-i 7 child" form is still unsupported
   (separate divergence test below). *)
let flag_before_command_short_bundled () =
  let intf = Flag.int ~name:"intf" ~short:'i' ~default:(-1) ~doc:"" () in
  let int_val = ref 0 in
  let run args = int_val := Args.get args intf; Error.success in
  let child = Command.make ~name:"child" ~flags:[ Flag.pack intf ] ~run () in
  let root =
    Command.make ~name:"root" ~run:(fun _ -> 0) ~subcommands:[ child ] ()
  in
  let code, _, _ = exec root [ "-i7"; "child" ] in
  success_code code;
  Alcotest.(check int) "intf" 7 !int_val

let flag_before_command_long_bundled () =
  let intf = Flag.int ~name:"intf" ~default:(-1) ~doc:"" () in
  let int_val = ref 0 in
  let run args = int_val := Args.get args intf; Error.success in
  let child = Command.make ~name:"child" ~flags:[ Flag.pack intf ] ~run () in
  let root =
    Command.make ~name:"root" ~run:(fun _ -> 0) ~subcommands:[ child ] ()
  in
  let code, _, _ = exec root [ "--intf=8"; "child" ] in
  success_code code;
  Alcotest.(check int) "intf" 8 !int_val

(* Unbundled case ["-i"; "7"; "child"]: mamba's prescan consults a
   whole-tree fallback table to determine that -i takes a value (it's
   declared on the child), so the prescan correctly skips two tokens
   and then descends into child. *)
let flag_before_command_unbundled () =
  let intf = Flag.int ~name:"intf" ~short:'i' ~default:(-1) ~doc:"" () in
  let int_val = ref 0 in
  let run args = int_val := Args.get args intf; Error.success in
  let child = Command.make ~name:"child" ~flags:[ Flag.pack intf ] ~run () in
  let root =
    Command.make ~name:"root" ~run:(fun _ -> 0) ~subcommands:[ child ] ()
  in
  let code, _, _ = exec root [ "-i"; "7"; "child" ] in
  success_code code;
  Alcotest.(check int) "intf" 7 !int_val

(* ------------------------------------------------------------------ *)
(* Find-equivalent: dispatch resolves to "child" for various argv      *)
(* shapes (Cobra TestFind, behavioral subset).                         *)
(* ------------------------------------------------------------------ *)

let find_root () =
  let foo = Flag.string ~name:"foo" ~short:'f' ~default:""          ~doc:"" () in
  let bar = Flag.string ~name:"bar" ~short:'b' ~default:"something" ~doc:"" () in
  let child = Command.make ~name:"child" ~args:Arg.any ~run:(fun _ -> 0) () in
  Command.make ~name:"root"
    ~persistent_flags:[ Flag.pack foo; Flag.pack bar ]
    ~subcommands:[ child ] ()

let find_resolves_to_child argv () =
  match dispatch_one (find_root ()) argv with
  | Run { command; _ } ->
    Alcotest.(check string) "leaf cmd name" "child" command.name
  | _ -> Alcotest.fail "expected Run"

(* ------------------------------------------------------------------ *)
(* Unknown flag at any position should fail                            *)
(* ------------------------------------------------------------------ *)

let unknown_flag_root () =
  let namespace = Flag.string ~name:"namespace" ~default:"" ~doc:"" () in
  let bar       = Flag.bool   ~name:"bar"       ~default:false ~doc:"" () in
  let child =
    Command.make ~name:"child" ~flags:[ Flag.pack bar ] ~run:(fun _ -> 0) ()
  in
  Command.make ~name:"root" ~run:(fun _ -> 0)
    ~persistent_flags:[ Flag.pack namespace ]
    ~subcommands:[ child ] ()

let unknown_flag_fails argv () =
  let code, _, _ = exec (unknown_flag_root ()) argv in
  failure_code code

(* ------------------------------------------------------------------ *)
(* Runner                                                              *)
(* ------------------------------------------------------------------ *)

let tc name f = Alcotest.test_case name `Quick f

let () =
  Alcotest.run "cobra_command"
    [
      "Dispatch",
      [ tc "SingleCommand"                 single_command
      ; tc "ChildCommand"                  child_command
      ; tc "CallCommandWithoutSubcommands" call_command_without_subcommands
      ; tc "RootExecuteUnknownCommand"     root_execute_unknown_command
      ; tc "EmptyInputs"                   empty_inputs
      ];
      "Aliases",
      [ tc "CommandAlias"     command_alias
      ; tc "ChildSameName"    child_same_name
      ; tc "GrandChildSameName" grandchild_same_name
      ];
      "Flags",
      [ tc "FlagLong"                    flag_long
      ; tc "FlagShort"                   flag_short
      ; tc "ChildFlag"                   child_flag
      ; tc "ChildFlagWithParentLocalFlag" child_flag_with_parent_local_flag
      ; tc "FlagInvalidInput"            flag_invalid_input
      ];
      "PersistentFlags",
      [ tc "PersistentFlagsOnSameCommand" persistent_flags_on_same_command
      ; tc "PersistentFlagsOnChild"       persistent_flags_on_child
      ];
      "Required",
      [ tc "RequiredFlags"           required_flags
      ; tc "PersistentRequiredFlags" persistent_required_flags
      ];
      "HelpVersion",
      [ tc "HelpCommandExecuted"            help_command_executed
      ; tc "HelpCommandExecutedOnChild"     help_command_executed_on_child
      ; tc "HelpFlagExecuted"               help_flag_executed
      ; tc "HelpFlagExecutedOnChild"        help_flag_executed_on_child
      ; tc "HelpExecutedOnNonRunnableChild" help_executed_on_non_runnable_child
      ; tc "VersionFlagExecuted"            version_flag_executed
      ];
      "DeprecatedHidden",
      [ tc "DeprecatedCommand"      deprecated_command
      ; tc "HiddenCommandExecutes"  hidden_command_executes
      ];
      "Hooks",
      [ tc "Hooks_all_five"             hooks_all_five
      ; tc "PersistentHooks_traversal"  persistent_hooks_traversal
      ];
      "Misc",
      [ tc "SubcommandExecuteC"                       subcommand_execute_c
      ; tc "FlagBeforeCommand short bundled (-i7)"    flag_before_command_short_bundled
      ; tc "FlagBeforeCommand long bundled (--intf=)" flag_before_command_long_bundled
      ; tc "FlagBeforeCommand unbundled (-i 7 child)" flag_before_command_unbundled
      ];
      "Find",
      [ tc {|["child"]|}
          (find_resolves_to_child [ "child" ])
      ; tc {|["child"; "child"]|}
          (find_resolves_to_child [ "child"; "child" ])
      ; tc {|["-f"; "child"; "child"]|}
          (find_resolves_to_child [ "-f"; "child"; "child" ])
      ; tc {|["child"; "-f"; "child"]|}
          (find_resolves_to_child [ "child"; "-f"; "child" ])
      ; tc {|["-b"; "child"; "child"]|}
          (find_resolves_to_child [ "-b"; "child"; "child" ])
      ; tc {|["child"; "-b"; "child"]|}
          (find_resolves_to_child [ "child"; "-b"; "child" ])
      ; tc {|["-b"; "-f"; "child"; "child"]|}
          (find_resolves_to_child [ "-b"; "-f"; "child"; "child" ])
      ; tc {|["-f"; "child"; "-b"; "something"; "child"]|}
          (find_resolves_to_child
             [ "-f"; "child"; "-b"; "something"; "child" ])
      ; tc {|["-f=child"; "-b=something"; "child"]|}
          (find_resolves_to_child [ "-f=child"; "-b=something"; "child" ])
      ; tc {|["--foo"; "child"; "--bar"; "something"; "child"]|}
          (find_resolves_to_child
             [ "--foo"; "child"; "--bar"; "something"; "child" ])
      ];
      "UnknownFlag",
      [ tc {|--namespace foo --unknown child --bar|}
          (unknown_flag_fails
             [ "--namespace"; "foo"; "--unknown"; "child"; "--bar" ])
      ; tc {|--namespace foo child --unknown --bar|}
          (unknown_flag_fails
             [ "--namespace"; "foo"; "child"; "--unknown"; "--bar" ])
      ; tc {|--namespace foo child --bar --unknown|}
          (unknown_flag_fails
             [ "--namespace"; "foo"; "child"; "--bar"; "--unknown" ])
      ; tc {|--unknown --namespace=foo child --bar|}
          (unknown_flag_fails
             [ "--unknown"; "--namespace=foo"; "child"; "--bar" ])
      ; tc {|--namespace=foo --unknown child --bar|}
          (unknown_flag_fails
             [ "--namespace=foo"; "--unknown"; "child"; "--bar" ])
      ; tc {|--namespace=foo child --unknown --bar|}
          (unknown_flag_fails
             [ "--namespace=foo"; "child"; "--unknown"; "--bar" ])
      ; tc {|--namespace=foo child --bar --unknown|}
          (unknown_flag_fails
             [ "--namespace=foo"; "child"; "--bar"; "--unknown" ])
      ; tc {|--unknown --namespace=foo child --bar=true|}
          (unknown_flag_fails
             [ "--unknown"; "--namespace=foo"; "child"; "--bar=true" ])
      ; tc {|--namespace=foo --unknown child --bar=true|}
          (unknown_flag_fails
             [ "--namespace=foo"; "--unknown"; "child"; "--bar=true" ])
      ; tc {|--namespace=foo child --unknown --bar=true|}
          (unknown_flag_fails
             [ "--namespace=foo"; "child"; "--unknown"; "--bar=true" ])
      ; tc {|--namespace=foo child --bar=true --unknown|}
          (unknown_flag_fails
             [ "--namespace=foo"; "child"; "--bar=true"; "--unknown" ])
      ];
    ]
