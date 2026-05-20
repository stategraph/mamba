(** Port of Cobra's TestCaseInsensitive (command_test.go).

    Cobra has a global [EnableCaseInsensitive] toggle that makes subcommand
    name AND alias matching case-insensitive. Mamba exposes this as
    [?case_insensitive] on [Program.make] (default [false]).

    The Cobra test sweeps 15 argv shapes against both modes; mamba mirrors
    that. Each argv yields one mamba test that runs it both ways and checks
    the expected exit code. *)

open Mamba

(* Cobra's test uses [Run: emptyRun] on each command and relies on the
   default legacyArgs validator to reject unknown words when subcommands
   exist. Mamba's default [Arg.any] doesn't reject; we use [Arg.none] on
   root + child (which both have subcommands) to preserve the assertion
   that wrong-case names ARE rejected in case-sensitive mode. *)
let mk_tree () =
  let grandchild =
    Command.make ~name:"GRANDCHILD" ~aliases:[ "ALIAS" ] ~args:Arg.any
      ~run:(fun _ -> 0) ()
  in
  let child =
    Command.make ~name:"child" ~aliases:[ "alternative" ] ~args:Arg.none
      ~run:(fun _ -> 0) ~subcommands:[ grandchild ] ()
  in
  Command.make ~name:"root" ~args:Arg.none
    ~run:(fun _ -> 0) ~subcommands:[ child ] ()

let run_argv ~case_insensitive argv =
  let root = mk_tree () in
  let out_buf = Buffer.create 64 in
  let err_buf = Buffer.create 64 in
  let prog =
    Program.make ~name:"root" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~case_insensitive
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  Program.run prog ~argv:(Array.of_list ("root" :: argv))

(* Asserts argv succeeds under [case_insensitive=true] and matches
   expectation under [case_insensitive=false]. *)
let case argv ~fails_when_sensitive () =
  let ci_code = run_argv ~case_insensitive:true  argv in
  let cs_code = run_argv ~case_insensitive:false argv in
  Alcotest.(check int) "case_insensitive=true succeeds"
    Error.success ci_code;
  if fails_when_sensitive then
    Alcotest.(check int) "case_insensitive=false fails"
      Error.parse_error cs_code
  else
    Alcotest.(check int) "case_insensitive=false succeeds"
      Error.success cs_code

let tc name f = Alcotest.test_case name `Quick f

(* Regression: case-insensitive matching must let the leaf's [~run]
   actually fire. A previous bug had the parser correctly descending
   case-insensitively but [Program.build_path_commands] re-walking the
   tree case-sensitively, so the leaf was lost and run silently exited 0. *)
let case_insensitive_run_actually_fires () =
  let ran = ref false in
  let leaf =
    Command.make ~name:"build" ~run:(fun _ -> ran := true; 0) ()
  in
  let root = Command.make ~name:"app" ~subcommands:[ leaf ] () in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false ~version_command:false
      ~case_insensitive:true
      ~out:(Format.formatter_of_buffer (Buffer.create 16))
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let code = Program.run prog ~argv:[| "app"; "BUILD" |] in
  Alcotest.(check int)  "exit 0"      0    code;
  Alcotest.(check bool) "run fired"   true !ran

let () =
  Alcotest.run "cobra_case_insensitive"
    [
      "Exact names (sensitive ok)",
      [ tc "[child]"               (case [ "child" ]                ~fails_when_sensitive:false)
      ; tc "[alternative]"         (case [ "alternative" ]          ~fails_when_sensitive:false)
      ; tc "[child;GRANDCHILD]"    (case [ "child"; "GRANDCHILD" ]  ~fails_when_sensitive:false)
      ; tc "[alternative;ALIAS]"   (case [ "alternative"; "ALIAS" ] ~fails_when_sensitive:false)
      ];
      "Wrong-case child (sensitive fails)",
      [ tc "[CHILD]"              (case [ "CHILD" ]   ~fails_when_sensitive:true)
      ; tc "[chILD]"              (case [ "chILD" ]   ~fails_when_sensitive:true)
      ; tc "[CHIld]"              (case [ "CHIld" ]   ~fails_when_sensitive:true)
      ];
      "Wrong-case alias (sensitive fails)",
      [ tc "[ALTERNATIVE]"        (case [ "ALTERNATIVE" ] ~fails_when_sensitive:true)
      ; tc "[ALTernatIVE]"        (case [ "ALTernatIVE" ] ~fails_when_sensitive:true)
      ; tc "[alternatiVE]"        (case [ "alternatiVE" ] ~fails_when_sensitive:true)
      ];
      "Wrong-case grandchild/alias (sensitive fails)",
      [ tc "[child;grandchild]"     (case [ "child"; "grandchild" ]     ~fails_when_sensitive:true)
      ; tc "[CHIld;GRANdchild]"     (case [ "CHIld"; "GRANdchild" ]     ~fails_when_sensitive:true)
      ; tc "[alternative;alias]"    (case [ "alternative"; "alias" ]    ~fails_when_sensitive:true)
      ; tc "[CHILD;alias]"          (case [ "CHILD"; "alias" ]          ~fails_when_sensitive:true)
      ; tc "[CHIld;aliAS]"          (case [ "CHIld"; "aliAS" ]          ~fails_when_sensitive:true)
      ];
      "Regression",
      [ tc "case-insensitive run callback actually fires"
          case_insensitive_run_actually_fires
      ];
    ]
