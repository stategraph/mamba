(** Port of Cobra's command-group tests (command_test.go).

    Tests covered: TestUsageWithGroup, TestUngroupedCommand, TestAddGroup,
    TestWrongGroupFirstLevel, TestWrongGroupNestedLevel, TestUsageHelpGroup,
    TestUsageCompletionGroup, TestWrongGroupForHelp, TestWrongGroupForCompletion.

    Cobra's mutator API ([SetHelpCommandGroupID], [SetCompletionCommandGroupID])
    is exposed in mamba as [?help_command_group_id] and
    [?completion_command_group_id] parameters on [Program.make]. *)

open Mamba

let contains haystack needle =
  let lh = String.length haystack and ln = String.length needle in
  if ln = 0 then true
  else
    let rec loop i =
      if i + ln > lh then false
      else if String.sub haystack i ln = needle then true
      else loop (i + 1)
    in
    loop 0

(* Run [root --help] and return captured stdout. *)
let help_output root =
  let out_buf = Buffer.create 256 in
  let err_buf = Buffer.create 64 in
  let prog =
    Program.make ~name:root.Command.name ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  let _ = Program.run prog ~argv:[| root.Command.name; "--help" |] in
  Buffer.contents out_buf

let must_contain label out needle =
  Alcotest.(check bool) (label ^ ": contains " ^ needle) true (contains out needle)

(* ------------------------------------------------------------------ *)
(* TestUsageWithGroup                                                  *)
(* ------------------------------------------------------------------ *)

let usage_with_group () =
  let cmd1 = Command.make ~name:"cmd1" ~group_id:"group1" ~run:(fun _ -> 0) () in
  let cmd2 = Command.make ~name:"cmd2" ~group_id:"group2" ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"root" ~short:"test"
      ~groups:[ "group1", "group1"; "group2", "group2" ]
      ~subcommands:[ cmd1; cmd2 ] ~run:(fun _ -> 0) ()
  in
  let out = help_output root in
  must_contain "group1 section" out "group1:";
  must_contain "cmd1 listed"   out "cmd1";
  must_contain "group2 section" out "group2:";
  must_contain "cmd2 listed"   out "cmd2"

(* ------------------------------------------------------------------ *)
(* TestUngroupedCommand                                                *)
(* ------------------------------------------------------------------ *)

let ungrouped_command () =
  let xxx = Command.make ~name:"xxx" ~group_id:"group" ~run:(fun _ -> 0) () in
  let yyy = Command.make ~name:"yyy" ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"root" ~short:"test"
      ~groups:[ "group", "group" ]
      ~subcommands:[ xxx; yyy ] ~run:(fun _ -> 0) ()
  in
  let out = help_output root in
  must_contain "group section" out "group:";
  must_contain "xxx in group"  out "xxx";
  must_contain "additional"    out "Additional Commands:";
  must_contain "yyy ungrouped" out "yyy"

(* ------------------------------------------------------------------ *)
(* TestAddGroup                                                        *)
(* ------------------------------------------------------------------ *)

let add_group () =
  let cmd = Command.make ~name:"cmd" ~group_id:"group" ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"root" ~short:"test"
      ~groups:[ "group", "Test group" ]
      ~subcommands:[ cmd ] ~run:(fun _ -> 0) ()
  in
  let out = help_output root in
  must_contain "Test group" out "Test group:";
  must_contain "cmd"        out "cmd"

(* ------------------------------------------------------------------ *)
(* TestWrongGroup{FirstLevel,NestedLevel}                              *)
(* ------------------------------------------------------------------ *)

(* mamba surfaces undefined-group references via [Program.make ->
   Invalid_argument] rather than Cobra's panic. *)
let assert_make_invalid root =
  try
    let _ =
      Program.make ~name:root.Command.name ~version:"0" ~root
        ~help_command:false ~completion_command:false
        ~out:(Format.formatter_of_buffer (Buffer.create 16))
        ~err:(Format.formatter_of_buffer (Buffer.create 16))
        ()
    in
    Alcotest.fail "expected Invalid_argument for undefined group"
  with Invalid_argument _ -> ()

let wrong_group_first_level () =
  let cmd = Command.make ~name:"cmd" ~group_id:"wrong" ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"root" ~short:"test"
      ~groups:[ "group", "Test group" ]
      ~subcommands:[ cmd ] ~run:(fun _ -> 0) ()
  in
  assert_make_invalid root

let wrong_group_nested_level () =
  let cmd =
    Command.make ~name:"cmd" ~group_id:"wrong" ~run:(fun _ -> 0) ()
  in
  let child =
    Command.make ~name:"child"
      ~groups:[ "group", "Test group" ]
      ~subcommands:[ cmd ] ~run:(fun _ -> 0) ()
  in
  let root =
    Command.make ~name:"root" ~subcommands:[ child ] ~run:(fun _ -> 0) ()
  in
  assert_make_invalid root

(* ------------------------------------------------------------------ *)
(* TestUsageHelpGroup / TestUsageCompletionGroup                       *)
(* ------------------------------------------------------------------ *)

(* TestUsageHelpGroup: with [help_command_group_id] set, the auto-injected
   "help" command appears under that group rather than "Additional Commands".
   Also disables completion + version subcommands so only [help] is auto-
   injected, mirroring Cobra's test (Cobra has no version subcommand and
   the test disables CompletionOptions.DisableDefaultCmd). *)
let usage_help_group () =
  let xxx = Command.make ~name:"xxx" ~group_id:"group" ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"root" ~short:"test"
      ~groups:[ "group", "group" ]
      ~subcommands:[ xxx ] ~run:(fun _ -> 0) ()
  in
  let out_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"root" ~version:"0" ~root
      ~help_command:true ~completion_command:false ~version_command:false
      ~help_command_group_id:"group"
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let _ = Program.run prog ~argv:[| "root"; "--help" |] in
  let out = Buffer.contents out_buf in
  must_contain "group section appears"        out "group:";
  must_contain "help appears under group"     out "help";
  Alcotest.(check bool) "no Additional Commands" false
    (contains out "Additional Commands:")

(* TestUsageCompletionGroup: similarly for the completion command. Version
   subcommand disabled to match Cobra's test surface. *)
let usage_completion_group () =
  let xxx = Command.make ~name:"xxx" ~group_id:"group" ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"root" ~short:"test"
      ~groups:[ "group", "group"; "help", "help" ]
      ~subcommands:[ xxx ] ~run:(fun _ -> 0) ()
  in
  let out_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"root" ~version:"0" ~root
      ~help_command:true ~completion_command:true ~version_command:false
      ~help_command_group_id:"help"
      ~completion_command_group_id:"group"
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer (Buffer.create 16))
      ()
  in
  let _ = Program.run prog ~argv:[| "root"; "--help" |] in
  let out = Buffer.contents out_buf in
  must_contain "help section"        out "help:";
  must_contain "group section"       out "group:";
  must_contain "completion in group" out "completion"

(* ------------------------------------------------------------------ *)
(* TestWrongGroupForHelp / TestWrongGroupForCompletion                 *)
(* ------------------------------------------------------------------ *)

(* mamba surfaces this via [Program.make -> Invalid_argument] from validate. *)
let wrong_group_for_help () =
  let root =
    Command.make ~name:"root" ~short:"test"
      ~groups:[ "group", "Test group" ]
      ~run:(fun _ -> 0) ()
  in
  try
    let _ =
      Program.make ~name:"root" ~version:"0" ~root
        ~help_command:true ~completion_command:false
        ~help_command_group_id:"wrong"
        ~out:(Format.formatter_of_buffer (Buffer.create 16))
        ~err:(Format.formatter_of_buffer (Buffer.create 16))
        ()
    in
    Alcotest.fail "expected Invalid_argument for undefined help group"
  with Invalid_argument _ -> ()

let wrong_group_for_completion () =
  let root =
    Command.make ~name:"root" ~short:"test"
      ~groups:[ "group", "Test group" ]
      ~run:(fun _ -> 0) ()
  in
  try
    let _ =
      Program.make ~name:"root" ~version:"0" ~root
        ~help_command:false ~completion_command:true
        ~completion_command_group_id:"wrong"
        ~out:(Format.formatter_of_buffer (Buffer.create 16))
        ~err:(Format.formatter_of_buffer (Buffer.create 16))
        ()
    in
    Alcotest.fail "expected Invalid_argument for undefined completion group"
  with Invalid_argument _ -> ()

(* ------------------------------------------------------------------ *)
(* Runner                                                              *)
(* ------------------------------------------------------------------ *)

let tc name f = Alcotest.test_case name `Quick f

let () =
  Alcotest.run "cobra_groups"
    [
      "Rendering",
      [ tc "UsageWithGroup"   usage_with_group
      ; tc "UngroupedCommand" ungrouped_command
      ; tc "AddGroup"         add_group
      ];
      "Validation",
      [ tc "WrongGroupFirstLevel"   wrong_group_first_level
      ; tc "WrongGroupNestedLevel"  wrong_group_nested_level
      ; tc "WrongGroupForHelp"      wrong_group_for_help
      ; tc "WrongGroupForCompletion" wrong_group_for_completion
      ];
      "HelpCompletionGroupID",
      [ tc "UsageHelpGroup"        usage_help_group
      ; tc "UsageCompletionGroup"  usage_completion_group
      ];
    ]
