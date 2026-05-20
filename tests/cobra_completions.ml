(** Port of Cobra's TestProgWith{Dash,Colon} from fish_completions_test.go,
    extended to bash and zsh since mamba defines internal functions in both.

    The assertion: function-name positions in the emitted script must use
    valid shell identifiers (no [-:] etc.), while the visible program name
    that appears in directives like `complete -c <name>`, `#compdef <name>`,
    and `complete -F <fn> <name>` is preserved verbatim.

    Tests intentionally omitted from completion-related Cobra files:
      - bash_completions_test.go: TestBashCompletions, TestBashCompletionHiddenFlag,
        TestBashCompletionDeprecatedFlag, TestBashCompletionTraverseChildren,
        TestBashCompletionNoActiveHelp
          All exercise Cobra-specific generator markers
          (must_have_one_flag, local_nonpersistent_flags, two_word_flags,
          active-help env var, flag-level Hidden/Deprecated/TraverseChildren).
      - zsh_completions_test.go: TestZshCompletionWithActiveHelp
          Active help not in mamba.
      - fish_completions_test.go: TestCompleteNoDesCmdInFishScript,
        TestCompleteCmdInFishScript, TestFishCompletionNoActiveHelp,
        TestGenFishCompletionFile, TestFailGenFishCompletionFile
          Runtime completion ($ShellCompRequestCmd), active help, and
          file-IO wrappers not in mamba.
      - completions_test.go: all 49 tests
          Test Cobra's runtime completion engine (ValidArgsFunction,
          RegisterFlagCompletionFunc); mamba has shell-script emitters only,
          no runtime completion. *)

open Mamba
open Test_util

let emit shell ~program_name root =
  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Completion.emit ~out:fmt ~shell ~program_name ~root;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let make_root name =
  let child = Command.make ~name:"child" ~run:(fun _ -> 0) () in
  Command.make ~name ~run:(fun _ -> 0) ~subcommands:[ child ] ()

(* ------------------------------------------------------------------ *)
(* Fish                                                                *)
(* ------------------------------------------------------------------ *)

(* Fish (in mamba) defines no internal functions keyed on program_name;
   it only emits `complete -c <name>` directives. The Cobra equivalent
   asserts the directive preserves the original name. *)
let fish_prog_with_dash () =
  let out = emit Completion.Fish ~program_name:"root-dash" (make_root "root-dash") in
  must_contain "fish dash" out "complete -c root-dash"

let fish_prog_with_colon () =
  let out = emit Completion.Fish ~program_name:"root:colon" (make_root "root:colon") in
  must_contain "fish colon" out "complete -c root:colon"

(* ------------------------------------------------------------------ *)
(* Bash                                                                *)
(* ------------------------------------------------------------------ *)

(* Bash function names must match [A-Za-z_][A-Za-z0-9_]*. mamba sanitizes
   special chars to '_' for the internal function name while keeping the
   program name verbatim in `complete -F <fn> <name>`. *)
let bash_prog_with_dash () =
  let out = emit Completion.Bash ~program_name:"root-dash" (make_root "root-dash") in
  must_contain "fn name sanitized"        out "_root_dash_complete";
  must_omit    "fn name unsanitized"      out "_root-dash_complete";
  must_contain "visible name preserved"   out "complete -F _root_dash_complete root-dash"

let bash_prog_with_colon () =
  let out = emit Completion.Bash ~program_name:"root:colon" (make_root "root:colon") in
  must_contain "fn name sanitized"        out "_root_colon_complete";
  must_omit    "fn name unsanitized"      out "_root:colon_complete";
  must_contain "visible name preserved"   out "complete -F _root_colon_complete root:colon"

(* ------------------------------------------------------------------ *)
(* Zsh                                                                 *)
(* ------------------------------------------------------------------ *)

(* Zsh function names also constrained to valid identifiers, but the
   `#compdef` directive must keep the original program name so the shell
   knows which command to attach completion to. *)
let zsh_prog_with_dash () =
  let out = emit Completion.Zsh ~program_name:"root-dash" (make_root "root-dash") in
  must_contain "fn name sanitized"        out "_root_dash()";
  must_omit    "fn name unsanitized"      out "_root-dash()";
  must_contain "compdef preserves name"   out "#compdef root-dash"

let zsh_prog_with_colon () =
  let out = emit Completion.Zsh ~program_name:"root:colon" (make_root "root:colon") in
  must_contain "fn name sanitized"        out "_root_colon()";
  must_omit    "fn name unsanitized"      out "_root:colon()";
  must_contain "compdef preserves name"   out "#compdef root:colon"

(* ------------------------------------------------------------------ *)
(* Sanity: subcommand names and flag names appear in bash output       *)
(* ------------------------------------------------------------------ *)

let bash_lists_subcommands () =
  let foo = Flag.string ~name:"foo" ~short:'f' ~default:"" ~doc:"" () in
  let child = Command.make ~name:"child" ~run:(fun _ -> 0) () in
  let root =
    Command.make ~name:"app"
      ~flags:[ Flag.pack foo ]
      ~subcommands:[ child ] ~run:(fun _ -> 0) ()
  in
  let out = emit Completion.Bash ~program_name:"app" root in
  must_contain "subcommand"   out "child";
  must_contain "long flag"    out "--foo";
  must_contain "short flag"   out "-f"

(* ------------------------------------------------------------------ *)
(* Runner                                                              *)
(* ------------------------------------------------------------------ *)

let tc name f = Alcotest.test_case name `Quick f

let () =
  Alcotest.run "cobra_completions"
    [
      "Fish",
      [ tc "ProgWithDash"  fish_prog_with_dash
      ; tc "ProgWithColon" fish_prog_with_colon
      ];
      "Bash",
      [ tc "ProgWithDash"        bash_prog_with_dash
      ; tc "ProgWithColon"       bash_prog_with_colon
      ; tc "lists subcommands"   bash_lists_subcommands
      ];
      "Zsh",
      [ tc "ProgWithDash"  zsh_prog_with_dash
      ; tc "ProgWithColon" zsh_prog_with_colon
      ];
    ]
