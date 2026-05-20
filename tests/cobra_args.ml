(** Port of Cobra's args_test.go (spf13/cobra v1.10.2).

    Maps Cobra's [PositionalArgs] validators onto mamba's [Arg.spec]:

      Cobra                  | mamba
      -----------------------+---------------------------------
      NoArgs                 | Arg.none
      ArbitraryArgs          | Arg.any
      OnlyValidArgs          | Arg.only_valid_of valid_args
      MinimumNArgs n         | Arg.minimum n
      MaximumNArgs n         | Arg.maximum n
      ExactArgs n            | Arg.exactly n
      RangeArgs (min,max)    | Arg.range ~min ~max
      MatchAll (a, b, ...)   | Arg.all_of [a; b; ...]
      ExactValidArgs n       | Arg.all_of [exactly n; only_valid_of vs]

    Cobra's [ValidArgs] is metadata on the [Command] that the
    [OnlyValidArgs] validator reads at check time. Mamba folds the allow-list
    into the validator itself ([Arg.only_valid_of]), so Cobra tests that pass
    [ValidArgs] without an [OnlyValidArgs] validator collapse to the
    "no valid set" variant -- they're still ported for 1:1 coverage.

    Assertions are behavioral: exit code [0] on success, [2] on validation
    failure. Error-message wording is not asserted; mamba's phrasing differs
    from Cobra's. *)

open Mamba

let valid_args = [ "one"; "two"; "three" ]
let only_valid = Arg.only_valid_of valid_args

(* Build a single-subcommand program: [app c <argv...>], silencing out/err
   so test output stays clean. Returns the exit code. *)
let run_one spec argv =
  let c    = Command.make ~name:"c" ~args:spec ~run:(fun _ -> 0) () in
  let root = Command.make ~name:"app" ~subcommands:[ c ] () in
  let out_buf = Buffer.create 64 in
  let err_buf = Buffer.create 64 in
  let prog =
    Program.make ~name:"app" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  Program.run prog ~argv:(Array.of_list ("app" :: "c" :: argv))

(* Variant for the Root/Child tests: argv is appended to ["root"]. *)
let run_root ~root_args ~child_args argv =
  let child = Command.make ~name:"child" ~args:child_args ~run:(fun _ -> 0) () in
  let root  =
    Command.make ~name:"root" ~args:root_args
      ~subcommands:[ child ] ~run:(fun _ -> 0) ()
  in
  let out_buf = Buffer.create 64 in
  let err_buf = Buffer.create 64 in
  let prog =
    Program.make ~name:"root" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  Program.run prog ~argv:(Array.of_list ("root" :: argv))

let success code =
  Alcotest.(check int) "exit 0" Error.success code

let failure code =
  Alcotest.(check int) "exit 2" Error.parse_error code

let ok  spec argv () = success (run_one spec argv)
let bad spec argv () = failure (run_one spec argv)

(* ------------------------------------------------------------------ *)
(* NoArgs                                                              *)
(* ------------------------------------------------------------------ *)

let no_args                          = ok  Arg.none []
let no_args_with_args                = bad Arg.none [ "one" ]
let no_args_with_valid_with_args     = bad Arg.none [ "one" ]
let no_args_with_valid_with_invalid  = bad Arg.none [ "a" ]
let no_args_with_valid_only_with_invalid =
  bad (Arg.all_of [ only_valid; Arg.none ]) [ "a" ]

(* ------------------------------------------------------------------ *)
(* OnlyValidArgs                                                       *)
(* ------------------------------------------------------------------ *)

let only_valid_ok                    = ok  only_valid [ "one"; "two" ]
let only_valid_with_invalid          = bad only_valid [ "a" ]

(* ------------------------------------------------------------------ *)
(* ArbitraryArgs                                                       *)
(* ------------------------------------------------------------------ *)

let arbitrary                        = ok  Arg.any [ "a"; "b" ]
let arbitrary_with_valid             = ok  Arg.any [ "one"; "two" ]
let arbitrary_with_valid_with_invalid = ok Arg.any [ "a" ]
let arbitrary_with_valid_only_with_invalid =
  bad (Arg.all_of [ only_valid; Arg.any ]) [ "a" ]

(* ------------------------------------------------------------------ *)
(* MinimumNArgs                                                        *)
(* ------------------------------------------------------------------ *)

let min2 = Arg.minimum 2
let min2_valid = Arg.all_of [ only_valid; Arg.minimum 2 ]

let minimum_n                              = ok  min2 [ "a"; "b"; "c" ]
let minimum_n_with_valid                   = ok  min2 [ "one"; "three" ]
let minimum_n_with_valid_with_invalid      = ok  min2 [ "a"; "b" ]
let minimum_n_with_valid_only_with_invalid = bad min2_valid [ "a"; "b" ]
let minimum_n_with_less                    = bad min2 [ "a" ]
let minimum_n_with_less_with_valid         = bad min2 [ "one" ]
let minimum_n_with_less_with_valid_with_invalid = bad min2 [ "a" ]
let minimum_n_with_less_with_valid_only_with_invalid = bad min2_valid [ "a" ]

(* ------------------------------------------------------------------ *)
(* MaximumNArgs                                                        *)
(* ------------------------------------------------------------------ *)

let max3 = Arg.maximum 3
let max2 = Arg.maximum 2
let max2_valid = Arg.all_of [ only_valid; Arg.maximum 2 ]

let maximum_n                              = ok  max3 [ "a"; "b" ]
let maximum_n_with_valid                   = ok  max2 [ "one"; "three" ]
let maximum_n_with_valid_with_invalid      = ok  max2 [ "a"; "b" ]
let maximum_n_with_valid_only_with_invalid = bad max2_valid [ "a"; "b" ]
let maximum_n_with_more                    = bad max2 [ "a"; "b"; "c" ]
let maximum_n_with_more_with_valid         = bad max2 [ "one"; "three"; "two" ]
let maximum_n_with_more_with_valid_with_invalid = bad max2 [ "a"; "b"; "c" ]
let maximum_n_with_more_with_valid_only_with_invalid =
  bad max2_valid [ "a"; "b"; "c" ]

(* ------------------------------------------------------------------ *)
(* ExactArgs                                                           *)
(* ------------------------------------------------------------------ *)

let exact2 = Arg.exactly 2
let exact3 = Arg.exactly 3
let exact3_valid = Arg.all_of [ only_valid; Arg.exactly 3 ]
let exact2_valid = Arg.all_of [ only_valid; Arg.exactly 2 ]

let exact                                   = ok  exact3 [ "a"; "b"; "c" ]
let exact_with_valid                        = ok  exact3 [ "three"; "one"; "two" ]
let exact_with_valid_with_invalid           = ok  exact3 [ "three"; "a"; "two" ]
let exact_with_valid_only_with_invalid      = bad exact3_valid [ "three"; "a"; "two" ]
let exact_with_invalid_count                = bad exact2 [ "a"; "b"; "c" ]
let exact_with_invalid_count_with_valid     = bad exact2 [ "three"; "one"; "two" ]
let exact_with_invalid_count_with_valid_with_invalid =
  bad exact2 [ "three"; "a"; "two" ]
let exact_with_invalid_count_with_valid_only_with_invalid =
  bad exact2_valid [ "three"; "a"; "two" ]

(* ------------------------------------------------------------------ *)
(* RangeArgs                                                           *)
(* ------------------------------------------------------------------ *)

let r24 = Arg.range ~min:2 ~max:4
let r24_valid = Arg.all_of [ only_valid; Arg.range ~min:2 ~max:4 ]

let range                                   = ok  r24 [ "a"; "b"; "c" ]
let range_with_valid                        = ok  r24 [ "three"; "one"; "two" ]
let range_with_valid_with_invalid           = ok  r24 [ "three"; "a"; "two" ]
let range_with_valid_only_with_invalid      = bad r24_valid [ "three"; "a"; "two" ]
let range_with_invalid_count                = bad r24 [ "a" ]
let range_with_invalid_count_with_valid     = bad r24 [ "two" ]
let range_with_invalid_count_with_valid_with_invalid = bad r24 [ "a" ]
let range_with_invalid_count_with_valid_only_with_invalid = bad r24_valid [ "a" ]

(* ------------------------------------------------------------------ *)
(* Root / Child takes args                                             *)
(* ------------------------------------------------------------------ *)

(* Cobra's default Args validator (legacyArgs) rejects extra args on the
   root when subcommands exist. Mamba's default is [Arg.any]; to preserve
   the same assertion we pass [Arg.none] explicitly. *)
let root_takes_no_args () =
  failure (run_root ~root_args:Arg.none ~child_args:Arg.any [ "illegal"; "args" ])

let root_takes_args () =
  success (run_root ~root_args:Arg.any ~child_args:Arg.any [ "legal"; "args" ])

let child_takes_no_args () =
  failure (run_root ~root_args:Arg.any ~child_args:Arg.none [ "child"; "illegal"; "args" ])

let child_takes_args () =
  success (run_root ~root_args:Arg.any ~child_args:Arg.any [ "child"; "legal"; "args" ])

(* ------------------------------------------------------------------ *)
(* MatchAll: exactly 3 args, each exactly 2 bytes                      *)
(* ------------------------------------------------------------------ *)

let match_all_spec =
  Arg.all_of
    [ Arg.exactly 3
    ; Arg.custom (fun args ->
        match List.find_opt (fun s -> String.length s <> 2) args with
        | None   -> Ok ()
        | Some _ -> Error "expected to be exactly 2 bytes long")
    ]

let match_all_happy        = ok  match_all_spec [ "aa"; "bb"; "cc" ]
let match_all_wrong_count  = bad match_all_spec [ "aa"; "bb"; "cc"; "dd" ]
let match_all_wrong_length = bad match_all_spec [ "aa"; "bb"; "abc" ]

(* ------------------------------------------------------------------ *)
(* ExactValidArgs (deprecated; expressed as exactly + only_valid_of)   *)
(* ------------------------------------------------------------------ *)

let exact_valid n = Arg.all_of [ Arg.exactly n; only_valid ]

let exact_valid_args                = ok  (exact_valid 3) [ "three"; "one"; "two" ]
let exact_valid_args_invalid_count  = bad (exact_valid 2) [ "three"; "one"; "two" ]
let exact_valid_args_invalid_count_invalid =
  bad (exact_valid 2) [ "three"; "a"; "two" ]
let exact_valid_args_invalid_args   = bad (exact_valid 2) [ "three"; "a" ]

(* ------------------------------------------------------------------ *)
(* Legacy: no explicit Args + no/leaf subcommands -> accepts           *)
(* ------------------------------------------------------------------ *)

let legacy_root_accepts () =
  let root =
    Command.make ~name:"root" ~run:(fun _ -> 0) ()
  in
  let out_buf = Buffer.create 64 in
  let err_buf = Buffer.create 64 in
  let prog =
    Program.make ~name:"root" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  success (Program.run prog ~argv:[| "root"; "somearg" |])

let legacy_subcmd_accepts () =
  let grandchild = Command.make ~name:"grandchild" ~run:(fun _ -> 0) () in
  let child =
    Command.make ~name:"child" ~run:(fun _ -> 0)
      ~subcommands:[ grandchild ] ()
  in
  let root = Command.make ~name:"root" ~run:(fun _ -> 0) ~subcommands:[ child ] () in
  let out_buf = Buffer.create 64 in
  let err_buf = Buffer.create 64 in
  let prog =
    Program.make ~name:"root" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  success (Program.run prog ~argv:[| "root"; "child"; "somearg" |])

(* ------------------------------------------------------------------ *)
(* runner                                                              *)
(* ------------------------------------------------------------------ *)

let tc name f = Alcotest.test_case name `Quick f

let () =
  Alcotest.run "cobra_args"
    [
      "NoArgs",
      [ tc "NoArgs"                              no_args
      ; tc "NoArgs_WithArgs"                     no_args_with_args
      ; tc "NoArgs_WithValid_WithArgs"           no_args_with_valid_with_args
      ; tc "NoArgs_WithValid_WithInvalidArgs"    no_args_with_valid_with_invalid
      ; tc "NoArgs_WithValidOnly_WithInvalidArgs" no_args_with_valid_only_with_invalid
      ];
      "OnlyValidArgs",
      [ tc "OnlyValidArgs"                only_valid_ok
      ; tc "OnlyValidArgs_WithInvalidArgs" only_valid_with_invalid
      ];
      "ArbitraryArgs",
      [ tc "ArbitraryArgs"                            arbitrary
      ; tc "ArbitraryArgs_WithValid"                  arbitrary_with_valid
      ; tc "ArbitraryArgs_WithValid_WithInvalidArgs"  arbitrary_with_valid_with_invalid
      ; tc "ArbitraryArgs_WithValidOnly_WithInvalidArgs"
          arbitrary_with_valid_only_with_invalid
      ];
      "MinimumNArgs",
      [ tc "MinimumNArgs"                            minimum_n
      ; tc "MinimumNArgs_WithValid"                  minimum_n_with_valid
      ; tc "MinimumNArgs_WithValid_WithInvalidArgs"  minimum_n_with_valid_with_invalid
      ; tc "MinimumNArgs_WithValidOnly_WithInvalidArgs"
          minimum_n_with_valid_only_with_invalid
      ; tc "MinimumNArgs_WithLessArgs"               minimum_n_with_less
      ; tc "MinimumNArgs_WithLessArgs_WithValid"     minimum_n_with_less_with_valid
      ; tc "MinimumNArgs_WithLessArgs_WithValid_WithInvalidArgs"
          minimum_n_with_less_with_valid_with_invalid
      ; tc "MinimumNArgs_WithLessArgs_WithValidOnly_WithInvalidArgs"
          minimum_n_with_less_with_valid_only_with_invalid
      ];
      "MaximumNArgs",
      [ tc "MaximumNArgs"                            maximum_n
      ; tc "MaximumNArgs_WithValid"                  maximum_n_with_valid
      ; tc "MaximumNArgs_WithValid_WithInvalidArgs"  maximum_n_with_valid_with_invalid
      ; tc "MaximumNArgs_WithValidOnly_WithInvalidArgs"
          maximum_n_with_valid_only_with_invalid
      ; tc "MaximumNArgs_WithMoreArgs"               maximum_n_with_more
      ; tc "MaximumNArgs_WithMoreArgs_WithValid"     maximum_n_with_more_with_valid
      ; tc "MaximumNArgs_WithMoreArgs_WithValid_WithInvalidArgs"
          maximum_n_with_more_with_valid_with_invalid
      ; tc "MaximumNArgs_WithMoreArgs_WithValidOnly_WithInvalidArgs"
          maximum_n_with_more_with_valid_only_with_invalid
      ];
      "ExactArgs",
      [ tc "ExactArgs"                            exact
      ; tc "ExactArgs_WithValid"                  exact_with_valid
      ; tc "ExactArgs_WithValid_WithInvalidArgs"  exact_with_valid_with_invalid
      ; tc "ExactArgs_WithValidOnly_WithInvalidArgs"
          exact_with_valid_only_with_invalid
      ; tc "ExactArgs_WithInvalidCount"           exact_with_invalid_count
      ; tc "ExactArgs_WithInvalidCount_WithValid" exact_with_invalid_count_with_valid
      ; tc "ExactArgs_WithInvalidCount_WithValid_WithInvalidArgs"
          exact_with_invalid_count_with_valid_with_invalid
      ; tc "ExactArgs_WithInvalidCount_WithValidOnly_WithInvalidArgs"
          exact_with_invalid_count_with_valid_only_with_invalid
      ];
      "RangeArgs",
      [ tc "RangeArgs"                            range
      ; tc "RangeArgs_WithValid"                  range_with_valid
      ; tc "RangeArgs_WithValid_WithInvalidArgs"  range_with_valid_with_invalid
      ; tc "RangeArgs_WithValidOnly_WithInvalidArgs"
          range_with_valid_only_with_invalid
      ; tc "RangeArgs_WithInvalidCount"           range_with_invalid_count
      ; tc "RangeArgs_WithInvalidCount_WithValid" range_with_invalid_count_with_valid
      ; tc "RangeArgs_WithInvalidCount_WithValid_WithInvalidArgs"
          range_with_invalid_count_with_valid_with_invalid
      ; tc "RangeArgs_WithInvalidCount_WithValidOnly_WithInvalidArgs"
          range_with_invalid_count_with_valid_only_with_invalid
      ];
      "RootChildTakesArgs",
      [ tc "RootTakesNoArgs"  root_takes_no_args
      ; tc "RootTakesArgs"    root_takes_args
      ; tc "ChildTakesNoArgs" child_takes_no_args
      ; tc "ChildTakesArgs"   child_takes_args
      ];
      "MatchAll",
      [ tc "happy path"                       match_all_happy
      ; tc "incorrect number of args"         match_all_wrong_count
      ; tc "incorrect number of bytes in one arg" match_all_wrong_length
      ];
      "ExactValidArgs",
      [ tc "ExactValidArgs"                  exact_valid_args
      ; tc "ExactValidArgs_WithInvalidCount" exact_valid_args_invalid_count
      ; tc "ExactValidArgs_WithInvalidCount_WithInvalidArgs"
          exact_valid_args_invalid_count_invalid
      ; tc "ExactValidArgs_WithInvalidArgs"  exact_valid_args_invalid_args
      ];
      "Legacy",
      [ tc "LegacyArgsRootAcceptsArgs"   legacy_root_accepts
      ; tc "LegacyArgsSubcmdAcceptsArgs" legacy_subcmd_accepts
      ];
    ]
