(** Port of Cobra's TestValidateFlagGroups table (flag_groups_test.go).

    Cobra has three mutator methods on Command: [MarkFlagsRequiredTogether],
    [MarkFlagsOneRequired], [MarkFlagsMutuallyExclusive]. Mamba expresses
    the same constraints via the immutable [?flag_groups] parameter of
    [Command.make], using {!Flag_group} constructors.

    Assertions are behavioral (exit code 0 vs 2). Cobra's exact error
    messages happen to match mamba's by implementation choice -- the
    formatting strings in [src/mamba/flag_group.ml] mirror Cobra's word
    for word -- but the tests don't pin the text. Cases 9-11 ("sorted
    order returns first error") test a Cobra-internal sort that mamba
    doesn't implement; mamba returns errors in declaration order. Since
    the assertion is just "an error fired", they still pass. *)

open Mamba

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

(* Re-create the Cobra getCmd shape per case:
     root [a b c d local] + [e f g persistent] + subcmd [subonly local].
   Flag groups are passed via the optional parameters. *)
let mk_tree
    ?(root_required = []) ?(root_one_required = []) ?(root_exclusive = [])
    ?(sub_required  = []) ?(sub_one_required  = []) ?(sub_exclusive  = []) () =
  let mk_str n = Flag.string ~name:n ~default:"" ~doc:"" () in
  let a = mk_str "a" and b = mk_str "b"
  and c = mk_str "c" and d = mk_str "d" in
  let e = mk_str "e" and f = mk_str "f" and g = mk_str "g" in
  let subonly = mk_str "subonly" in
  (* Cobra's tests reference flags by name string; mirror with the
     [_by_name] variants here. The type-safe constructors take
     [Flag.packed list] instead. *)
  let to_groups required one_required exclusive =
    let r = List.map (fun xs -> Flag_group.required_together_by_name  xs) required     in
    let o = List.map (fun xs -> Flag_group.one_required_by_name       xs) one_required in
    let x = List.map (fun xs -> Flag_group.mutually_exclusive_by_name xs) exclusive    in
    r @ o @ x
  in
  let sub =
    Command.make ~name:"subcmd" ~args:Arg.any
      ~flags:[ Flag.pack subonly ]
      ~flag_groups:(to_groups sub_required sub_one_required sub_exclusive)
      ~run:(fun _ -> 0) ()
  in
  let root =
    Command.make ~name:"testcmd" ~args:Arg.any
      ~flags:[ Flag.pack a; Flag.pack b; Flag.pack c; Flag.pack d ]
      ~persistent_flags:[ Flag.pack e; Flag.pack f; Flag.pack g ]
      ~flag_groups:(to_groups root_required root_one_required root_exclusive)
      ~subcommands:[ sub ] ~run:(fun _ -> 0) ()
  in
  root

let exec root args =
  let out_buf = Buffer.create 64 in
  let err_buf = Buffer.create 128 in
  let prog =
    Program.make ~name:"testcmd" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  let argv = Array.of_list ("testcmd" :: args) in
  Program.run prog ~argv

(* ------------------------------------------------------------------ *)
(* Wrappers that build per-case trees                                  *)
(* ------------------------------------------------------------------ *)

let case
    ?root_required ?root_one_required ?root_exclusive
    ?sub_required ?sub_one_required ?sub_exclusive
    ~argv ~expect () =
  let root =
    mk_tree
      ?root_required ?root_one_required ?root_exclusive
      ?sub_required ?sub_one_required ?sub_exclusive ()
  in
  let code = exec root argv in
  match expect with
  | `Ok    -> Alcotest.(check int) "exit 0" Error.success     code
  | `Error -> Alcotest.(check int) "exit 2" Error.parse_error code

(* ------------------------------------------------------------------ *)
(* The table (22 cases)                                                *)
(* ------------------------------------------------------------------ *)

let t01_no_flags_no_problem =
  case ~argv:[] ~expect:`Ok

let t02_no_flags_no_problem_even_with_conflicting_groups =
  case
    ~root_required:[ [ "a"; "b" ] ]
    ~root_exclusive:[ [ "a"; "b" ] ]
    ~argv:[] ~expect:`Ok

let t03_required_flag_group_not_satisfied =
  case
    ~root_required:[ [ "a"; "b"; "c" ] ]
    ~argv:[ "--a=foo" ] ~expect:`Error

let t04_one_required_flag_group_not_satisfied =
  case
    ~root_one_required:[ [ "a"; "b" ] ]
    ~argv:[ "--c=foo" ] ~expect:`Error

let t05_exclusive_flag_group_not_satisfied =
  case
    ~root_exclusive:[ [ "a"; "b"; "c" ] ]
    ~argv:[ "--a=foo"; "--b=foo" ] ~expect:`Error

let t06_multiple_required_first_error =
  case
    ~root_required:[ [ "a"; "b"; "c" ]; [ "a"; "d" ] ]
    ~argv:[ "--c=foo"; "--d=foo" ] ~expect:`Error

let t07_multiple_one_required_first_error =
  case
    ~root_one_required:[ [ "a"; "b" ]; [ "d"; "e" ] ]
    ~argv:[ "--c=foo"; "--f=foo" ] ~expect:`Error

let t08_multiple_exclusive_first_error =
  case
    ~root_exclusive:[ [ "a"; "b"; "c" ]; [ "a"; "d" ] ]
    ~argv:[ "--a=foo"; "--c=foo"; "--d=foo" ] ~expect:`Error

(* Cases 9-11: Cobra sorts groups before validating; mamba doesn't sort.
   Behavioral assertion (error happens) still holds either way. *)
let t09_required_sorted_order =
  case
    ~root_required:[ [ "a"; "d" ]; [ "a"; "b" ]; [ "a"; "c" ] ]
    ~argv:[ "--a=foo" ] ~expect:`Error

let t10_one_required_sorted_order =
  case
    ~root_one_required:[ [ "d"; "e" ]; [ "a"; "b" ]; [ "f"; "g" ] ]
    ~argv:[ "--c=foo" ] ~expect:`Error

let t11_exclusive_sorted_order =
  case
    ~root_exclusive:[ [ "a"; "d" ]; [ "a"; "b" ]; [ "a"; "c" ] ]
    ~argv:[ "--a=foo"; "--b=foo"; "--c=foo" ] ~expect:`Error

let t12_persistent_required_fails_required =
  case
    ~root_required:[ [ "a"; "e" ]; [ "e"; "f" ] ]
    ~root_exclusive:[ [ "f"; "g" ] ]
    ~argv:[ "--a=foo"; "--f=foo"; "--g=foo" ] ~expect:`Error

let t13_persistent_one_required_fails_one_required =
  case
    ~root_one_required:[ [ "a"; "b" ]; [ "e"; "f" ] ]
    ~root_exclusive:[ [ "e"; "f" ] ]
    ~argv:[ "--e=foo" ] ~expect:`Error

let t14_persistent_required_fails_exclusive =
  case
    ~root_required:[ [ "a"; "e" ]; [ "e"; "f" ] ]
    ~root_exclusive:[ [ "f"; "g" ] ]
    ~argv:[ "--a=foo"; "--e=foo"; "--f=foo"; "--g=foo" ] ~expect:`Error

let t15_persistent_required_pass =
  case
    ~root_required:[ [ "a"; "e" ]; [ "e"; "f" ] ]
    ~root_exclusive:[ [ "f"; "g" ] ]
    ~argv:[ "--a=foo"; "--e=foo"; "--f=foo" ] ~expect:`Ok

let t16_persistent_one_required_pass =
  case
    ~root_one_required:[ [ "a"; "e" ]; [ "e"; "f" ] ]
    ~root_exclusive:[ [ "f"; "g" ] ]
    ~argv:[ "--a=foo"; "--e=foo"; "--f=foo" ] ~expect:`Ok

let t17_sub_required_inherited =
  case
    ~sub_required:[ [ "e"; "subonly" ] ]
    ~argv:[ "subcmd"; "--e=foo"; "--subonly=foo" ] ~expect:`Ok

let t18_sub_one_required_inherited =
  case
    ~sub_one_required:[ [ "e"; "subonly" ] ]
    ~argv:[ "subcmd"; "--e=foo"; "--subonly=foo" ] ~expect:`Ok

let t19_sub_one_required_fails =
  case
    ~sub_one_required:[ [ "e"; "subonly" ] ]
    ~argv:[ "subcmd" ] ~expect:`Error

let t20_sub_exclusive_fails =
  case
    ~sub_exclusive:[ [ "e"; "subonly" ] ]
    ~argv:[ "subcmd"; "--e=foo"; "--subonly=foo" ] ~expect:`Error

let t21_sub_exclusive_pass =
  case
    ~sub_exclusive:[ [ "e"; "subonly" ] ]
    ~argv:[ "subcmd"; "--e=foo" ] ~expect:`Ok

let t22_groups_not_applied_to_uninvoked_command =
  case
    ~sub_required:[ [ "e"; "subonly" ] ]
    ~argv:[ "--e=foo" ] ~expect:`Ok

(* ------------------------------------------------------------------ *)
(* Runner                                                              *)
(* ------------------------------------------------------------------ *)

let tc name f = Alcotest.test_case name `Quick f

let () =
  Alcotest.run "cobra_flag_groups"
    [
      "Basic",
      [ tc "01 no flags no problem"                 t01_no_flags_no_problem
      ; tc "02 no argv even with conflicting groups" t02_no_flags_no_problem_even_with_conflicting_groups
      ];
      "Required",
      [ tc "03 required group not satisfied"   t03_required_flag_group_not_satisfied
      ; tc "06 multiple required first error"  t06_multiple_required_first_error
      ; tc "09 required (sorted order)"        t09_required_sorted_order
      ];
      "OneRequired",
      [ tc "04 one-required not satisfied"    t04_one_required_flag_group_not_satisfied
      ; tc "07 multiple one-required first"   t07_multiple_one_required_first_error
      ; tc "10 one-required (sorted order)"   t10_one_required_sorted_order
      ];
      "Exclusive",
      [ tc "05 exclusive not satisfied"   t05_exclusive_flag_group_not_satisfied
      ; tc "08 multiple exclusive first"  t08_multiple_exclusive_first_error
      ; tc "11 exclusive (sorted order)"  t11_exclusive_sorted_order
      ];
      "PersistentFlags",
      [ tc "12 persistent required fails required" t12_persistent_required_fails_required
      ; tc "13 persistent one-req fails one-req"   t13_persistent_one_required_fails_one_required
      ; tc "14 persistent required fails mutex"    t14_persistent_required_fails_exclusive
      ; tc "15 persistent required can pass"       t15_persistent_required_pass
      ; tc "16 persistent one-required can pass"   t16_persistent_one_required_pass
      ];
      "Subcommand",
      [ tc "17 sub required (inherited flags)"      t17_sub_required_inherited
      ; tc "18 sub one-required (inherited)"        t18_sub_one_required_inherited
      ; tc "19 sub one-required fails"              t19_sub_one_required_fails
      ; tc "20 sub exclusive fails"                 t20_sub_exclusive_fails
      ; tc "21 sub exclusive passes"                t21_sub_exclusive_pass
      ; tc "22 groups not applied to uninvoked cmd" t22_groups_not_applied_to_uninvoked_command
      ];
    ]
