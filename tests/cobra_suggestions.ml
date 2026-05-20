(** Port of Cobra's TestSuggestions (command_test.go).

    Cobra's [SuggestionsFor] combines three signals:
      1. Damerau-Levenshtein distance <= SuggestionsMinimumDistance (default 2)
      2. Case-insensitive prefix match
      3. Explicit SuggestFor metadata on each command (exact-match aliases)

    Mamba supports (1) (with default max_distance=3) and (3) via
    [Command.suggest_for]. Only (2) -- prefix match -- remains as a
    documented divergence. *)

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

(* Root has a single child [times] and no [run], so an unknown positional
   triggers Program.run's "unknown command" + Did-you-mean path. [times]
   advertises "counts" as a SuggestFor alias, mirroring Cobra's test setup. *)
let suggestion_root () =
  let times =
    Command.make ~name:"times" ~suggest_for:[ "counts" ] ~run:(fun _ -> 0) ()
  in
  Command.make ~name:"root" ~subcommands:[ times ] ()

let run_with_typo typo =
  let root = suggestion_root () in
  let out_buf = Buffer.create 64 in
  let err_buf = Buffer.create 256 in
  let prog =
    Program.make ~name:"root" ~version:"0" ~root
      ~help_command:false ~completion_command:false
      ~out:(Format.formatter_of_buffer out_buf)
      ~err:(Format.formatter_of_buffer err_buf)
      ()
  in
  let code = Program.run prog ~argv:[| "root"; typo |] in
  (code, Buffer.contents err_buf)

let suggests_times typo () =
  let code, err = run_with_typo typo in
  Alcotest.(check int)  "exit 2"        Error.parse_error code;
  Alcotest.(check bool) "Did-you-mean"  true (contains err {|Did you mean "times"|})

let no_suggestion typo () =
  let code, err = run_with_typo typo in
  Alcotest.(check int)  "exit 2"        Error.parse_error code;
  Alcotest.(check bool) "no suggestion" false (contains err "Did you mean")

let tc name f = Alcotest.test_case name `Quick f

let () =
  Alcotest.run "cobra_suggestions"
    [
      (* Damerau-Levenshtein distance <= 3 from "times" -- mamba and Cobra agree. *)
      "DistanceMatch",
      [ tc "time   (del s,             d=1)" (suggests_times "time")
      ; tc "tiems  (transpose em→me,   d=1)" (suggests_times "tiems")
      ; tc "tims   (del e,             d=1)" (suggests_times "tims")
      ; tc "timeS  (sub S→s,           d=1)" (suggests_times "timeS")
      ; tc "rimes  (sub t→r,           d=1)" (suggests_times "rimes")
      ; tc "timely (sub+del,           d=2)" (suggests_times "timely")
      ; tc "ti     (3 inserts,         d=3)" (suggests_times "ti")
      ];
      (* Too far for any reasonable threshold -- mamba and Cobra agree. *)
      "NoMatch",
      [ tc "ri"       (no_suggestion "ri")
      ; tc "timezone" (no_suggestion "timezone")
      ; tc "foo"      (no_suggestion "foo")
      ];
      "SuggestFor",
      [ tc "counts -> times via SuggestFor" (suggests_times "counts")
      ];
      (* Documented divergence: Cobra's prefix-match heuristic suggests
         "times" for "t" (distance 4, no Levenshtein match in mamba).
         mamba doesn't implement prefix-match. *)
      "Divergence",
      [ tc "t      (Cobra: prefix-match -> times; mamba: no)"
          (no_suggestion "t")
      ];
    ]
