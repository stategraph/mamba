(* ANSI color helpers. No public .mli on purpose -- this is internal. *)

let isatty_stdout () =
  try Unix.isatty Unix.stdout with _ -> false

let want_color = function
  | `Always -> true
  | `Never  -> false
  | `Auto   ->
    Sys.getenv_opt "NO_COLOR" = None
    && Sys.getenv_opt "TERM" <> Some "dumb"
    && isatty_stdout ()

let wrap ~color code s =
  if color then "\027[" ^ code ^ "m" ^ s ^ "\027[0m" else s

let bold   ~color s = wrap ~color "1"  s
let dim    ~color s = wrap ~color "2"  s
let red    ~color s = wrap ~color "31" s
let yellow ~color s = wrap ~color "33" s
let cyan   ~color s = wrap ~color "36" s
