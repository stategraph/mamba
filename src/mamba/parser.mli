(** argv → dispatch decision.

    [Parser.dispatch] is side-effect-free: it does not touch stdout, stderr,
    or call [exit]. The caller (typically [Program.run]) decides what to do
    with the result. *)

type result =
  | Run     of { command : Command.t; path : string list; args : Args.t }
  | Help    of { command : Command.t; path : string list }
  | Version of { command : Command.t; path : string list }
  | Error   of { message : string; code : int; path : string list }

val dispatch :
  case_insensitive:bool ->
  program_name:string ->
  root:Command.t ->
  argv:string array ->
  result
