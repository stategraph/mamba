(** Shell-completion script generation. *)

type shell = Bash | Zsh | Fish

val of_string : string -> (shell, string) result

val name_of_shell : shell -> string

(** Emit a completion script for [program_name] driving the command tree
    rooted at [root]. The script is written to [out] without a trailing
    page break. *)
val emit :
  out:Format.formatter ->
  shell:shell ->
  program_name:string ->
  root:Command.t ->
  unit
