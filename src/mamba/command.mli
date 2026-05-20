(** A node in the command tree. *)

type t = {
  name                : string;
  aliases             : string list;
  suggest_for         : string list;
  (** Additional names that should suggest THIS command in did-you-mean
      output when the user mistypes any of them. Mirrors Cobra's
      [SuggestFor]. *)
  short               : string;
  long                : string;
  example             : string;
  usage               : string option;
  (** Overrides the args portion of the auto-generated Usage line.
      Set to e.g. ["<name>..."] so help reads ["pkg install [flags] <name>..."]
      instead of the generic ["<arg> [args...]"]. *)
  args                : Arg.spec;
  flags               : Flag.packed list;
  persistent_flags    : Flag.packed list;
  flag_groups         : Flag_group.t list;
  group_id            : string option;
  (** ID of the group this command belongs to in its parent's help layout.
      The parent's [groups] list must contain this ID. *)
  groups              : (string * string) list;
  (** Groups this command DEFINES to organize its children's help output.
      [(id, title)] pairs; rendered in declaration order. Children whose
      [group_id] matches an entry are bucketed under [title]; ungrouped
      children fall under "Additional Commands". *)
  subcommands         : t list;
  hidden              : bool;
  deprecated          : string option;
  persistent_pre_run  : Hook.t option;
  pre_run             : Hook.t option;
  run                 : (Args.t -> int) option;
  post_run            : Hook.t option;
  persistent_post_run : Hook.t option;
}

(** Smart constructor with sensible defaults for every optional field. *)
val make :
  name:string ->
  ?aliases:string list ->
  ?suggest_for:string list ->
  ?short:string ->
  ?long:string ->
  ?example:string ->
  ?usage:string ->
  ?args:Arg.spec ->
  ?flags:Flag.packed list ->
  ?persistent_flags:Flag.packed list ->
  ?flag_groups:Flag_group.t list ->
  ?group_id:string ->
  ?groups:(string * string) list ->
  ?subcommands:t list ->
  ?hidden:bool ->
  ?deprecated:string ->
  ?persistent_pre_run:Hook.t ->
  ?pre_run:Hook.t ->
  ?run:(Args.t -> int) ->
  ?post_run:Hook.t ->
  ?persistent_post_run:Hook.t ->
  unit -> t

(** Convenience: a command with subcommands but no [run]. Invoking it shows
    the help for the group. *)
val group :
  name:string ->
  ?aliases:string list ->
  ?short:string ->
  ?long:string ->
  subcommands:t list ->
  unit -> t

(** Find a direct child matching [name] or any of its aliases. *)
val find_subcommand : t -> string -> t option

(** All flag names registered on this command (local + persistent), used
    by completion and help. *)
val all_flag_names : t -> string list
