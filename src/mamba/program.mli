(** A configured CLI program ready to be [run]. *)

type t

val make :
  name:string ->
  version:string ->
  ?description:string ->
  ?author:string ->
  ?completion_command:bool ->
  ?help_command:bool ->
  ?version_command:bool ->
  ?help_command_group_id:string ->
  ?completion_command_group_id:string ->
  ?version_command_group_id:string ->
  ?case_insensitive:bool ->
  ?color:[ `Auto | `Always | `Never ] ->
  ?out:Format.formatter ->
  ?err:Format.formatter ->
  root:Command.t ->
  unit -> t

(** Run the program. Returns the exit code; does {b not} call [exit]. *)
val run : ?argv:string array -> t -> int

(** Run the program and exit the process with the returned code. *)
val run_exn : ?argv:string array -> t -> 'a

(** Side-effect-free dispatcher; useful for tests. *)
val dispatch : t -> argv:string array -> Parser.result

(** Validate the tree at construction time: duplicate subcommand names,
    duplicate flag names, etc. Errors are returned as a human-readable
    string. [make] also calls this and raises [Invalid_argument] on failure. *)
val validate : t -> (unit, string) result

(** Accessors. *)

val name           : t -> string
val version        : t -> string
val effective_root : t -> Command.t
val out            : t -> Format.formatter
val err            : t -> Format.formatter
val color          : t -> [ `Auto | `Always | `Never ]
