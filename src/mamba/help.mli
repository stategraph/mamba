(** Cobra-shaped help renderer. *)

val render :
  ?has_version:bool ->
  out:Format.formatter ->
  color:bool ->
  path_commands:Command.t list ->
  command:Command.t ->
  unit -> unit

(** Short error message + a "Run --help for usage" tail. *)
val render_error :
  err:Format.formatter ->
  color:bool ->
  path_commands:Command.t list ->
  message:string ->
  unit
