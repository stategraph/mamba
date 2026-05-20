(** A parsed command-line: flag values, positional arguments, raw passthrough,
    and the resolved command path. *)

type t

(** Raised by {!get} when the flag has no parsed value, no default, no env
    fallback, and was not supplied in argv. The string is the flag's long
    name. Caught by [Lifecycle.run] and rendered as a user-facing error,
    so user code can usually let it propagate. *)
exception Missing_flag of string

(** Look up a flag value. Raises {!Missing_flag} if the flag has no value
    by any source (argv / env / default). *)
val get : t -> 'a Flag.t -> 'a

(** Same as {!get} but returns [None] instead of raising. *)
val get_opt : t -> 'a Flag.t -> 'a option

(** Positional arguments in argv order. Includes tokens that appear
    after a literal [--] separator (POSIX semantics — [--] ends flag
    parsing but doesn't hide the trailing tokens from positional
    accounting). *)
val positional : t -> string list

(** {2 Fixed-arity accessors}

    These pair-by-convention with {!Arg.exactly}: if your command sets
    [~args:(Arg.exactly 2)], then {!positional_2} in the run callback
    returns the two values as a tuple directly. They raise
    [Invalid_argument] if the actual arity doesn't match (which should
    not happen if {!Arg.check} has run, but is a programmer error). *)

val positional_at : t -> int -> string
(** [positional_at args i] is the [i]th positional argument. *)

val positional_1 : t -> string
val positional_2 : t -> string * string
val positional_3 : t -> string * string * string

(** Tokens after a literal [--] separator. These also appear in
    {!positional}; mamba exposes them separately so wrapper tools can
    forward them to a child process without re-splitting on [--]. *)
val raw : t -> string list

(** Resolved command path including the program name, root-to-leaf.
    e.g. [["myapp"; "remote"; "add"]] for [myapp remote add origin URL]. *)
val cmd_path : t -> string list

(** [was_set args name] is [true] iff the named flag was explicitly given
    in argv. Defaults and env-derived values are not considered "set". *)
val was_set : t -> string -> bool

(** {1 Internal — used by Parser} *)

type entry = Entry : 'a Flag.t * 'a -> entry

val make :
  ?set_flags:string list ->
  entries:entry list ->
  positional:string list ->
  raw:string list ->
  cmd_path:string list ->
  unit -> t
