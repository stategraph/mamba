(** Positional-argument shape validators.

    A [spec] checks the final list of positional arguments after flag
    parsing. To validate a {e value} (e.g. "this string must be a positive
    int"), use a custom [Flag.parser] instead. *)

type spec

val none           : spec
(** Reject any positional arguments. *)

val any            : spec
(** Accept any number of positionals (the default). *)

val exactly        : int -> spec
(** Exactly [n] positionals, no more, no less. *)

val named          : string list -> spec
(** Exact arity matching [List.length names], with the names rendered
    in the Usage line: [Arg.named ["key"; "value"]] yields a Usage
    suffix of [<key> <value>] instead of the generic [<arg 2>]. *)

(** Typed pairings: [(spec, accessor)]. The accessor's return type
    matches the arity, so the names list and the read-site can't drift
    apart. Use {!named} + manual indexing for larger or dynamic arities.

    {[
      let (spec, get_kv) = Arg.named2 "key" "value" in
      Command.make ~name:"set" ~args:spec
        ~run:(fun args -> let (k, v) = get_kv args in ...)
    ]}
*)
val named1 : string -> spec * (Args.t -> string)
val named2 : string -> string -> spec * (Args.t -> string * string)
val named3 : string -> string -> string -> spec * (Args.t -> string * string * string)

(** Named variadic positional: validates "at least [min]" positionals and
    renders [<name>...] in the Usage line. [min] defaults to 1.

    {[
      Command.make ~name:"cat" ~args:(Arg.variadic "file") ~run ()
      (* Usage: cat <file>... *)
    ]}
    Use [~min:0] for "zero or more", [~min:2] for "two or more", etc. *)
val variadic : ?min:int -> string -> spec

val minimum        : int -> spec

(** Alias for {!minimum} (matches argparse / cmdliner muscle memory). *)
val at_least       : int -> spec

val maximum        : int -> spec

(** Alias for {!maximum}. *)
val at_most        : int -> spec
val range          : min:int -> max:int -> spec

val only_valid_of  : string list -> spec
(** Every positional must be in the given allow-list. *)

val custom         : (string list -> (unit, string) result) -> spec
(** Roll-your-own validator. *)

val all_of         : spec list -> spec
(** Compose validators; all must pass. *)

(** {1 Internal} *)

val check : spec -> string list -> (unit, string) result
val describe : spec -> string
(** Short, human-readable form used in [--help] usage lines. *)
