(** Flag-group constraints attached per-command via [Command.flag_groups].

    Three rule shapes, mirroring Cobra/pflag:
      - {!required_together}: if any flag in the group is set in argv, all
        must be set.
      - {!one_required}: at least one of the flags must be set.
      - {!mutually_exclusive}: at most one of the flags can be set.

    "Set" means explicitly supplied in argv -- defaults and env-derived
    values do not count. Validation runs against the leaf command after
    parsing; flag groups on ancestor commands aren't applied when a
    descendant is invoked. *)

type t

(** {1 Primary, type-safe constructors}

    These take the actual [Flag.t]s (packed). A typo on a flag is a
    compile-time error, and {!Program.validate} additionally verifies that
    each referenced flag is registered on the command. *)

val required_together  : Flag.packed list -> t
val one_required       : Flag.packed list -> t
val mutually_exclusive : Flag.packed list -> t

(** {1 Name-keyed escape hatch}

    Use these when flag names are computed dynamically (e.g. from a config
    file). Typos won't be caught at compile time; {!Program.validate} still
    catches unknown-name references at construction. *)

val required_together_by_name  : string list -> t
val one_required_by_name       : string list -> t
val mutually_exclusive_by_name : string list -> t

(** Names of the flags this group references, in declaration order.
    Used by {!Program.validate} and by the help renderer. *)
val flag_names : t -> string list

(** Validate one rule. [is_set name] should return [true] iff the named
    flag was explicitly set in argv. *)
val check : t -> (string -> bool) -> (unit, string) result

(** [check_all rs is_set] returns the first violation in declaration order,
    [Ok ()] if all pass. *)
val check_all : t list -> (string -> bool) -> (unit, string) result

(** Tag indicating which constraint a rule encodes (for help annotation). *)
type kind = Required_together_k | One_required_k | Mutually_exclusive_k
val kind : t -> kind
