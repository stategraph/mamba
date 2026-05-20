(** Lifecycle hooks.

    A hook returns [None] to continue, [Some n] to short-circuit the
    pipeline with exit code [n]. Uncaught exceptions in a hook are caught
    by [Program.run] and become exit code {!Error.runtime}. *)

type t = Args.t -> int option
