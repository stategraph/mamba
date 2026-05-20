(** Run the lifecycle of a resolved command.

    Hook order for a path [root -> A -> B] (B being the resolved leaf):

    {ol
      {- [persistent_pre_run] of root, A, B (every set hook fires).}
      {- [pre_run] of B.}
      {- [run] of B.}
      {- [post_run] of B.}
      {- [persistent_post_run] of B, A, root.}}

    A hook returning [Some n] short-circuits the chain with exit code [n].
    [post_run] / [persistent_post_run] do not fire after a short-circuit.

    Uncaught exceptions in user code are caught and turned into exit code
    {!Error.runtime}; the exception is printed to [err]. *)

val run :
  err:Format.formatter ->
  path_commands:Command.t list ->
  args:Args.t ->
  int
