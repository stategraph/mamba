(** Exit code conventions used by Mamba.

    Aligned with POSIX: [2] is reserved for "misuse" (argv parse / validator
    failures); [1] for runtime errors raised in user code; [0] for success. *)

(** [0] *)
val success : int

(** [1] *)
val runtime : int

(** [2] *)
val parse_error : int
