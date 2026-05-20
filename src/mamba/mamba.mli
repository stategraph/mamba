(** A CLI library for OCaml, inspired by Go's Cobra.

    {1 Quick start}

    {[
      open Mamba

      let count = Flag.int  ~name:"count" ~short:'n' ~default:1     ~doc:"how many" ()
      let upper = Flag.bool ~name:"upper" ~default:false             ~doc:"uppercase" ()

      let say =
        Command.make ~name:"say" ~short:"print a greeting"
          ~args:(Arg.named1 "who" |> fst)     (* names the positional in --help *)
          ~flags:[Flag.pack count; Flag.pack upper]
          ~run:(fun args ->
            let n   = Args.get args count in
            let u   = Args.get args upper in
            let who = Args.positional_1 args in
            for _ = 1 to n do
              print_endline (if u then String.uppercase_ascii who else who)
            done;
            0)
          ()

      let root = Command.make ~name:"hello" ~subcommands:[say] ()
      let () = Program.make ~name:"hello" ~version:"0.1.0" ~root () |> Program.run_exn
    ]}

    {1 Big picture}

    - {!Flag} declares typed flags; pack them with {!Flag.pack} to attach to a
      command.
    - {!Arg} declares the positional-args shape (count, names, validators).
    - {!Command.make} composes flags, args, hooks, and subcommands into an
      immutable command tree.
    - {!Program.make} wires the tree into a runnable program with help,
      completion, and version-command auto-injection.
    - {!Args} is the runtime accessor a command's [~run] callback receives.

    See the README for "Common patterns" (variadics, mutex flag groups,
    optional flags, etc.) and "Tips and pitfalls". *)

module Flag : sig
  type 'a parser  = string -> ('a, string) result
  type 'a printer = 'a -> string

  type _ kind =
    | Value  : _ kind
    | Switch : _ kind
    | Count  : int kind
    | Multi  : _ kind

  type 'a t

  val make :
    name:string ->
    ?short:char ->
    ?aliases:string list ->
    ?env:string ->
    ?default:'a ->
    ?required:bool ->
    ?hidden:bool ->
    ?deprecated:string ->
    ?placeholder:string ->
    ?kind:'a kind ->
    ?multi_combine:('a -> 'a -> 'a) ->
    ?multi_empty:'a ->
    doc:string ->
    parser:'a parser ->
    printer:'a printer ->
    unit -> 'a t

  val bool   : name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:bool   -> ?hidden:bool -> ?deprecated:string -> doc:string -> unit -> bool   t
  val int    : name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:int    -> ?required:bool -> ?hidden:bool -> ?deprecated:string -> ?placeholder:string -> doc:string -> unit -> int    t
  val string : name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:string -> ?required:bool -> ?hidden:bool -> ?deprecated:string -> ?placeholder:string -> doc:string -> unit -> string t
  val float  : name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:float  -> ?required:bool -> ?hidden:bool -> ?deprecated:string -> ?placeholder:string -> doc:string -> unit -> float  t

  val enum :
    name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:'a ->
    values:(string * 'a) list -> doc:string -> unit -> 'a t

  val path :
    name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:string ->
    ?must_exist:bool -> doc:string -> unit -> string t

  val list     : sep:char -> 'a t -> 'a list t
  val repeated : 'a t -> 'a list t
  val count    : name:string -> short:char -> doc:string -> unit -> int t

  type packed = P : 'a t -> packed
  val pack : 'a t -> packed

  val name        : 'a t -> string
  val short       : 'a t -> char option
  val aliases     : 'a t -> string list
  val env         : 'a t -> string option
  val default     : 'a t -> 'a option
  val required    : 'a t -> bool
  val doc         : 'a t -> string
end

(** Runtime view of one command invocation: parsed flag values, positional
    arguments, raw passthrough tokens, and the resolved command path.
    A command's [~run] callback receives this. *)
module Args : sig
  type t

  (** Raised by {!get} when a flag has no value from argv, env, or default.
      Lifecycle catches it and renders as
      [error: required flag --<name> not set]. *)
  exception Missing_flag of string

  (** {2 Flag values} *)

  (** Look up a flag value. Raises {!Missing_flag} if the flag has no
      value by any source (argv / env / default). *)
  val get        : t -> 'a Flag.t -> 'a

  (** Look up a flag value, returning [None] if it has no value by any
      source. Use this for truly optional flags (omit both [~default]
      and [~required] when constructing the flag). *)
  val get_opt    : t -> 'a Flag.t -> 'a option

  (** {2 Positionals} *)

  (** All positional arguments in argv order. Includes tokens after a
      literal [--] separator (POSIX: [--] ends flag interpretation but
      doesn't hide the trailing tokens). *)
  val positional : t -> string list

  (** Tokens after a literal [--] separator. These also appear in
      {!positional}; exposed separately for wrapper tools that need to
      forward them to a child process. *)
  val raw        : t -> string list

  (** Fixed-arity convenience accessors. Pair with {!Arg.exactly} or
      {!Arg.named}: if your command sets [~args:(Arg.exactly 2)],
      {!positional_2} returns the pair directly. Raise
      [Invalid_argument] on arity mismatch. For type-safe pairing of
      spec+accessor, prefer {!Arg.named1} / {!Arg.named2} / {!Arg.named3}. *)
  val positional_at : t -> int -> string
  val positional_1  : t -> string
  val positional_2  : t -> string * string
  val positional_3  : t -> string * string * string

  (** {2 Introspection} *)

  (** The resolved command path including the program name, root-to-leaf.
      e.g. [["myapp"; "remote"; "add"]] for [myapp remote add origin URL].
      Useful inside hooks to branch on which subcommand is being run. *)
  val cmd_path   : t -> string list

  (** [was_set args name] is [true] iff the flag with the given long name
      was explicitly given in argv. Defaults and env-derived values are
      not considered "set". *)
  val was_set    : t -> string -> bool
end

(** Positional-argument shape validators.

    Defaults: when {!Command.make}'s [?args] is omitted, mamba picks
    {!none} for leaf commands that declare named flags + a run callback
    (the implicit contract is "all inputs are flags"; stray positionals
    are typos), and {!any} otherwise. Pass [?args] explicitly to override. *)
module Arg : sig
  type spec

  (** Reject any positional arguments. *)
  val none           : spec

  (** Accept any number of positional arguments. *)
  val any            : spec

  val exactly        : int -> spec

  (** Like {!exactly}, but names each positional so the Usage line shows
      e.g. [<key> <value>] instead of [<arg 2>]. *)
  val named          : string list -> spec

  (** Typed pairings of (spec, accessor) so the names list and read-site
      can't drift apart. Use {!named} + manual indexing for >3 or
      dynamic arity.

      {[
        let (spec, get_kv) = Arg.named2 "key" "value" in
        Command.make ~args:spec
          ~run:(fun args -> let (k, v) = get_kv args in ...)
      ]} *)
  val named1 : string -> spec * (Args.t -> string)
  val named2 : string -> string -> spec * (Args.t -> string * string)
  val named3 : string -> string -> string -> spec * (Args.t -> string * string * string)

  (** Named variadic: yields [<name>...] in the Usage line. Default
      [?min:1] requires at least one positional; [?min:0] allows zero.

      {[
        Command.make ~name:"cat" ~args:(Arg.variadic "file") ~run ()
        (* Usage: cat <file>... *)
      ]} *)
  val variadic : ?min:int -> string -> spec

  (** "At least [n] positionals." Two spellings — pick the one that
      reads better in your code. *)
  val minimum        : int -> spec
  val at_least       : int -> spec

  (** "At most [n] positionals." Two spellings — pick the one that
      reads better in your code. *)
  val maximum        : int -> spec
  val at_most        : int -> spec
  val range          : min:int -> max:int -> spec
  val only_valid_of  : string list -> spec
  val custom         : (string list -> (unit, string) result) -> spec
  val all_of         : spec list -> spec
end

(** A lifecycle hook for a command. Returning [Some n] short-circuits
    the run with exit code [n] (skips later hooks and the [run] callback);
    [None] proceeds to the next hook. *)
module Hook : sig
  type t = Args.t -> int option
end

(** Constraints across multiple flags. Three shapes:

    - {!required_together}: if any flag in the group is set, all must be set.
    - {!one_required}: at least one of the flags must be set.
    - {!mutually_exclusive}: at most one of the flags can be set.

    Constraints are checked after argv parsing, on the leaf command's
    flag groups. Violations exit with code 2 and a short message.
    Members are also annotated in [--help] output. *)
module Flag_group : sig
  type t

  (** Type-safe primary constructors taking the flags themselves.
      Typos are compile-time errors. *)
  val required_together  : Flag.packed list -> t
  val one_required       : Flag.packed list -> t
  val mutually_exclusive : Flag.packed list -> t

  (** Name-keyed variants for dynamic-name use cases (config-driven, etc.).
      Names not registered on the command are caught at {!Program.make}
      time via {!Program.validate}. *)
  val required_together_by_name  : string list -> t
  val one_required_by_name       : string list -> t
  val mutually_exclusive_by_name : string list -> t
end

module Command : sig
  type t = {
    name                : string;
    aliases             : string list;
    suggest_for         : string list;
    short               : string;
    long                : string;
    example             : string;
    usage               : string option;
    args                : Arg.spec;
    flags               : Flag.packed list;
    persistent_flags    : Flag.packed list;
    flag_groups         : Flag_group.t list;
    group_id            : string option;
    groups              : (string * string) list;
    subcommands         : t list;
    hidden              : bool;
    deprecated          : string option;
    persistent_pre_run  : Hook.t option;
    pre_run             : Hook.t option;
    run                 : (Args.t -> int) option;
    post_run            : Hook.t option;
    persistent_post_run : Hook.t option;
  }

  (** Build a command. Every optional field has a sensible default.

      Notable behaviour:
      - When [?args] is omitted, mamba picks {!Arg.none} for leaf commands
        that declare named flags + a [~run] callback (input is via flags;
        stray positionals are typos), and {!Arg.any} otherwise. Pass
        [?args] explicitly to override (e.g. [~args:(Arg.variadic "file")]
        for a positional-taking command).
      - [?run] is what gets invoked. A command without [~run] is a "group"
        — invoking it without a subcommand prints help.
      - [?aliases] / [?suggest_for]: alternative spellings; suggest_for
        adds did-you-mean hits without making the alternative invokable.
      - See the README "Builder parameter cheatsheet" for the full list. *)
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

  val group :
    name:string ->
    ?aliases:string list ->
    ?short:string ->
    ?long:string ->
    subcommands:t list ->
    unit -> t
end

(** Dispatch result returned by {!Program.dispatch}. *)
module Parser : sig
  type result =
    | Run     of { command : Command.t; path : string list; args : Args.t }
    | Help    of { command : Command.t; path : string list }
    | Version of { command : Command.t; path : string list }
    | Error   of { message : string; code : int; path : string list }
end

module Program : sig
  type t

  (** Build a runnable program.

      Auto-injection: when not overridden, [Program.make] adds three
      subcommands to the root:
      - [help]       — see {!completion_command}, default [true]
      - [completion] — emits bash/zsh/fish scripts; default [true]
      - [version]    — prints [<name> version <version>]; default [true]
                       when [~version] is non-empty

      Each is skipped automatically if the user already declared a
      subcommand with the same name.

      Other notable parameters:
      - [?*_command_group_id]: places the auto-injected subcommands
        under a user-declared group on the root.
      - [?case_insensitive]: makes subcommand and alias matching
        case-insensitive (e.g. [APP CHILD] and [app child] both work).
      - [?color]: [`Auto] respects terminal detection + NO_COLOR;
        [`Always] forces ANSI on; [`Never] forces off. Default [`Auto]. *)
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

  val run     : ?argv:string array -> t -> int
  val run_exn : ?argv:string array -> t -> 'a

  (** Side-effect-free dispatcher: parse argv into a {!Parser.result}
      without running any hooks, printing help, or calling [exit]. *)
  val dispatch : t -> argv:string array -> Parser.result

  val validate : t -> (unit, string) result
end

(** {1 Exposed for advanced use} *)

(** Exit-code conventions. *)
module Error : sig
  val success     : int
  val runtime     : int
  val parse_error : int
end

(** Damerau-Levenshtein string distance (used internally for "did you
    mean?" suggestions; exposed in case users want their own variants). *)
module Suggest : sig
  val distance : string -> string -> int
  val closest  : ?max_distance:int -> string -> string list -> string list
end

(** Shell-completion script generators. Used by the auto-registered
    [completion] subcommand, also callable directly. *)
module Completion : sig
  type shell = Bash | Zsh | Fish
  val of_string    : string -> (shell, string) result
  val name_of_shell : shell -> string
  val emit :
    out:Format.formatter ->
    shell:shell ->
    program_name:string ->
    root:Command.t ->
    unit
end

(** groff man-page generator. *)
module Man : sig
  val emit :
    out:Format.formatter ->
    program_version:string ->
    command_path:string list ->
    command:Command.t ->
    unit
  val write_all :
    dir:string ->
    program_name:string ->
    program_version:string ->
    root:Command.t ->
    string list
end
