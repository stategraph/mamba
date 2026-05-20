(** Typed command-line flags.

    Each [_ Flag.t] carries a [Stdlib.Type.Id] witness so that a later
    {!Mamba.Args.get} call recovers the original type with no casts and no
    string-keyed lookups. *)

type 'a parser = string -> ('a, string) result
type 'a printer = 'a -> string

(** How a flag consumes argv.

    Encoded as a GADT so that {!count} is statically tied to [int] -- this
    lets the parser store and retrieve count values without unsafe casts. *)
type _ kind =
  | Value  : _ kind
    (** Takes one argument: [--port 8080] or [--port=8080] or [-p 8080] or [-p8080]. *)
  | Switch : _ kind
    (** No argument: presence sets the value. Optional explicit form:
        [--flag=true] / [--flag=false]. *)
  | Count  : int kind
    (** No argument: each occurrence increments. Always typed as [int t]. *)
  | Multi  : _ kind
    (** Repeatable. Each occurrence is parsed independently and the results
        are folded with [multi_combine] starting from [multi_empty]. Used
        internally by {!list} and {!repeated}; you usually don't construct
        a [Multi] flag by hand. *)

type 'a t

(** Generic flag constructor. Prefer the typed builders below where they fit.
    [?multi_combine] and [?multi_empty] are required for {!Multi}-kind flags
    (the parser uses them to fold per-occurrence values into one result); for
    other kinds they're ignored. *)
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

(** {1 Typed builders} *)

val bool   : name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:bool   -> ?hidden:bool -> ?deprecated:string -> doc:string -> unit -> bool   t
val int    : name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:int    -> ?required:bool -> ?hidden:bool -> ?deprecated:string -> ?placeholder:string -> doc:string -> unit -> int    t
val string : name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:string -> ?required:bool -> ?hidden:bool -> ?deprecated:string -> ?placeholder:string -> doc:string -> unit -> string t
val float  : name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:float  -> ?required:bool -> ?hidden:bool -> ?deprecated:string -> ?placeholder:string -> doc:string -> unit -> float  t

(** Enumerated choice. The first matching value wins; printing uses the
    first key whose [a = v] (structural). *)
val enum :
  name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:'a ->
  values:(string * 'a) list -> doc:string -> unit -> 'a t

(** Filesystem path. If [must_exist] is [true], parsing fails when the file
    is not present. *)
val path :
  name:string -> ?short:char -> ?aliases:string list -> ?env:string -> ?default:string ->
  ?must_exist:bool -> doc:string -> unit -> string t

(** Separator-based list (pflag's [StringSlice] analogue). Each occurrence's
    value is split on [sep] and items are accumulated across occurrences,
    so [--tags=a,b --tags=c] yields [\["a"; "b"; "c"\]]. *)
val list : sep:char -> 'a t -> 'a list t

(** Repeated single-value flag (pflag's [StringArray] analogue). Each
    occurrence contributes one item; no splitting. So [-f a.yaml -f b.yaml]
    yields [\["a.yaml"; "b.yaml"\]]. Use this when the item itself might
    contain a separator character like comma. *)
val repeated : 'a t -> 'a list t

(** Count flag: [-vvv] sets the value to [3]. The [short] character is
    required because count semantics are only useful for short flags. *)
val count : name:string -> short:char -> doc:string -> unit -> int t

(** {1 Type-erased registration} *)

(** Existential wrapper so flags of differing element types can sit in the
    same [list] inside a [Command.t]. *)
type packed = P : 'a t -> packed

val pack : 'a t -> packed

(** {1 Accessors} *)

val name          : 'a t -> string
val short         : 'a t -> char option
val aliases       : 'a t -> string list
val env           : 'a t -> string option
val default       : 'a t -> 'a option
val required      : 'a t -> bool
val hidden        : 'a t -> bool
val deprecated    : 'a t -> string option
val placeholder   : 'a t -> string option
val doc           : 'a t -> string
val parser        : 'a t -> 'a parser
val printer       : 'a t -> 'a printer
val kind          : 'a t -> 'a kind
val multi_combine : 'a t -> ('a -> 'a -> 'a) option
val multi_empty   : 'a t -> 'a option
val type_id       : 'a t -> 'a Type.Id.t

(** Same as [name], but for a packed flag. *)
val packed_name : packed -> string

(** Pretty short name of a flag for help/error output:
    [--port, -p] or [--port] depending on whether a [short] is set. *)
val display : 'a t -> string
val packed_display : packed -> string
