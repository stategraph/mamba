(** groff (man 7) man-page generator.

    Generates one man page per node in the command tree, suitable for
    installation under section 1 of [MANPATH]. *)

(** Render a single man page for [command] given its full path
    (e.g. [["myapp"; "remote"; "add"]]) to [out]. *)
val emit :
  out:Format.formatter ->
  program_version:string ->
  command_path:string list ->
  command:Command.t ->
  unit

(** Walk the command tree and write one man-page file per node into
    [dir]. Files are named [program-name.1], [program-name-cmd.1],
    [program-name-cmd-subcmd.1], etc. Returns the list of paths written
    (in tree-traversal order). *)
val write_all :
  dir:string ->
  program_name:string ->
  program_version:string ->
  root:Command.t ->
  string list
