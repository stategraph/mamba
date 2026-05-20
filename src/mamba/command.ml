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

(* Smart default for ~args when not supplied:
     leaf (no subcommands) + declared flags + has run -> Arg.none
       (user signalled an input contract; stray positionals are typos)
     anything else                                    -> Arg.any
       (preserves positional-only commands and runnable groups)
   When the caller explicitly passes ~args, that always wins. *)
let default_args ~flags ~persistent_flags ~subcommands ~run =
  let is_leaf  = subcommands = [] in
  let has_flag = flags <> [] || persistent_flags <> [] in
  let has_run  = run <> None in
  if is_leaf && has_flag && has_run then Arg.none else Arg.any

let make
    ~name
    ?(aliases = [])
    ?(suggest_for = [])
    ?(short = "")
    ?(long = "")
    ?(example = "")
    ?usage
    ?args
    ?(flags = [])
    ?(persistent_flags = [])
    ?(flag_groups = [])
    ?group_id
    ?(groups = [])
    ?(subcommands = [])
    ?(hidden = false)
    ?deprecated
    ?persistent_pre_run
    ?pre_run
    ?run
    ?post_run
    ?persistent_post_run
    () =
  let args =
    Option.value args
      ~default:(default_args ~flags ~persistent_flags ~subcommands ~run)
  in
  {
    name;
    aliases;
    suggest_for;
    short;
    long;
    example;
    usage;
    args;
    flags;
    persistent_flags;
    flag_groups;
    group_id;
    groups;
    subcommands;
    hidden;
    deprecated;
    persistent_pre_run;
    pre_run;
    run;
    post_run;
    persistent_post_run;
  }

let group ~name ?aliases ?short ?long ~subcommands () =
  make ~name ?aliases ?short ?long ~subcommands ()

let find_subcommand t name =
  List.find_opt
    (fun c -> c.name = name || List.mem name c.aliases)
    t.subcommands

let all_flag_names t =
  List.map Flag.packed_name t.flags
  @ List.map Flag.packed_name t.persistent_flags
