let pp_section ~out ~color title =
  Format.fprintf out "%s@." (Style.bold ~color title)

let flag_display (Flag.P f) =
  let head =
    match Flag.short f with
    | Some c -> Printf.sprintf "-%c, --%s" c (Flag.name f)
    | None   -> Printf.sprintf "    --%s" (Flag.name f)
  in
  match Flag.kind f with
  | Flag.Value | Flag.Multi ->
    head ^ " " ^ Option.value ~default:"<value>" (Flag.placeholder f)
  | Flag.Switch | Flag.Count -> head

let flag_doc (Flag.P f) =
  let d = Flag.doc f in
  let with_default =
    match Flag.default f with
    | Some v ->
      (* For list/multi flags, the default printed value is the empty
         string when default = []; "(default: )" reads as broken. Suppress.
         For Switch flags, the default is conventionally false (= absent);
         "(default: false)" is just noise. Suppress that too. *)
      let printed = (Flag.printer f) v in
      let is_switch_false =
        match Flag.kind f with
        | Flag.Switch -> printed = "false"
        | Flag.Value | Flag.Count | Flag.Multi -> false
      in
      if printed = "" || is_switch_false then d
      else Printf.sprintf "%s (default: %s)" d printed
    | None ->
      if Flag.required f then d ^ " (required)" else d
  in
  match Flag.env f with
  | Some e -> Printf.sprintf "%s [env: %s]" with_default e
  | None   -> with_default

(* Append constraint notes for any flag-group memberships on this command.
   Renders "(mutually exclusive with --x)", "(required with --y)", or
   "(at least one of --x, --y, --z)" so users see group constraints in
   --help instead of discovering them only at runtime. *)
let flag_doc_with_groups (Flag.P f) (cmd : Command.t) =
  let base = flag_doc (Flag.P f) in
  let flag_name = Flag.name f in
  let fmt_others names =
    names
    |> List.filter (fun n -> n <> flag_name)
    |> List.map (fun n -> "--" ^ n)
    |> String.concat ", "
  in
  let notes =
    List.filter_map (fun grp ->
      let names = Flag_group.flag_names grp in
      if not (List.mem flag_name names) then None
      else
        match Flag_group.kind grp with
        | Flag_group.Mutually_exclusive_k ->
          Some (Printf.sprintf "(mutually exclusive with %s)"
                  (fmt_others names))
        | Flag_group.Required_together_k ->
          Some (Printf.sprintf "(must be set together with %s)"
                  (fmt_others names))
        | Flag_group.One_required_k ->
          Some (Printf.sprintf "(at least one of %s required)"
                  (String.concat ", " (List.map (fun n -> "--" ^ n) names))))
      cmd.Command.flag_groups
  in
  match notes with
  | [] -> base
  | _ -> base ^ " " ^ String.concat " " notes

let render_two_column ~out items =
  if items = [] then ()
  else
    let max_left =
      List.fold_left (fun acc (l, _) -> max acc (String.length l)) 0 items
    in
    List.iter
      (fun (l, r) -> Format.fprintf out "  %-*s   %s@." max_left l r)
      items

let path_str path_commands =
  String.concat " " (List.map (fun (c : Command.t) -> c.name) path_commands)

let visible_flags flags =
  List.filter
    (fun (Flag.P f) -> not (Flag.hidden f) && Flag.deprecated f = None)
    flags

let render ?(has_version = false) ~out ~color ~path_commands ~command () =
  let full_name = path_str path_commands in
  let description =
    if command.Command.long <> "" then command.Command.long
    else command.Command.short
  in
  if description <> "" then
    Format.fprintf out "%s@.@." description;
  (* Usage *)
  pp_section ~out ~color "Usage:";
  let has_subs = command.Command.subcommands <> [] in
  let args_part =
    match command.Command.usage with
    | Some s -> " " ^ s   (* user-supplied; trust them *)
    | None ->
      let argspec = Arg.describe command.Command.args in
      (* Suppress the generic "[args...]" token (= the Arg.any output)
         when the command has subcommands: it just clutters the usage
         line. Explicit specs like "<arg 2>" still render because they
         convey real information. *)
      if argspec = "" then ""
      else if has_subs && argspec = "[args...]" then ""
      else " " ^ argspec
  in
  let flags_part =
    if command.Command.flags = [] && command.Command.persistent_flags = []
    then ""
    else " [flags]"
  in
  Format.fprintf out "  %s%s%s@." full_name flags_part args_part;
  (match command.Command.subcommands with
   | [] -> ()
   | _ -> Format.fprintf out "  %s [command]@." full_name);
  Format.fprintf out "@.";
  (* Aliases -- list ONLY the aliases, not the canonical name. Listing
     "install, i" reads as if there are two aliases when only `i` is one. *)
  (match command.Command.aliases with
   | [] -> ()
   | aliases ->
     pp_section ~out ~color "Aliases:";
     Format.fprintf out "  %s@.@." (String.concat ", " aliases));
  (* Examples *)
  if command.Command.example <> "" then begin
    pp_section ~out ~color "Examples:";
    Format.fprintf out "%s@.@." command.Command.example
  end;
  (* Subcommands -- ungrouped or bucketed by [groups] *)
  let visible_subs =
    List.filter (fun (c : Command.t) -> not c.hidden) command.Command.subcommands
  in
  let render_section heading items =
    if items <> [] then begin
      pp_section ~out ~color heading;
      let rendered =
        List.map (fun (c : Command.t) -> (c.name, c.short)) items
      in
      render_two_column ~out rendered;
      Format.fprintf out "@."
    end
  in
  (* Auto-injected built-ins (help / completion / version) are pulled out
     into "Additional Commands:" so they don't sit next to user commands
     as if equal peers. Detection is by well-known name -- users are
     unlikely to define top-level commands with these names, and the
     hardcoding is contained to this rendering decision. *)
  let is_builtin (c : Command.t) =
    match c.name with
    | "help" | "completion" | "version" -> true
    | _ -> false
  in
  (match visible_subs, command.Command.groups with
   | [], _ -> ()
   | subs, [] ->
     (* No user-declared groups: split user commands from built-ins. *)
     let user_subs    = List.filter (fun c -> not (is_builtin c)) subs in
     let builtin_subs = List.filter is_builtin subs in
     render_section "Available Commands:"  user_subs;
     render_section "Additional Commands:" builtin_subs
   | subs, groups ->
     (* Render each declared group in order, then "Additional Commands:"
        for any child without a [group_id] or with an unknown group_id
        (validate should have rejected unknown IDs at construction). *)
     List.iter (fun (gid, title) ->
       let in_group =
         List.filter
           (fun (c : Command.t) -> c.group_id = Some gid)
           subs
       in
       render_section (title ^ ":") in_group)
       groups;
     let group_ids = List.map fst groups in
     let ungrouped =
       List.filter (fun (c : Command.t) ->
         match c.group_id with
         | None -> true
         | Some g -> not (List.mem g group_ids))
         subs
     in
     render_section "Additional Commands:" ungrouped);
  (* Local flags + synthetic -h/--help and --version (root only).
     mamba handles --help and -v|--version specially in the parser, so they
     aren't registered as user flags. Render them anyway so the help output
     matches user expectations from Cobra/kubectl. *)
  let synthetic =
    let help_entry = ("-h, --help", "help for " ^ command.Command.name) in
    let version_entry =
      if has_version && path_commands <> [] &&
         List.length path_commands = 1
      then [ ("    --version", "version for " ^ command.Command.name) ]
      else []
    in
    help_entry :: version_entry
  in
  let local = visible_flags command.Command.flags in
  if local <> [] || synthetic <> [] then begin
    pp_section ~out ~color "Flags:";
    let items =
      List.map (fun f -> (flag_display f, flag_doc_with_groups f command)) local
    in
    render_two_column ~out (items @ synthetic);
    Format.fprintf out "@."
  end;
  (* Persistent (global) flags from ancestors + own persistent *)
  let ancestor_persistent =
    let rec ancestors = function
      | [] | [_] -> []
      | x :: rest -> x :: ancestors rest
    in
    List.concat_map
      (fun (c : Command.t) -> c.persistent_flags)
      (ancestors path_commands)
  in
  let own_persistent = command.Command.persistent_flags in
  let all_persistent = visible_flags (ancestor_persistent @ own_persistent) in
  (match all_persistent with
   | [] -> ()
   | _ ->
     pp_section ~out ~color "Global Flags:";
     let items =
       List.map
         (fun f -> (flag_display f, flag_doc_with_groups f command))
         all_persistent
     in
     render_two_column ~out items;
     Format.fprintf out "@.");
  (* Footer *)
  if visible_subs <> [] then
    Format.fprintf out
      "Use %S for more information about a command.@."
      (full_name ^ " [command] --help")

let render_error ~err ~color ~path_commands ~message =
  let prefix = Style.red ~color "error:" in
  Format.fprintf err "%s %s@." prefix message;
  let full_name = path_str path_commands in
  Format.fprintf err "Run %S for usage.@." (full_name ^ " --help")
