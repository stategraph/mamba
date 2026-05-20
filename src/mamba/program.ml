type t = {
  name             : string;
  version          : string;
  description      : string;
  author           : string;
  color_mode       : [ `Auto | `Always | `Never ];
  case_insensitive : bool;
  out_             : Format.formatter;
  err_             : Format.formatter;
  effective_root_  : Command.t;
}
[@@warning "-69"]
(* description / author are reserved for the man-page generator. *)

(* Walk a string-path under [root], collecting Command.t at each step.
   Returns the full root..leaf chain. The first element of [path] should
   equal [root.name]; we drop it. Respects [case_insensitive] so paths
   recorded via case-insensitive matching still resolve. *)
let build_path_commands ~case_insensitive (root : Command.t) (path : string list)
  : Command.t list =
  let eq a b =
    if case_insensitive
    then String.lowercase_ascii a = String.lowercase_ascii b
    else String.equal a b
  in
  let find_sub (cmd : Command.t) name =
    List.find_opt
      (fun (c : Command.t) ->
        eq c.Command.name name || List.exists (eq name) c.Command.aliases)
      cmd.Command.subcommands
  in
  let rec walk (cmd : Command.t) acc = function
    | [] -> List.rev (cmd :: acc)
    | name :: rest ->
      (match find_sub cmd name with
       | Some sub -> walk sub (cmd :: acc) rest
       | None -> List.rev (cmd :: acc))
  in
  match path with
  | [] -> [root]
  | _ :: rest -> walk root [] rest

let make_help_command ?group_id ~root_ref ~color_mode ~out ~has_version () =
  let run args =
    let words = Args.positional args in
    let rec nav (cmd : Command.t) path = function
      | [] -> (cmd, path)
      | w :: rest ->
        (match Command.find_subcommand cmd w with
         | Some sub -> nav sub (sub :: path) rest
         | None -> (cmd, path))
    in
    let target, rev_path = nav !root_ref [ !root_ref ] words in
    let color = Style.want_color color_mode in
    Help.render ~has_version ~out ~color
      ~path_commands:(List.rev rev_path) ~command:target ();
    Error.success
  in
  Command.make ~name:"help"
    ~short:"Help about any command"
    ~long:"Help provides help for any command in the application."
    ~args:Arg.any
    ?group_id
    ~run
    ()

let make_version_command ?group_id ~program_name ~program_version ~out () =
  let run _ =
    Format.fprintf out "%s version %s@." program_name program_version;
    Error.success
  in
  Command.make ~name:"version"
    ~short:"Print version information"
    ~args:Arg.none   (* no positionals expected *)
    ?group_id
    ~run
    ()

let make_completion_command ?group_id ~program_name ~root_ref ~out () =
  let run args =
    match Args.positional args with
    | [ s ] ->
      (match Completion.of_string s with
       | Ok shell ->
         Completion.emit ~out ~shell ~program_name ~root:!root_ref;
         Error.success
       | Error msg ->
         Format.eprintf "%s@." msg;
         Error.parse_error)
    | _ ->
      Format.eprintf "usage: %s completion <bash|zsh|fish>@." program_name;
      Error.parse_error
  in
  Command.make ~name:"completion"
    ~short:"Generate shell completion script"
    ~long:"Emit a shell completion script for bash, zsh, or fish.\n\
           Source the output in your shell to enable tab-completion."
    ~example:(Printf.sprintf
                "  # bash\n  $ %s completion bash | sudo tee /etc/bash_completion.d/%s\n\
                 \  # zsh\n  $ %s completion zsh > \"${fpath[1]}/_%s\""
                program_name program_name program_name program_name)
    ~args:(Arg.exactly 1)
    ?group_id
    ~run
    ()

let augment_root ?(help_command = true) ?(completion_command = true)
    ?(version_command = true)
    ?help_command_group_id ?completion_command_group_id
    ?version_command_group_id
    ~program_name ~program_version ~root_ref ~color_mode ~out ~has_version
    (root : Command.t) : Command.t =
  (* Skip auto-injection when the user already declared a subcommand of
     the same name -- they're opting out by collision. *)
  let user_has name =
    List.exists (fun (c : Command.t) -> c.name = name) root.subcommands
  in
  let extras =
    (if help_command && not (user_has "help") then
       [ make_help_command ?group_id:help_command_group_id
           ~root_ref ~color_mode ~out ~has_version () ]
     else [])
    @ (if completion_command && not (user_has "completion") then
         [ make_completion_command ?group_id:completion_command_group_id
             ~program_name ~root_ref ~out () ]
       else [])
    @ (if version_command && has_version && not (user_has "version") then
         [ make_version_command ?group_id:version_command_group_id
             ~program_name ~program_version ~out () ]
       else [])
  in
  { root with subcommands = root.subcommands @ extras }

(* Check for duplicate subcommand names (including aliases), duplicate
   flag long-names, undefined group_id references, and flag_group entries
   that reference unknown flags, recursively. *)
let validate t =
  let errs = ref [] in
  let add e = errs := e :: !errs in
  let rec check ~ancestor_persistent_names (cmd : Command.t) =
    let names = cmd.name :: cmd.aliases in
    let seen_sub = Hashtbl.create 8 in
    List.iter (fun (sub : Command.t) ->
      List.iter (fun n ->
        if Hashtbl.mem seen_sub n
        then add (Printf.sprintf "duplicate subcommand or alias %S under %s" n cmd.name)
        else Hashtbl.add seen_sub n ())
        (sub.name :: sub.aliases))
      cmd.subcommands;
    ignore names;
    let seen_flag = Hashtbl.create 8 in
    let consider flags =
      List.iter (fun fp ->
        let n = Flag.packed_name fp in
        if Hashtbl.mem seen_flag n
        then add (Printf.sprintf "duplicate flag --%s on %s" n cmd.name)
        else Hashtbl.add seen_flag n ())
        flags
    in
    consider cmd.persistent_flags;
    consider cmd.flags;
    (* Each child's group_id must be declared in this command's groups. *)
    let group_ids = List.map fst cmd.groups in
    List.iter (fun (sub : Command.t) ->
      match sub.group_id with
      | None -> ()
      | Some g when List.mem g group_ids -> ()
      | Some g ->
        add (Printf.sprintf
               "subcommand %S references undefined group %S under %s"
               sub.name g cmd.name))
      cmd.subcommands;
    (* Each flag_group entry must reference a flag visible at this command:
       local flags, this command's persistent flags, or an ancestor's
       persistent flags. Catches typos in [_by_name] constructors and
       flags packed from the wrong command. *)
    let own_persistent_names =
      List.map Flag.packed_name cmd.persistent_flags
    in
    let local_names = List.map Flag.packed_name cmd.flags in
    let visible_here =
      local_names @ own_persistent_names @ ancestor_persistent_names
    in
    List.iter (fun grp ->
      List.iter (fun name ->
        if not (List.mem name visible_here) then
          add (Printf.sprintf
                 "flag group on %s references unknown flag --%s"
                 cmd.name name))
        (Flag_group.flag_names grp))
      cmd.flag_groups;
    let next_ancestor =
      ancestor_persistent_names @ own_persistent_names
    in
    List.iter (check ~ancestor_persistent_names:next_ancestor) cmd.subcommands
  in
  check ~ancestor_persistent_names:[] t.effective_root_;
  match !errs with
  | [] -> Ok ()
  | es -> Error (String.concat "; " (List.rev es))

let make ~name ~version ?(description = "") ?(author = "")
    ?(completion_command = true) ?(help_command = true)
    ?(version_command = true)
    ?help_command_group_id ?completion_command_group_id
    ?version_command_group_id
    ?(case_insensitive = false)
    ?(color = `Auto)
    ?(out = Format.std_formatter)
    ?(err = Format.err_formatter)
    ~root () : t =
  let root_ref = ref root in
  let has_version = version <> "" in
  let effective_root =
    augment_root ~help_command ~completion_command ~version_command
      ?help_command_group_id ?completion_command_group_id
      ?version_command_group_id
      ~program_name:name ~program_version:version
      ~root_ref ~color_mode:color ~out
      ~has_version root
  in
  root_ref := effective_root;
  let t = {
    name;
    version;
    description;
    author;
    color_mode = color;
    case_insensitive;
    out_       = out;
    err_       = err;
    effective_root_ = effective_root;
  } in
  (match validate t with
   | Ok () -> ()
   | Error msg -> invalid_arg ("mamba: invalid command tree: " ^ msg));
  t

let dispatch (t : t) ~argv : Parser.result =
  Parser.dispatch ~case_insensitive:t.case_insensitive
    ~program_name:t.name ~root:t.effective_root_ ~argv

let run ?argv t =
  let argv = Option.value ~default:Sys.argv argv in
  match dispatch t ~argv with
  | Run { command; path; args } ->
    let path_commands = build_path_commands ~case_insensitive:t.case_insensitive t.effective_root_ path in
    if command.run = None then begin
      let positional = Args.positional args in
      let color = Style.want_color t.color_mode in
      match positional with
      | [] ->
        Help.render ~has_version:(t.version <> "")
          ~out:t.out_ ~color ~path_commands ~command ();
        Error.success
      | bad :: _ ->
        let visible_sub_names =
          List.filter_map
            (fun (c : Command.t) ->
              if c.hidden then None else Some c.name)
            command.subcommands
        in
        let lev_suggestions = Suggest.closest bad visible_sub_names in
        (* Cobra's SuggestFor: exact-match alternative names. *)
        let suggest_for_matches =
          List.filter_map (fun (c : Command.t) ->
            if c.hidden then None
            else if List.mem bad c.suggest_for then Some c.name
            else None)
            command.subcommands
        in
        let suggestions =
          (* Prefer SuggestFor exact matches first, then Levenshtein. *)
          let seen = Hashtbl.create 4 in
          let dedupe xs =
            List.filter (fun x ->
              if Hashtbl.mem seen x then false
              else (Hashtbl.add seen x (); true))
              xs
          in
          dedupe (suggest_for_matches @ lev_suggestions)
        in
        let context =
          String.concat " "
            (List.map (fun (c : Command.t) -> c.name) path_commands)
        in
        let msg = match suggestions with
          | s :: _ ->
            Printf.sprintf "unknown command %S for %S. Did you mean %S?"
              bad context s
          | [] ->
            Printf.sprintf "unknown command %S for %S" bad context
        in
        Help.render_error ~err:t.err_ ~color ~path_commands ~message:msg;
        Error.parse_error
    end
    else begin
      (* Emit a deprecation warning for any deprecated flag the user
         explicitly set in argv. The flag is still honoured -- the
         warning just goes to stderr before invoking [run]. *)
      let warn_deprecated () =
        let all_flags =
          List.concat_map
            (fun (c : Command.t) -> c.flags @ c.persistent_flags)
            path_commands
        in
        List.iter (fun (Flag.P f) ->
          match Flag.deprecated f with
          | Some msg when Args.was_set args (Flag.name f) ->
            Format.fprintf t.err_
              "warning: flag --%s has been deprecated, %s@."
              (Flag.name f) msg
          | Some _ | None -> ())
          all_flags
      in
      warn_deprecated ();
      match
        Flag_group.check_all command.flag_groups (Args.was_set args)
      with
      | Error msg ->
        let color = Style.want_color t.color_mode in
        Help.render_error ~err:t.err_ ~color ~path_commands ~message:msg;
        Error.parse_error
      | Ok () ->
        Lifecycle.run ~err:t.err_ ~path_commands ~args
    end
  | Help { command; path } ->
    let path_commands = build_path_commands ~case_insensitive:t.case_insensitive t.effective_root_ path in
    let color = Style.want_color t.color_mode in
    Help.render ~has_version:(t.version <> "")
      ~out:t.out_ ~color ~path_commands ~command ();
    Error.success
  | Version { command = _; path = _ } ->
    Format.fprintf t.out_ "%s version %s@." t.name t.version;
    Error.success
  | Error { message; code; path } ->
    let color = Style.want_color t.color_mode in
    let path_commands = build_path_commands ~case_insensitive:t.case_insensitive t.effective_root_ path in
    Help.render_error ~err:t.err_ ~color ~path_commands ~message;
    code

let run_exn ?argv t = exit (run ?argv t)

let name t = t.name
let version t = t.version
let effective_root t = t.effective_root_
let out t = t.out_
let err t = t.err_
let color t = t.color_mode
