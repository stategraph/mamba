(* Escape minimal set of characters that confuse groff: backslash, dot at
   start of line, and dash. We keep it simple. *)
let escape s =
  let b = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '\\' -> Buffer.add_string b "\\\\"
    | '-'  -> Buffer.add_string b "\\-"
    | c    -> Buffer.add_char b c)
    s;
  Buffer.contents b

let today_iso () =
  let t = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d"
    (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday

let flag_synopsis (Flag.P f) =
  let head =
    match Flag.short f with
    | Some c -> Printf.sprintf "-%c, --%s" c (Flag.name f)
    | None   -> Printf.sprintf "--%s" (Flag.name f)
  in
  match Flag.kind f with
  | Flag.Value | Flag.Multi ->
    head ^ " " ^ Option.value ~default:"<value>" (Flag.placeholder f)
  | Flag.Switch | Flag.Count -> head

let flag_doc (Flag.P f) =
  let d = Flag.doc f in
  let with_default =
    match Flag.default f with
    | Some v -> Printf.sprintf "%s (default: %s)" d ((Flag.printer f) v)
    | None ->
      if Flag.required f then d ^ " (required)" else d
  in
  match Flag.env f with
  | Some e -> Printf.sprintf "%s. Env: %s" with_default e
  | None -> with_default

let emit ~out ~program_version ~command_path ~command =
  let pp fmt = Format.fprintf out fmt in
  let full_name = String.concat " " command_path in
  let dash_name = String.concat "-" command_path in
  let prog_name = match command_path with h :: _ -> h | [] -> "?" in
  let short =
    if command.Command.short = "" then "no description" else command.short
  in
  pp ".TH \"%s\" \"1\" \"%s\" \"%s %s\" \"User Commands\"@."
    (String.uppercase_ascii dash_name) (today_iso ()) prog_name program_version;
  pp ".SH NAME@.";
  pp "%s \\- %s@." (escape dash_name) (escape short);
  pp ".SH SYNOPSIS@.";
  let flags_part =
    if command.flags <> [] || command.persistent_flags <> []
    then " [\\fIflags\\fR]" else ""
  in
  let argspec = Arg.describe command.args in
  let args_part = if argspec = "" then "" else " " ^ argspec in
  pp ".B %s@." (escape full_name);
  pp "%s%s@." flags_part (escape args_part);
  pp ".SH DESCRIPTION@.";
  let desc =
    if command.long <> "" then command.long
    else if command.short <> "" then command.short
    else "(no description)"
  in
  pp "%s@." (escape desc);
  (match command.aliases with
   | [] -> ()
   | xs ->
     pp ".SH ALIASES@.";
     pp "%s@." (escape (String.concat ", " xs)));
  let visible_subs =
    List.filter (fun (c : Command.t) -> not c.hidden) command.subcommands
  in
  (match visible_subs with
   | [] -> ()
   | _ ->
     pp ".SH COMMANDS@.";
     List.iter (fun (s : Command.t) ->
       pp ".TP@.";
       pp ".B %s@." (escape s.name);
       pp "%s@." (escape (if s.short = "" then "(no description)" else s.short))
     ) visible_subs);
  let visible_flags =
    List.filter
      (fun (Flag.P f) -> not (Flag.hidden f) && Flag.deprecated f = None)
  in
  (match visible_flags command.flags with
   | [] -> ()
   | flags ->
     pp ".SH FLAGS@.";
     List.iter (fun fp ->
       pp ".TP@.";
       pp "\\fB%s\\fR@." (escape (flag_synopsis fp));
       pp "%s@." (escape (flag_doc fp))
     ) flags);
  (match visible_flags command.persistent_flags with
   | [] -> ()
   | flags ->
     pp ".SH GLOBAL FLAGS@.";
     List.iter (fun fp ->
       pp ".TP@.";
       pp "\\fB%s\\fR@." (escape (flag_synopsis fp));
       pp "%s@." (escape (flag_doc fp))
     ) flags);
  if command.example <> "" then begin
    pp ".SH EXAMPLES@.";
    pp ".nf@.";
    pp "%s@." (escape command.example);
    pp ".fi@."
  end;
  (match command.deprecated with
   | None -> ()
   | Some msg ->
     pp ".SH DEPRECATION@.";
     pp "%s@." (escape msg));
  if visible_subs <> [] then begin
    pp ".SH SEE ALSO@.";
    let see =
      List.map (fun (s : Command.t) ->
        Printf.sprintf "%s-%s(1)" dash_name s.name)
        visible_subs
    in
    pp "%s@." (escape (String.concat ", " see))
  end

let write_all ~dir ~program_name ~program_version ~root =
  let written = ref [] in
  let rec walk command_path (cmd : Command.t) =
    let dash = String.concat "-" command_path in
    let path_str = Filename.concat dir (dash ^ ".1") in
    let oc = open_out path_str in
    let fmt = Format.formatter_of_out_channel oc in
    emit ~out:fmt ~program_version ~command_path ~command:cmd;
    Format.pp_print_flush fmt ();
    close_out oc;
    written := path_str :: !written;
    List.iter
      (fun (s : Command.t) ->
        if not s.hidden then walk (command_path @ [ s.name ]) s)
      cmd.subcommands
  in
  walk [ program_name ] root;
  List.rev !written
