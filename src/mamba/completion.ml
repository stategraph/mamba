type shell = Bash | Zsh | Fish

let of_string = function
  | "bash" -> Ok Bash
  | "zsh"  -> Ok Zsh
  | "fish" -> Ok Fish
  | s -> Error (Printf.sprintf "unknown shell %S (expected: bash, zsh, fish)" s)

let name_of_shell = function
  | Bash -> "bash"
  | Zsh  -> "zsh"
  | Fish -> "fish"

(* Visible (non-hidden) subcommands of [c], including their aliases. *)
let visible_sub_names (c : Command.t) =
  c.subcommands
  |> List.filter (fun (s : Command.t) -> not s.hidden)
  |> List.concat_map (fun (s : Command.t) -> s.name :: s.aliases)

let visible_subs (c : Command.t) =
  List.filter (fun (s : Command.t) -> not s.hidden) c.subcommands

(* Map characters that aren't valid in a POSIX/bash/zsh identifier to '_'
   so we can use a program name like "root-dash" as part of a function name
   without producing invalid shell syntax. Used only for emitted function
   names; the directive arguments (e.g. `complete -c root-dash`) keep the
   original spelling. *)
let sanitize_ident s =
  String.map
    (fun c ->
      match c with
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> c
      | _ -> '_')
    s

(* All long flag names registered at this command (local + persistent),
   prefixed with "--", plus short flags as "-x". Hidden and deprecated flags
   are skipped. *)
let flag_completions (c : Command.t) : string list =
  let collect (Flag.P f) =
    if Flag.hidden f || Flag.deprecated f <> None then []
    else begin
      let long = "--" ^ Flag.name f in
      let aliases = List.map (fun a -> "--" ^ a) (Flag.aliases f) in
      let short = match Flag.short f with Some c -> [ Printf.sprintf "-%c" c ] | None -> [] in
      long :: aliases @ short
    end
  in
  List.concat_map collect (c.flags @ c.persistent_flags)

(* --- Bash --- *)
let emit_bash ~out ~program_name ~root =
  let fn = "_" ^ sanitize_ident program_name ^ "_complete" in
  let pp fmt = Format.fprintf out fmt in
  pp "# %s bash completion@." program_name;
  pp "%s() {@." fn;
  pp "  local cur prev words cword@.";
  pp "  COMPREPLY=()@.";
  pp "  cur=\"${COMP_WORDS[COMP_CWORD]}\"@.";
  pp "  # Identify which command the user is currently on by joining@.";
  pp "  # COMP_WORDS[1..CWORD-1] (excluding flags) into a path.@.";
  pp "  local path=()@.";
  pp "  local i@.";
  pp "  for ((i=1; i<COMP_CWORD; i++)); do@.";
  pp "    case \"${COMP_WORDS[i]}\" in@.";
  pp "      -*) ;;@.";
  pp "      *) path+=(\"${COMP_WORDS[i]}\") ;;@.";
  pp "    esac@.";
  pp "  done@.";
  pp "  local key=\"$(IFS=/; echo \"${path[*]}\")\"@.";
  pp "  local subs=\"\"@.";
  pp "  local flags=\"\"@.";
  pp "  case \"$key\" in@.";
  (* For each node in the tree, emit a case branch. The key is the
     slash-joined path of subcommand names from root (exclusive) down to
     the current node. *)
  let rec walk (cmd : Command.t) acc_path =
    let key = String.concat "/" (List.rev acc_path) in
    let subs = visible_sub_names cmd in
    let flags = flag_completions cmd in
    pp "    %s)@." (if key = "" then "\"\"" else Printf.sprintf "\"%s\"" key);
    pp "      subs=\"%s\"@." (String.concat " " subs);
    pp "      flags=\"%s\"@." (String.concat " " flags);
    pp "      ;;@.";
    List.iter
      (fun (sub : Command.t) -> walk sub (sub.name :: acc_path))
      (visible_subs cmd)
  in
  walk root [];
  pp "    *)@.";
  pp "      subs=\"\"@.";
  pp "      flags=\"\"@.";
  pp "      ;;@.";
  pp "  esac@.";
  pp "  if [[ \"$cur\" == -* ]]; then@.";
  pp "    COMPREPLY=( $(compgen -W \"$flags\" -- \"$cur\") )@.";
  pp "  else@.";
  pp "    COMPREPLY=( $(compgen -W \"$subs\" -- \"$cur\") )@.";
  pp "  fi@.";
  pp "  return 0@.";
  pp "}@.";
  pp "complete -F %s %s@." fn program_name

(* --- Zsh --- *)
let emit_zsh ~out ~program_name ~root =
  let pp fmt = Format.fprintf out fmt in
  let escape s = String.map (fun c -> if c = ':' then ' ' else c) s in
  let fn = sanitize_ident program_name in
  pp "#compdef %s@." program_name;
  pp "@.";
  pp "_%s() {@." fn;
  pp "  local context state line@.";
  pp "  local -a words_path@.";
  pp "  local i@.";
  pp "  for (( i=2; i < ${#words}; i++ )); do@.";
  pp "    [[ \"${words[i]}\" == -* ]] || words_path+=(\"${words[i]}\")@.";
  pp "  done@.";
  pp "  local key=\"${(j:/:)words_path}\"@.";
  pp "  local -a subs@.";
  pp "  case \"$key\" in@.";
  let rec walk (cmd : Command.t) acc_path =
    let key = String.concat "/" (List.rev acc_path) in
    let subs = visible_subs cmd in
    pp "    %s)@." (if key = "" then "\"\"" else Printf.sprintf "\"%s\"" key);
    if subs <> [] then begin
      pp "      subs=(@.";
      List.iter
        (fun (s : Command.t) ->
          pp "        '%s:%s'@." s.name (escape s.short))
        subs;
      pp "      )@."
    end
    else
      pp "      subs=()@.";
    pp "      ;;@.";
    List.iter
      (fun (sub : Command.t) -> walk sub (sub.name :: acc_path))
      subs
  in
  walk root [];
  pp "    *)@.";
  pp "      subs=()@.";
  pp "      ;;@.";
  pp "  esac@.";
  pp "  if (( ${#subs} > 0 )); then@.";
  pp "    _describe '%s' subs@." program_name;
  pp "  fi@.";
  pp "}@.";
  pp "@.";
  pp "_%s \"$@\"@." fn

(* --- Fish --- *)
let emit_fish ~out ~program_name ~root =
  let pp fmt = Format.fprintf out fmt in
  pp "# %s fish completion@." program_name;
  let visible_flags fs =
    List.filter
      (fun (Flag.P f) -> not (Flag.hidden f) && Flag.deprecated f = None) fs
  in
  let rec walk path_names (cmd : Command.t) =
    let visible = visible_subs cmd in
    let flags = visible_flags (cmd.flags @ cmd.persistent_flags) in
    (* root completions: when no subcommand seen yet *)
    if path_names = [] then begin
      List.iter (fun (s : Command.t) ->
        pp "complete -c %s -n '__fish_use_subcommand' -a %s -d %S@."
          program_name s.name s.short
      ) visible;
      List.iter (fun (Flag.P f) ->
        pp "complete -c %s -n '__fish_use_subcommand' -l %s -d %S@."
          program_name (Flag.name f) (Flag.doc f)
      ) flags
    end
    else begin
      let path_pred =
        match path_names with
        | [ x ] -> Printf.sprintf "'__fish_seen_subcommand_from %s'" x
        | xs -> Printf.sprintf "'__fish_seen_subcommand_from %s'" (String.concat " " xs)
      in
      List.iter (fun (s : Command.t) ->
        pp "complete -c %s -n %s -a %s -d %S@."
          program_name path_pred s.name s.short
      ) visible;
      List.iter (fun (Flag.P f) ->
        pp "complete -c %s -n %s -l %s -d %S@."
          program_name path_pred (Flag.name f) (Flag.doc f)
      ) flags
    end;
    List.iter
      (fun (s : Command.t) -> walk (path_names @ [ s.name ]) s)
      visible
  in
  walk [] root

let emit ~out ~shell ~program_name ~root =
  match shell with
  | Bash -> emit_bash ~out ~program_name ~root
  | Zsh  -> emit_zsh  ~out ~program_name ~root
  | Fish -> emit_fish ~out ~program_name ~root
