type result =
  | Run     of { command : Command.t; path : string list; args : Args.t }
  | Help    of { command : Command.t; path : string list }
  | Version of { command : Command.t; path : string list }
  | Error   of { message : string; code : int; path : string list }

exception Parse_err of string

type token =
  | Long       of string * string option   (* --name or --name=value *)
  | Short      of string                   (* characters after the leading '-' *)
  | Sep        (* the literal "--" *)
  | Word       of string                   (* anything else *)

let classify (t : string) : token =
  let len = String.length t in
  if t = "--" then Sep
  else if len >= 2 && t.[0] = '-' && t.[1] = '-' then
    let rest = String.sub t 2 (len - 2) in
    match String.index_opt rest '=' with
    | Some i ->
      Long
        ( String.sub rest 0 i
        , Some (String.sub rest (i + 1) (String.length rest - i - 1)) )
    | None -> Long (rest, None)
  else if len >= 2 && t.[0] = '-' then
    Short (String.sub t 1 (len - 1))
  else
    Word t

(* Mutable per-dispatch state. *)
type state = {
  argv                    : string array;
  n                       : int;
  mutable i               : int;
  mutable cmd_path        : string list;            (* reversed; head = last descended *)
  mutable cmd_chain       : Command.t list;         (* reversed; head = current *)
  mutable remaining_chain : Command.t list;         (* forward; head = next to descend *)
  mutable current         : Command.t;
  flags_by_long           : (string, Flag.packed) Hashtbl.t;
  flags_by_short          : (char,   Flag.packed) Hashtbl.t;
  mutable flag_value      : (Flag.packed * string) list;   (* last-wins on duplicate *)
  count_table             : (int, Flag.packed * int) Hashtbl.t;  (* keyed by Type.Id.uid *)
  mutable positional      : string list;            (* reversed *)
  mutable raw             : string list;
  mutable positional_started : bool;
  mutable want_help       : bool;
  mutable want_version    : bool;
  case_insensitive        : bool;
}

(* Comparison helpers honouring case sensitivity. *)
let str_eq ~case_insensitive a b =
  if case_insensitive
  then String.lowercase_ascii a = String.lowercase_ascii b
  else String.equal a b

let find_sub_ci ~case_insensitive (cmd : Command.t) (name : string)
  : Command.t option =
  List.find_opt
    (fun (c : Command.t) ->
       str_eq ~case_insensitive c.name name
       || List.exists (str_eq ~case_insensitive name) c.aliases)
    cmd.subcommands

(* [chain] is forward (root :: ... :: leaf); already includes [root]. *)
let make_state ~argv ~root ~chain ~case_insensitive =
  let remaining =
    match chain with
    | [] | [_] -> []
    | _ :: rest -> rest
  in
  {
    argv;
    n              = Array.length argv;
    i              = 1;
    cmd_path       = [];
    cmd_chain      = [ root ];
    remaining_chain = remaining;
    current        = root;
    flags_by_long  = Hashtbl.create 32;
    flags_by_short = Hashtbl.create 32;
    flag_value     = [];
    count_table    = Hashtbl.create 8;
    positional     = [];
    raw            = [];
    positional_started = false;
    want_help      = false;
    want_version   = false;
    case_insensitive;
  }

let packed_uid (Flag.P f) = Type.Id.uid (Flag.type_id f)

let register_flag (st : state) (Flag.P f as p) =
  Hashtbl.replace st.flags_by_long (Flag.name f) p;
  List.iter (fun a -> Hashtbl.replace st.flags_by_long a p) (Flag.aliases f);
  (match Flag.short f with
   | Some c -> Hashtbl.replace st.flags_by_short c p
   | None   -> ())

(* Resolved chain: descended (reversed cmd_chain) ++ remaining_chain. *)
let resolved_chain (st : state) : Command.t list =
  List.rev st.cmd_chain @ st.remaining_chain

(* Register every command-along-the-chain's persistent_flags, plus the leaf's
   local flags. The leaf is the last element of the resolved chain (which
   may equal [current] if we haven't yet descended past prescan's view).
   Called once per dispatch; not on each [try_descend] (the chain is fixed
   up-front by [prescan]). *)
let rebuild_flag_tables (st : state) =
  Hashtbl.reset st.flags_by_long;
  Hashtbl.reset st.flags_by_short;
  let resolved = resolved_chain st in
  List.iter
    (fun (c : Command.t) -> List.iter (register_flag st) c.persistent_flags)
    resolved;
  let leaf =
    match List.rev resolved with
    | [] -> failwith "Parser.rebuild_flag_tables: empty chain"
    | x :: _ -> x
  in
  List.iter (register_flag st) leaf.flags

let bump_count (st : state) (p : Flag.packed) =
  let key = packed_uid p in
  let n =
    match Hashtbl.find_opt st.count_table key with
    | Some (_, n) -> n + 1
    | None -> 1
  in
  Hashtbl.replace st.count_table key (p, n)

(* Attempt to consume a flag starting at st.i. Returns true if consumed
   (and advances st.i), false otherwise (st.i unchanged). May raise Parse_err
   on a malformed but recognized flag (e.g. missing required value). *)
let try_consume_flag (st : state) : bool =
  if st.i >= st.n then false
  else
    let tok = st.argv.(st.i) in
    match classify tok with
    | Sep | Word _ -> false
    | Long ("help", _) ->
      st.want_help <- true;
      st.i <- st.i + 1;
      true
    | Long ("version", _) ->
      st.want_version <- true;
      st.i <- st.i + 1;
      true
    | Long (name, optarg) ->
      (match Hashtbl.find_opt st.flags_by_long name with
       | None -> false
       | Some (Flag.P f as p) ->
         (match Flag.kind f with
          | Switch ->
            let v = match optarg with Some s -> s | None -> "true" in
            st.flag_value <- (p, v) :: st.flag_value;
            st.i <- st.i + 1;
            true
          | Count ->
            bump_count st p;
            st.i <- st.i + 1;
            true
          | Value | Multi ->
            let value =
              match optarg with
              | Some s -> s
              | None ->
                if st.i + 1 >= st.n then
                  raise (Parse_err (Printf.sprintf "flag --%s requires a value" name));
                st.argv.(st.i + 1)
            in
            st.flag_value <- (p, value) :: st.flag_value;
            st.i <- st.i + (if optarg = None then 2 else 1);
            true))
    | Short cluster ->
      (* Walk cluster char-by-char. Commit changes only if at least one
         char is recognized; otherwise return false so the caller can
         decide. *)
      let len = String.length cluster in
      let j = ref 0 in
      let pending_value : (Flag.packed * string) list ref = ref [] in
      let pending_count : Flag.packed list ref = ref [] in
      let extra_token = ref 0 in
      let consumed_any = ref false in
      let stop = ref false in
      let want_help_after = ref false in
      while not !stop && !j < len do
        let c = cluster.[!j] in
        if c = 'h' && not (Hashtbl.mem st.flags_by_short 'h') then begin
          want_help_after := true;
          consumed_any := true;
          j := !j + 1
        end
        else
          match Hashtbl.find_opt st.flags_by_short c with
          | None ->
            if !consumed_any then
              raise (Parse_err (Printf.sprintf "unknown flag -%c" c));
            stop := true
          | Some (Flag.P f as p) ->
            (match Flag.kind f with
             | Switch ->
               pending_value := (p, "true") :: !pending_value;
               consumed_any := true;
               j := !j + 1
             | Count ->
               pending_count := p :: !pending_count;
               consumed_any := true;
               j := !j + 1
             | Value | Multi ->
               let value =
                 if !j < len - 1 then begin
                   let v = String.sub cluster (!j + 1) (len - !j - 1) in
                   j := len;
                   v
                 end
                 else begin
                   if st.i + 1 >= st.n then
                     raise (Parse_err (Printf.sprintf "flag -%c requires a value" c));
                   incr extra_token;
                   j := !j + 1;
                   st.argv.(st.i + 1)
                 end
               in
               pending_value := (p, value) :: !pending_value;
               consumed_any := true)
      done;
      if !consumed_any then begin
        List.iter (fun pv -> st.flag_value <- pv :: st.flag_value) !pending_value;
        List.iter (bump_count st) !pending_count;
        if !want_help_after then st.want_help <- true;
        st.i <- st.i + 1 + !extra_token;
        true
      end
      else false

(* Attempt to descend into a subcommand at st.i. The prescan stage has
   already determined the chain; we descend only when the current word
   matches the next link. The flag tables don't need rebuilding -- they
   were populated for the entire chain at [dispatch] entry. *)
let try_descend (st : state) : bool =
  if st.positional_started then false
  else if st.i >= st.n then false
  else
    let eq = str_eq ~case_insensitive:st.case_insensitive in
    match classify st.argv.(st.i) with
    | Word w ->
      (match st.remaining_chain with
       | next :: rest
         when eq w next.Command.name || List.exists (eq w) next.Command.aliases ->
         st.cmd_path        <- w :: st.cmd_path;
         st.cmd_chain       <- next :: st.cmd_chain;
         st.remaining_chain <- rest;
         st.current         <- next;
         st.i <- st.i + 1;
         true
       | [] | _ :: _ -> false)
    | Long (_, _) | Short _ | Sep -> false

(* For an unknown long flag, look up the closest registered name (against
   the flag table, including aliases) and format a "Did you mean" tail.
   Mirrors the unknown-subcommand suggestion path. *)
let suggest_long_flag (st : state) (tok : string) : string =
  let len = String.length tok in
  if len < 3 || tok.[0] <> '-' || tok.[1] <> '-' then ""
  else
    let rest = String.sub tok 2 (len - 2) in
    let name =
      match String.index_opt rest '=' with
      | Some i -> String.sub rest 0 i
      | None -> rest
    in
    let candidates =
      Hashtbl.fold (fun k _ acc -> k :: acc) st.flags_by_long []
    in
    match Suggest.closest name candidates with
    | s :: _ -> Printf.sprintf ". Did you mean --%s?" s
    | [] -> ""

(* After flag consumption, env fallback / defaults / required / typed parsing. *)
let finalize_entries (st : state) : Args.entry list =
  (* Build a set of all visible packed flags so we can iterate. *)
  let seen : (int, Flag.packed) Hashtbl.t = Hashtbl.create 32 in
  Hashtbl.iter (fun _ p -> Hashtbl.replace seen (packed_uid p) p) st.flags_by_long;
  let entries = ref [] in
  (* Split flag_value by kind:
       - non-Multi flags collapse to last-wins in [value_map]
       - Multi flags accumulate every occurrence in [multi_values]
     st.flag_value is reversed (most-recent first); walk it in original
     argv order so Multi accumulation is left-to-right. *)
  let value_map   : (int, string)      Hashtbl.t = Hashtbl.create 16 in
  let multi_values : (int, string list) Hashtbl.t = Hashtbl.create 8 in
  List.iter
    (fun (p, v) ->
       let (Flag.P f) = p in
       let uid = packed_uid p in
       match Flag.kind f with
       | Multi ->
         let prev =
           match Hashtbl.find_opt multi_values uid with
           | Some xs -> xs | None -> []
         in
         Hashtbl.replace multi_values uid (prev @ [ v ])
       | Value | Switch | Count ->
         Hashtbl.replace value_map uid v)
    (List.rev st.flag_value);
  Hashtbl.iter (fun uid (Flag.P f) ->
    match Flag.kind f with
    | Count ->
      (* GADT refinement: Count : int kind means f : int Flag.t here. *)
      let n =
        match Hashtbl.find_opt st.count_table uid with
        | Some (_, n) -> n
        | None -> Option.value ~default:0 (Flag.default f)
      in
      entries := Args.Entry (f, n) :: !entries
    | Multi ->
      let occurrences =
        Option.value (Hashtbl.find_opt multi_values uid) ~default:[]
      in
      (match occurrences with
       | [] ->
         (* env fallback, treated as a single occurrence *)
         let env_val =
           match Flag.env f with
           | Some name -> Sys.getenv_opt name
           | None -> None
         in
         (match env_val with
          | Some s ->
            (match (Flag.parser f) s with
             | Ok v -> entries := Args.Entry (f, v) :: !entries
             | Error e ->
               raise (Parse_err (Printf.sprintf "invalid value for %s: %s"
                                   (Flag.display f) e)))
          | None ->
            (match Flag.default f with
             | Some v -> entries := Args.Entry (f, v) :: !entries
             | None ->
               if Flag.required f then
                 raise (Parse_err (Printf.sprintf "required flag %s not set"
                                     (Flag.display f)))))
       | _ :: _ ->
         (* Fold parser results with [multi_combine] starting from [multi_empty].
            The Multi builders set both; if a user constructed a Multi flag by
            hand without them, error out. *)
         (match Flag.multi_combine f, Flag.multi_empty f with
          | Some combine, Some empty ->
            let folded =
              List.fold_left
                (fun acc s ->
                   match (Flag.parser f) s with
                   | Ok v -> combine acc v
                   | Error e ->
                     raise (Parse_err
                              (Printf.sprintf "invalid value for %s: %s"
                                 (Flag.display f) e)))
                empty occurrences
            in
            entries := Args.Entry (f, folded) :: !entries
          | None, _ | Some _, None ->
            raise (Parse_err
                     (Printf.sprintf
                        "internal: Multi flag %s missing multi_combine/multi_empty"
                        (Flag.display f)))))
    | Value | Switch ->
      let raw =
        match Hashtbl.find_opt value_map uid with
        | Some _ as s -> s
        | None -> Option.bind (Flag.env f) Sys.getenv_opt
      in
      (match raw with
       | Some s ->
         (match (Flag.parser f) s with
          | Ok v -> entries := Args.Entry (f, v) :: !entries
          | Error e ->
            raise (Parse_err (Printf.sprintf "invalid value for %s: %s" (Flag.display f) e)))
       | None ->
         (match Flag.default f with
          | Some v -> entries := Args.Entry (f, v) :: !entries
          | None ->
            if Flag.required f then
              raise (Parse_err (Printf.sprintf "required flag %s not set" (Flag.display f))))))
    seen;
  !entries

(* Pre-scan argv to find the resolved command chain BEFORE flag attribution.
   Walks left-to-right, conservatively skipping flag-likes (using a running
   flag table built from each descended command's persistent_flags +
   current command's flags) and descending into subcommands when a Word
   matches a child.

   This lets a child's bundled-form flag appear before the child's name in
   argv, mirroring Cobra's [Find]/[stripFlags]:

     app -i7 child      -> chain = [app; child] (-i is child's, bundled)
     app --kc=path sub  -> chain = [app; sub] (--kc bundled with =)

   The unbundled case [app -i 7 child] still fails: at root, -i is unknown
   and prescan defaults to "single token skip", so [7] is tried as a
   subcommand of root, fails, and prescan stops at the root. This matches
   the cases Cobra's [TestFlagBeforeCommand] tests (only bundled forms). *)
let prescan ~root ~argv ~case_insensitive : Command.t list =
  let n = Array.length argv in
  let chain   = ref [ root ] in
  let current = ref root in
  let flag_long  : (string, Flag.packed) Hashtbl.t = Hashtbl.create 16 in
  let flag_short : (char,   Flag.packed) Hashtbl.t = Hashtbl.create 16 in
  let register_for (c : Command.t) =
    let reg (Flag.P f as p) =
      Hashtbl.replace flag_long (Flag.name f) p;
      List.iter (fun a -> Hashtbl.replace flag_long a p) (Flag.aliases f);
      (match Flag.short f with
       | Some ch -> Hashtbl.replace flag_short ch p
       | None -> ())
    in
    List.iter reg c.persistent_flags;
    List.iter reg c.flags
  in
  register_for root;
  let takes_value (Flag.P f) =
    match Flag.kind f with
    | Value | Multi -> true
    | Switch | Count -> false
  in
  (* Whole-tree fallback: a Value/Multi flag may belong to a descendant we
     haven't descended into yet. Build a flat set of such flag names so
     "app -i 7 child" (where -i is child's flag) skips the right number of
     tokens during prescan. The mapping is by name only -- if two siblings
     have a same-named flag with different kinds, we err on "value-taking"
     since the unbundled form requires it. *)
  let any_value_long  : (string, unit) Hashtbl.t = Hashtbl.create 32 in
  let any_value_short : (char,   unit) Hashtbl.t = Hashtbl.create 16 in
  let rec collect (c : Command.t) =
    let mark (Flag.P f as p) =
      if takes_value p then begin
        Hashtbl.replace any_value_long (Flag.name f) ();
        List.iter (fun a -> Hashtbl.replace any_value_long a ()) (Flag.aliases f);
        (match Flag.short f with
         | Some ch -> Hashtbl.replace any_value_short ch ()
         | None -> ())
      end
    in
    List.iter mark (c.flags @ c.persistent_flags);
    List.iter collect c.subcommands
  in
  collect root;
  let i = ref 1 in
  let stop = ref false in
  while not !stop && !i < n do
    let tok = argv.(!i) in
    let len = String.length tok in
    if tok = "--" then
      stop := true  (* separator: rest is raw, no more subcommands *)
    else if len >= 2 && tok.[0] = '-' && tok.[1] = '-' then begin
      (* --name or --name=value *)
      let rest = String.sub tok 2 (len - 2) in
      match String.index_opt rest '=' with
      | Some _ ->
        i := !i + 1  (* bundled, one token *)
      | None ->
        (match Hashtbl.find_opt flag_long rest with
         | Some p when takes_value p && !i + 1 < n -> i := !i + 2
         | Some _ -> i := !i + 1
         | None ->
           (* Local table doesn't know it; consult the whole-tree fallback. *)
           if Hashtbl.mem any_value_long rest && !i + 1 < n
           then i := !i + 2
           else i := !i + 1)
    end
    else if len >= 2 && tok.[0] = '-' then begin
      (* short cluster: -x, -xvalue, -xy combined *)
      let cluster = String.sub tok 1 (len - 1) in
      if String.length cluster > 1 then
        i := !i + 1   (* combined: -xvalue or -xy, single token regardless *)
      else
        let first = cluster.[0] in
        (match Hashtbl.find_opt flag_short first with
         | Some p when takes_value p && !i + 1 < n -> i := !i + 2
         | Some _ -> i := !i + 1
         | None ->
           if Hashtbl.mem any_value_short first && !i + 1 < n
           then i := !i + 2
           else i := !i + 1)
    end
    else begin
      (* Word: try to descend. *)
      match find_sub_ci ~case_insensitive !current tok with
      | Some sub ->
        chain := sub :: !chain;
        current := sub;
        register_for sub;
        i := !i + 1
      | None ->
        stop := true   (* unknown word -- positional region begins *)
    end
  done;
  List.rev !chain

let dispatch ~case_insensitive ~program_name ~root ~argv : result =
  let chain = prescan ~root ~argv ~case_insensitive in
  let st = make_state ~argv ~root ~chain ~case_insensitive in
  st.cmd_path <- [program_name];
  rebuild_flag_tables st;
  try
    (* Phase 1: alternate flag/subcommand consumption. *)
    let progress = ref true in
    while !progress do
      progress := false;
      if try_consume_flag st then progress := true;
      if try_descend st then progress := true
    done;
    (* Phase 2: remaining argv. *)
    let stop = ref false in
    while not !stop && st.i < st.n do
      let tok = st.argv.(st.i) in
      match classify tok with
      | Sep ->
        st.i <- st.i + 1;
        let after = Array.sub st.argv st.i (st.n - st.i) |> Array.to_list in
        st.raw <- after;
        (* POSIX/GNU convention: tokens after `--` are positional (no flag
           interpretation). Surface them in [positional] so [Arg.spec]
           validators see them and `cmd -- ARGS` works as users expect.
           [Args.raw] keeps the original mamba-specific affordance for
           wrapper tools that need to know which tokens were post-`--`. *)
        st.positional_started <- true;
        List.iter (fun w -> st.positional <- w :: st.positional) after;
        st.i <- st.n
      | Long _ | Short _ ->
        if try_consume_flag st then ()
        else
          raise (Parse_err
                   (Printf.sprintf "unknown flag: %s%s"
                      tok (suggest_long_flag st tok)))
      | Word w ->
        st.positional_started <- true;
        st.positional <- w :: st.positional;
        st.i <- st.i + 1
      ;
      if st.i >= st.n then stop := true
    done;
    let path = List.rev st.cmd_path in
    if st.want_help then Help { command = st.current; path }
    else if st.want_version then Version { command = st.current; path }
    else begin
      let entries = finalize_entries st in
      let positional = List.rev st.positional in
      (* validate positionals *)
      (match Arg.check st.current.args positional with
       | Ok () -> ()
       | Error e -> raise (Parse_err e));
      (* Names of flags explicitly supplied in argv (deduped). Used by
         Flag_group validation and exposed via Args.was_set. *)
      let set_flags =
        let acc = Hashtbl.create 8 in
        List.iter
          (fun (Flag.P f, _) -> Hashtbl.replace acc (Flag.name f) ())
          st.flag_value;
        Hashtbl.iter
          (fun _ (Flag.P f, _) -> Hashtbl.replace acc (Flag.name f) ())
          st.count_table;
        Hashtbl.fold (fun k () acc -> k :: acc) acc []
      in
      let args =
        Args.make ~set_flags ~entries ~positional ~raw:st.raw ~cmd_path:path ()
      in
      Run { command = st.current; path; args }
    end
  with Parse_err msg ->
    Error { message = msg; code = Error.parse_error; path = List.rev st.cmd_path }
