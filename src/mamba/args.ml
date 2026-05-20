type entry = Entry : 'a Flag.t * 'a -> entry

type t = {
  entries    : entry list;
  positional : string list;
  raw        : string list;
  cmd_path   : string list;
  set_flags  : string list;
  (* Names of flags explicitly set in argv (excludes env/default sources).
     Used by Flag_group validation and exposed via [was_set]. Stored as a
     list rather than a Set so the [t] stays free of Stdlib.Set deps. *)
}

let make ?(set_flags = []) ~entries ~positional ~raw ~cmd_path () =
  { entries; positional; raw; cmd_path; set_flags }

let positional t = t.positional
let raw t = t.raw
let cmd_path t = t.cmd_path

let was_set t name = List.mem name t.set_flags

let positional_at t i =
  match List.nth_opt t.positional i with
  | Some s -> s
  | None ->
    invalid_arg
      (Printf.sprintf
         "Args.positional_at: index %d out of range (only %d positionals)"
         i (List.length t.positional))

let positional_1 t =
  match t.positional with
  | [ a ] -> a
  | xs ->
    invalid_arg
      (Printf.sprintf "Args.positional_1: expected 1 positional, got %d"
         (List.length xs))

let positional_2 t =
  match t.positional with
  | [ a; b ] -> (a, b)
  | xs ->
    invalid_arg
      (Printf.sprintf "Args.positional_2: expected 2 positionals, got %d"
         (List.length xs))

let positional_3 t =
  match t.positional with
  | [ a; b; c ] -> (a, b, c)
  | xs ->
    invalid_arg
      (Printf.sprintf "Args.positional_3: expected 3 positionals, got %d"
         (List.length xs))

let get_opt (type a) (t : t) (flag : a Flag.t) : a option =
  let id = Flag.type_id flag in
  let rec find : entry list -> a option = function
    | [] -> None
    | Entry (f, v) :: rest ->
      (match Type.Id.provably_equal id (Flag.type_id f) with
       | Some Type.Equal -> Some v
       | None -> find rest)
  in
  find t.entries

exception Missing_flag of string

let get (type a) (t : t) (flag : a Flag.t) : a =
  match get_opt t flag with
  | Some v -> v
  | None -> raise (Missing_flag (Flag.name flag))
