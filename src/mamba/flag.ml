type 'a parser = string -> ('a, string) result
type 'a printer = 'a -> string

type _ kind =
  | Value  : _ kind
  | Switch : _ kind
  | Count  : int kind
  | Multi  : _ kind
    (** Repeatable. Each occurrence in argv is parsed independently and the
        results are folded with [multi_combine] starting from [multi_empty].
        Used by [Flag.list] and [Flag.repeated]; rarely constructed by
        hand. *)

type 'a t = {
  name          : string;
  short         : char option;
  aliases       : string list;
  env           : string option;
  default       : 'a option;
  required      : bool;
  hidden        : bool;
  deprecated    : string option;
  placeholder   : string option;
  doc           : string;
  parser        : 'a parser;
  printer       : 'a printer;
  kind          : 'a kind;
  multi_combine : ('a -> 'a -> 'a) option;
  multi_empty   : 'a option;
  id            : 'a Type.Id.t;
}

type packed = P : 'a t -> packed

let make
    ~name ?short ?(aliases = []) ?env ?default ?(required = false)
    ?(hidden = false) ?deprecated
    ?placeholder ?(kind = Value) ?multi_combine ?multi_empty
    ~doc ~parser ~printer () =
  {
    name; short; aliases; env; default; required; hidden; deprecated;
    placeholder; doc;
    parser; printer; kind;
    multi_combine; multi_empty;
    id = Type.Id.make ();
  }

let bool ~name ?short ?aliases ?env ?(default = false) ?hidden ?deprecated ~doc () =
  make
    ~name ?short ?aliases ?env ~default ?hidden ?deprecated ~doc ~kind:Switch
    ~parser:(fun s ->
      match String.lowercase_ascii s with
      | "true"  | "t" | "1" | "yes" | "y" | "on"  -> Ok true
      | "false" | "f" | "0" | "no"  | "n" | "off" -> Ok false
      | _ -> Error (Printf.sprintf "invalid bool %S (expected true/false)" s))
    ~printer:string_of_bool
    ()

let int ~name ?short ?aliases ?env ?default ?required ?hidden ?deprecated ?(placeholder = "<n>") ~doc () =
  make
    ~name ?short ?aliases ?env ?default ?required ?hidden ?deprecated ~placeholder ~doc ~kind:Value
    ~parser:(fun s ->
      match int_of_string_opt s with
      | Some n -> Ok n
      | None -> Error (Printf.sprintf "invalid int %S" s))
    ~printer:string_of_int
    ()

let string ~name ?short ?aliases ?env ?default ?required ?hidden ?deprecated ?(placeholder = "<string>") ~doc () =
  make
    ~name ?short ?aliases ?env ?default ?required ?hidden ?deprecated ~placeholder ~doc ~kind:Value
    ~parser:(fun s -> Ok s)
    ~printer:Fun.id
    ()

let float ~name ?short ?aliases ?env ?default ?required ?hidden ?deprecated ?(placeholder = "<x>") ~doc () =
  make
    ~name ?short ?aliases ?env ?default ?required ?hidden ?deprecated ~placeholder ~doc ~kind:Value
    ~parser:(fun s ->
      match float_of_string_opt s with
      | Some f -> Ok f
      | None -> Error (Printf.sprintf "invalid float %S" s))
    ~printer:string_of_float
    ()

let enum ~name ?short ?aliases ?env ?default ~values ~doc () =
  let key_doc =
    "(one of: " ^ String.concat ", " (List.map fst values) ^ ")"
  in
  make
    ~name ?short ?aliases ?env ?default ~doc:(doc ^ " " ^ key_doc) ~kind:Value
    ~placeholder:"<choice>"
    ~parser:(fun s ->
      match List.assoc_opt s values with
      | Some v -> Ok v
      | None ->
        Error
          (Printf.sprintf "%S is not one of: %s"
             s
             (String.concat ", " (List.map fst values))))
    ~printer:(fun v ->
      let rec lookup = function
        | [] -> "?"
        | (k, v') :: rest -> if v = v' then k else lookup rest
      in
      lookup values)
    ()

let path ~name ?short ?aliases ?env ?default ?(must_exist = false) ~doc () =
  make
    ~name ?short ?aliases ?env ?default ~doc:doc ~kind:Value ~placeholder:"<path>"
    ~parser:(fun s ->
      if must_exist && not (Sys.file_exists s)
      then Error (Printf.sprintf "no such file: %s" s)
      else Ok s)
    ~printer:Fun.id
    ()

let list ~sep inner =
  let sep_s = String.make 1 sep in
  let inner_parser  = inner.parser in
  let inner_printer = inner.printer in
  make
    ~name:inner.name
    ?short:inner.short
    ~aliases:inner.aliases
    ?env:inner.env
    ~default:[]
    ~doc:inner.doc
    ~kind:Multi
    ~placeholder:(Printf.sprintf "<v%cv%c...>" sep sep)
    ~parser:(fun s ->
      let parts = String.split_on_char sep s in
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | p :: rest ->
          (match inner_parser p with
           | Ok v -> collect (v :: acc) rest
           | Error e -> Error e)
      in
      collect [] parts)
    ~printer:(fun xs -> String.concat sep_s (List.map inner_printer xs))
    ~multi_combine:List.append
    ~multi_empty:[]
    ()

(* Pflag's StringArray analogue: each occurrence in argv contributes one
   item. No splitting -- if you want comma-separated, use [Flag.list]. *)
let repeated inner =
  let inner_parser  = inner.parser in
  let inner_printer = inner.printer in
  make
    ~name:inner.name
    ?short:inner.short
    ~aliases:inner.aliases
    ?env:inner.env
    ~default:[]
    ~doc:inner.doc
    ~kind:Multi
    ~placeholder:(Option.value ~default:"<v>" inner.placeholder)
    ~parser:(fun s ->
      match inner_parser s with
      | Ok v -> Ok [ v ]
      | Error e -> Error e)
    ~printer:(fun xs ->
      "[" ^ String.concat ", " (List.map inner_printer xs) ^ "]")
    ~multi_combine:List.append
    ~multi_empty:[]
    ()

let count ~name ~short ~doc () =
  make
    ~name ~short ~default:0 ~doc ~kind:Count
    ~parser:(fun _ ->
      (* Never invoked: the parser feeds occurrence count directly. *)
      Ok 0)
    ~printer:string_of_int
    ()

let pack f = P f

let name          t = t.name
let short         t = t.short
let aliases       t = t.aliases
let env           t = t.env
let default       t = t.default
let required      t = t.required
let hidden        t = t.hidden
let deprecated    t = t.deprecated
let placeholder   t = t.placeholder
let doc           t = t.doc
let parser        t = t.parser
let printer       t = t.printer
let kind          t = t.kind
let multi_combine t = t.multi_combine
let multi_empty   t = t.multi_empty
let type_id       t = t.id

let packed_name (P f) = f.name

let display f =
  match f.short with
  | Some c -> Printf.sprintf "-%c, --%s" c f.name
  | None   -> Printf.sprintf "--%s" f.name

let packed_display (P f) = display f
