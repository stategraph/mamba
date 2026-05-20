type spec =
  | None_
  | Any
  | Exactly  of int
  | Named    of string list
  | Variadic of { min : int; name : string }
  | Minimum  of int
  | Maximum  of int
  | Range    of { min : int; max : int }
  | One_of   of string list
  | Custom   of (string list -> (unit, string) result)
  | All_of   of spec list

let none           = None_
let any            = Any
let exactly n      = Exactly n
let named names    = Named names

(* Typed pairings of (spec, accessor) so the names list and the read site
   can't drift. Use these for fixed small arities; for larger or dynamic
   arities, use [named] + manual Args.positional indexing. *)
let named1 a       = (Named [ a ],       fun args -> Args.positional_1 args)
let named2 a b     = (Named [ a; b ],    fun args -> Args.positional_2 args)
let named3 a b c   = (Named [ a; b; c ], fun args -> Args.positional_3 args)

let variadic ?(min = 1) name = Variadic { min; name }
let minimum n      = Minimum n
let at_least n     = Minimum n  (* alias for muscle memory from argparse/cmdliner *)
let maximum n      = Maximum n
let at_most n      = Maximum n  (* alias *)
let range ~min ~max = Range { min; max }
let only_valid_of xs = One_of xs
let custom f       = Custom f
let all_of xs      = All_of xs

let plural k word = if k = 1 then word else word ^ "s"

let rec check spec args =
  let n = List.length args in
  match spec with
  | None_ ->
    if n = 0 then Ok ()
    else Error (Printf.sprintf "expected no positional arguments, got %d" n)
  | Any -> Ok ()
  | Exactly k ->
    if n = k then Ok ()
    else Error (Printf.sprintf "expected exactly %d positional %s, got %d"
                  k (plural k "argument") n)
  | Named names ->
    let k = List.length names in
    if n = k then Ok ()
    else Error (Printf.sprintf "expected %d positional %s, got %d"
                  k (plural k "argument") n)
  | Variadic { min; name = _ } ->
    if n >= min then Ok ()
    else Error (Printf.sprintf "expected at least %d positional %s, got %d"
                  min (plural min "argument") n)
  | Minimum k ->
    if n >= k then Ok ()
    else Error (Printf.sprintf "expected at least %d positional %s, got %d"
                  k (plural k "argument") n)
  | Maximum k ->
    if n <= k then Ok ()
    else Error (Printf.sprintf "expected at most %d positional %s, got %d"
                  k (plural k "argument") n)
  | Range { min; max } ->
    if n >= min && n <= max then Ok ()
    else
      Error
        (Printf.sprintf "expected %d-%d positional arguments, got %d" min max n)
  | One_of allowed ->
    let bad =
      List.find_opt (fun a -> not (List.mem a allowed)) args
    in
    (match bad with
     | None -> Ok ()
     | Some b ->
       Error
         (Printf.sprintf "invalid argument %S (expected one of: %s)"
            b (String.concat ", " allowed)))
  | Custom f -> f args
  | All_of specs ->
    let rec loop = function
      | [] -> Ok ()
      | s :: rest ->
        (match check s args with
         | Ok () -> loop rest
         | Error _ as e -> e)
    in
    loop specs

let rec describe = function
  | None_     -> ""
  | Any       -> "[args...]"
  | Exactly k -> Printf.sprintf "<arg %d>" k
  | Named names -> String.concat " " (List.map (fun n -> "<" ^ n ^ ">") names)
  | Variadic { min = 0; name } -> Printf.sprintf "[<%s>...]" name
  | Variadic { min = 1; name } -> Printf.sprintf "<%s>..." name
  | Variadic { min; name } ->
    let req = List.init min (fun _ -> "<" ^ name ^ ">") |> String.concat " " in
    req ^ " ..."
  | Minimum k -> Printf.sprintf "<arg>%s [args...]" (String.concat "" (List.init (max 0 (k-1)) (fun _ -> " <arg>")))
  | Maximum k -> Printf.sprintf "[args... (max %d)]" k
  | Range { min; max } -> Printf.sprintf "[args... (%d-%d)]" min max
  | One_of xs -> Printf.sprintf "{%s}" (String.concat "|" xs)
  | Custom _  -> "[args...]"
  | All_of xs ->
    String.concat " " (List.filter (fun s -> s <> "") (List.map describe xs))
