type t =
  | Required_together  of string list
  | One_required       of string list
  | Mutually_exclusive of string list

let names_of_packed = List.map (fun (Flag.P f) -> Flag.name f)

(* Primary, type-safe constructors. *)
let required_together  ps = Required_together  (names_of_packed ps)
let one_required       ps = One_required       (names_of_packed ps)
let mutually_exclusive ps = Mutually_exclusive (names_of_packed ps)

(* Name-keyed escape hatch (dynamic names from config etc.). *)
let required_together_by_name  xs = Required_together  xs
let one_required_by_name       xs = One_required       xs
let mutually_exclusive_by_name xs = Mutually_exclusive xs

let flag_names = function
  | Required_together xs | One_required xs | Mutually_exclusive xs -> xs

type kind = Required_together_k | One_required_k | Mutually_exclusive_k
let kind = function
  | Required_together  _ -> Required_together_k
  | One_required       _ -> One_required_k
  | Mutually_exclusive _ -> Mutually_exclusive_k

let with_dashes names = List.map (fun n -> "--" ^ n) names
let join_csv ns = String.concat ", " (with_dashes ns)
let join_and ns =
  match with_dashes ns with
  | []  -> ""
  | [a] -> a
  | xs ->
    let rev = List.rev xs in
    let last = List.hd rev in
    let init = List.rev (List.tl rev) in
    String.concat ", " init ^ " and " ^ last

let check (rule : t) (is_set : string -> bool) : (unit, string) result =
  match rule with
  | Required_together flags ->
    let set, unset = List.partition is_set flags in
    if set = [] || unset = [] then Ok ()
    else
      Error
        (Printf.sprintf
           "%s requires %s to also be set"
           (join_and set) (join_csv unset))
  | One_required flags ->
    if List.exists is_set flags then Ok ()
    else
      Error
        (Printf.sprintf "at least one of %s is required" (join_csv flags))
  | Mutually_exclusive flags ->
    let set = List.filter is_set flags in
    if List.length set <= 1 then Ok ()
    else
      Error
        (Printf.sprintf
           "cannot use %s together (mutually exclusive)"
           (join_and set))

let check_all (rules : t list) (is_set : string -> bool) : (unit, string) result =
  let rec loop = function
    | [] -> Ok ()
    | r :: rest ->
      (match check r is_set with
       | Ok () -> loop rest
       | Error _ as e -> e)
  in
  loop rules
