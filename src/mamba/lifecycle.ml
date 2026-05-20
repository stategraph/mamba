(* path_commands is root..leaf. *)

exception Short_circuit of int

let run_hook hook args =
  match hook with
  | None -> ()
  | Some h ->
    (match h args with
     | None -> ()
     | Some n -> raise (Short_circuit n))

let run ~err ~path_commands ~args =
  let leaf =
    match List.rev path_commands with
    | [] -> failwith "Lifecycle.run: empty path"
    | x :: _ -> x
  in
  try
    (* Persistent pre_run root..leaf *)
    List.iter
      (fun (c : Command.t) -> run_hook c.persistent_pre_run args)
      path_commands;
    (* Leaf pre_run *)
    run_hook leaf.Command.pre_run args;
    (* Deprecation notice *)
    (match leaf.Command.deprecated with
     | Some msg ->
       Format.fprintf err "warning: command %S is deprecated: %s@."
         leaf.Command.name msg
     | None -> ());
    (* Main run *)
    let exit_code =
      match leaf.Command.run with
      | Some f -> f args
      | None ->
        (* A group command with no run prints help. The Program layer
           catches this signal and renders the help itself, but if we get
           here directly, just succeed with 0. *)
        Error.success
    in
    (* Leaf post_run *)
    run_hook leaf.Command.post_run args;
    (* Persistent post_run leaf..root *)
    List.iter
      (fun (c : Command.t) -> run_hook c.persistent_post_run args)
      (List.rev path_commands);
    exit_code
  with
  | Short_circuit n -> n
  | Args.Missing_flag name ->
    Format.fprintf err "error: required flag --%s not set@." name;
    let path =
      String.concat " " (List.map (fun (c : Command.t) -> c.name) path_commands)
    in
    Format.fprintf err "Run %S for usage.@." (path ^ " --help");
    Error.parse_error
  | exn ->
    Format.fprintf err "error: %s@." (Printexc.to_string exn);
    Error.runtime
