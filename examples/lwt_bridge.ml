(* How to drive an async runtime from a synchronous mamba [run].

   Mamba's run signature is [Args.t -> int]. To use Lwt (or Eio, Abb,
   ...) inside, just call your runtime's main loop and return its result
   as an exit code.

   This file does NOT actually depend on Lwt -- it just demonstrates the
   shape. Replace the [pretend_await] function with [Lwt_main.run] or
   [Eio_main.run] in a real project. *)

open Mamba

let pretend_await ~name ~delay_s =
  (* Stand-in for an async operation. *)
  ignore delay_s;
  Printf.printf "fetched %s\n" name;
  Ok 0

let url = Flag.string ~name:"url" ~short:'u' ~required:true ~doc:"URL to fetch" ()

let fetch =
  Command.make ~name:"fetch"
    ~short:"fetch a URL"
    ~flags:[ Flag.pack url ]
    ~run:(fun args ->
      let target = Args.get args url in
      match pretend_await ~name:target ~delay_s:0.0 with
      | Ok n -> n
      | Error _ -> Error.runtime)
    ()

let root =
  Command.make ~name:"fetcher"
    ~short:"async-bridge example"
    ~subcommands:[ fetch ]
    ()

let () =
  Program.make ~name:"fetcher" ~version:"0.1.0" ~root ()
  |> Program.run_exn
