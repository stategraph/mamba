# mamba

A CLI library for OCaml, inspired by Go's [Cobra](https://github.com/spf13/cobra).

`mamba` lets you build modern command-line applications with nested subcommands,
typed and inherited flags, lifecycle hooks, "did you mean?" suggestions, shell
completions, and auto-generated help and man pages -- all with no dependencies
beyond the OCaml standard library.

## Hello, world

```ocaml
open Mamba

(* Persistent flag: defined on root, visible to every subcommand. *)
let verbose = Flag.count ~name:"verbose" ~short:'v' ~doc:"verbosity level" ()

(* Local flags: scoped to the subcommand that declares them. *)
let count = Flag.int  ~name:"count" ~short:'n' ~default:1     ~doc:"how many times" ()
let upper = Flag.bool ~name:"upper"             ~default:false ~doc:"uppercase" ()

let say =
  Command.make ~name:"say" ~short:"print a greeting"
    ~args:(Arg.exactly 1)
    ~flags:[Flag.pack count; Flag.pack upper]
    ~run:(fun args ->
      let n   = Args.get args count in
      let u   = Args.get args upper in
      let v   = Args.get args verbose in        (* inherited from root *)
      let who = Args.positional_1 args in
      if v > 0 then prerr_endline "say: running";
      for _ = 1 to n do
        print_endline (if u then String.uppercase_ascii who else who)
      done;
      0)
    ()

let root =
  Command.make ~name:"hello" ~short:"greet things"
    ~persistent_flags:[Flag.pack verbose]
    ~subcommands:[say] ()

let () =
  Program.make ~name:"hello" ~version:"0.1.0" ~root ()
  |> Program.run_exn
```

Run it:

```
$ hello say world -n 3 --upper
WORLD
WORLD
WORLD

$ hello sya world
Error: "sya" is not a valid command. Did you mean "say"?

$ hello completion bash > /etc/bash_completion.d/hello
```

## Common patterns

Things you'll want past the first command. All are explained further in
"Tips and pitfalls" below.

**Default args spec.** A leaf command with named flags + a `~run` callback
defaults to `~args:Arg.none` — the contract "all input is via flags" is
implicit, and stray positionals are typos. Commands that take positionals
AND flags must set `~args` explicitly:

```ocaml
(* No ~args needed: this leaf has flags + run, smart default = Arg.none *)
Command.make ~name:"deploy"
  ~flags:[Flag.pack target] ~run ()

(* Has positionals: be explicit *)
Command.make ~name:"install"
  ~args:(Arg.at_least 1) ~usage:"<name>..."
  ~flags:[Flag.pack dev] ~run ()
```

For commands with no flags (purely positional), the default is `Arg.any`.

**Named positionals (type-safe).** `Arg.named2` returns both the spec
AND a typed accessor — arity and read-site can't drift:

```ocaml
let (set_spec, get_kv) = Arg.named2 "key" "value" in
Command.make ~name:"set" ~args:set_spec
  ~run:(fun args -> let (k, v) = get_kv args in ...)
(* Usage: app set <key> <value> *)
```

`Arg.named1` and `Arg.named3` follow the same shape. For larger arity or
dynamic names, use `Arg.named ["a"; "b"; "c"; ...]` and read positionals
manually.

**Mutually exclusive flags.** `Flag_group` enforces the constraint AND
surfaces it in `--help`:

```ocaml
Command.make ~name:"list"
  ~flags:[Flag.pack installed; Flag.pack available]
  ~flag_groups:[
    Flag_group.mutually_exclusive [Flag.pack installed; Flag.pack available]
  ] ~run ()
(* --help: "list installed packages (mutually exclusive with --available)" *)
(* runtime: "cannot use --installed and --available together (mutually exclusive)" *)
```

**Group commands (parent without `~run`).** A `Command.make` that omits
`~run` but provides `~subcommands:[...]` is a pure dispatcher — invoking
it with no subcommand prints help and exits 0:

```ocaml
let config_cmd =
  Command.make ~name:"config"
    ~subcommands:[get_cmd; set_cmd; unset_cmd]
    ()   (* no ~run -- "app config" prints help *)
```

**Variadic positionals.** Use `Arg.variadic "name"` — yields `<name>...`
in the Usage line automatically and validates "at least one":

```ocaml
Command.make ~name:"install"
  ~args:(Arg.variadic "name")
  ~run:(fun args -> List.iter install (Args.positional args); 0)
(* Usage: install <name>... *)
```

Use `~min:0` for "zero or more", `~min:2` for "two or more", etc.

**Truly optional flags.** Omit both `~default` and `~required` and read
with `Args.get_opt`: the flag is `None` when not supplied, `Some v`
otherwise. Useful when an empty-string sentinel would be ambiguous:

```ocaml
let type_filter = Flag.string ~name:"type" ~doc:"filter by type" ()
(* no ~default, no ~required *)

Command.make ~name:"find"
  ~flags:[Flag.pack type_filter]
  ~run:(fun args ->
    match Args.get_opt args type_filter with
    | Some t -> filter_by t
    | None   -> no_filter);
  ...
```

**Naming convention.** Long flag and command names use `kebab-case`
(`--save-dev`, `port-forward`) — `~name:"save-dev"`, not `"save_dev"`.

## Highlights

- **Declarative**, immutable `Command.t` records -- no mutation, no surprises.
- **Type-safe flags**: `Args.get args count` is statically `int` because
  `count : int Flag.t`.
- **Persistent (inherited) flags** that propagate down the subcommand tree.
- **Lifecycle hooks** (`pre_run`, `post_run`, plus persistent variants) that fire
  in a clean middleware-stack order.
- **Shell completions** for `bash`, `zsh`, `fish` via a built-in `completion`
  subcommand.
- **Did-you-mean** suggestions powered by Damerau-Levenshtein.
- **Man pages** (`groff`) generated from the same metadata as `--help`.
- **Synchronous** `run` signature (`Args.t -> int`); plug in your own async
  runtime (Lwt, Eio, Abb) inside the callback.

## Comparison with cmdliner

| Feature                          | cmdliner | mamba |
|----------------------------------|:--------:|:-----:|
| Subcommand tree                  | yes      | yes   |
| Auto-generated `--help`          | yes      | yes   |
| Man-page generation              | yes      | yes   |
| Persistent (inherited) flags     | no       | yes   |
| Lifecycle hooks                  | no       | yes   |
| "Did you mean?" suggestions      | no       | yes   |
| Shell completion generation      | no       | yes   |
| Arg-shape validators             | partial  | yes   |
| Project scaffolding              | no       | yes (`mamba-cli new`) |
| Style                            | applicative | declarative records |
| Dependencies                     | stdlib   | stdlib |

## Installation

```
opam install mamba
opam install mamba-cli      # optional, for the scaffolder
```

## Documentation

- `examples/hello`     -- minimal one-subcommand CLI
- `examples/git_like`  -- subcommand tree with persistent flags and hooks
- `examples/lwt_bridge` -- driving `Lwt_main.run` from a synchronous `run`
- `examples/infra_cli` -- exercises most features (persistent flags, nested
  subcommands, enum flags, hooks); `FRICTION.md` next to it documents the
  ergonomic edges surfaced while building it

## Builder parameter cheatsheet

`Flag.string` / `Flag.int` / `Flag.bool` / `Flag.float` all take the same
optional shape (`Flag.count` is the exception — it requires `~short` and
has no `~default`, since its semantics are tied to short-flag clustering):

```ocaml
Flag.string
  ~name:"foo"            (* canonical long name: --foo *)
  ?short:'f'             (* short alias: -f *)
  ?aliases:["bar"]       (* extra long names: --bar *)
  ?env:"APP_FOO"         (* fall back to env var if not in argv *)
  ?default:""            (* used if neither argv nor env supplies *)
  ?required:true         (* error if no default + no env + not in argv *)
  ?hidden:true           (* omit from --help and completion *)
  ?deprecated:"use --baz instead"   (* warn when set; hide from help *)
  ?placeholder:"<value>" (* override the help syntax token *)
  ~doc:"description"
  ()
```

`Command.make` exposes the same kinds of polish:

```ocaml
Command.make
  ~name:"do"
  ?aliases:["d"; "run"]
  ?suggest_for:["dispatch"]   (* did-you-mean alias *)
  ?short:"one-line summary"
  ?long:"longer description shown in --help"
  ?example:"  $ app do thing"
  ?usage:"<name>..."          (* override the auto Usage line's args portion *)
  ?args:(Arg.exactly 1)
  ?flags:[ Flag.pack foo; Flag.pack bar ]
    (* each typed flag is wrapped with Flag.pack so the list can hold
       different value types — Flag.string t, Flag.int t, etc. *)
  ?persistent_flags:[ Flag.pack g_verbose ]   (* inherited by all descendants *)
  ?flag_groups:[ Flag_group.mutually_exclusive [ ... ] ]
  ?group_id:"basic"           (* place under parent's groups section *)
  ?groups:[("basic","Basic Commands")]  (* define groups for THIS command's children *)
  ?hidden:true
  ?deprecated:"use [new-cmd] instead"
  ?persistent_pre_run:hook
  ?pre_run:hook
  ?run:(fun args -> ... ; 0)
  ?post_run:hook
  ?persistent_post_run:hook
  ()
```

## Tips and pitfalls

- **Smart args default**: leaf commands with named flags + `~run`
  default to `~args:Arg.none` automatically. Commands with positional
  args need `~args` set explicitly (any of `at_least`, `exactly`,
  `named`, `named2`, etc.).

- **Reading positional args** with a fixed arity: prefer
  `Args.positional_1`, `Args.positional_2`, `Args.positional_3` (or
  `Args.positional_at args i`) over `List.nth`. They pair naturally with
  `Arg.exactly`:
  ```ocaml
  Command.make ~name:"mv" ~args:(Arg.exactly 2)
    ~run:(fun args ->
      let (src, dst) = Args.positional_2 args in
      ...)
  ```

- **Missing flag values**: `Args.get` raises `Args.Missing_flag "name"` if a
  flag has no parsed value, no env fallback, and no default. The lifecycle
  catches this and renders `error: required flag --name not set` -- so
  letting it propagate from your `run` callback is the right move when the
  flag should have been supplied.

- **Constraining flag combinations**: `Flag_group` expresses three common
  shapes — `required_together`, `one_required`, `mutually_exclusive`. The
  constructors take the flags themselves (type-safe; typos are compile
  errors); the `_by_name` variants exist for dynamic-name use cases. The
  constraint appears in `--help` automatically:
  ```ocaml
  let installed = Flag.bool ~name:"installed" ~doc:"list installed" ()
  let available = Flag.bool ~name:"available" ~doc:"list available" ()
  Command.make ~name:"list"
    ~flags:[Flag.pack installed; Flag.pack available]
    ~flag_groups:[
      Flag_group.mutually_exclusive [Flag.pack installed; Flag.pack available]
    ]
    ~run ()
  ```
  ```
  $ pkg list --installed --available
  error: cannot use --installed and --available together (mutually exclusive)
  ```

- **Naming positional args in `--help`**: by default the auto-generated
  Usage line shows `<arg> [args...]`. Pass `~usage:"<name>..."` to
  `Command.make` to write the syntax yourself:
  ```ocaml
  Command.make ~name:"install"
    ~usage:"<name>..."
    ~args:(Arg.at_least 1)
    ...
  (* renders: Usage: pkg install [flags] <name>... *)
  ```

- **The `--` separator** ends flag interpretation (POSIX). Tokens after
  it appear in `Args.positional` like any other positional, AND are also
  surfaced through `Args.raw` for wrapper tools that need to know
  specifically which tokens came after `--`:
  ```
  $ myapp shell -- --any-tool-flag value
  ```
  ```ocaml
  Args.positional args = ["--any-tool-flag"; "value"]
  Args.raw        args = ["--any-tool-flag"; "value"]
  ```
  For a command like `kubectl exec POD -- ls /tmp`, `Args.positional` is
  `["POD"; "ls"; "/tmp"]` and `Args.raw` is `["ls"; "/tmp"]`; the wrapper
  uses `raw` to feed the inner command.

## License

ISC.
