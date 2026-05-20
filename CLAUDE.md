# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`mamba` is a Cobra-inspired CLI library for OCaml. See README.md for the user-facing pitch.

The repo ships **two opam packages**:
- `mamba` — the library (`src/mamba/`)
- `mamba-cli` — a project scaffolder binary (`src/mamba_cli/`)

## Architectural conventions

- **No `Obj.magic`, no `Hashtbl` of `Obj.t`.** Typed flag lookup uses `Stdlib.Type.Id`. If you ever feel tempted to use `Obj.magic`, instead carry a `'a Type.Id.t` witness and use `Type.Id.provably_equal`.
- **No mutation in the public API.** `Command.t`, `Flag.t`, `Program.t`, `Args.t` are all immutable values.
- **Hooks are pure-ish.** A hook returns `int option`; `Some n` means short-circuit with exit code `n`. Exceptions in user code are caught at the top of `Program.run` and turn into exit `1`.
- **Hook order is the middleware stack**, not Cobra's "most specific only" quirk. For path root -> A -> B: persistent_pre_run runs root, A, B in order; then pre_run B, run B, post_run B; then persistent_post_run B, A, root.
- **Exit codes**: `0` success; `1` user code failure / uncaught exn; `2` argv parse or validator failure. Mirrors POSIX.
- **Synchronous runtime only (v1).** `run` returns `int`. Async users bridge inside their callback (see `examples/lwt_bridge`).
- **Stdlib only for the core library.** Test dep is `alcotest`. Don't add `containers`, `Re`, or other deps without a strong reason.

## Library layout

```
src/mamba/
  flag.ml/.mli       typed Flag.t + builders + Type.Id witness
  arg.ml/.mli        Arg.spec (positional arg-count validators)
  args.ml/.mli       hetero-map keyed by Type.Id witnesses
  hook.ml/.mli       type Hook.t = Args.t -> int option
  command.ml/.mli    record + smart constructor + group
  program.ml/.mli    user-facing entry; wires parser+lifecycle+help+completion
  parser.ml/.mli     internal: argv -> dispatch result
  lifecycle.ml/.mli  internal: walks hook chain
  suggest.ml/.mli    Damerau-Levenshtein
  help.ml/.mli       Cobra-shaped help renderer
  style.ml           ANSI helpers + isatty / NO_COLOR
  man.ml/.mli        groff emitter
  error.ml/.mli      exit codes
  completion/        bash/zsh/fish emitters
  mamba.ml/.mli      umbrella that re-exports public modules
```

Wrap: `(wrapped true)` so external consumers see `Mamba.Flag.int ...`.

## Request flow

The cross-file picture for "what happens when the user runs `myapp foo --bar=baz qux`":

1. `Program.run argv` enters `program.ml`.
2. `Parser.parse` (`parser.ml`) walks the command tree from root. It does a
   **pre-scan** for flags that appear before the first subcommand token
   (bundled `-j8`, separated `-j 8`, or whole-tree fallback), then resumes
   normal left-to-right tokenization with the chain of commands accumulating
   their local + persistent flags as we descend.
3. The parser produces an `Args.t` — a `Type.Id`-keyed heterogeneous map. Each
   `Flag.t` carries its own `Type.Id.t` witness; `Args.get args flag` uses
   `Type.Id.provably_equal` to recover the type. This is the **typed-flag
   triangle**: `'a Flag.t` ↔ `Flag.packed` (existential for lists) ↔ `Args.t`
   (the map). Never reach for `Obj.magic`; carry the witness.
4. `Lifecycle.run` (`lifecycle.ml`) walks the path `[root; A; B; …; leaf]`:
   persistent-pre-runs from root down, then leaf's pre-run, then leaf's `run`,
   then leaf's post-run, then persistent-post-runs from leaf back up.
   Any hook returning `Some n` short-circuits with exit code `n`.
5. Exceptions in user code (incl. `Args.Missing_flag`) are caught at the top
   of `Program.run` and converted into the right exit code with a help footer.

`help.ml`, `man.ml`, and `completion/` are independent renderers that consume
the same `Command.t` tree the parser walks — there is no separate metadata
description.

## Smart defaults to remember

- `Command.make` infers `~args:Arg.none` when a leaf has flags + a `~run`
  callback. Commands that take positionals MUST set `~args` explicitly.
  Commands with no flags default to `~args:Arg.any`.
- A `Command.make` without `~run` but with `~subcommands` is a pure group
  dispatcher; invoking it bare prints its help and exits 0.

## Style

- 100-col soft limit, 2-space indent (dune default).
- Prefer `Stdlib.<thing>` over an opened prelude. No `open` at top of modules
  except `open Mamba` in user-facing examples.
- Comments are sparse. Document the *why*, not the *what*. `.mli` files are the docs.
- Avoid functors for v1; reach for them only if there's a real need.

## Build / test

```
dune build                          # whole repo (lib + examples + tests)
dune runtest                        # all 10 test suites
dune exec examples/hello/main.exe -- --help

# Run a single test suite:
dune exec tests/test_mamba.exe

# Run a single alcotest case (after the `--` is alcotest's own argv):
dune exec tests/test_mamba.exe -- test "Flag" 0
dune exec tests/test_kitchen_sink.exe -- list      # show suite/case names
```

Test corpus under `tests/`:
- `test_mamba.ml` — primary unit tests for the library.
- `cobra_*.ml` — eight files ported from Cobra's Go corpus (args, command,
  completions, suggestions, flag groups, command groups, case-insensitive).
- `kubectl_parity.ml` — sanity-checks the kitchen_sink-style "real-world"
  shape against kubectl's known surface.
- `test_kitchen_sink.ml` — **integration fixture**: shells out to the built
  `examples/kitchen_sink.exe` and asserts on exit codes + stdout/stderr
  substrings. The dune stanza declares `(deps ../examples/kitchen_sink.exe)`
  so the binary is always built first.

### Adding a new public feature (workflow)

This is policy, not suggestion:

1. Implement + ship a `.mli` change.
2. Add one or more unit tests in `tests/test_mamba.ml` (or a topical
   `cobra_*.ml` file if a Cobra analogue exists).
3. Add a line exercising the feature in `examples/kitchen_sink.ml`.
4. Add a scenario in `tests/test_kitchen_sink.ml` that asserts the feature's
   end-to-end behavior through the binary.
5. Update `examples/kitchen_sink/FEATURES.md` (the verdict matrix).

The integration fixture exists precisely to catch features that pass unit
tests but break in combination (the case-insensitive routing bug was
invisible to 200+ unit tests; kitchen_sink found it).

## OCaml conventions

Mamba is a stdlib-only library; conventions are chosen to stay compiler-
verified and self-contained.

- Pattern matching with `when` clauses over nested `if`.
- Result type: `Ok` branch before `Error` branch when matching.
- Exhaustive variant patterns: never `_` for remaining variants — list
  every constructor so the compiler catches additions.
- Exhaustive record patterns: never `{ Foo.field; _ }` — explicitly
  spell out every field, using `field = _` for ones you ignore.
- No top-level `open` inside library modules (already enforced).
- `let module` aliases for long type/module names.
- `.mli` for every `.ml` (already enforced).
- Parameter ordering: optional `?` first, then required `~`, then positional.
- Optional defaults inline: `?(name = default)`, not a downstream `match`.
- Record name punning: `{ Foo.field; other }` over
  `{ Foo.field = field; other = other }`.
- Map over options / results with the combinator, not `match`:
  - `Option.map f x` not `match x with Some v -> Some (f v) | None -> None`
  - `Option.value x ~default` not `match x with Some v -> v | None -> default`
  - `Result.map`, `Result.fold` analogously.
- Use `Stdlib.Option`, `Stdlib.List`, etc. — do **not** introduce
  Containers (`CCOption`, `CCList`). Same combinators, stdlib spelling.
- Internal files stay flat (`arg.ml`, `command.ml`) — the `Mamba.` umbrella
  re-exports them, so no `mamba_arg.ml`-style prefixes.
- No `@@deriving` (would pull in `ppx_deriving`, violating stdlib-only).
- Zero compiler warnings.
