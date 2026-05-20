# mamba friction log — building infra_cli

Rules:
- One entry per friction moment, terse, with HH:MM:SS.
- Friction = doc/source lookup, type error confusion, "I expected X but API has Y",
  boilerplate that felt like it should be one line, or any pause longer than a few
  seconds wondering "how do I do that".
- No fixing in the moment. Just log and keep moving.
- Mark `[CHEAT]` when I used internal knowledge a real new user wouldn't have.

Start: 07:15:22

## Entries

### TTFW phase (07:15-07:16)

- 07:15:30 — `examples/dune` is one shared executables stanza; I copied
  hello.ml's structure but had to guess whether to add a subdir or a flat
  file. Hello convention is flat .ml; followed that. There's an empty
  `examples/hello/` subdirectory which is confusing — looks abandoned.
  Affordance gap: no comment in `examples/dune` saying "add new examples
  here".

- 07:15:45 — When writing `Flag.string ~name:"workspace" ~short:'w' ~doc ()`
  I forgot to add either `~default` or `~required:true`. Compiler accepted
  it. Should it? Either way, the runtime consequence is BAD: see below.
  [Severity: HIGH]

- 07:16:00 — `Args.get args workspace` — fresh muscle memory from Cobra
  would be `args.workspace` (Go struct access) or `workspace.value(args)`.
  Mamba's `Args.get` taking the flag as second arg means **the flag is
  used as both a token (in argv parsing) and a typed key (in args
  lookup)**, which is mamba's whole Type.Id story. This is a strength but
  not obvious from hello.ml alone — hello.ml uses it once but doesn't
  explain why this works. [Severity: LOW — but a docs gap.]

### First run (07:16-07:17)

- 07:16:30 — **`infra plan` (no flag) prints `error: Not_found`.**
  This is `Args.get` raising `Not_found` because the flag has no default
  AND no env AND wasn't supplied. The exception name leaks through
  Lifecycle.run's catch-all (lifecycle.ml:52). A new user has no idea
  what happened. [Severity: HIGH] Cobra would have either:
    (a) made the flag required at the API level (Flag.string would have a
        required arg if no default — but mamba intentionally separates
        these for flexibility), OR
    (b) defaulted to "" silently (Cobra's StringVar uses "" if no
        explicit default), OR
    (c) raised an exception with the FLAG NAME, not a generic Not_found.
  At minimum, `Args.get` should raise something like
  `Mamba.Missing_flag "workspace"` so the catch in lifecycle.ml can
  produce a usable error.

- 07:16:35 — Usage line shows `infra [args...]` for root. But root has
  no `args:` spec set, only subcommands. The default is `Arg.any` which
  describes itself as `[args...]`. For a pure-subcommand-dispatcher root,
  showing `[args...]` is misleading — implies positional args are
  meaningful at root. [Severity: MEDIUM] Cobra suppresses this when the
  command has subcommands and no Run that uses args. Mamba could detect:
  if there's no `run` and `args` is the default Any, skip `[args...]`.

- 07:16:40 — `Available Commands:` section lists `help` and `completion`
  alongside my user-defined `plan` as if equals. Cobra typically shows
  these under a separate "Additional Commands" group or via the
  CompletionOptions.DisableDefaultCmd toggle. [Severity: LOW]
  Per-section grouping IS available in mamba (`?groups` on Command) but
  the auto-injected help/completion don't get a group by default; would
  need `help_command_group_id` + a "system" group declared on root.

- 07:16:45 — The Plan flag is described as `workspace to operate on`
  in --help, with no indication it's a required-or-error situation
  because I didn't set ~required:true. The renderer can't tell, so it
  just lists the flag normally. But the runtime DOES error if absent.
  This is the broken design: required-vs-optional vs has-default-vs-not
  isn't visible at the API level in a single place.

### Layering more commands (07:17)

- 07:17:00 — Defining each subcommand as a separate `let` binding is
  fine but verbose. Cobra has `c.AddCommand(...)` mutators that read
  more naturally for setup blocks. Mamba's immutable-record style means
  every command exists at definition time; you can't define `state_cmd`
  THEN attach `state_list` later. Forces top-down structuring. Not
  necessarily bad, but a style adjustment. [Severity: LOW; design
  choice flagged in CLAUDE.md.]

- 07:17:10 — `Flag.enum ~values:[("debug",`Debug);...]` is type-safe
  but verbose for short cases. Real-world enums often have 4-6 values;
  the boilerplate adds up. Could use a PPX or a helper that takes a list
  of pairs... but the current API is honest about what it does.
  [Severity: LOW]

- 07:17:20 — Reading positional args via
  `List.hd (Args.positional args)` and `List.nth p 0` / `List.nth p 1`
  is ergonomically poor. For a command with `~args:(Arg.exactly 2)`,
  the type system can't carry that constraint to the run callback —
  the run gets `Args.t` and has to manually destructure a `string list`.
  Cobra's `args []string` is the same shape but Go users expect it.
  In OCaml, a tuple type for fixed-arity args would feel more idiomatic.
  [Severity: MEDIUM — friction every time you write a command with
  positional args.]

### Reading help output (07:18)

- **`Usage: infra [flags] [args...]`** on a root that has subcommands
  and no own args spec. The `[args...]` is wrong — root doesn't take
  positionals meaningfully. help.ml renders `Arg.describe command.args`
  which for default `Arg.any` yields `[args...]`. Should suppress when
  command has subcommands and no own run that uses positionals.
  [Severity: MEDIUM — visible bug, every help screen has it]

- Same issue on `infra state --help`: shows both
  `infra state [args...]` AND `infra state [command]`. The args line
  is noise. [Severity: MEDIUM, same root cause as above]

- `help` and `completion` listed in `Available Commands:` alongside
  user-defined subcommands. Default behavior makes them look like
  first-class app features. Real apps want them grouped or hidden.
  Workaround exists (`~groups` + `~help_command_group_id`) but it's
  opt-in and most users won't know. [Severity: LOW — affects polish,
  not correctness]

- `--log-level garbage` error: "invalid value for --log-level:
  invalid value \"garbage\" (expected one of: ...)". The double
  "invalid value" reads like a duplicate. One is from parser.ml's
  wrapper, the other from Flag.enum's parser. [Severity: LOW — cosmetic]

- Missing-args error: "expected exactly 2 positional argument(s),
  got 1" doesn't name the command. Cobra includes "for 'state mv'".
  [Severity: LOW]

### Stress-test pass (07:19)

- **`infra plan extra-positional` succeeds silently.** `plan` has
  `~flags:[workspace]` but no `~args:` spec, so it defaults to
  `Arg.any` and accepts any extra positionals. The run callback reads
  `workspace` (default value) and ignores the positional entirely.
  User gets no feedback that their input was malformed. [Severity:
  HIGH — silent acceptance of nonsense input is the WORST CLI failure
  mode.] Same issue with `infra state list extra-arg`. The fix is
  either: (a) make `Arg.none` the default when a command has no
  positional args used; (b) add a docs warning; (c) at minimum, when
  positionals exist but the run doesn't reference them, no help.

- `infra version` (no dashes) doesn't work — only `--version`. Cobra
  apps commonly auto-inject a `version` SUBCOMMAND too. Mamba doesn't.
  Minor expectation mismatch. [Severity: LOW]

- did-you-mean suggestions work great at every nesting level
  (`infra plna` → "plan", `infra state lst` → "list"). [Strength
  worth recording, not friction.]

- `--bad-flag` errors cleanly with "unknown flag: --bad-flag". Good.

- `infra --config /etc/hosts plan` — root-level persistent flag with
  separate value, before a subcommand. Pre-scan handles it. [Strength.]

- `infra plan -- --random --raw` runs plan with the `--` separator.
  But there's no easy way for the user to SEE what landed in
  `Args.raw` unless they thread the raw list out themselves. No
  built-in "print raw passthrough" in default help. The user has to
  remember `Args.raw` exists at all. [Severity: LOW — discoverability
  gap]

- `Args.positional` vs `Args.raw` — both are `string list`. The
  difference (positional = pre-`--` words, raw = post-`--`) is only
  documented in the .mli. A new user might confuse them. Worth a
  README example. [Severity: LOW]

### Cross-check items I "knew" without looking up [CHEAT]

For honesty: I never read README.md (only hello.ml), never opened
mamba.mli, never grepped for examples. I worked from muscle memory
of the API I helped build. A real new user would have:
  - Read README to understand the import line
  - Looked at hello.ml AND mamba.mli to see what's available
  - Likely been mildly confused by GADT kind types if browsing
  - Possibly hit "where does run get its args from" until they saw
    Args.get
The biggest unknowns I'd flag for a real new user:
  - `Flag.pack` necessity (heterogeneous list of typed flags)
  - The `() -> int` exit code convention in run
  - Args.get's flag-as-key trick

## Summary themes

1. **Silent acceptance of stray positionals** [HIGHEST priority]
   Default `Arg.any` is too permissive. Real users will write
   commands with named flags, forget `~args:`, and ship CLIs that
   silently swallow typos as positionals. Either change the default
   when no positional usage is detected, or document loudly.

2. **`Not_found` exception leak on missing-no-default value flag**
   [HIGH]
   `Args.get` raises `Not_found` if a flag has no default, no env,
   and wasn't supplied. The exception name leaks. Should raise a
   typed `Missing_flag of string` that Lifecycle catches and renders
   with the flag name.

3. **Usage line shows `[args...]` for pure-dispatcher roots**
   [MEDIUM]
   `Arg.describe Arg.any` always yields `[args...]`. Help renderer
   should suppress for commands with subcommands and no run that
   actually reads positionals. Or expose a `~hide_positionals_in_help`
   flag.

4. **Positional args ergonomics** [MEDIUM]
   `Args.exactly 2` constrains argv but not the run callback's view —
   user destructures `string list` manually. Tuple-typed positionals
   would feel more OCaml-idiomatic.

5. **Auto-injected help / completion not visually distinct** [LOW]
   They appear in `Available Commands:` alongside user commands.
   Workaround exists (`~groups` + `~help_command_group_id`) but
   nothing nudges users toward it.

6. **Error message phrasing is inconsistent** [LOW]
   - "invalid value for --log-level: invalid value..." (double word)
   - "expected exactly 2 positional argument(s), got 1" (no cmd name)
   Both readable but rough.

7. **`Args.positional` vs `Args.raw` discoverability** [LOW]
   The `--` separator and how to read post-`--` tokens aren't shown
   in any example. New users will miss the affordance.

8. **No `version` subcommand auto-injected** [LOW]
   Only `--version`. Some users expect `app version`.

9. **Subcommand definition style** [LOW / design choice]
   Each command is a let binding; you can't define a child and attach
   to a parent later. Matches CLAUDE.md's immutable-tree commitment.

## TTFW

- Started: 07:15:22
- First build clean: 07:16:17 (~55s)
- First successful subcommand invocation: 07:16:30 (~70s)
- But: heavily aided by copying from hello.ml verbatim and by my own
  prior knowledge of the API. A real new user, reading README and
  mamba.mli, would likely take 5-15 minutes for the same.

## Resolutions

### Theme #2 (Not_found leak) — FIXED

`exception Args.Missing_flag of string` added. `Args.get` raises it
with the flag's long name when no value is available through any
source. `Lifecycle.run` catches it ABOVE the generic exn handler and
emits `error: required flag --<name> not set` + returns
`Error.parse_error` (exit 2, not runtime). Test in
`test_mamba.ml::test_missing_flag_friendly_error` pins the behaviour.

Before: `error: Not_found`
After:  `error: required flag --workspace not set`

### Theme #1 (silent positional acceptance) — attempted, reverted

First attempt was a "smart default" in `Command.make`: when a leaf
command has named flags and a run callback but no explicit `~args:`,
default to `Arg.none` instead of `Arg.any`. Reasoning: explicit flag
declaration signals an explicit input contract.

Reverted because it broke real-world cases. `kubectl get pods` has
`Args.exactly` not set explicitly but the `get` command DOES accept
positionals (the resource type). Same with `kubectl delete`. The
"leaf + flags + accepts positionals" pattern is extremely common in
real CLIs — Cobra's permissive default exists for a reason.

**Conclusion: this is a documentation gap, not an API defect.** The
fix is to:
  - Note in mamba.mli/README that `Arg.any` is the default and that
    leaves wanting strict positional checking should use `~args:Arg.none`.
  - Possibly add a `~strict_positionals:bool` opt-in at Program.make
    level for users who want all leaves-without-explicit-args to
    default to none. (Not done in this round; flagged for later.)

The friction was real but the resolution is education, not a default
change. The original Cobra `legacyArgs` is similarly permissive and
real CLI authors are accustomed to declaring positional expectations
explicitly.

### Themes #3-9 — not fixed in this round

Still on the table:
  - `Usage: <cmd> [args...]` for pure-dispatcher commands [MEDIUM]
  - Positional-args ergonomics (tuple-typed for fixed-arity) [MEDIUM]
  - help/completion auto-injection visual grouping [LOW]
  - "invalid value for ... invalid value ..." doubled phrasing [LOW]
  - Missing-args error doesn't name the command [LOW]
  - Args.positional vs Args.raw discoverability [LOW]
  - No auto-injected `version` subcommand [LOW]

The MEDIUM `[args...]` issue is the next most worth fixing — it's
visible in every help screen of a multi-command CLI. The positional-
ergonomics one would require a real type-system rethink (tuple-shaped
Args.t given Arg.exactly N).
