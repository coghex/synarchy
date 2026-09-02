# Synarchy Copilot Instructions

**Read [`../CLAUDE.md`](../CLAUDE.md) — it is the source of truth for the
rules that prevent damage in this repository, and everything that used to
be summarized here now lives there or one layer below it.** `AGENTS.md` is
a symlink to the same file, so every agent starts from one document.

This file is deliberately a pointer rather than a summary. The summary that
used to live here drifted: it recommended wiring new shared state through
`EngineEnv` long after CLAUDE.md's capability-split epic made that the wrong
default and `tools/engine_env_capability_audit.py` started failing CI for it,
and it referenced a `debug-console.sh` that no longer exists. A second,
hand-maintained copy of the project's conventions cannot be kept honest, so
there isn't one any more.

Start with these sections of `CLAUDE.md`:

- **Build, run, test** and its **Testing tiers** — what to build, which
  suite to run, and the rule that matters most: pick the cheapest tier that
  covers the change and never run the full gates as an iteration loop.
- **Launch rules** — the hard safety rules. Never launch the app without
  `--dump`, `--headless`, or `--offscreen` (a plain or `--preview` launch
  opens a window and steals the user's focus), and never
  `pkill -f synarchy`.
- **Language & conventions** — `NoImplicitPrelude`/`UnicodeSyntax`, the
  `UPrelude` import, and the five enforced Unicode operators.
- **Architecture** — the `Base.hs`/`Types.hs` split, `EngineM`, the threading
  model, and (before adding any shared state) the `EngineEnv` capability
  rules.
- **Working-tree discipline** — where a file you write but do not commit is
  allowed to live.
- **AI asset generation** — art is tracked work: stop at an art blocker and
  ask rather than shipping a placeholder.
- **Domain contracts** — one entry per gameplay or engine area: the rule to
  know on sight, its gate, and where the mechanism lives.

The root file is only the top layer. Per-area rules live in nested
`CLAUDE.md` files (each with an `AGENTS.md` symlink beside it) under
`app/`, `scripts/`, `src/World/`, `src/World/Save/`, and
`src/Unit/Atlas/`. Copilot does not load those automatically: **read the
matching file before any work under one of those directories.** The
headless boot commands, dump layers, console workflow, and query API are
in [`../docs/headless_console.md`](../docs/headless_console.md).

Deeper as-built mechanics behind those rules are in
[`../docs/engine_contracts.md`](../docs/engine_contracts.md), which the root
and nested files point at by section name.
