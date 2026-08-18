# Synarchy Copilot Instructions

**Read [`../CLAUDE.md`](../CLAUDE.md) — it is the single source of truth for
this repository, and everything that used to be summarized here now lives
there.** `AGENTS.md` is a symlink to the same file, so every agent reads one
document.

This file is deliberately a pointer rather than a summary. The summary that
used to live here drifted: it recommended wiring new shared state through
`EngineEnv` long after CLAUDE.md's capability-split epic made that the wrong
default and `tools/engine_env_capability_audit.py` started failing CI for it,
and it referenced a `debug-console.sh` that no longer exists. A second,
hand-maintained copy of the project's conventions cannot be kept honest, so
there isn't one any more.

Start with these sections of `CLAUDE.md`:

- **Build Commands** and **Testing Tiers** — what to build, which suite to
  run, and the rule that matters most: pick the cheapest tier that covers the
  change and never run the full gates as an iteration loop.
- **Headless Mode & Debug Console → Tips for agents (read first)** — the
  hard safety rules. Never launch the app without `--dump`, `--headless`, or
  `--offscreen` (a plain or `--preview` launch opens a window and steals the
  user's focus), and never `pkill -f synarchy`.
- **Language & Conventions** — `NoImplicitPrelude`/`UnicodeSyntax`, the
  `UPrelude` import, and the five enforced Unicode operators.
- **Architecture** — the `Base.hs`/`Types.hs` split, `EngineM`, the threading
  model, and (before adding any shared state) the `EngineEnv` capability
  rules.
- **Working-tree discipline** — where a file you write but do not commit is
  allowed to live.
- **AI Asset Generation** — art is tracked work: stop at an art blocker and
  ask rather than shipping a placeholder.

Deeper as-built mechanics behind those rules are in
[`../docs/engine_contracts.md`](../docs/engine_contracts.md), which CLAUDE.md
points at by section name.
