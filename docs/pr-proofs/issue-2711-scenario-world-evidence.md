# #2711 — scenario terrain, fluid and flora: validation evidence

Branch `issue-2711-scenario-world` from `master` at `cac5564b8`. Logs under
`~/.local/state/synarchy-evidence/2711/`. Commands run from the issue worktree with
the production profile and `cabal-3.16.1.0`.

## Hspec groups (focused, as the issue's acceptance block names them)

| Group | Examples | Failures |
|---|---|---|
| `Scenario.World` (new: pure build/capture, live world-thread commit, ordinary-save round trip, non-residency, eviction/readmission, lifetime rules, sim admission and closed edge, budgets, non-arena refusal) | 49 | 0 |
| `Scenario` (Schema incl. format v2, Items, Units, World) | 191 | 0 |
| `arena finite bounds` | 90 | 0 |
| `Gameplay.Tags` | 128 | 0 |
| `Sim.Fluid.Exact` | 42 | 0 |
| `sim chunk admission` | 12 | 0 |
| `flora instance persistence` / every `flora` group | 14 / 94 | 0 |
| `crop plant invalidation` | 25 | 0 |
| `persistence contract` | 8 | 0 |
| `save components` | 177 | 0 |
| `save migrations` (incl. `z16-scenario-world`, v5 → v6) | 61 | 0 |

## Audits and self-tests

`persistence_inventory_audit`, `engine_env_capability_audit`, `enum_append_only_audit`
(three appends recorded: `WorldEdit`, `WorldEditDTO`, and the newly guarded frozen
`WorldEditDTOv4`), `save_compat_audit` (44 baselines / 50 fixtures intact),
`haskell_module_budget`, `unicode_operator_audit`, `haddock_link_audit`,
`cabal_module_audit`, `ci_parity_audit`; `test_save_compat_audit.py` (103 members, the
descriptor oracle row for `z16-scenario-world.bin` added) and `test_persistence_inventory_audit.py`.

## Engine-backed gates (probe slot)

Run before the final hardening commit (cell budget, non-arena refusal, forgetting a
replaced non-resident plant, capture diagnostics). The files changed since are
`src/Scenario/World/{Terrain,Install,Capture,Types}.hs` and tests; none touches the save
format, the codec or the arena restore path these gates exercise.

| Command | Result |
|---|---|
| `python3 tools/test_save_compat_audit.py --only-reproducibility` | PASS, 2 of 103 members, 18 assertions |
| `python3 tools/persistence_contract_probe.py --port 9241` | PASS, 0 failed |
| `python3 tools/multiworld_save_probe.py --arena --port 9219` | PASS, 0 failed |

## Worldgen-output tier

Not applicable. The change adds the scenario adapters, one edit constructor and its
replay clause (`World.Edit.Apply`), the `world-edits` v6 DTO chain, and exports
(`growthRate`, `mkInstance`, `speciesMaxAge`); no procedural-generation output changes, so
`src/World/CLAUDE.md`'s full tier (world_check, baselines) does not run — the
manager agreed (#syn-issue-2711, 2026-10-19).

## Reproducing the fixture

`docs/pr-proofs/issue-2711-make-z16.py.txt` derives `z16-scenario-world` from the
tracked `z15-arena-finite` fixture through `synarchy-save-codec seed-tags` (the
planting/`WeSetFloraState` edits, the wheat plot, the regrowth timer, the cursor and the
tags), normalizes the timestamp, dumps the canonical summary from the real decoder and
registers the baseline.
