# Project Review Findings: PRs #627–#614

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #627, #626, #625, #624, #623, #621, #620, #619, #617, #616, #615, and #614 — for later one-at-a-time disposition. The same first-parent window also contains direct CI-comment commit `e0e55105`.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The `World.Slope`, `Unit.Thread.Movement`, `unit_ai`, `Unit.Thread.Command`, `Engine.Core.Log`, `app/Main.hs`, `unit_resources`, and debug-overlay splits retain their intended boundaries in the current tree. Both module-budget guards pass; `world_check.py --quick` matches all six tracked worldgen baselines; the five focused camera-scroll examples, 119 blood examples, three real-Lua debug-overlay input examples, and the blood-decal behavior probe pass. #626's acknowledged missing world-page GPU teardown was later addressed by #788 and is covered by the current `Blood.Teardown` suite rather than re-filed here. The direct commit only clarified a CI comment. Two current concerns remain: high-agility wildlife can mostly bypass #616's exhaustion slowdown while using the ambient meander gait, and #617 left the repository's active profiling recipe pointing at the pre-split `Main.hs` locations.

## Status

- [x] PRR-1. High-agility wildlife mostly bypasses the exhaustion slowdown while meandering — [#1948]
- [x] PRR-2. The profiling recipe still points at pre-split `Main.hs` locations — [#1950]

## 1. Exhaustion and the ambient meander gait

### [#1948] PRR-1. High-agility wildlife mostly bypasses the exhaustion slowdown while meandering

> **Captured note:** Make the near-empty exhaustion penalty observably affect ambient wildlife movement as well as sprint and goal-directed gaits. A red squirrel that has accumulated physical fatigue should not continue its ordinary aimless movement at almost its fully rested speed merely because `meander`'s raw `max_speed` cap wins.

**Verification:** Verified in the current engine with a real `red_squirrel` and by tracing the two branches of `movement_speed.meander`. #616 multiplies exhaustion into `sprint`; `comfort` and ordered movement inherit it. `meander`, however, is the minimum of an exhaustion-sensitive half-comfort branch and a raw `max_speed * 0.25` branch that applies only encumbrance. The raw branch binds for the squirrel's high agility while rested and continues to bind through most of the fatigue ramp, hiding nearly all of the exhaustion penalty. In a focused headless-engine measurement, one ordinary generated squirrel (agility 2.432920) went from sprint 12.164598 / meander 1.250000 while rested to sprint 6.709297 / meander 1.181392 at empty exhaustion: the configured floor cut sprint by 44.8% but meander by only 5.5%. At the YAML base stats the same formulas produce roughly a 4% meander reduction, and sufficiently high-agility individuals can keep the raw cap binding even at the exhaustion floor.

**Evidence:**

- PR #616 / issue #610 added short-horizon exhaustion to the acolyte, brown bear, and red squirrel and chose a near-empty movement-speed penalty as the resource's passive effect. The merged code's comment says exhaustion slows the "whole speed band," like an injury or salt cramp.
- `scripts/exhaustion.lua:38-49` ramps the movement multiplier from 1.0 to a 0.55 floor below 35% exhaustion. The multiplier itself behaves as designed.
- `scripts/movement_speed.lua:73-92` composes that multiplier into `sprint`, from which `comfort` is derived. `ordered` is also derived exclusively from those exhaustion-sensitive values.
- `scripts/movement_speed.lua:126-143` deliberately keeps ambient meander independent of agility, but implements that as `min(max_speed * 0.25 * encumbranceMultiplier, comfort * 0.5)`. The accompanying comment already explains why a modifier present only through `comfort` can be hidden on an agile unit: the raw cap binds. It applies encumbrance to both branches for exactly that reason, but does not give exhaustion equivalent treatment.
- `data/units/red_squirrel.yaml:9-11,33-36` combines `max_speed: 5.0` with base agility 2.5. `scripts/unit_resource_config.lua:271-281` gives the species the full exhaustion resource, and `scripts/red_squirrel_ai.lua:160-170` sends its aimless wander through `mv.meander(uid)`.
- The focused live measurement loaded the real unit definitions and production Lua modules, spawned a red squirrel, set its live exhaustion first to its derived maximum and then to zero, and queried `sprint` and `meander` after each change. The resulting 44.8% versus 5.5% reductions reproduce the branch analysis in the running engine.
- `tools/physiology_probe.py:188-276` covers exhaustion drain, recovery, and sprint speed only on acolytes. Its wildlife assertion checks only that a brown bear has `max_exhaustion`; it neither exercises red-squirrel exhaustion nor compares a wildlife meander gait before and after fatigue, so the current passing probe cannot catch this branch-specific gap.
- A full open-tracker inventory plus targeted all-state searches for exhaustion/fatigue combined with wildlife, squirrels, wander, meander, and movement modifiers found no existing issue for this behavior. Closed epic #479 and issue #610 establish the feature context but do not record the meander exception.

**Handoff context:**

- **Current behavior:** Exhaustion substantially slows sprint, comfort, and ordered movement. An agile unit's ambient meander instead remains pinned near `max_speed * 0.25` until fatigue is severe enough for half-comfort to fall below that cap; a representative squirrel at completely empty exhaustion still moved at 94.5% of its rested meander speed.
- **Expected behavior:** Near-empty exhaustion has an observable passive effect on every movement gait owned by the affected unit, including aimless wildlife meander. The exact meander curve is a design choice, but it should not accidentally disappear because a branch that omits the modifier wins the `min`.
- **Scope and constraints:** Surfaced from PR #616 / issue #610. Preserve the deliberate properties that meander is not agility-scaled, remains well below comfort, continues to recover stamina, and responds to encumbrance. Avoid changing the exhaustion drain/recovery calibration or adding forced collapse/AI goal wiring. Add focused coverage using an agile wildlife definition and compare rested versus near-empty `meander`, not only `sprint`.
- **Remaining uncertainty:** #610 required a near-empty passive effect but did not numerically specify how strongly every gait must slow, and #616's test plan explicitly measured sprint rather than meander. The current "whole speed band" code comment and the fact that red squirrels both own exhaustion and consume `meander` make the near-bypass look unintended; the processor can decide whether a small ambient slowdown is nevertheless the desired balance.

## 2. Profiling-recipe ownership references

### [#1950] PRR-2. The profiling recipe still points at pre-split `Main.hs` locations

> **Captured note:** Update the worldgen profiling recipe's dump-watchdog references after the application boot split. Readers following the current profiling instructions should land on the actual `runDump` and `waitForInit` implementation rather than obsolete `Main.hs` line numbers.

**Verification:** Verified against both the #617 review and the current tree. #617 intentionally moved the dump boot path and all of its polling, timeout, teardown, and serialization helpers from `app/Main.hs` to `app/App/Dump.hs`. Its canonical review called out two stale references in `docs/history/worldgen_timeline_profile_2026-07.md` as a non-blocking nit. Those exact references remain unchanged: the recipe still labels `waitForInit` as `app/Main.hs:353` and the `runDump` failure branch as `app/Main.hs:470`. Current `Main.hs` only imports and dispatches to `runDump`; the named implementation is in `App.Dump`. Despite its `docs/history` location, the repository instructions still designate this document as the full cost-centre profiling recipe, so the stale navigation is current operator-facing documentation rather than merely a historical annotation.

**Evidence:**

- PR #617 / issue #574 split the 656-line application entry point into `App.Cli`, `App.Exception`, `App.Graphical`, `App.Headless`, and `App.Dump`. The PR describes `App.Dump` as owning `runDump` plus its polling and JSON-serialization helpers.
- The merged PR review explicitly noted that `docs/history/worldgen_timeline_profile_2026-07.md:27` and `:32` still pointed at old `app/Main.hs` locations for `waitForInit` and `runDump`, and recommended updating them when touching the profiling note.
- `docs/history/worldgen_timeline_profile_2026-07.md:25-34` still cites `app/Main.hs:353` for `waitForInit` and `app/Main.hs:470` for the `runDump` `Left` branch while explaining why a profiled run must not use `--dump`.
- `app/Main.hs:29,115` now only imports `App.Dump.runDump` and dispatches to it. `app/App/Dump.hs:55-56` defines `runDump`, `:113` invokes the watchdog, and `:261-262` defines `waitForInit`; the timeout cleanup path described by the recipe lives in that module as well.
- The repository's active build guidance points readers to `docs/history/worldgen_timeline_profile_2026-07.md` as the "Full recipe" for cost-centre profiling, including the mandatory `-N1`, headless-driver, and watchdog guidance. That explicit reference keeps the document operationally authoritative even though it preserves a dated investigation.
- Targeted all-state GitHub searches for the profiling recipe, `runDump`, `waitForInit`, `App.Dump`, and stale `Main.hs` references found no issue for this cleanup. Findings-report searches found profiling topics but no existing item for the ownership/path drift.

**Handoff context:**

- **Current behavior:** The recipe's behavioral explanation remains valid, but both source links are dead coordinates in the wrong owner module. A reader inspecting the watchdog or shutdown path is directed into unrelated current `Main.hs` content.
- **Expected behavior:** The active recipe names the current owner and stable symbols for the dump watchdog and timeout cleanup, while retaining the historical profiling measurements and conclusions.
- **Scope and constraints:** Surfaced from PR #617 / issue #574. Restrict the change to correcting navigation/ownership references in the profiling recipe; do not rewrite its dated measurements or change application behavior. Prefer symbol-oriented references where practical so future line movement does not immediately recreate the defect.
- **Remaining uncertainty:** None about the path drift. The processor may reasonably decide this is too small for a standalone tracker issue and disposition it as documentation maintenance, but the recorded references are demonstrably stale in a document the repository still tells maintainers to use.
