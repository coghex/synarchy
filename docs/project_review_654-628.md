# Project Review Findings: PRs #654–#628

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #654, #653, #640, #639, #651, #637, #634, #633, #631, #630, #629, and #628 — for later one-at-a-time disposition. The same first-parent window also contains ten direct CI commits (`4233b2c5`, `d82718fc`, `86292c15`, `7f1e679b`, `b5108075`, `c2458a76`, `0fdd9187`, `1f3b7cda`, `bceade0d`, and `b9ef2643`) from the temporary fixed-image/ghcup repair sequence.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The `World.Generate.Config`, `UI.Tooltip`, `unit_info_v2`, `World.Geology.Coastal`, `Combat.Wounds`, and `Engine.Graphics.Font.Load` splits retain their intended surfaces in the current tree. Both module-budget guards pass. Focused headless suites for tooltip behavior, wounds, impact blood, font fallback, and the SDF atlas repertoire pass; the six selected behavior probes for sleep, circadian behavior, species-specific circadian behavior, tilling, impact blood, and strict Lua messages all pass; and `world_check.py --quick` matches all six tracked worldgen baselines. The direct CI-image sequence and #651's ghcup-path repair were later superseded by #671's baked toolchain and #784's content-addressed reusable workflow, so their mutable-image race is fixed rather than re-filed here. One current concern remains: #640 can miss the first dawn after sleep begins because it does not capture a pre-crossing sun-angle baseline when the pose first becomes `Sleeping`.

## Status

- [x] PRR-1. A unit can miss dawn between entering Sleeping and its first wake check — [#1939]

## 1. Dawn-crossing initialization

### [#1939] PRR-1. A unit can miss dawn between entering Sleeping and its first wake check

> **Captured note:** Seed the dawn-crossing baseline at the moment the unit enters the real `Sleeping` phase. A unit that finishes lying down just before dawn should not sleep through that dawn merely because its next utility-AI thought occurs just after the boundary.

**Verification:** Verified by tracing the current #640 state machine and its scheduler. When the pose chain first reports `sleeping`, `sleepExecute` changes `sleepPhase` to `"sleeping"`, explicitly sets `sleepLastSunAngle` to `nil`, and returns without sampling the current angle. The dispatcher then schedules the next ordinary acolyte thought 0.5–1.5 game seconds later. On that next call, `dawnHasArrived` stores the current angle but requires the previous value to be non-nil before it can report a crossing. Therefore, if the phase change happened below 0.25 and the next thought happens above 0.25, the first post-sleep dawn has occurred but the predicate deterministically returns false; with the stored angle now above dawn, it cannot fire again until the following day. The current sleep probe passes because it deliberately waits two seconds for a pre-dawn sleeping-phase sample before moving the clock across dawn, excluding this boundary from its coverage.

**Evidence:**

- PR #640 / issue #612 introduced the real `Sleeping` pose and the `go_to_sleep` action. The issue left the exact wake policy open; the merged module's as-built contract chose sleep-pressure recovery or "the first dawn crossing since falling asleep — whichever comes first."
- `scripts/unit_ai_sleep.lua:42-54` implements dawn as an edge detector over `sleepLastSunAngle`. It updates the stored angle every check but returns true only when the old value is non-nil, below `DAWN_ANGLE`, and the new value is at or above it.
- `scripts/unit_ai_sleep.lua:160-168` is the actual lying-down-to-sleeping boundary. It sets `sleepPhase = "sleeping"`, clears `sleepLastSunAngle`, and returns; it neither calls the edge detector nor snapshots `world.getSunAngleAt` at the moment sleeping begins.
- `scripts/unit_ai_sleep.lua:171-178` does not run `shouldWake` until a later execution of the sleeping branch.
- `scripts/unit_ai.lua:366-371` calls the action and then schedules the next thought. `scripts/unit_ai_tunables.lua:42-44` gives an acolyte a 1.0-second interval with ±0.5 jitter, so this is a real nonzero observation gap rather than two checks in one tick.
- The predicate's boundary truth table is direct: the intended sampled sequence `0.249 → 0.251` returns true, while clearing the first sample makes the first call at `0.251` return false and store `0.251`. Subsequent pre-wrap samples cannot satisfy `prev < 0.25`, so the missed crossing is unrecoverable that day.
- `tools/sleep_probe.py:341-352` places the clock before dawn, enters sleep, then explicitly sleeps for two real seconds "so the module's crossing-detector has a baseline sample below the threshold" before jumping past dawn. That proves the sampled case, but guarantees the missing-baseline case never occurs.
- The current `sleep_probe.py` and both circadian probes pass, confirming that the pose chain, pressure recovery, ordinary sampled dawn wake, and circadian signal still work; none contradict the uncovered initialization gap.
- Full open-tracker inventory, a targeted all-state GitHub search for dawn-crossing sleep failures, and a findings-report search found no existing item for this missing first sample. `docs/project_review_668-655.md` PRR-1 is adjacent but distinct: it records the fixed dawn wake phase conflicting with a dawn-centered nocturnal bear's sleep-drive phase. This finding still affects the default diurnal action even if that species-policy conflict is resolved.

**Handoff context:**

- **Current behavior:** Completing the `Sleeping` pose below dawn does not itself establish a below-dawn baseline. If dawn passes before the next AI thought, pressure recovery or an explicit `wakeUnit` call becomes the only wake path until the next day's crossing.
- **Expected behavior:** The phase transition records the unit's local sun angle when sleeping actually begins. A later first sleeping-phase check can then recognize a crossing that happened during the scheduler gap, while a unit that genuinely starts sleeping after dawn retains the documented behavior of waiting for the next crossing rather than waking immediately.
- **Scope and constraints:** Surfaced from PR #640 / issue #612. Preserve longitude-aware `world.getSunAngleAt`, pressure-full wake, explicit wake requests, the uninterruptible pose chain, per-unit thought jitter, and the rule that falling asleep after dawn does not cause an immediate wake. Add focused coverage for both sides of the phase boundary: enter `Sleeping` just below dawn and first check just above it (wake), versus enter `Sleeping` already above dawn (remain asleep).
- **Remaining uncertainty:** Under normal unaccelerated time this is a narrow timing window, and the exact player-visible frequency depends on day length, thought jitter, and pose completion timing. It becomes deterministic under clock jumps or sufficiently accelerated time. The processor may choose to combine it with the adjacent species/wake-policy work if that issue is filed first, but the missing edge-detector baseline is independently observable and does not depend on the bear configuration.
