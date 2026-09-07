# Project Review Findings: PRs #2466–#2453

Reviewed twelve merged PRs in newest-first merge order: #2466, #2465, #2463,
#2461, #2460, #2459, #2458, #2457, #2456, #2455, #2454, and #2453. Each review
covered the linked issue, PR description, commit messages, merged patch, and
surviving code and consumers. All twelve GitHub diffs matched their actual
first-parent merge diffs. The interval `145cee8cb^..903dd0e72` contains exactly
those twelve PR merges and no direct commits. No concern was explicitly
excluded. Current behavior was verified on 2026-09-07 at
`3e158738b` (PR #2467 landed during this review and is not counted as reviewed).

The Acacia family passed its eleven-file PNG acceptance audit and was inspected
visually; its PR records the owner's approval. Focused modal-boundary, App.Cli,
worker-lifecycle, and Unit.Anim groups passed: 98 examples, zero failures.
A separate live headless check confirmed five worker-start lines, zero
post-fork lines, and a clean shutdown. The remaining current preview/worker
comment concerns encountered are already tracked by #2201, #2186, #2188, and
#2193; they are not new entries here. No fixed-later defect from the selected
patches needs a separate handoff.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]` reviewed and deliberately never to be filed · `[deferred]` blocked on a concrete precondition

## Status

- [ ] PRR-1. Pose transitions can revive a dead unit despite the terminal-death contract

## 1. Terminal unit death

### PRR-1. Pose transitions can revive a dead unit despite the terminal-death contract

> **Captured note:** Enforce terminal death when executing an ordinary pose
> transition. PR #2453 / issue #1967 documents `Dead` as terminal but accepts
> that premise without checking whether `unit.transitionTo` can leave it.
> The runtime defect predates this comment-only PR; the PR did not introduce
> the resurrection behavior.

**Verification:** Confirmed through the production Lua API in a freshly built
headless executable, and traced through the command handler at current HEAD.
After loading `data/units/acolyte.yaml`, initializing and showing an arena, and
spawning an acolyte, `unit.kill(uid)` returned `true` and polling
`unit.getPose(uid)` observed `dead`. Calling
`unit.transitionTo(uid, "standing")` then returned `true`; polling observed
`standing`, and `unit.getActivity(uid)` subsequently reported `walking`.
This changes the actual unit pose, not merely an API acceptance result.
The process exited 0 after `engine.quit()`.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Units/Spawn.hs:581` — the newly corrected
  comment promises terminal `Dead`. The binding at line 590 validates the
  requested destination through `parsePose`, but does not inspect the unit's
  current pose before queuing `UnitTransitionTo`.
- `src/Unit/Thread/Command.hs:100` — dispatches that command directly to
  `handleUnitTransitionToCommand`, with no intervening dead-unit guard.
- `src/Unit/Thread/Command/Pose.hs:118` — killing a unit stores `Dead` with
  `Idle` activity and clears the transition deadline and other in-flight state.
- `src/Unit/Thread/Command/Pose.hs:304` — transition duration is resolved from
  the current pose and animation library; absent clips produce duration zero.
  The shipped acolyte declares no `dead-to-standing` transition.
- `src/Unit/Thread/Command/Pose.hs:344` — the only rejection guards are
  already-at-target and already-transitioning. A killed `Dead`/`Idle` unit
  satisfies neither; the zero-duration branch at line 357 overwrites its pose
  with the living target. The positive-duration branch also admits it.
- `src/Unit/Thread.hs:363` — publishes the changed authoritative pose through
  `poseTag` into `uiPose`, which `unit.getPose` reads.
- `scripts/unit_ai.lua:258` — defines death as terminal, with no revival;
  line 280 suppresses ordinary AI while the published pose is `dead`.
- `test-headless/Test/Headless/Unit/Anim.hs:42` — the issue's acceptance group
  tests animation-key resolution and death-animation precedence. Its twelve
  passing examples never execute `UnitKill` followed by `UnitTransitionTo`.
- `git blame -L 340,365 -- src/Unit/Thread/Command/Pose.hs` — attributes the
  unguarded mutation to `c39b5d8fdc`, confirming it predates PR #2453.

**Handoff context:**

- **Current behavior:** An ordinary public pose-transition request can move
  a corpse into a living pose and restore eligibility for ordinary AI. It does
  not reverse the other cleanup performed by death; this is not a coherent
  resurrection operation.
- **Expected behavior:** Once `UnitKill` has committed terminal death, ordinary
  pose transitions must preserve it. Enforce the rule against authoritative
  state when the queued request executes, including a request queued before
  death but drained after the kill. A Lua-side precheck alone cannot cover
  that ordering.
- **Scope and constraints:** Preserve the accepted living-pose vocabulary,
  legitimate living-unit transitions, and recovery of collapsed/crawling units.
  No new resurrection feature, animation art, or save-schema change is needed.
  The distinction between refusing `dead` as a destination and refusing to
  leave an already-dead state must remain explicit. This is a behavioral
  follow-on to the flawed correctness premise in #1967, not a reason to weaken
  the terminal-death documentation.
- **Verification target:** Exercise the production command handler or queue:
  kill a real unit, apply living-target transitions, and assert that its
  authoritative and published poses remain dead with no new transition.
  Cover both missing/zero-duration and available-animation paths, ordered
  kill/transition requests, and non-dead controls that still transition.
  Repeat the public Lua kill → poll dead → transition → poll pose sequence.
- **Deduplication:** Open tracker inventory and all-state searches for
  `transitionTo dead`, `revive dead terminal`, `"dead" "transition"`,
  `resurrect`, `"transitionTo" in:title`, and `"resurrection"` found no
  existing correction for this failure. Closed #1967 and its originating
  `docs/project_review_411-399.md` concern stale labels, not terminal-state
  enforcement; #916 and #1397 concern encounters and death-probe assertions.
- **Remaining uncertainty:** The direct public-API failure is reproduced.
  The frequency of a naturally occurring queued AI request arriving after
  combat death was not measured; it is not needed to establish the missing
  execution-time invariant. No claim is made that health or inventory are
  fully restored when the pose changes.
