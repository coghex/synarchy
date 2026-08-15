# Project Review Findings: PRs #693–#682

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #693, #692, #691, #690, #689, #688, #687, #686, #685, #684, #683, and #682 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The lake-identification, hydrology-simulation, weather-generation, Lua-message, Lua-thread, Lua-items, Lua-forage, combat-resolution, and chunk-generation splits retain their intended behavior in the current tree. PR #687's framebuffer screenshot path and PR #683's player manual likewise produced no separate current concern. PR #690's synthetic split holds do not preserve modifier ownership across their independently invoked down/up verbs. Adjacent current concerns in touched world-generation code are already tracked by #1267 and #1111 and are not duplicated here.

## Status

- [ ] PRR-1. Synthetic split holds do not preserve modifier ownership

## 1. Synthetic modifier lifetime across split holds

### PRR-1. Synthetic split holds do not preserve modifier ownership

> **Captured note:** Give modifier keys an ownership-safe lifetime across `input.keyDown`/`keyUp` and `input.mouseDown`/`mouseUp`. A split gesture must neither leave its own modifiers stuck when the release omits a repeated `mods` list nor release a modifier that was already held independently.

**Verification:** Verified through the current offscreen engine and its real input thread. `input.keyDown("W", {"shift"})` followed by `input.keyUp("W")` left `W` released but `Shift` still reported down. The equivalent mouse pair left Shift down as well. Supplying `{"shift"}` again on the release avoids that leak, but a second live sequence showed the inverse ownership failure: after an independent `input.keyDown("Shift")`, a modifier-bearing W down/up pair released Shift even though the outer hold had not ended. The focused `Input.Inject` suite passes because it only compares constructed sequences and, for mouse holds, repeats the same modifier list on both halves.

**Evidence:**

- PR #690 / issue #644 exposed `mouseDown`/`mouseUp` and `keyDown`/`keyUp` as separate console verbs, each with an independently optional `mods` argument. The issue requires split holds and drags to preserve normal down/up routing without desynchronizing input state; the PR describes modifier events as bracketing the synthetic action like physical input.
- `src/Engine/Scripting/Lua/API/InputInject.hs:250-267` maps an absent modifier argument to an empty modifier set. `mouseVerb` at lines 303-333 and `keyVerb` at lines 347-377 parse each invocation independently; neither layer remembers which modifiers a preceding down half introduced.
- `src/Engine/Input/Inject.hs:193-214` presses every modifier supplied to `mouseDownSequence`, while `mouseUpSequence` releases only the modifiers supplied to that later call. The key equivalents have the same split at lines 232-242.
- `src/Engine/Input/Inject.hs:152-172` represents modifiers as ordinary key press/release events and defers releases for callback visibility, but it has no ownership or reference count. Consequently an omitted release list leaks the synthetic press, while a repeated list cannot distinguish a modifier introduced by this gesture from one already held.
- In a live offscreen run, `keyDown("W", {"shift"})` made both W and Shift true; `keyUp("W")` made W false while Shift remained true. `mouseDown(..., {"shift"})` followed by `mouseUp(...)` reproduced the same stuck Shift state.
- In a separate live run, `keyDown("Shift")`, `keyDown("W", {"shift"})`, and `keyUp("W", {"shift"})` left Shift false before the independent `keyUp("Shift")`. Thus merely documenting that callers must repeat `mods` would not preserve independently held state.
- `test-headless/Test/Headless/Input/Inject.hs:96-113` exercises a mouse split by explicitly giving `shiftMod` to both builders, and lines 196-200 exercise the key split only with no modifiers. There is no stateful regression for omitted release modifiers or pre-held modifier ownership.
- The full focused `Input.Inject` suite currently passes all 23 examples, confirming that the existing sequence-shape coverage does not detect the live state failure.
- Full tracker and findings-report searches found the closed source issue #644 and later tap/callback-lifetime work such as #697, but no follow-up covering modifier ownership across split holds.

**Handoff context:**

- **Current behavior:** A caller must repeat the down half's modifier list on the up half to avoid a stuck synthetic modifier, yet doing so can release a modifier that was already down for another synthetic gesture or physical input. Both outcomes make `engine.isKeyDown` diverge from the actual active holds.
- **Expected behavior:** A matched split gesture cleans up the modifiers it introduced without releasing independently held modifier state. Its release callbacks should continue to observe the modifier for the same lifetime promised by the tap/click fence.
- **Scope and constraints:** Surfaced from PR #690 / issue #644. Preserve real input-queue routing, blocking acknowledgements, framebuffer-coordinate conversion, hold/drag behavior across simulation steps, callback-visible modifier lifetime from #697, and ordinary physical input. Add coverage at the stateful input-thread boundary rather than only asserting list shapes.
- **Remaining uncertainty:** The public contract needs an explicit design choice. The implementation could remember modifier ownership per held primary input, reference-count synthetic holds against existing key state, or remove modifier lists from split verbs in favor of explicit modifier `keyDown`/`keyUp` calls with validation. The reproducible defect is that the current independently optional lists cannot provide ownership-safe pairing; this report does not prescribe which surface should replace that ambiguity.
