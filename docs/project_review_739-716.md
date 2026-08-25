# Project Review Findings: PRs #739–#716

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #739, #738, #736, #735, #732, #734, #733, #731, #719, #720, #718, and #716 — for later one-at-a-time disposition. The first-parent window contains no direct non-PR commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #736's tri-state playtest step, #735's action-outcome oracle key, #732's sequential input acknowledgements, #734/#733's probe registration and port isolation, #731's persisted world identity, #719's Create World cleanup, #718's trace phases, and #716's mental-state implementation retain their intended behavior in the current tree. PR #739's expanded catalogue is currently valid, but its compatibility rule lacks a ratchet. PR #738's runtime instrumentation remains present while its coverage report has drifted from the split implementation. PR #720's value-only event-log comparison retains a rare rollover ambiguity. No separate current concern was found for the other reviewed PRs.

## Status

- [x] PRR-1. The F4 coverage report falsely marks six Tier-1 routes as gaps — [#1704]
- [ ] PRR-2. Value-only event-log deltas can lose repeated rows at ring rollover
- [ ] PRR-3. The add-only concept catalogue has no compatibility ratchet

## 1. Action-outcome coverage after the input-thread split

### [#1704] PRR-1. The F4 coverage report falsely marks six Tier-1 routes as gaps

> **Captured note:** Update the action-outcome coverage map when an instrumented implementation is split or extracted, and give the checker a real-tree regression so synthetic unit fixtures cannot stay green while the repository report becomes false.

**Verification:** Verified. The five Layer A input families added by PR #738 still emit action outcomes and their focused runtime suite passes, but the coverage checker continues to scan the old monolithic `Engine.Input.Thread` module after PR #787 moved those producers into domain modules. A sixth Tier-1 route, `buildTool.commitPlacement`, is similarly reported missing after PR #779 extracted its portal commit path into a helper. The checker's self-test passes because it exercises synthetic source strings rather than the current repository path mapping.

**Evidence:**

- PR #738 / issue #730 added keyboard, character, scroll, click/release, and drag instrumentation and describes Layer A as complete. `tools/action_outcome_coverage.py:30-33` likewise says Tier 1 is expected to read 100%.
- Running `python3 tools/action_outcome_coverage.py` on the current tree exits successfully but reports eleven gaps, including six Tier-1 gaps: the five Layer A click/swallowed/key/type/scroll areas and `buildTool.commitPlacement`.
- `tools/action_outcome_coverage.py:373-402` still reads `src/Engine/Input/Thread.hs` for all five input checks. That file is now a lifecycle facade; the matching producers live in `src/Engine/Input/Thread/Mouse.hs`, `Keyboard.hs`, `Char.hs`, and `Scroll.hs` after PR #787.
- The live producer modules still contain the expected calls and domains. The focused `Input.LayerA` suite passes all 26 examples, establishing that the report gaps are not absent runtime instrumentation.
- `tools/action_outcome_coverage.py:411-418` still evaluates placement through `_build_tool_check`'s historical `handleMouseDown` structure. Current `scripts/build_tool.lua:918-941` records the portal result inside `commitStartingPlacement`, and `handleMouseDown` delegates to that helper at lines 1003-1022 after PR #779.
- `tools/action_outcome_coverage.py:929-1007` proves the Layer A patterns against constructed strings. `python3 tools/action_outcome_coverage.py --self-test` passes even though the same tool's real-tree report marks the supposedly complete tier incomplete.
- The report intentionally returns zero for coverage gaps at `tools/action_outcome_coverage.py:501-516`, so neither normal use nor CI exposes this as a failing check.
- Full tracker and findings-report searches found the closed source issue #730 and unrelated later maintenance, but no follow-up for the stale split-module paths or the current false Tier-1 status.

**Handoff context:**

- **Current behavior:** The runtime emits the reviewed F4 outcomes, but the repository's visibility report tells maintainers that six completed Tier-1 routes are gaps. Its passing self-test lends that stale answer extra credibility.
- **Expected behavior:** The report follows the current producer boundaries and reads DONE for instrumented routes. A focused regression exercises the actual checked-in file mapping or otherwise fails when a refactor strands that mapping.
- **Scope and constraints:** Surfaced from PR #738 / issue #730, with drift introduced by later structural PRs #787 and #779. Preserve the report's deliberately non-blocking exit status, its all-routes-present semantics, and the distinction between expected Tier 2/3 gaps and Tier 1 regressions. The handoff need not reverse the input-thread or build-tool extractions.
- **Remaining uncertainty:** The processor should decide whether the real-tree assertion belongs in `--self-test`, a separate audit, or a focused Haskell/Lua test. The defect is the false result, not a prescribed enforcement mechanism.

## 2. Repeated player events at capacity

### PRR-2. Value-only event-log deltas can lose repeated rows at ring rollover

> **Captured note:** Track player-event progress with stable event identity or an engine-side cursor/drain rather than inferring append history only from row values. At ring capacity, a repeated suffix can be indistinguishable from retained old rows and silently disappear from the playtest oracle.

**Verification:** Partially verified. The current pure delta algorithm demonstrably loses new rows for periodic snapshots: with a prior snapshot `[A, B, A, B]`, dropping its first two rows and appending new rows `[A, B]` produces the identical current value `[A, B, A, B]`, for which `_event_log_delta` returns `[]`. A related `[A, B, A, A]` to `[A, A, A, B]` rollover returns only `[B]`, losing the newly appended `A`. The bounded store and non-coalesced event categories make exact repeated rows possible in principle, particularly during a large same-game-time burst, but that full 1,000-row runtime pattern was not reproduced.

**Evidence:**

- PR #720 / issue #699 replaced a length cursor with `_event_log_delta` so front eviction and tail-moving coalescence would not make new event rows invisible.
- `tools/playtest/engine.py:50-74` finds the longest prefix of the current snapshot that is a subsequence of the previous snapshot, then returns the remaining suffix. It compares complete row values and has no stable event sequence or identity.
- Applying the current helper directly to `previous = [A, B, A, B]` and `current = [A, B, A, B]` returns no delta even when the operational history was “evict `A, B`; append new `A, B`.” The same final values admit both “nothing changed” and “two events arrived,” so no value-only comparison can disambiguate them.
- `src/Engine/PlayerEvent/Emit.hs:141-155` appends a row and drops from the front at the configured cap. Several categories in `data/notification_categories.yaml:66-107` have no coalescing window, so identical serialized events are not universally collapsed before reaching the ring.
- `tools/playtest/engine.py:292-307` publishes only the helper's result as `event_log_new`; rows omitted there are absent from the trace evidence consumed by the playtest critic.
- `tools/playtest/run.py:1109-1152` covers ordinary append, unchanged snapshots, one coalesced move, rollover, and one new row equal to an evicted row. It does not cover multiple repeated rollover rows or a periodic full-buffer snapshot.
- The canonical review for PR #720 recognized a residual theoretical collision window and accepted it as out of scope because it requires a very large burst of exact-value collisions. The present finding preserves that rarity rather than claiming a common failure.
- Full tracker and findings-report searches found the closed source issue #699 but no follow-up for repeated-value ambiguity at ring rollover.

**Handoff context:**

- **Current behavior:** Ordinary append, coalescence, and distinct rollover deltas work. Once a full ring's retained prefix and newly appended suffix repeat old values in the right pattern, the oracle can omit some or all new rows without any error.
- **Expected behavior:** Every player event added since the previous oracle observation is represented exactly once even across front eviction, coalescence, and repeated equal payloads.
- **Scope and constraints:** Surfaced from PR #720 / issue #699. Preserve the initial full-baseline behavior, the bounded 1,000-row in-game store, category coalescing, and non-duplicating ordinary snapshots. A monotonic sequence, cursor-aware API, or destructive oracle-only drain are possible directions rather than a required design.
- **Remaining uncertainty:** Operational reachability was established only from the store and category semantics, not by producing 1,000 suitably repeating rows in a live engine turn. Exact equality includes fields such as `gameTime`, so this likely requires a paused or same-tick high-volume event pattern. The processor should verify realistic frequency before choosing severity.

## 3. Semantic catalogue compatibility

### PRR-3. The add-only concept catalogue has no compatibility ratchet

> **Captured note:** Pin the shipped semantic concept inventory—and whichever authored fields are compatibility-significant—in a checked manifest, golden, or audit. Count and shape tests permit an existing ID to be removed, renamed, or repurposed while replacement entries keep every current test green.

**Verification:** Partially verified structurally. The current 151-entry catalogue is valid and its focused suite passes all 35 examples. The production tests enforce a minimum count, domain balance, and all four English forms, but do not compare the complete historical ID inventory or meanings. A future change can therefore remove or repurpose an existing ID and add a conforming replacement without violating those tests, despite the source contract that IDs may only be added. Since later work persists `NameExpr` values containing those IDs, such drift can make old etymologies unavailable or reinterpret their stored meaning.

**Evidence:**

- PR #739 / issue #713 expanded the catalogue while retaining the existing entries byte-for-meaning and states that existing concept IDs are add-only rather than renameable, removable, or repurposable.
- `src/Language/Semantic/Types.hs:2-17` calls `ConceptId` stable and load-bearing, explains that renaming re-roots generated languages, and explicitly says IDs may be added but never renamed or reused.
- The current catalogue contains 151 valid entries across all six domains. The focused `Semantic proper names` suite passes 35 examples.
- `test-headless/Test/Headless/Language/Semantic.hs:58-90` pins catalogue version 1, a count of at least 150, all six domains, a 20-30 count for each original domain, and all four forms for every entry. Those aggregate checks do not identify which concepts or authored values shipped previously.
- The same suite pins selected acceptance glosses, and language generation has deterministic seed goldens, but repository search found no complete manifest or audit comparing every historical concept ID and semantic field against the current catalogue.
- `src/Language/Semantic/Types.hs:49-58` derives serialization for `ConceptId` so a `NameExpr` can be stored in #1104's optional etymology source. `src/Language/Etymology/Source.hs:54-64` persists that original expression, and lines 102-106 explicitly describe a concept ID that no longer exists as unavailable at decomposition time.
- `src/World/Page/Types.hs:25-62` retains optional etymology on persisted world identity, and the page/worldgen save components serialize that structure. Catalogue compatibility is therefore observable across save versions, not only in newly generated content.
- Full tracker and findings-report searches found the closed source issue #713 and later naming/persistence work, but no follow-up that mechanically enforces the catalogue's add-only compatibility rule.

**Handoff context:**

- **Current behavior:** Human review preserved the original entries during PR #739, and the current data is valid. Automated coverage checks only aggregate shape, so a later replacement can satisfy them while breaking a historical ID or changing what a persisted expression means.
- **Expected behavior:** Previously shipped IDs remain resolvable with their compatibility-significant semantics, while new IDs may be appended. Any intentionally permitted editorial changes are distinguished from forbidden rename/removal/repurposing.
- **Scope and constraints:** Surfaced from PR #739 / issue #713 and made persistence-relevant by later #1104 work. Preserve authored irregular English forms, deterministic native-root derivation from ID strings, valid catalogue growth, and useful validation errors. Avoid freezing presentation fields that maintainers explicitly decide are safe to edit; the ratchet should encode the actual compatibility boundary.
- **Remaining uncertainty:** The original rule may have been intended as a documented authoring convention enforced through review rather than CI. Seed goldens catch some downstream output changes but do not inventory every entry. The processor should confirm whether all four English forms and the domain are immutable, or whether only ID presence and concept identity need pinning.
