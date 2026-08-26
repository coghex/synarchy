# Project Review Findings: PRs #71–#33

These entries record focused evidence from the senior review of the next twelve merged PRs in first-parent order — #71, #63, #50, #62, #57, #52, #51, #47, #41, #40, #34, and #33 — for later one-at-a-time disposition. The same interval also contains the direct commits `a6eb9c3` (issue #27 river-mouth classification), `9c3a61a` (repository guidance and documentation), and `96cccbf` (world-view structural-texture rebinding), which were reviewed with the PRs rather than omitted between windows.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

PR #71's pause-menu Save now reaches the registered save transaction; its adjacent notification-resume defect is already captured as PRR-3 in `docs/project_review_167-80.md` and is not duplicated here. PR #63's current transition duration covers the renderer's final clamped frame and its instant branch cancels movement. PR #62's error logger remains registered. PR #57's no-listener dump sentinel and PR #52's seconds-based dump waits have since gained boot-policy and final-boundary coverage. PR #51 still rejects nonexistent worlds, PR #47's preview fields and PR #41's passive-fluid identity pass remain absent, and PR #40's cross-chunk side faces now pass through the later canonical seam lookup. PR #33's calendar conversion is now paired with a clock that advances the date; direct commit `a6eb9c3`'s old river graph was superseded by the current hydrology pipeline, while `96cccbf`'s structural texture helpers remain on the stable-handle path. A targeted chunk-frame Hspec passed, a live headless w8 session reproduced the alias-queue count, and `tools/test_determinism.py` passed all five groups. No graphical/offscreen session, full headless suite, world check, or `make ci` was run. Three non-duplicate concerns remain, including two deliberately retained code-health/guidance gaps for processor verification.

## Status

- [x] PRR-1. Pending-chunk dedup treats wrap aliases as different chunks — [#1723]
- [x] PRR-2. The determinism contract self-test is outside every maintained gate — [#1724]
- [x] PRR-3. Copilot guidance still directs new shared state into EngineEnv — [no-issue]

## 1. Chunk queue canonical identity

### [#1723] PRR-1. Pending-chunk dedup treats wrap aliases as different chunks

> **Captured note:** PR #50 deduplicates `world.loadChunksInRegion` in raw `ChunkCoord` space, while the current queue consumer canonicalizes coordinates only after dequeue. Two wrap aliases of one physical chunk are therefore still accepted and reported as two newly queued chunks.

**Verification:** Verified in the current executable. In a fully initialized and visible world of size 8, one Lua evaluation called `world.loadChunksInRegion(4,0,4,0)` and then `world.loadChunksInRegion(0,4,0,4)`. Both returned `1` (`{"alias":1,"first":1}`), even though the checked-in seam fixture establishes that raw `(4,0)` canonicalizes to stored `(0,4)` in that world. The focused `init-queue chunks land under canonical keys` Hspec also passed, confirming that the consumer does canonicalize the first request rather than these being two physical chunks.

**Evidence:**

- `src/Engine/Scripting/Lua/API/WorldQuery/Chunk.hs:87-107` constructs the request rectangle as raw `ChunkCoord`s and promises to return the number of chunks queued.
- `src/Engine/Scripting/Lua/API/WorldQuery/Chunk.hs:128-138` builds its loaded/pending membership tests and returned count from those raw coordinates. It never reads the page's world size or applies `wrapChunkCoordU` before comparing or appending.
- `src/World/Thread/ChunkLoading.hs:231-257` documents that this producer is unwrapped, then canonicalizes and `nub`s only the dequeued batch. That prevents duplicate generation within one batch but occurs after the API has accepted and counted both aliases.
- `src/World/Thread/ChunkLoading.hs:258-271` skips a canonical chunk already in `wsTilesRef`, so aliases drained in later batches waste queue work rather than regenerating settled content; the externally visible count/backlog is still inflated.
- `test-headless/Test/Headless/World/ChunkQueueFrame.hs:41-70` defines the exact w8 equivalence `(4,0) → (0,4)` and describes its raw enqueue as the harness analogue of `world.loadChunksInRegion`.
- Issue #43 / PR #50 promises that already pending chunks are not re-enqueued and that the returned count represents new work. Tracker and pending-report searches for queue aliases, canonical pending chunks, and `loadChunksInRegion` wrap dedup found no owner.

**Handoff context:**

- **Current behavior:** Two coordinate spellings for the same cylindrical chunk can both enter `wsInitQueueRef` and both increment the API's return value. The consumer eventually collapses/skips the duplicate canonical work, but `world.waitForChunks` observes the inflated raw queue until then, and alias-heavy regions can perform unnecessary batch scans.
- **Expected behavior:** Loaded, pending, and requested chunks share one page-specific canonical identity before deduplication and counting, so a second alias of an already loaded/pending physical chunk returns `0` and adds no queue entry.
- **Scope and constraints:** Surfaced from PR #50 / issue #43 and the later canonical queue invariant. Preserve the queue-first/tiles-second handoff ordering that closes the load race, the world thread as sole consumer, append safety, canonical stored keys, and non-wrapping arena behavior. Canonicalization must use the selected page's own generation parameters rather than a global/default size.
- **Remaining uncertainty:** The live repro proves the count and queue contract, not a content overwrite. The current consumer's per-batch `nub` and loaded check appear to prevent duplicate generation, so the processor should measure whether the practical severity is limited to misleading counts/backpressure and decide whether that still warrants an issue.

## 2. Determinism-tool regression coverage

### [#1724] PRR-2. The determinism contract self-test is outside every maintained gate

> **Captured note:** PR #34 added `tools/test_determinism.py` to pin the deliberate content-identity semantics, but the test is not invoked by CI, `make ci`, Hspec, or another maintained aggregate. The exact array/key-order contract that justified the PR can silently drift while all normal gates stay green.

**Verification:** Verified as a current coverage gap. `python3 tools/test_determinism.py` passes all five test groups, but repository-wide invocation searches find the filename only in the test itself. The 16-step local/CI mirror runs `world_check --quick`, which imports the canonical hash helpers against ordinary engine dumps, but it never supplies the synthetic reordered-array and reordered-key pairs needed to distinguish content identity from byte/order identity.

**Evidence:**

- `tools/world_determinism.py:4-24,73-88` defines content identity by sorting tiles and object keys and explicitly excludes output order from the contract.
- `tools/test_determinism.py:51-67` contains the only direct checks that reversed tile arrays and reordered object keys hash identically; `:70-101` pins actual content-change, missing-tile, and stable-canonical-form behavior.
- `tools/ci-local.sh:57-113` enumerates every `make ci` step. It runs Hspec, audit self-tests, and `world_check --quick`, but never runs `tools/test_determinism.py`; the GitHub workflow mirrors that gate.
- `tools/world_check.py:43,236` imports and hashes current dumps, so it exercises the helper on the engine's stable output order but cannot prove that harmless permutations remain ignored.
- PR #34 / issue #23 cited `python3 tools/test_determinism.py` as its validation and made the new pure test the executable specification of the chosen fork. Tracker and pending-report searches for an ungated determinism self-test found no owner.

**Handoff context:**

- **Current behavior:** The self-test passes when someone remembers its standalone command, but a change that accidentally makes the checker order-sensitive can merge without executing the only tests designed to fail on that regression.
- **Expected behavior:** The canonical content-identity contract has an automatically maintained, cheap regression path, either by running this self-test in the standard gate or by moving its cases into an already-gated test family.
- **Scope and constraints:** Surfaced from PR #34 / issue #23. Keep the test pure and sub-second; do not add another world generation to CI. Preserve the intentional distinction between current-run determinism, baseline identity, and JSON serialization order.
- **Remaining uncertainty:** This is coverage debt rather than a presently failing behavior. The processor may close it as no-issue if standalone tool self-tests are intentionally manual, but should first reconcile that policy with the repository's current pattern of running audit/tool self-tests beside their production checks.

## 3. Agent guidance drift

### [no-issue] PRR-3. Copilot guidance still directs new shared state into EngineEnv

> **Disposition:** No issue — fixed by commit `b9bd3637` (2026-08-18), three days after capture. The Copilot summary was replaced by a pointer to `CLAUDE.md`; the STM-backed `EngineM` description, the `debug-console.sh` reference, the pre-split `EngineEnv`/`EngineState` summary, and the "wire new shared state through `EngineEnv` and `Engine.Core.Init`" instruction were all deleted. `AGENTS.md` is a symlink to `CLAUDE.md`, so all three agent surfaces now read one document.

> **Captured note:** Direct commit `9c3a61a` introduced `.github/copilot-instructions.md`, and its surviving architecture guidance now conflicts with the completed EngineEnv capability split. It tells an agent adding shared state to wire it through `EngineEnv`, omitting the mandatory ownership/capability decision that usually says the state belongs somewhere else.

**Verification:** Verified against current authoritative guidance and implementation. The Copilot instruction remains live and is still attributed to `9c3a61a`. `CLAUDE.md` instead requires reading the capability inventory before any field/lifecycle change and says most new state belongs in `WorldState`, a manager, `EngineState`, or a local. The same Copilot file also says `EngineM` reads environment/state through STM-backed variables, while the current monad returns the immutable environment directly and stores main-thread state in an unsynchronized `IORef`; it additionally names a `debug-console.sh` that does not exist in the tree.

**Evidence:**

- `.github/copilot-instructions.md:18-24` presents the pre-split architecture, including the false STM-backed `EngineM` description and the nonexistent `debug-console.sh` consumer.
- `.github/copilot-instructions.md:34` instructs agents to wire new shared queues/refs/global state through `EngineEnv` and `Engine.Core.Init`, with no ownership inventory, projection, lifecycle, or approval gate.
- `CLAUDE.md:144-155` declares the capability split complete, names `docs/engineenv_capability_inventory.md` as authoritative, and says the first question is whether new state avoids `EngineEnv` entirely.
- `CLAUDE.md:176-193` records the closed unrestricted-import boundary and requires explicit maintainer approval plus synchronized audit changes for the two escape hatches.
- `src/Engine/Core/Monad.hs:19-30,74-86` shows `EngineM`'s environment is returned with no STM and `EngineState` is read/written through a main-thread-only `IORef`.
- `docs/engineenv_capability_inventory.md:1002-1059` specifies the required inventory/audit work for a genuine new field and the high bar for a ninth capability. Tracker and pending-report searches for stale Copilot/EngineEnv guidance found no owner.

**Handoff context:**

- **Current behavior:** An agent using GitHub's repository-specific Copilot instructions receives a materially different state-placement rule from Claude/Codex agents. Following it can push state into the global record first and discover the capability/import audit only after implementation, or carry forward a false synchronization model when reviewing concurrency.
- **Expected behavior:** Every repository-facing agent guide points to the same authoritative state-ownership and capability procedure, describes `EngineM`'s actual IORef-backed state model, and names only current control/debug surfaces.
- **Scope and constraints:** Surfaced from direct commit `9c3a61a`. Keep the short Copilot document useful rather than duplicating the full 83-field inventory; a durable pointer plus the non-negotiable decision/approval rules is enough. Preserve its current testing-tier and Unicode guidance, which has been updated and remains accurate.
- **Remaining uncertainty:** None at capture time.
