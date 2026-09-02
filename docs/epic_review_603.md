# Epic Review Findings: Epic #603 — Procedural injury blood decals and bleeding trails

This report records the current review of completed epic #603 against its
reconciled eight-child scope: #604, #606, #607, #788, #882, #883, #884, and
#885. All eight children are closed. Issue #884 was deliberately closed as not
planned after the owner decided blood should remain transient; #885 absorbed
that decision into the final architecture record and closure gate. At reviewed
snapshot `master@d93b57955`, the bounded decal/texture model, deterministic
procedural pixels, impact marks, moving trails, stationary pools, GPU teardown,
Lua observation surface, and save/load transience compose as intended. Two new
current steering-document mistakes remain.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. Blood.Pool still presents the abandoned persistence slice as forthcoming — [#2179]
- [x] ER-2. The canonical blood test inventory omits Blood.LuaApi — [no-issue]

## 1. Persistence steering

### [#2179] ER-1. Blood.Pool still presents the abandoned persistence slice as forthcoming

> **Captured note:** The stationary-pool module still says its immutable decal
> design locks a persisted record shape “ahead of #884,” although #884 was
> deliberately closed as not planned and the epic's settled contract is that
> blood is transient.

**Verification:** #883 correctly made pool growth additive: new immutable
decals are layered instead of mutating an existing record. Its surviving module
header still justifies that choice as preparation for #884's future persistence
work. The owner later closed #884 as not planned and explicitly said the
immutability constraint remains useful on its own merits, independent of
persistence. The final #885 architecture record and both current state
inventories consistently describe blood as transient by design. The comment is
therefore no longer historical context with a date or a settled claim; it is
present-tense implementation steering toward work the epic intentionally
removed.

**Evidence:**

- `src/Blood/Pool.hs:13-15` — the live module contract says decal records are
  never mutated, “which locks the persisted record shape ahead of #884.”
- `src/Blood/Types.hs:18-23` — the model's current contract says blood is
  deliberately never persisted and names closed #884 as the settled decision.
- `docs/blood_decals.md:336-343` — the final architecture record says both the
  page store and per-unit trail/pool accumulator are transient by design.
- `docs/persistence_state_inventory.md:219` — the audited root-owner inventory
  classifies `wsBloodStoreRef` as `Exclude` and names closed #884.
- Issue #884's owner closure comment says immutable records remain worthwhile
  for bounded layering and are independent of persistence; reopening that issue
  is the explicit reversal path.

**Handoff context:**

- **Current behavior:** A maintainer reading `Blood.Pool` is told that its data
  shape is constrained in preparation for a persistence child that the same
  epic deliberately abandoned.
- **Expected behavior:** Keep the additive/immutable pool contract, but explain
  it in terms of bounded layered growth, stable per-layer identity, and the
  shared decal lifecycle. If #884 is mentioned, identify it as the closed
  reversal path rather than forthcoming work.
- **Scope and constraints:** Comment-only correction. Do not add persistence,
  change the record shape, weaken the per-cluster layer bound, or rewrite the
  historical #883/#884 issue record.
- **Verification target:** Current `Blood.Pool`, `Blood.Types`,
  `docs/blood_decals.md`, and `docs/persistence_state_inventory.md` all describe
  the same transient contract; a source search has no live “ahead of #884”
  claim.
- **Deduplication:** All-state tracker searches for the exact phrase,
  “persisted record shape,” and `Blood.Pool` + #884 returned only the historical
  source issues #883/#884, finalization issue #885, and the later determinism
  repair #1377. None owns the current stale-comment correction. Exact and
  semantic searches of the primary and docs-worktree report corpora found no
  pending duplicate.
- **Remaining uncertainty:** None.

## 2. Test steering

### [no-issue] ER-2. The canonical blood test inventory omits Blood.LuaApi

> **Disposition:** No issue — fixed directly in the docs lane. The correction
> is two tokens in CLAUDE.md ("Five" → "Six", `Blood.LuaApi` added to the
> list), below the tracker-and-PR bar this report files at, and a
> CLAUDE.md-only deliverable has no PR lane in any case. Verified at
> `master@3002eb49`: `test-headless/Spec.hs:166,518` registers
> `Test.Headless.Blood.LuaApi` (`describe "Blood.LuaApi blood.gpuHandles
> (#1585)"`) on its own isolated engine beside the five groups at
> `:734-738`. The edit sits in the `docs-wip` worktree pending the owner's
> batch landing. Same category as CH-136.

> **Captured note:** CLAUDE.md still advertises five Blood hspec groups and
> lists the original five, but #1585/#1644 added a sixth registered group,
> `Blood.LuaApi`, to gate the read-only GPU-handle query used by the lifecycle
> probe.

**Verification:** The canonical subsystem entry is the place
`docs/blood_decals.md` directs test authors for exact `--match` targets. It says
there are five groups and omits `Blood.LuaApi`. The current test runner imports
and registers that module on its own isolated headless engine, and the group
contains six registered-Lua examples covering returned handle identity,
registry membership, malformed input, empty results, post-teardown lookup, and
observational purity. A current `--match Blood` run executes it alongside the
five documented groups. The inventory was accurate when CH-136 disposed the
older documentation concern, but became stale when #1644 added the new gate.

**Evidence:**

- `CLAUDE.md:1071-1078` — the canonical blood entry says “Five” and lists
  `Blood.Types`, `Blood.Texture`, `Blood.Impact`, `Blood.Trail`, and
  `Blood.Teardown`, but not `Blood.LuaApi`.
- `docs/blood_decals.md:452-457` — the architecture record explicitly delegates
  the turnkey hspec target inventory to that CLAUDE.md entry.
- `test-headless/Spec.hs:149-154` and `test-headless/Spec.hs:480-484` — the live
  suite imports `Blood.LuaApi` and registers it on an isolated headless engine.
- `test-headless/Test/Headless/Blood/LuaApi.hs:150-175` — the sixth group's
  describe name and its ownership-aware registered-Lua coverage.
- Focused validation at `master@d93b57955` — `cabal test
  synarchy-test-headless --test-options='--match "Blood"'` completed 128
  examples with zero failures, including all six `Blood.LuaApi` examples.

**Handoff context:**

- **Current behavior:** A contributor following the canonical inventory can
  overlook the only registered-Lua blood group, particularly when changing
  `blood.gpuHandles` or the GPU lifecycle probe's ownership oracle.
- **Expected behavior:** State that there are six Blood hspec groups and add
  `Blood.LuaApi` to the exact list. Keep the four-probe inventory and transience
  warning unchanged.
- **Scope and constraints:** Steering-doc correction only. Do not merge the
  isolated `Blood.LuaApi` fixture into the shared engine; its page manager and
  texture-size-cache mutations are why `Spec.hs` gives it a dedicated engine.
- **Verification target:** The count and names in CLAUDE.md exactly match the
  six registered `Test.Headless.Blood.*` groups, and `--match Blood` continues
  to execute the ownership-aware Lua cases.
- **Deduplication:** Exact and semantic all-state tracker searches for
  `Blood.LuaApi`, the five-group wording, and a blood hspec Lua inventory found
  no owning issue or pull request. CH-136's `[no-issue]` disposition records the
  then-correct five-group inventory before #1585/#1644 and therefore does not
  own this later drift. Searches of the primary and docs-worktree report
  corpora found no pending duplicate.
- **Remaining uncertainty:** None.

