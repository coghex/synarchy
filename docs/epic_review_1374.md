# Epic Review Findings: Epic #1374 — Make determinism tests actually test determinism

This report records the current-HEAD review of epic #1374 at
`b94fdb65466078656762b05274fa7ae375af6efd`. The epic has no native GitHub
sub-issues; its body declares sixteen children, reconciled as #1367, #1368,
#1369, #1370, and #1375 through #1386. Every child is closed as completed and
each implementation PR is merged. The original CIT-9 through CIT-12, CIT-16
through CIT-20, CIT-22 through CIT-27, and CIT-29 dispositions remain coherent:
their literal goldens, independent-input properties, canonicalization check,
and real world-init integration fixture are present and their focused current
checks pass. Four new, deduplicated same-input recurrence scopes survive outside
that original child set, so the epic's completed repairs compose but the defect
class has demonstrably recurred.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] ER-1. Item-contents identity examples compare signatures to themselves
- [ ] ER-2. Arena seed contract examples reduce both sides to seed zero
- [ ] ER-3. Encounter-roll stability compares one result vector to itself
- [ ] ER-4. Repeated capability projections cannot prove that pure projections allocate no refs

## 1. Same-input assertions added by later feature work

### ER-1. Item-contents identity examples compare signatures to themselves

> **Captured note:** PR #1625 added two examples named for equal flat and nested
> item signatures, but each compares the same pure `Text` expression to itself.

**Verification:** `sigWith id` and `deepSigWith id` each call
`itemContentsSig` once per side with identical arguments. Their result type is
`Text`, whose ordinary reflexive equality leaves no NaN-like exception. Any
deterministic change to the signature, including a constant or an omitted
represented field, moves both sides together. The neighboring one-field
`shouldNotBe` cases and the two deliberately excluded-field comparisons are the
real flat/deep coverage and remain green.

**Evidence:**

- `test-headless/Test/Headless/Item/ContentsSignature.hs:51` — `sigWith` is a pure
  `Text`-valued helper over one child fixture.
- `test-headless/Test/Headless/Item/ContentsSignature.hs:72` — “identical
  children” asserts `sigWith id `shouldBe` sigWith id`.
- `test-headless/Test/Headless/Item/ContentsSignature.hs:105` — “identical
  grandchildren” repeats the same shape through `deepSigWith`.
- Focused current check: `--match "Item.ContentsSignature"` passes 23 examples,
  including both assertions.

**Handoff context:**

- **Current behavior:** Two examples advertise flat and recursive identity
  coverage but can fail only if evaluation throws.
- **Expected behavior:** Delete them as redundant, or replace them only if there
  is an independently stated signature oracle worth maintaining; preserve the
  field-sensitivity, exclusion, weight-distribution, and order tests.
- **Scope and constraints:** Test-only scope around PR #1625 / issue #1597;
  `itemContentsSig`'s production field set and grouping policy are not in
  question.
- **Verification target:** The focused `Item.ContentsSignature` group passes and
  contains no same-expression equality; a represented-field mutation must still
  fail its existing flat and deep checks.
- **Deduplication:** All-state tracker searches for the exact descriptions,
  `sigWith id`, and `itemContentsSig` found only closed source issue #1597 and
  adjacent completed work. The docs-worktree report corpus contains
  `project_review_279-261.md`'s already-filed production defect, not this
  false-green test concern.
- **Remaining uncertainty:** None.

### ER-2. Arena seed contract examples reduce both sides to seed zero

> **Captured note:** PR #1798's pure arena-seeding group contains one textual
> self-comparison and one semantic self-comparison through a fixture whose
> `wgpSeed` is the same literal `arenaSeed` on the other side.

**Verification:** `generateArenaChunks` and `arenaGenForSeed` are pure. The first
example invokes the identical expression twice. The second compares
`arenaGenForSeed (wgpSeed arenaParams)` with `arenaGenForSeed arenaSeed`, while
`arenaParams` is constructed immediately above with `wgpSeed = arenaSeed` and
`arenaSeed = 0`; it therefore has no stored-versus-rebuilt asymmetry. A
deterministic change to seed-zero generation moves both sides together. The
different-seed vegetation check, seed-blind topology checks, live world-init
comparison, and fresh-process save probe remain the meaningful coverage.

**Evidence:**

- `test-headless/Test/Headless/World/ArenaSeed.hs:49` — `arenaSeed` is the literal
  zero required by arena recognition.
- `test-headless/Test/Headless/World/ArenaSeed.hs:58` — `arenaParams` directly
  stores that same `arenaSeed`.
- `test-headless/Test/Headless/World/ArenaSeed.hs:66` — the first contract example
  compares two identical `generateArenaChunks (arenaGenForSeed arenaSeed)` calls.
- `test-headless/Test/Headless/World/ArenaSeed.hs:71` — the second contract
  example merely reads the same literal back from the hand-built fixture.
- `test-headless/Test/Headless/World/ArenaSeed.hs:108` — the live engine example
  instead compares initialized chunk state with reconstruction from the params
  the engine actually recorded.
- Focused current check: `--match "Arena base seeding"` passes all 6 examples.

**Handoff context:**

- **Current behavior:** Two pure examples claim same-seed rebuild and params-seed
  derivation coverage without independent inputs or an oracle.
- **Expected behavior:** Remove the redundant examples, or pin a compact literal
  seed-to-vegetation result if that mapping is intentionally compatible across
  code revisions. Preserve the different-seed/topology properties, live wiring
  test, and save/load probe.
- **Scope and constraints:** Test-only correction around PR #1798 / issue #1718;
  do not change canonical arena seed zero, generator topology, or save/load
  behavior to satisfy the test.
- **Verification target:** `--match "Arena base seeding"` remains green with no
  same-input equality. The live engine comparison remains present; if a literal
  oracle is chosen, a controlled seed-mapping mutation must fail it.
- **Deduplication:** All-state searches for `generateArenaChunks`, the exact
  example names, and arena tautologies found no corrective tracker item. Closed
  #1718 owns the production seed mismatch, and
  `project_review_459-450.md` records that original defect; neither owns these
  assertions.
- **Remaining uncertainty:** Whether seed-to-vegetation output is a long-term
  compatibility surface or only the fresh/load agreement is contractual; that
  choice determines removal versus a small golden.

### ER-3. Encounter-roll stability compares one result vector to itself

> **Captured note:** PR #1900 added a persistent-encounter example whose final
> assertion compares `map rolled [0 .. 255]` to an identical copy, so it does not
> establish the “stable” part of its description.

**Verification:** `rolled` projects the `Int` count from the pure construction
`newLocationInstanceWithSeed seed ...`. The preceding assertions genuinely pin
the inclusive range and reachability of all four outcomes, but neither would
notice a deterministic remapping of every `(page seed, instance id)` pair. The
last equality also cannot notice such a change because both sides use the same
mapping.

**Evidence:**

- `test-headless/Test/Headless/Location/Instance.hs:473` — `rolled` derives one
  encounter count from the supplied seed and stable instance id.
- `test-headless/Test/Headless/Location/Instance.hs:480` — the example promises a
  stable inclusive count from those two inputs.
- `test-headless/Test/Headless/Location/Instance.hs:482` — range and outcome
  reachability are asserted independently.
- `test-headless/Test/Headless/Location/Instance.hs:487` — the purported stability
  assertion is `map rolled [0 .. 255] `shouldBe` map rolled [0 .. 255]`.
- Focused current check: `--match "Location instance identity"` passes all 75
  examples, including this line and the real literal identity oracle from child
  #1384.

**Handoff context:**

- **Current behavior:** The suite proves bounds and outcome coverage but not a
  stable seed/id-to-count mapping, despite saying it does.
- **Expected behavior:** Add a small literal seed/id vector if replay stability
  is a contract, or delete the equality and narrow the description to range and
  reachability. Do not retain a second invocation of the production mixer as the
  expectation.
- **Scope and constraints:** Test-only correction around PR #1900 / issue #916;
  preserve encounter persistence, allocator identity, lifecycle, migration, and
  roster tests.
- **Verification target:** The focused `Location instance identity` group passes;
  a deliberately changed mixer must fail any retained stability oracle while
  range/reachability coverage continues to pass.
- **Deduplication:** All-state searches for the exact example text, `map rolled`,
  encounter-roll determinism, and tautological encounter tests found no tracker
  owner. `project_review_1981-1968.md` covers a separate stale registry-count
  description from PR #1900, not this assertion.
- **Remaining uncertainty:** Whether new-encounter rolls must be stable across
  code revisions or only persisted once after allocation; the implementation
  currently documents a stateless seed/id derivation, but the durable
  compatibility boundary should decide golden versus deletion.

## 2. Pure capability projections repeat an impossible failure mode

### ER-4. Repeated capability projections cannot prove that pure projections allocate no refs

> **Captured note:** Ten capability modules contain eleven “stable across
> repeated projection (no fresh containers)” examples, and the read-only view
> suite adds a same-handle wrapper example. They compare container identities
> obtained from two applications of the same pure projection or wrapper.

**Verification:** Every `to*Capability` function under test has a pure
`EngineEnv → Capability` signature, and `toReadOnlyRef` is a pure newtype-style
`IORef α → ReadOnlyRef α` conversion. Without hidden unsafe IO, applying one of
those functions twice cannot allocate two fresh `IORef`, `TVar`, or queue
containers. The repeated examples therefore move both sides together. Each
suite's direct capability-field-versus-`EngineEnv` checks, cross-capability
checks, and `ReadOnlyRef` write-then-read liveness checks are the real guards
against copying, swapping, or snapshotting and should remain.

**Evidence:**

- `src/Engine/Core/Capability/Building.hs:105` and the corresponding pure
  signatures in `ContentRegistriesView.hs:85`, `Events.hs:120`, `Input.hs:86`,
  `InputView.hs:85`, `Render.hs:121`, `RenderHandoff.hs:173`, `SaveLoad.hs:119`,
  `Ui.hs:133`, `UnitCombat.hs:183`, and `WorldSim.hs:155` — every projection is
  pure.
- `src/Engine/Core/ReadOnlyRef.hs:65` — `toReadOnlyRef` is likewise pure.
- `test-headless/Test/Headless/Capability/Building.hs:59`, `Events.hs:72`,
  `Input.hs:99`, `Input.hs:134`, `Render.hs:81`, `RenderHandoff.hs:85`,
  `SaveLoad.hs:71`, `Ui.hs:55`, `UnitCombat.hs:81`, `WorldSim.hs:66`, and
  `ContentRegistriesView.hs:102` — the eleven repeated-projection examples.
- `test-headless/Test/Headless/Capability/ContentRegistriesView.hs:75` — two
  identical `toReadOnlyRef raw` calls are compared for container identity.
- Focused current check: `--match "Capability"` passes all 135 examples,
  including these twelve and the adjacent direct-alias assertions.

**Handoff context:**

- **Current behavior:** Twelve examples can fail only after an unsafe/effectful
  implementation violates their pure APIs; they do not add coverage beyond the
  direct live-container assertions already present.
- **Expected behavior:** Remove the repeated-projection and two-wrap examples,
  preserving field-to-`EngineEnv`, view-to-writer, cross-field, and mutation
  liveness assertions. If allocating on projection ever becomes intentional,
  make the API effectful and test that effect explicitly rather than relying on
  equal evaluations of a pure expression.
- **Scope and constraints:** Test-only cleanup spanning the ten capability
  modules. Do not weaken the capability inventory, field ownership, or direct
  aliasing contract.
- **Verification target:** `--match "Capability"` passes with every named field
  still compared to its live owner and no pure repeated-projection equality.
- **Deduplication:** All-state searches for `stable across repeated projection`,
  `sameContainer` plus repeated projection, and `two wraps of one handle` found
  no corrective issue; `no fresh containers` found only closed source issue
  #895. The docs-worktree report corpus has no matching pending concern.
- **Remaining uncertainty:** None about the current false-green mechanism. The
  broader choice to add a detector is separate: these custom-comparator and
  semantic-binding shapes show that a trustworthy gate would need more than a
  textual `shouldBe` scan.
