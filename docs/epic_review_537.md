# Epic Review Findings: Epic #537 — Split EngineEnv into capability-scoped surfaces

This report records the current-HEAD review of epic #537 at
`9ada343081e09ddd4994d9106894bcb0ccdb1ebb`. The epic has one native GitHub
sub-issue, #876, and its body declares eleven implementation children, #889
through #899; the reconciled union is therefore twelve children. Every child is
closed as completed and every implementation PR is merged. The resulting
architecture remains coherent at current HEAD: all 89 live `EngineEnv` fields
are classified, the unrestricted-access boundary contains 24 permanent modules
and zero temporary exceptions, the capability audit and its 156 self-test groups
pass, and the focused capability suite passes 135 examples. Two new current
mistakes survive: the closed epic's body still presents the split as active and
largely unimplemented and leaves every implementation child unchecked, while
the authoritative inventory understates two capability-record sizes after later
fields joined them.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. Epic #537's live body still says the completed capability split is unimplemented — [#537]
- [ ] ER-2. The capability summary understates the live WorldSim and RenderHandoff records

## 1. Epic closure steering

### [#537] ER-1. Epic #537's live body still says the completed capability split is unimplemented

> **Captured note:** Epic #537 is closed and its owner closure comment verifies
> all twelve children and the permanent-only boundary, but the body still says
> the epic is “still active and largely unimplemented,” carries a stale 85-field
> snapshot, and leaves #889 through #899 unchecked.

**Verification:** The contradiction is present in the live tracker rather than
in the implementation. Child #899 explicitly required a post-merge update to
epic #537's checklist/status and required the implementation PR not to close the
epic itself. PR #1027 correctly left that tracker action for after merge. The
owner then closed #537 with a comment confirming all twelve children, an empty
temporary ceiling, 24 permanent full-access boundaries, and the completed
acceptance condition, but did not update the body. Current repository guidance
and the focused gates agree with the closure comment, not with the stale body.

**Evidence:**

- [Epic #537's live body](https://github.com/coghex/synarchy/issues/537) — the
  status block says the epic is active and largely unimplemented, reports 85
  fields, and leaves all eleven implementation-child boxes unchecked, while the
  issue itself is closed and its owner closure comment says all twelve children
  are complete.
- [Child #899, requirement 8](https://github.com/coghex/synarchy/issues/899) —
  made updating #537's checklist/status an explicit post-merge acceptance step.
- [PR #1027](https://github.com/coghex/synarchy/pull/1027) — merged the final
  capability child while correctly leaving epic closure to the promised
  post-merge action.
- `docs/engineenv_capability_inventory.md:3` — the authoritative as-built
  document says the split is complete, §6.2 is empty, and the permanent-only
  boundary is machine-enforced.
- `docs/engineenv_capability_inventory.md:41` — the audited live declaration has
  89 fields, not the body's dated 85-field snapshot.
- Current focused checks: `tools/engine_env_capability_audit.py` passes with 89
  classified fields, 24 permanent full-access modules, and zero temporary
  exceptions; its self-test passes all 156 groups; the persistence inventory
  audit passes; and `--match "Capability"` passes 135 examples.

**Handoff context:**

- **Current behavior:** A reader opening the closed epic sees an authoritative-
  looking status and unchecked roadmap that contradict the issue state, its
  closure comment, every completed child, and the current repository contract.
- **Expected behavior:** Update #537's status and child checklist to the landed
  state. Preserve the 2026-07-23 measurements only if they are clearly labeled
  as a historical pre-migration snapshot; prefer linking the audited inventory
  over copying another live field/import count into the tracker.
- **Scope and constraints:** Tracker-body-only correction to epic #537. Do not
  change capability code, the current inventory, audit authority data, or any
  child issue merely to make the stale prose true.
- **Verification target:** The closed epic's body says the capability split is
  complete, marks #889 through #899 complete, retains the deliberate §6.1
  permanent exceptions, and agrees with its owner closure comment and the
  authoritative inventory.
- **Deduplication:** All-state searches for the exact stale sentence, epic #537
  checklist/status, and #537's capability-split status found only #537 itself
  and #899's unfulfilled post-merge requirement. The docs-worktree report corpus
  has no pending owner for this tracker correction. Epic #1890 and open issues
  #2059, #2062, #2064, and #2071 own separate later mutation-authority, audit,
  and test concerns, not #537's stale body.
- **Remaining uncertainty:** None about the contradiction. The only editorial
  choice is whether to retain the dated pre-migration snapshot as explicitly
  historical context or replace it with a concise completion summary.

## 2. Capability summary drift

### ER-2. The capability summary understates the live WorldSim and RenderHandoff records

> **Captured note:** The authoritative capability inventory's §2.1 summary
> still describes `WorldSimCapability` as 9 fields and
> `RenderHandoffCapability` as 7, but later accepted fields have grown the live
> records to 11 and 10 respectively.

**Verification:** The record declarations are unambiguous. `WorldSimCapability`
now carries the original nine fields plus `wsPlayerIntentGenRef` and
`wsEnginePauseGenRef`. `RenderHandoffCapability` now carries the original seven
plus `rhSceneStatsRef`, `rhStructureWallCatalogRef`, and
`rhStructureArtCatalogRef`. Every field is classified by the passing 89-field
inventory audit, so this is not missing ownership metadata or an implementation
boundary failure; it is stale steering prose in §2.1's canonical record table.

**Evidence:**

- `docs/engineenv_capability_inventory.md:145` — §2.1 introduces the canonical
  capability record/view summary.
- `docs/engineenv_capability_inventory.md:150` — reports
  `WorldSimCapability` as 9 fields and `RenderHandoffCapability` as 7.
- `src/Engine/Core/Capability/WorldSim.hs:75` — the live record declares 11
  fields through `wsSimQueue`.
- `src/Engine/Core/Capability/RenderHandoff.hs:96` — the live record declares 10
  fields through `rhStructureArtCatalogRef`.
- Current focused check: `tools/engine_env_capability_audit.py` passes with all
  89 `EngineEnv` fields classified, confirming that the live additions are
  otherwise represented by the authoritative inventory and boundary data.

**Handoff context:**

- **Current behavior:** Readers of the canonical convention table get two wrong
  record sizes even though the detailed field inventory and live types are
  current.
- **Expected behavior:** Make §2.1 agree with the live records. Prefer removing
  the hand-maintained parenthetical sizes or deriving/checking them mechanically
  so later legitimate field additions cannot repeat the drift.
- **Scope and constraints:** Documentation and, only if chosen, a focused audit
  assertion. Do not remove accepted fields, change capability ownership, or
  broaden unrestricted access to make the old counts true.
- **Verification target:** §2.1 either omits these volatile counts or reports 11
  `WorldSimCapability` fields and 10 `RenderHandoffCapability` fields; the
  capability audit and its self-test remain green.
- **Deduplication:** All-state searches for both capability names, their stale
  counts, record-size drift, and hand-maintained capability-inventory figures
  found no open corrective issue. Closed #1669 owns the separately marked total
  `EngineEnv` field sentence, not per-record sizes. The active mutation-authority
  design notes an earlier 8/11 version of this drift but explicitly excludes
  fixing §2.1's counts from that arc, and no pending findings report owns it.
- **Remaining uncertainty:** None about the mismatches. Removing the volatile
  counts versus teaching the audit to verify them is a maintainability choice.
