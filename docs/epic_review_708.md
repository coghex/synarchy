# Epic Review Findings: Epic #708 — Language-aware world naming

This report records the completed-arc review of epic #708 at
`master@16cf413f481c`, against its reconciled 18-child scope: body-declared
Phase 1 children #706, #707, #709, #710, and #713; and native/body-declared
corrective and integration children #1092, #1094, #1095, #1096, #1097,
#1098, #1100, #1101, #1102, #1104, #1105, #1106, and #1107. The runtime arc
is coherent and its focused current gates pass; one new source-contract
mistake remains.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. River naming still documents the pre-#1868 free-root instability — [#2231]

## 1. Generated-language contract maintenance

### [#2231] ER-1. River naming still documents the pre-#1868 free-root instability

> **Captured note:** Correct `World.River.Naming`'s write-once rationale after
> #1868: append-only concept ordinals keep every existing free root stable
> when a concept is added. The remaining catalogue-growth risk is that
> generator-v4+ bound-form selection ranks the complete current concept set
> and may move a bound form, not that collision resolution moves a later free
> root.

**Verification:** Verified. The river-naming module still describes the exact
ascending-id displacement defect #1868 removed. Current root assignment walks
the catalogue's append-only ordinal order, and its focused tests prove that an
added id leaves every incumbent free root unchanged across every supported
generator version. The write-once rule itself remains correct: bound-form
selection is deliberately outside that addition-stability guarantee and can
still alter a dependent rendering reconstructed from the current catalogue.

**Evidence:**

- `src/World/River/Naming.hs:21` — the module's write-once contract says
  catalogue growth can move a later concept's root through complete-set
  collision resolution.
- `src/Language/Generated/Root.hs:44` — the live root contract says placement
  is append-only ordinal order and an addition leaves every existing free root
  unchanged, while lines 56–59 identify bound forms as the residual risk.
- `test-headless/Test/Headless/Language/Generated.hs:701` — the current
  multi-version, multi-seed regression proves an added id leaves every
  existing concept's root untouched.
- `docs/engine_contracts.md:209` — the authoritative project contract scopes
  addition stability to free roots and explicitly attributes remaining
  movement to bound-form selection.

**Handoff context:**

- **Current behavior:** Runtime naming and persistence are correct, but the
  river-naming source contract tells maintainers that catalogue additions can
  re-root an incumbent through the mechanism #1868 replaced.
- **Expected behavior:** The module explains that append-only ordinals protect
  free roots, while write-once display text remains load-bearing because a
  catalogue addition can still change a selected bound form and because
  persisted names must never be re-rendered generally.
- **Scope and constraints:** This is a source-comment correction in
  `World.River.Naming`, not authorization to change root assignment,
  bound-form selection, generator versions, persisted names, or save schemas.
- **Verification target:** The focused `concept roots` group and
  `tools/concept_id_inventory_audit.py` remain green; generated-name vectors
  and `currentGeneratorVersion` do not change.
- **Deduplication:** All-state tracker searches for the module, catalogue
  growth, free-root additions, and bound-form additions found only closed
  source issue #1868. The docs-worktree report corpus contains #1868's
  historical BUG-6 record but no owner for this still-current stale module
  comment.
- **Remaining uncertainty:** None.
