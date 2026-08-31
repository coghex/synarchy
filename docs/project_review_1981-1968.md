# Project Review Findings: PRs #1981–#1968

This report records the senior review of the next twelve uncovered merged pull requests in merge order — #1981, #1979, #1977, #1976, #1975, #1974, #1973, #1900, #1972, #1971, #1970, and #1968 — plus direct first-parent commit `4960d4d9` in the same landing interval. The review read each pull request, its linked specification, merged diff and commits, then traced the surviving behavior at current HEAD. One new current documentation-contract mistake is retained below for later one-at-a-time disposition. The other eleven pull requests and the direct documentation commit produced no separate current concern, and no concern was explicitly excluded from this batch.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [ ] PRR-1. The persistence inventory omits one encounter reset registry

## 1. Transient unit-AI state inventory

### PRR-1. The persistence inventory omits one encounter reset registry

> **Captured note:** Update the authoritative persistence inventory and its nearby owner comment to enumerate all eleven transient unit-AI registries and all four ruin-encounter overlays introduced by PR #1900. Both still describe ten registries and three overlays because they omit `localEpisodeAggression`, even though the live reset registry and focused gate include it.

**Verification:** Verified by a complete static owner-to-registry trace and the focused reset gate. `unit_ai_encounter.lua` enrolls four overlay tables through `claims.track`, `unit_ai_claims.lua` declares the resulting eleven-table contract, and the focused test pins and passes that count. The authoritative inventory and the reset-hook comment each still state the pre-review count of ten/three.

**Evidence:**

- `docs/persistence_state_inventory.md:502` — the authoritative `unit_ai_claims` row says `resetAll()` empties ten registries and names only `localEpisodeActive`, `localEpisodeDisengaged`, and `localParticipation` as the encounter overlays.
- `scripts/unit_ai_save.lua:300-303` — the reset-hook owner comment likewise says there are three same-tick encounter overlays.
- `scripts/unit_ai_encounter.lua:27-30` — the live encounter module enrolls four tables: `localEpisodeActive`, `localEpisodeAggression`, `localEpisodeDisengaged`, and `localParticipation`.
- `scripts/unit_ai_claims.lua:1-6` — the registry owner documents eleven module-local tables and four encounter overlays, matching the live registrations.
- `test-headless/Test/Headless/Lua/UnitAiLoadReset.hs:329-336` — the focused oracle identifies the five coordinate claims, two repair tables, and four encounter overlays, then requires `trackedCount() == 11`. `cabal test synarchy-test-headless --test-options='--match "unit AI load reset"'` passed all eight examples during this review.
- Blame attributes both stale descriptions to PR #1900's implementation commit `bc44a5cc`; later merges in the reviewed interval do not correct them. The up-front open-issue inventory and all-state searches for the exact count and `localEpisodeAggression` found no tracker owner or findings-report duplicate.

**Handoff context:**

- **Current behavior:** Runtime reset behavior is correct and covered, but the authoritative persistence inventory and the reset-hook owner comment undercount the family and omit the aggression overlay. A maintainer relying on those descriptions receives an incomplete ownership/reset inventory for load replacement and session teardown.
- **Expected behavior:** Every authoritative or owner-facing enumeration agrees that the family contains eleven registries, including four encounter overlays, and names `localEpisodeAggression` alongside the other three.
- **Scope and constraints:** This is documentation/comment drift from PR #1900 / issue #916, not a runtime persistence defect. Preserve the existing `claims.track` registrations, in-place reset behavior, save-component exclusion, and the eleven-table focused oracle.
- **Verification target:** Update both enumerations, keep the focused `unit AI load reset` group passing, and ensure the persistence inventory still identifies the reset hook rather than misclassifying these transient tables as saved state.
- **Deduplication:** The open-issue inventory and all-state keyword searches for the registry count, encounter overlays, and `localEpisodeAggression` found no existing issue; the project-review and findings-report corpus contains no matching current entry.
- **Remaining uncertainty:** None.
