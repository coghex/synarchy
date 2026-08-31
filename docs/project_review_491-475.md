# Project Review Findings: PRs #491–#475

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #491, #488, #490, #487, #486, #484, #482, #481, #480, #477, #476, and #475 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The current strict-TVar updates, probe runner, `unsafePerformIO` replacements, C boundary hardening, named UI/simulation constants, night-perception calculation, Cabal semaphore workaround, `fromJust` removal, and comment cleanup retain their intended core behavior. Later work fixed two defects adjacent to this batch: #489 / PR #597 supplied the whetstone texture that #488's new audit already detected, and #622 added the separate cross-thread strictness boundary that #491 did not cover. The graphical-suite documentation limitation from #484 is already tracked by open #1153, while the bare-name icon-audit gap and repair ground-target omission are recorded as PRR-4 and PRR-2 in `docs/project_review_504-492.md`; none is duplicated here. The focused `Craft.Execute` suite passed 28/28, `python3 tools/texture_subset_audit.py` passed all 13 subsets, and the probe-runner/audit Python modules compiled. No graphical session, repair behavior probe, full suite, world check, or `make ci` was run. Two non-duplicate concerns remain; both are deliberately marked lower-confidence for the processor to verify before drafting an issue.

## Status

- [x] PRR-1. Repair recipes publish work values that repair never consumes — [#1965]
- [x] PRR-2. The texture audit treats generic assets as per-subset unknowns — [no-issue]

## 1. Repair effort semantics

### [#1965] PRR-1. Repair recipes publish work values that repair never consumes

> **Captured note:** Give repair recipe `work` one coherent meaning. The two shipped repairs author different effort values and expose them through `repair.get`, but autonomous repair ignores the field and calls `repair.repairAt` on the first tick after reaching the station. Reforging with `work: 15` and honing with `work: 10` therefore have the same one-call completion behavior.

**Verification:** Partially verified. The current data flow proves that repair never reads or burns down `work`; only ordinary crafting copies `recipe.work` into a job and advances progress from it. What remains uncertain is the intended correction: repair may be missing a timed work phase, or its recipes may intentionally be instantaneous and should not advertise meaningful effort values. No live repair probe was run because the structural omission is unconditional, but the existing probes also assert only catalogue shape and eventual/full repair, not elapsed effort.

**Evidence:**

- Issue #301 explicitly left restore amounts/rates for the repair model to settle. PR #482 chose full restoration per visit but also authored distinct `work` values: `data/recipes/repair.yaml:18-40` assigns 15 to condition repair and 10 to sharpness repair.
- `src/Craft/Types.hs:64-67` defines `rdWork` as effort that the AI burns down, scaled by skill. `src/Engine/Scripting/Lua/API/Craft/Recipe.hs:95-114` publishes that same `work` field through the table shape shared by `craft.get` and `repair.get`.
- `scripts/unit_ai_repair.lua:215-245` fetches the repair recipe only to read its first input and records no work/progress value on the candidate or job.
- Once adjacent to the station, `scripts/unit_ai_repair.lua:354-385` changes from `walking` to `repairing`, then calls `repair.repairAt` on the next action tick without consulting elapsed time, skill, or `recipe.work`.
- `src/Engine/Scripting/Lua/API/Repair.hs:145-196` validates the recipe and station, consumes ingredients, and restores the selected axis atomically. It reads `rdRepairAxis`, inputs, and fuel through `consumeIngredients`, but never reads `rdWork`.
- By comparison, ordinary crafting copies `cand.recipe.work` in `scripts/unit_ai_craft.lua:339-350` and divides elapsed, skill-scaled progress by that value in `:421-454` before executing the recipe.
- `tools/repair_probe.py:178-198` checks the repair catalogue without asserting `work`; `:259-303` checks immediate API outcomes and resource consumption. `tools/repair_ai_probe.py:329-350` waits only for eventual full repair and XP. Neither distinguishes the two authored effort values.
- All-state tracker and findings-report searches for repair work, duration, or timing found the closed repair epic/model issues but no issue recording this surviving mismatch.

**Handoff context:**

- **Current behavior:** After travel and consumable sourcing, every successful repair completes in one synchronous action regardless of its authored `work`. The API nevertheless tells Lua callers that condition repair costs 15 work and sharpness repair costs 10.
- **Expected behavior:** Accepted repair content has an honest effort contract: authored work affects repair progress/duration consistently with the chosen model, or repair recipes and their public table shape clearly declare that repair is instantaneous and carries no operative work value.
- **Scope and constraints:** Surfaced from PR #482 / issue #301. Preserve full-axis restoration, axis-specific stations and consumables, exact item identity, atomic refusal/rollback, autonomous-job preemption, and smithing XP. Do not conflate restoration amount with elapsed effort; they are separate choices. If repair becomes timed, cover both the autonomous path and any intended direct `repair.repairAt` contract rather than adding a delay in only one caller.
- **Remaining uncertainty:** The YAML comment deliberately says one visit restores fully, but it does not say the visit is instantaneous. The generic `RecipeDef` schema may require `work` even when unused, and the distinct 15/10 values may be vestigial content rather than an omitted feature. The processor should settle that product intent before drafting acceptance criteria.

## 2. Texture fallback contract

### [no-issue] PRR-2. The texture audit treats generic assets as per-subset unknowns

> **Disposition:** No issue — the audit already distinguishes shared fallbacks
> from per-subset assets via a `shared_fallback` reason printed on all four
> affected rows, and `utility/blanktexture.png` is not a substitute for
> vegetation's unknown asset but the engine's own unmapped-vegetation-id
> texture (`World/Vegetation.hs:128`, `World/Render/TileQuads.hs:128`), so the
> load-failure path and the unmapped-id path agree by design. It is also a
> single flat colour (1 distinct colour, luminance stdev 0.00) on the same
> 96x64 3936-pixel diamond mask as all 77 shipped vegetation textures, each of
> which carries 5-6 colours — so a missing path reads as an obvious void, not
> as ordinary terrain. #478's acceptance ("zero yaml entities silently relying
> on a generic fallback") holds: the audit reports zero unresolved declared
> paths across all 13 subsets.

> **Captured note:** Reconcile #478's per-subset unknown-texture contract with the implementation that closed it. Vegetation still substitutes the generic gray `utility/blanktexture.png`, and the audit labels that shared asset as vegetation's canonical unknown, even though the issue explicitly required `unknown_<subset>.png` assets and zero entities silently relying on generic fallbacks.

**Verification:** Partially verified. The contract deviation and current fallback wiring are direct in source, and visual inspection confirmed that `blanktexture.png` is a gray isometric terrain tile rather than a vegetation-specific missing-asset marker. The gap is dormant in shipped content because every current YAML-declared texture resolves and the audit passes. It is also possible that the explicit generic-asset exception in PR #488 represents a deliberate product decision that superseded the issue text but was never recorded as such.

**Evidence:**

- Issue #478's goal required every texture subset to have a canonical `unknown_<subset>.png`; its acceptance required zero YAML entities relying on generic, non-subset fallbacks. Its process note specifically required pausing for user-authored art rather than fabricating or silently substituting placeholders.
- `src/Engine/Scripting/Lua/API/YamlTextures.hs:39-52` implements the guarded missing-file path, but `:154-165` passes `assets/textures/utility/blanktexture.png` as the fallback for every vegetation variant.
- `tools/texture_subset_audit.py:2-11` says the tool verifies each subset's canonical unknown/placeholder. Its exception block at `:13-25` then declares several generic assets intentional, and the vegetation entry at `:137-147` records the shared utility texture as its `unknown` asset.
- The focused audit run reported vegetation as `[OK]` with `utility/blanktexture.png` and concluded that every subset has its unknown texture. Thus the gate cannot distinguish the implemented shared fallback from the per-subset asset that #478 required.
- Visual inspection during this review found `assets/textures/utility/blanktexture.png` to be a 96×64 gray isometric tile. A missing vegetation texture can therefore render as ordinary-looking terrain rather than an unmistakable category-specific error marker.
- The same audit run found no currently missing YAML paths. The one missing path present when this work landed, `whetstone.png`, was separately tracked by #489 and supplied by PR #597; it is not duplicated here.
- The audit's separate exclusion of bare-name icon mappings is already captured as PRR-4 in `docs/project_review_504-492.md`; this finding concerns only the generic-fallback exception and does not duplicate that coverage gap.
- All-state tracker and findings-report searches found closed #478 but no follow-up that explicitly accepts, replaces, or repairs the generic vegetation exception.

**Handoff context:**

- **Current behavior:** Missing vegetation art logs a warning and degrades without crashing, but it renders through a shared blank terrain tile. CI describes that asset as vegetation's canonical unknown and remains green, so future missing vegetation art does not expose the contract relaxation.
- **Expected behavior:** The repository has one explicit, truthful policy: either vegetation uses a visually distinguishable subset-owned unknown asset as #478 specified, or the shared blank tile is an accepted fallback and the audit/docs stop claiming that every subset owns a canonical unknown. In either case, the gate should describe exactly what it enforces.
- **Scope and constraints:** Surfaced from PR #488 / issue #478. Preserve the guarded load and warning behavior. Focus first on vegetation; world-material `notexture.png`, item placeholders, and equipment silhouettes have different visual and loader semantics and need not be swept into one remedy without evidence. If new art is still required, follow #478's user-authored-art constraint rather than generating it automatically.
- **Remaining uncertainty:** PR #488 made the exception conspicuous in code, so it may be an intentional simplification rather than an accidental miss. No current asset reference exercises the fallback, and the gray tile may be considered an adequate vegetation degradation in the actual renderer. A processor should reproduce one deliberately missing vegetation path graphically or obtain a maintainer decision before converting this note into prescriptive acceptance criteria.
