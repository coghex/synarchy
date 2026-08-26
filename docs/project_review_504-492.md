# Project Review Findings: PRs #504–#492

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #504, #503, #502, #501, #499, #498, #497, #496, #495, #494, #493, and #492 — for later one-at-a-time disposition. The first-parent window also contains two direct commits between #494 and #493: `db44dfc9` (notification settings) and `d9ce2f82` (unit texture assets).

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The current fabrication recipes, craft-bill backend, slope traversal, location stamp marker, seam-aware location placement, seam-aware front-wall lift, and lenient Lua-boundary decoding retain their intended core behavior. Later work completed #492's intentionally partial decoder sweep and #502's profiling follow-up (#500 via PR #508). The known craft/construction capacity loop is already captured as `LUA-2`, and the repair claim/priority replacement-load leak as `LUA-3`, in `docs/lua_script_findings.md`; neither is duplicated here. The focused `Item.QualityTier` suite passed 15/15 and the existing `maxStaminaFor`-named mental-effectiveness example passed 1/1, but the latter does not construct any stat modifier. `python3 tools/texture_subset_audit.py` passed all 13 subsets while explicitly excluding per-entity bare-name icon validation. No graphical session, repair/craft/movement/location probe, full suite, world check, or `make ci` was run. Five non-duplicate current concerns remain; PRR-4 and PRR-5 are deliberately lower-confidence maintenance findings for the processor to verify before drafting an issue.

## Status

- [x] PRR-1. Haskell combat ignores modifiers when resolving max stamina — [#1735]
- [x] PRR-2. Repair AI still cannot claim degraded ground items — [#1737]
- [x] PRR-3. A non-exhaustive quality-tier override silently removes the tier — [#1739]
- [x] PRR-4. Injury icon mappings sit outside the asset audit and document the wrong fallback — [#1740]
- [ ] PRR-5. The direct unit-asset commit leaves 485 frames outside gameplay manifests

## 1. Combat stat coherence

### [#1735] PRR-1. Haskell combat ignores modifiers when resolving max stamina

> **Captured note:** Make Haskell combat resolve `max_stamina` with the same effective-stat semantics as Lua's canonical `stats.get`. `maxStaminaFor` reads raw `uiStats`, so an active modifier on either an explicit `max_stamina` attribute or its derived `endurance` input changes the pool reported to Lua without changing combat's stamina cost or stamina-fraction denominator.

**Verification:** Verified structurally and with a concrete arithmetic case against current source. For raw `endurance = 1` plus a live `+1` modifier, Lua obtains effective endurance 2 and derives `max_stamina = 20`; Haskell reads raw endurance 1 and returns 10. A heavy attack therefore drains 2.5 rather than 5 stamina, and damage calculation divides current stamina by 10 rather than 20. The same disagreement occurs when a unit has an explicit raw `max_stamina` plus a modifier on that attribute. No live engine reproduction was needed to establish the two deterministic code paths, but no modifier-bearing regression test currently compares them.

**Evidence:**

- Issue #388 / PR #494 set out to unify `max_stamina` and explicitly claimed the mirror follows Lua's dispatch, including live stat changes. The current comment in `src/Combat/Resolution/Wear.hs:156-159` still promises that a fresh endurance buff or wound changes the attack cost.
- `scripts/unit_stats.lua:136-143` resolves an explicit attribute through `unit.getStat` first and only then invokes the derived formula; `:75-83` derives a missing `max_stamina` from `unit.getStat(uid, "endurance") * 10`.
- `src/Engine/Scripting/Lua/API/Units/Stats.hs:275-286` defines `unit.getStat` as the effective value, and `:608-624` applies the active `uiModifiers` list through `effectiveStat` at the current game time.
- `src/Engine/Scripting/Lua/API/Units/Stats.hs:376-429` allows a modifier on any named stat, including `endurance` and `max_stamina`, with additive, percentage, and expiring forms.
- `src/Combat/Resolution/Common.hs:52-67` instead looks directly in `uiStats`: an explicit `max_stamina` is returned raw, and the fallback multiplies raw `endurance` by 10. It receives neither `uiModifiers` nor the game time needed to expire them.
- `src/Combat/Resolution/Wear.hs:160-170` uses that raw mirror to size every quick/heavy stamina drain; `src/Combat/Resolution/Damage.hs:246-252` uses it again for the winded attacker's effectiveness.
- The focused `--match "maxStaminaFor"` run passed one example, but `test-headless/Test/Headless/Combat/MentalEffectiveness.hs:259-278` only proves concentration/mental state do not affect the pool. It does not put a modifier on `endurance` or `max_stamina` and therefore cannot catch this split.
- All-state tracker and findings-report searches for `maxStaminaFor`, modifier-aware `max_stamina`, and modified endurance found only closed #388 and closed #307, neither of which records this remaining Haskell/Lua disagreement.

**Handoff context:**

- **Current behavior:** Lua resource/UI/AI consumers see the modifier-aware pool, while Haskell combat charges attacks and evaluates winded damage against the raw pool. Buff expiry can also make Lua change back while the Haskell result remains unchanged throughout.
- **Expected behavior:** One unit at one game time has one effective `max_stamina` for resource display, AI, damage effectiveness, and attack drain, including explicit overrides and active additive/percentage modifiers.
- **Scope and constraints:** Surfaced from PR #494 / issue #388. Preserve the explicit-`max_stamina`-wins dispatch and modifier expiry semantics. `maxStaminaFor` is currently pure and receives only `UnitInstance`; a repair may need to accept game time, consume an already-resolved effective value, or move the derivation to a shared engine-side helper that Lua also calls. Add focused cases for modifiers on both the explicit attribute and derived endurance input.
- **Remaining uncertainty:** No shipped item or unit definition currently appears to author an endurance modifier, so ordinary stock gameplay may not expose the split today. The public runtime modifier API makes it a supported live state, and the stale promise specifically names buffs/wounds, but the processor should decide whether that is enough for a correctness issue now or contract hardening before such content ships.

## 2. Repair source coverage

### [#1737] PRR-2. Repair AI still cannot claim degraded ground items

> **Captured note:** Complete the repair target sourcing ladder promised by #302. A degraded weapon or armor instance dropped on the ground is invisible to `repair_job`; ground pickup is used only for repair consumables, even though `item.pickupGround` preserves the exact target instance and `item.listGround` now exposes its condition.

**Verification:** Verified against the issue contract, the current candidate scan, the ground-item API, and the repair probe. The action considers the worker's inventory/equipment/accessories and the nearest technomule's held items only. Its own comment declares ground targets out of scope based on an API description that is now partly stale: `item.listGround` does expose the ground id and condition, but not the underlying item instance id or sharpness. Consequently a condition-degraded ground target could already be identified by ground id, while sharpness targets need one small API extension; neither path exists. No live reproduction was run because the absence of a ground-target scan makes the result unconditional.

**Evidence:**

- Issue #302 scoped the target ladder explicitly as own inventory → ground item → technomule and required the degraded item to be returned after repair. PR #504 chose autonomous rather than player-designated repair, but ground sourcing was not one of the issue's deliberately open choices.
- `scripts/unit_ai_repair.lua:3-18` documents ground-item targeting as out of scope and says `item.listGround` exposes no instance id, condition, or sharpness.
- `scripts/unit_ai_repair.lua:158-196` implements exactly two target owners: `scanHeldItems` over the worker's held/equipped/accessory items, then the nearest mule. It never enumerates ground targets.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:124-178` currently exposes each ground entry's stable page-local ground id, definition, position, quality, condition, and weight. It still omits `iiInstanceId` and `iiSharpness`, explaining part but not all of the original limitation.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:278-337` moves a selected ground id into the unit while preserving the full `ItemInstance`; after pickup the ordinary inventory API can reveal its instance id and sharpness for the existing claim/repair machinery.
- `scripts/unit_ai_fetch.lua:96-144` already supplies the walk-to-ground, capacity, raced-pickup, and instance-preserving transfer mechanics used for consumables and crafting inputs.
- `tools/repair_ai_probe.py:9-51` covers own, equipped, mule-held, dead-claimant, abort-return, collision, role, and priority cases. Its only ground phase puts a whetstone consumable on the ground for an equipped target; it never drops the degraded target itself.
- Targeted all-state tracker and findings-report searches for a ground-target repair follow-up found none. `LUA-3` concerns the persistence of `repairClaims`/`repairPriority`, not source coverage.

**Handoff context:**

- **Current behavior:** Dropping damaged gear on the ground removes it from autonomous maintenance indefinitely. A worker may walk past it to repair a less urgent spare on the mule; the UI cannot prioritize the ground instance because priority is keyed by an unavailable `instanceId`.
- **Expected behavior:** A repairable ground instance participates in the same severity/claim/station/consumable decision as held and mule items, is picked up without identity substitution, and is returned to its original source policy after the repair or a late abort.
- **Scope and constraints:** Surfaced from PR #504 / issue #302. Decide whether the durable claim key remains `instanceId` (which argues for exposing it in a page-scoped ground query) or becomes a typed `(page, ground id)` until pickup. Add `sharpness` if ground sharpness repair is supported. Preserve #1208's owning-page rule, exact-instance transfer, capacity/race behavior, and abort rollback; add a focused degraded-ground-target phase rather than conflating it with the existing ground-consumable case.
- **Remaining uncertainty:** The precise return policy was not settled by the merged implementation: “return the item” plausibly means restore a ground-sourced target near its original position, but autonomous repair could instead intentionally keep it with the worker or put it in cargo. That product choice should be made explicitly before drafting acceptance criteria.

## 3. Quality-tier authoring

### [#1739] PRR-3. A non-exhaustive quality-tier override silently removes the tier

> **Captured note:** Validate each authored `quality_tiers` table as a total, unambiguous mapping over the supported quality range. Any non-empty override replaces the safe default table wholesale, but YAML loading accepts a lowest threshold above zero; instances below it then receive no `qualityTier` field and silently lose the suffix instead of rejecting malformed content.

**Verification:** Verified through the pure resolver and parser. For `quality_tiers: [{min: 80, label: masterwork}]`, `qualityTierLabel def 50` sorts the one entry, finds no threshold that 50 clears, and returns `Nothing`. All Lua item surfaces interpret `Nothing` as “omit the field,” making malformed authoring indistinguishable from an item with no tier. The focused suite passed 15/15 but constructs only exhaustive custom tables with a zero floor and tests only that both YAML fields are present.

**Evidence:**

- Issue #345 / PR #503 promised a correct tier across all threshold bands, with a sensible default and per-item overrides.
- `src/Item/Types.hs:348-358` explains that the default table's zero-floor guarantees a result for every non-negative quality.
- `src/Item/Types.hs:360-369` discards that table whenever `idQualityTiers` is non-empty and returns `Nothing` if the quality clears no authored minimum. It does not supply the default's bottom band as a fallback.
- `src/Engine/Asset/YamlItems.hs:83-94` parses each `{min, label}` pair without checking range, finiteness, duplicate thresholds, empty labels, or an exhaustive floor; `:202-213` accepts the resulting list directly on every item definition.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:155-165` simply omits `qualityTier` when resolution returns `Nothing`; the inventory/equipment render helpers use the same resolver and omission shape.
- `test-headless/Test/Headless/Item/QualityTier.hs:56-75` uses `[80 masterwork, 0 crude]` for the only custom-table behavior case and verifies only the syntactic absence of `label`. There is no negative authoring case for a missing floor, duplicate/out-of-range threshold, empty label, or non-finite value.
- No shipped `data/**/*.yaml` currently declares `quality_tiers`, so this is a dormant data-contract defect rather than a broken stock item. All-state tracker and findings-report searches found no existing validation follow-up.

**Handoff context:**

- **Current behavior:** The first future content author to omit a zero-floor band can load successfully and get tier suffixes only for the top of the quality range. Duplicate or nonsensical thresholds also load, with behavior determined by sorting and first-match details rather than a declared schema rule.
- **Expected behavior:** Every accepted non-empty override yields exactly one non-empty label for every supported quality value. Invalid authoring fails at content load with the item id and offending tier; alternatively the resolver deliberately fills the uncovered bottom range from a documented fallback policy.
- **Scope and constraints:** Surfaced from PR #503 / issue #345. At minimum decide and gate exhaustiveness, finite/ranged minima, duplicate minima, and non-empty labels. Keep author order irrelevant if threshold sorting remains part of the contract. Focused pure/YAML tests are sufficient; no GUI test is needed for rejection semantics.
- **Remaining uncertainty:** The issue did not explicitly require malformed-content rejection, and no production override uses the schema yet. The processor may choose a smaller contract-hardening issue or defer until the first per-item table is authored, but the current API advertises an override shape that can silently violate its total-tier promise.

## 4. Injury icon integrity

### [#1740] PRR-4. Injury icon mappings sit outside the asset audit and document the wrong fallback

> **Captured note:** Put the bare-name injury icon map under a deterministic asset-integrity check and update its fallback contract. #496 manually verified every mapped file, but the repository's texture audits explicitly exclude these runtime-resolved names; deleting or misspelling `severed.png` can therefore pass while the panel quietly shows `injury_unknown`, despite `injuries.lua` still saying the row degrades to dim text.

**Verification:** Partially verified. Every current value in `KIND_ICON` and `INJURY_ICON` has a matching PNG under `assets/textures/icons/injury`, including `severed.png`, so there is no present missing-art failure. The gap is durable enforcement and a stale contract comment: the runtime now resolves a missing injury basename to the family unknown texture, whereas the map's comment describes a text fallback. No graphical session was run.

**Evidence:**

- PR #496 closed issue #372 by removing one stale TODO after manually checking `severed.png` and the other mapped injury assets. It added no automated asset-map coverage.
- `scripts/injuries.lua:124-145` is the authoritative bare-name map. Its comment says a miss degrades to a dim text label.
- `scripts/unit_info_v2_panel_engine.lua:26-59` builds the runtime basename index and, on a miss, resolves `kind .. "_unknown"`; an injury row therefore loads `injury_unknown.png` when present rather than falling directly to text.
- `tools/texture_subset_audit.py:27-35` explicitly states that per-entity bare-name validation for injury and the other icon families is out of scope. The successful 13-subset run during this review consequently proves only that the unknown injury fallback exists, not that the eight defaults and three overrides resolve.
- A direct inventory during this review found all eleven current mapping values on disk. That is the same manual relationship #496 relied upon and can drift independently on the next art/map edit.
- Tracker and findings-report searches found no issue covering bare-name icon-map integrity or the stale injury fallback statement.

**Handoff context:**

- **Current behavior:** Current injury rows resolve correctly. A future missing/misspelled mapping silently falls back to the generic injury icon while the asset audits stay green and the source comment tells maintainers to expect a different degradation path.
- **Expected behavior:** CI checks every authoritative injury basename against the family directory (or a declared intentional-fallback allowlist), and the comment accurately states the unknown-icon-then-text behavior.
- **Scope and constraints:** Surfaced from PR #496 / issue #372. Prefer extending the existing texture audit or a small pure Lua/asset check over booting the engine. If generalized across all bare-name icon families, keep family ownership explicit and do not reject deliberate uses of the family unknown icon.
- **Remaining uncertainty:** This is a regression-prevention and documentation finding, not a current visual defect. The processor may combine it with a broader icon-family audit or mark it no-issue if the unknown fallback is considered sufficient protection.

## 5. Direct asset inventory

### PRR-5. The direct unit-asset commit leaves 485 frames outside gameplay manifests

> **Captured note:** Decide the lifecycle of the 485 PNGs added by direct commit `d9ce2f82`. The complete white-tailed-deer animation set has no unit definition, and the technomule's added `hit_react` frames are absent from its YAML animation map, so the ordinary unit renderer cannot select any of them.

**Verification:** Partially verified by commit inventory, current reference search, image decoding, and renderer inspection. The commit added 465 white-tailed-deer frames and 20 technomule `hit_react` frames (about 1.9 MiB total). All 485 decode successfully and each animation/direction directory has internally consistent dimensions/modes. No current source, script, data file, test, tool, or ordinary documentation names `white_tailed_deer`; `data/units/technomule.yaml` declares idle/walk/run but not `hit_react`. The preview browser can intentionally discover YAML-less asset folders, so these files are not unreachable from every developer tool, but they are unreachable from gameplay's `UnitDef`-driven frame selection.

**Evidence:**

- First-parent commit `d9ce2f82a91194ea7d8017224cc8bd7676432ec9` (`Add unit texture assets`) added 485 binary files without a PR body, linked issue, unit YAML, manifest note, or test change.
- The current tree contains 465 files under `assets/textures/units/white_tailed_deer/animations` (about 1.8 MiB) across alert, attack, death, drink, eat, hit-reaction, idle, sleep-transition, run, and walk families. A repository-wide reference search returns no `white_tailed_deer` consumer.
- The same commit added 20 files under `assets/textures/units/technomule/animations/hit_react` (about 80 KiB). `data/units/technomule.yaml:181-186` maps standing idle/walk/run and its `animations` block contains only those three names; no `hit_react` reference exists elsewhere.
- `src/Unit/Render.hs:48-70` selects frames only by looking up the instance's current animation in `udAnimations`; an absent definition falls back to the T-pose. Merely existing under the asset directory does not make a clip playable.
- `src/Engine/Preview/Unit.hs` deliberately discovers directories and supports YAML-less units, so the art remains browseable in the developer preview. That may be the intended staging model, but neither the direct commit nor current docs records it as staged/unshipped content.
- A Pillow verification during this review decoded all 485 files with zero errors and found no mixed image mode or dimensions within any one direction directory. The concern is wiring/ownership, not corrupt art.
- All-state tracker and findings-report searches for the deer assets, unwired technomule hit reaction, and unreferenced animation assets found no existing entry.

**Handoff context:**

- **Current behavior:** Nearly two MiB of finished-looking unit animation art ships in the repository and preview catalogue but cannot appear on a gameplay unit. Maintainers have no durable signal whether it is future content, abandoned content, or accidentally omitted YAML wiring.
- **Expected behavior:** Either add the intended unit/animation definitions and behavior hooks, explicitly inventory these files as staged art with an owner/follow-up, or remove/move abandoned assets so the gameplay asset tree describes usable content.
- **Scope and constraints:** Surfaced from the direct commit inside the #504–#492 first-parent window. Do not invent a deer design from filenames alone. If wiring is intended, settle unit stats/body/AI/animation-state semantics separately from the mechanical frame manifest; if staging is intentional, a lightweight documented inventory may be enough.
- **Remaining uncertainty:** High. Asset pre-provisioning may have been deliberate, and the preview browser intentionally supports exactly this YAML-less state. The finding asks the processor to recover the intended ownership decision, not to assume deletion or to draft a gameplay feature without product direction.
