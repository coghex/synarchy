# Project Review Findings: PRs #411–#399

These entries record focused evidence from the senior review of the next twelve merged PRs in merge order — #411, #410, #409, #401, #408, #407, #404, #405, #406, #402, #400, and #399 — for later one-at-a-time disposition. The first-parent window contains exactly those twelve PR merges and no direct commits.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

The contextual-error additions, removed worldgen/channel-mask/Lua/UI/engine/fall-impact scaffolding, broken-overlay handle reuse, and active-world unit listing retain their intended core behavior in current source. The save-history heading error introduced beside #410 was repaired later by #984, and the retained hydrology meander seed still participates in persisted geological history and detailed logging rather than terrain generation. The focused `Unit.Render.pickFrame` suite passed 26/26 and `Wound.woundEffSeverity` passed 4/4. No graphical selection session, dynamic-chunk logging probe, full headless suite, world check, behavior probe, or `make ci` was run. Six non-duplicate concerns remain; the smaller documentation and dead-surface candidates are intentionally recorded with their uncertainty for the processor to verify before drafting any issue.

## Status

- [x] PRR-1. Unit hit-testing does not follow rendered climb and animation geometry — [#1957]
- [x] PRR-2. Directoryless Lua chunks lose their log source label — [#1960]
- [x] PRR-3. The wound tick still reimplements the shared effective-severity formula — [#1963]
- [x] PRR-4. Removing `Engine.Event` left an orphan exported `AssetEvent` — [#1966]
- [x] PRR-5. Unit activity and pose label comments have drifted from their sources — [#1967]
- [x] PRR-6. `unit.getWounds` advertises only a fraction of its returned schema — [#1969]

## 1. Unit render and interaction geometry

### [#1957] PRR-1. Unit hit-testing does not follow rendered climb and animation geometry

> **Captured note:** Make click and box selection derive their sprite quad from the same immutable render inputs as `Unit.Render.unitToQuad`. The renderer positions a climbing unit at continuous `uiRealZ` and sizes it from the active animation frame, while both hit-test paths use integer `uiGridZ` and the directional T-pose texture, so the selectable region can lag behind the sprite that is actually on screen.

**Verification:** Partially verified. Current source establishes both geometry differences, and `uiRealZ` is explicitly documented as differing from `uiGridZ` during a climb. The focused resolver/frame tests pass but never call either production hit-test function. No graphical or synthetic-engine reproduction clicked a unit partway through a climb, and the shipped animation frames inspected during this review currently keep uniform dimensions within each unit family, making the frame-size half of the mismatch latent for present assets.

**Evidence:**

- PR #406 / issue #389 fixed a narrower version of this contract: W/SW/NW selection used to size the hit box from the fallback texture instead of the mirrored sprite. The acceptance criterion said the hit box should match the rendered sprite, and the merged PR extracted direction resolution into `Unit.Sprite` so render and hit-test shared the mirror fallback.
- `src/Unit/Render.hs:118-155` reads the game clock and unit definitions before constructing quads. `unitToQuad` selects the current animation frame with `pickFrame` at `:171-193`, then uses that handle's live dimensions at `:191-198`.
- The renderer uses `relativeZf = uiRealZ inst - fromIntegral zSlice` at `src/Unit/Render.hs:171-176` and applies it to the vertical offset at `:200-219`. The comment explicitly reserves integer `uiGridZ` for visibility/slice culling while continuous `uiRealZ` controls visual placement.
- `src/Unit/Types/Instance.hs:41-45` states that `uiRealZ` mirrors continuous `usRealZ`, is used for smooth climbing, and equals integer `uiGridZ` only outside a climb.
- Click selection instead resolves only the directional T-pose via `resolveTextureH` at `src/Unit/HitTest.hs:90-105` and computes `heightOffset` from `uiGridZ - zSlice` at `:92-119`. It does not read the clock, current animation name/start, unit definition, or `uiRealZ` needed to reproduce the render quad.
- Box selection repeats both choices in `unitCenter` at `src/Unit/HitTest.hs:170-197`: T-pose dimensions from `resolveTextureH`, followed by an integer-Z offset. Thus click and drag selection agree with each other but not necessarily with the painted unit.
- The passing 26-case `Unit.Render.pickFrame` run covers camera-relative direction resolution, mirrored fallback, animation fallback, and frame timing. Repository search found no test invoking `hitTestUnitAt` or `hitTestUnitsInRect`, so none locks render and interaction geometry together.
- Tracker and pending-report searches found no separate current concern for climbing-unit selection or hit-test use of `uiRealZ`; closed #389 covers only the mirror-fallback defect this review is extending.

**Handoff context:**

- **Current behavior:** During a climb the unit is painted at a fractional vertical position while selection remains anchored to an integer floor. If a present or future animation frame has dimensions different from the T-pose, selection also uses a differently sized rectangle. Direction mirroring itself now agrees after #406.
- **Expected behavior:** Click selection and box-selection center/extent are calculated from the same frame handle, texture dimensions, continuous position, and anchor formula used for the visible quad, while visibility and painter ordering retain their intentional integer-grid rules.
- **Scope and constraints:** Surfaced from PR #406 / issue #389. Preserve active-world filtering, camera-facing rotation, mirror flags, z-slice/effective-depth culling, click tie-breaking, box-center semantics, base-radius anchoring, stable texture handles, and draw-time bindless resolution. Prefer a shared pure sprite-geometry input/result over another hand-copied formula. Add an integration-level case with `uiRealZ /= fromIntegral uiGridZ`, plus a deliberately different-sized active frame, and assert both click and box selection against the rendered quad.
- **Remaining uncertainty:** The climb offset has not been reproduced through the graphical input path, and current shipped frame dimensions reduce the immediate animation-size impact. The processor should confirm one mid-climb click before assigning player-facing severity, while treating the two source formulas as demonstrably divergent.

## 2. Lua diagnostic source labels

### [#1960] PRR-2. Directoryless Lua chunks lose their log source label

> **Captured note:** Preserve a Lua debug source when it has no slash. The shared `dropDir` helper correctly removes the leading directory from file-backed chunks, but its recursive fallback consumes a slashless name to the empty string; console and sandbox chunks can therefore emit `[:line]` instead of an identifying source in all four Lua log functions.

**Verification:** Partially verified by the string transformation and dynamic-chunk construction paths. For every non-empty string without `/`, the current equations reduce to `""`; both current dynamic loaders supply source forms that need not contain a slash. No engine session captured an actual logger line, so the exact `lua_Debug.short_src` presentation for each HsLua loader remains to be confirmed in this build.

**Evidence:**

- Issue #389 identified three copied `dropDir` helpers and a `logDebugFn` omission. PR #406 hoisted the same equations to module scope and routed all four log functions through them, fixing debug/file-backed consistency but not changing their slashless-input behavior.
- `src/Engine/Scripting/Lua/API/Log.hs:23-30` defines `dropDir`. It returns the suffix immediately after the first `/`, but its general `(_:ss)` case recurses until the string is empty. Consequently `dropDir "chunk"`, `dropDir "<unknown>"`, and any other slashless label are all `""`.
- `logInfoFn`, `logWarnFn`, `logErrorFn`, and `logDebugFn` apply the helper before constructing `[source:line]` at `src/Engine/Scripting/Lua/API/Log.hs:32-109`. Even the explicit `"<unknown>"` fallback is erased.
- `src/Engine/Scripting/Lua/Debug.hs:22-42` obtains Lua's `short_src` through the C debug bridge, rather than guaranteeing a repository-relative file path. The helper's input domain therefore includes display labels as well as paths.
- The remote/debug console compiles arbitrary text with `Lua.loadbuffer` and a source name derived directly from the entered code at `src/Engine/Scripting/Lua/Thread/Console.hs:141-156`. That name is deliberately not a script path and commonly contains no `/`.
- The in-game shell compiles expressions/statements with `Lua.loadstring` at `src/Engine/Scripting/Lua/API/Shell.hs:31-50`; its generated chunk source is likewise not guaranteed to contain a path separator.
- `dropDir` is private and has no focused unit coverage. Tracker and pending-report searches found no follow-up for slashless source labels; closed #389 is the reviewed implementation whose boundary case remains.

**Handoff context:**

- **Current behavior:** File sources such as `./scripts/foo.lua` become `foo.lua`, but a directoryless `short_src` and the explicit unknown fallback become empty before logging. Diagnostic lines retain their number and message while losing the only source identity.
- **Expected behavior:** Path-backed sources retain the intended shortened path, while already-short labels remain intact. Unknown or generated chunks produce a useful stable label rather than an empty bracket field.
- **Scope and constraints:** Surfaced from PR #406 / issue #389. Preserve identical formatting across info/warn/error/debug, repository-path shortening, lenient message decoding, log categories, stack-depth selection, and non-throwing behavior. Test `./scripts/foo.lua`, a multi-segment relative path, an absolute-looking path, `foo.lua`, `=[generated]`/the actual `short_src` form, the unknown fallback, and empty input. A focused real-Lua test should pin the source strings produced by both `loadbuffer` and `loadstring` rather than assuming their display convention.
- **Remaining uncertainty:** Static evaluation proves the helper erases slashless inputs, but this review did not observe the C bridge's exact output during a real dynamic log call. If Lua always injects a slash for one loader, that route may escape; the explicit slashless fallback and other generated-source forms still warrant boundary coverage.

## 3. Wound effective-severity ownership

### [#1963] PRR-3. The wound tick still reimplements the shared effective-severity formula

> **Captured note:** Make the wound tick consume the same authoritative effective-severity operation as every downstream caller, using a wound value updated with the tick's fresh heal and necrosis fields. PR #401 removed three weaker copies, but the tick still spells out `max (severity * (1 - heal)) necrosis` separately, leaving the claimed single-source contract and its comments vulnerable to another split.

**Verification:** Partially verified as a maintenance/specification gap, not as a current numerical defect. The two formulas are textually separate but currently identical when the helper is applied to the tick's updated wound value. The focused helper tests pass 4/4, including healing, necrosis, and negative-heal cases. No current output disagreement was found, and PR #401 explicitly left the tick copy in place because it needed freshly computed values.

**Evidence:**

- Issue #376 asked for one shared helper reused by `bleedRateFor`, medic scoring, render publication, “and any other consumer,” while preserving the already-correct per-tick math. PR #401 added `woundEffSeverity` and routed the named downstream consumers through it, but its description explicitly exempted the tick rather than making the fresh-state call possible.
- `src/Unit/Types/Wound.hs:106-117` calls `woundEffSeverity` authoritative and defines it as `max (woundSeverity * (1 - woundHeal)) woundNecrosis`. Its Haddock says it “mirrors” the tick's `effSev`, making the tick rather than the shared function the actual source of truth.
- `src/Combat/Wounds/Tick.hs:314-336` computes fresh `newHeal` and `newNec`, repeats the full formula locally as `effSev`, uses it for bleeding and cleanup, and only then constructs `w'` with those new fields. Constructing the updated value first would allow the helper to operate on the same fresh state without changing the formula.
- The `Wound` field documentation at `src/Unit/Types/Wound.hs:30-36` and `:57-66` still describes effective severity solely as `severity * (1 - heal)`, omitting the necrosis floor that the helper and tick both enforce. The stale prose is already evidence that the duplicated semantic contract has drifted even while the arithmetic remains equal.
- The passing 4-case `Wound.woundEffSeverity` run locks the helper itself. Existing wound-tick behavior tests exercise healing and infection outcomes, but repository search found no assertion that the value governing tick bleed/cleanup equals the exported helper on the freshly updated wound.
- Tracker and pending-report searches found no new issue for the remaining tick/helper duplication. Closed #376 is the reviewed contract and implementation.

**Handoff context:**

- **Current behavior:** The tick's local arithmetic and the exported helper agree today, but a future necrosis/healing rule change must update both. Comments already disagree on whether necrosis is part of “effective severity.”
- **Expected behavior:** One operation owns the formula. The tick updates or projects the fresh wound state and calls that operation; downstream consumers call the same operation on their current snapshot. Documentation identifies the same owner and explains the fresh-versus-stored timing distinction.
- **Scope and constraints:** Surfaced from PR #401 / issue #376. Preserve use of `newHeal`/`newNec` in the same tick, infection worsening below zero heal, necrosis floor, bleed calculation order, cleanup/scar decisions, serialization, and all downstream severity consumers. This should not change save data or rebalance wounds. Add a focused tick-level lockstep case rather than only another pure-helper test.
- **Remaining uncertainty:** The merged PR deliberately considered the tick authoritative, so the processor may decide the small duplication is clearer than reordering local construction and disposition this as no issue. If kept, the “single shared helper” claim and field comments should at least be narrowed so maintainers know there are two formula sites.

## 4. Residual asset event surface

### [#1966] PRR-4. Removing `Engine.Event` left an orphan exported `AssetEvent`

> **Captured note:** Remove the residual `AssetEvent` type or give it an explicit live owner. PR #402 deleted the only event constructor that consumed it and knowingly left the unrelated export behind, so the exposed asset base module still advertises an event concept with no producer, queue, handler, or consumer.

**Verification:** Verified as current dead exported surface. Repository-wide Haskell search finds `AssetEvent` only at its declaration, while `AssetStatus` remains used by the asset manager and tests. This is a low-severity cleanup rather than a runtime bug, and external users of the Cabal library were not surveyed.

**Evidence:**

- Issue #382 requested removal of the never-wired `Engine.Event` queue, event/action types, and no-op frame handler. PR #402 completed that removal.
- PR #402's own notes state that `AssetEvent` had only been referenced by the removed `EventAsset` constructor, was now unused, and was left solely because it was exported from an unrelated `Base` module and considered outside the issue scope.
- `src/Engine/Asset/Base.hs:17-19` still defines `AssetEvent = AssetStatus AssetId`. The module has no explicit export list, so the type and constructor are exported automatically.
- `synarchy.cabal:168` lists `Engine.Asset.Base` among the library's exposed modules. The dead declaration is therefore part of the package surface rather than a private warning-clean local binding.
- Current Haskell search finds no `AssetEvent` reference outside that declaration. In contrast, `AssetStatus` is imported by `src/Engine/Asset/Types.hs` and the headless asset tests, confirming that deleting the event wrapper need not imply deleting live status tracking.
- Tracker and pending-report searches found no issue covering the deliberately deferred `AssetEvent` cleanup.

**Handoff context:**

- **Current behavior:** The package exposes a one-constructor event wrapper whose only historical consumer was deleted with the event bus. Its presence suggests an asset-event stream that no longer exists and can attract new code toward the removed architecture.
- **Expected behavior:** Dead residual event vocabulary is removed. If an asset notification mechanism is genuinely planned, it is introduced with a real producer/consumer lifecycle rather than preserved as an unreferenced type.
- **Scope and constraints:** Surfaced directly from PR #402 / issue #382's documented out-of-scope note. Preserve `AssetId`, live `AssetStatus`, asset-manager behavior, and test imports. Check for out-of-repository consumers before treating exposed-module cleanup as compatibility-neutral; no event queue or `Engine.Event` scaffolding should be reintroduced merely to justify the type.
- **Remaining uncertainty:** The only uncertainty is package-API compatibility and priority. The repository itself has no consumer, but an external library client could import the exposed constructor. The processor may reasonably mark this no-issue if the package is not treated as a public API or if the dead surface is too small to warrant tracker overhead.

## 5. Mirrored unit-state documentation

### [#1967] PRR-5. Unit activity and pose label comments have drifted from their sources

> **Captured note:** Stop hand-maintaining partial activity and pose enumerations on the render-mirror fields. The authoritative label functions and Lua API Haddocks are current, but `UnitInstance` still documents smaller historical sets, so a maintainer inspecting the data boundary is told that valid `running`, `eating`, `dead`, `climbing`, `falling`, and `sleeping` values cannot occur.

**Verification:** Verified as a documentation mismatch. The current total label functions enumerate more values than the field comments, and the public getters explicitly document the full sets. No incorrect runtime branch was found that relies on the comments, so this is code-health/documentation debt rather than demonstrated behavior loss.

**Evidence:**

- Issue #380 / PR #409 repaired the Lua-facing `unit.getActivity` documentation by comparing it to `Unit.Thread.activityLabel`, establishing that these Haddocks serve as the Lua API reference and that the label function is the authoritative set.
- `src/Unit/Thread.hs:242-251` currently maps activities to `idle`, `walking`, `running`, `drinking`, `eating`, `pickup`, and `transitioning`.
- `src/Unit/Types/Instance.hs:54-57` documents the mirrored `uiActivity` field as only `idle`, `walking`, `drinking`, `pickup`, and `transitioning`, omitting `running` and `eating`.
- `src/Unit/Anim.hs:18-27` currently maps poses to `standing`, `crouching`, `crawling`, `collapsed`, `dead`, `climbing`, `falling`, and `sleeping`.
- `src/Unit/Types/Instance.hs:58-60` documents `uiPose` as only `standing`, `crouching`, `crawling`, and `collapsed`, omitting half of the valid set.
- The Lua-facing comments are complete: `src/Engine/Scripting/Lua/API/Units/Query.hs:393-399` names the full activity set, and `src/Engine/Scripting/Lua/API/Units/Spawn.hs:443-448` names the full pose set and points to `Unit.Anim.poseTag`.
- Tracker and pending-report searches found no matching stale-field-comment concern. The prior CH-101 discussion concerns representing pose/activity as strings versus enums, not agreement of the existing string contract, and was already dispositioned separately.

**Handoff context:**

- **Current behavior:** Runtime publication and Lua getters emit all current labels correctly. Only the nearby `UnitInstance` contract is stale, encouraging incomplete pattern handling or another manually copied list when new consumers are written.
- **Expected behavior:** The mirror fields point maintainers at `activityLabel` and `poseTag` as their authoritative domains, optionally giving examples without claiming an incomplete exhaustive list. If exhaustive prose is retained, it stays mechanically or testably synchronized.
- **Scope and constraints:** Surfaced while checking PR #409 / issue #380 against the current split modules. Preserve serialized/runtime text values, total label functions, public Lua contracts, animation state keys, and the separation between activity and pose. A comment-only repair is likely sufficient; avoid introducing a broad enum-marshalling refactor under this finding.
- **Remaining uncertainty:** This mismatch was introduced or preserved during later module splits rather than proven to be a direct defect in PR #409. The processor should decide whether a two-comment repair merits an issue or belongs in a nearby documentation cleanup.

## 6. Lua wound-result schema

### [#1969] PRR-6. `unit.getWounds` advertises only a fraction of its returned schema

> **Captured note:** Document the complete stable return shape of `unit.getWounds`, especially the distinction among acute `severity`, full `severityEffective`, original `severityInflicted`, and healing/necrosis fields. The function's top-level Lua API Haddock still advertises only four fields, even though callers receive a much richer medical record whose severity choice changes gameplay semantics.

**Verification:** Verified as a source-level documentation mismatch. The function unconditionally populates the additional keys for every returned wound, but its API headline names only `part`, `kind`, `severity`, and `at`. Existing implementation comments explain many individual fields, yet they are inside the function body rather than in the public API contract. No generated documentation or external Lua consumer was inspected to determine whether another complete reference compensates for this Haddock.

**Evidence:**

- Issue #376 / PR #401 changed the Lua wound result as part of effective-severity convergence. The PR first routed `severity` through the helper; review then exposed a separate `severityEffective` field so acute-trauma consumers and impairment/bleed consumers could intentionally choose different semantics.
- Issue #380 / PR #409 explicitly characterizes Lua API Haddocks as the effective Lua reference and repaired incorrect value/clock/arity contracts elsewhere, making omission of semantically important result keys consequential documentation debt rather than purely internal prose.
- `src/Engine/Scripting/Lua/API/Units/Combat.hs:71-75` currently advertises `array of { part, kind, severity, at } | nil` plus ordering/lifetime behavior.
- The implementation at `src/Engine/Scripting/Lua/API/Units/Combat.hs:109-198` also writes `macro`, `vital`, `severityEffective`, `severityInflicted`, `heal`, `bandage`, `clot`, `dressing`, `infection`, `clean`, `infectionType`, `infectionName`, `infectionIcon`, `infectionCategory`, and `necrosis`.
- The inline contract at `src/Engine/Scripting/Lua/API/Units/Combat.hs:120-149` makes the distinction important: `severity` is acute `inflicted * (1 - heal)` without the necrosis floor; `severityEffective` includes that floor for impairment/bleed lockstep; `severityInflicted` is the original value. A caller following only the four-field headline cannot discover the intended full-severity field.
- Repository search found no focused test that asserts the production `unit.getWounds` table schema, and tracker/pending-report searches found no issue for its incomplete public contract.

**Handoff context:**

- **Current behavior:** Lua receives the full wound record, and existing in-repository consumers can discover fields by reading implementation or precedent. The de facto API headline documents only four keys and does not explain which severity measure a new caller should use.
- **Expected behavior:** The public Haddock records every stable field, its type/optional form, and the acute-versus-full-versus-inflicted severity distinction. A schema-level test or centralized contract makes accidental field loss/rename visible.
- **Scope and constraints:** Surfaced from PR #401 / issue #376 and the documentation standard reinforced by PR #409 / issue #380. Preserve the current table keys, newest-first ordering, nil-on-missing behavior, macro/vital resolution, acute organ-failure semantics, impairment severity, infection metadata, and existing Lua callers. This finding does not propose renaming fields or collapsing the three severity concepts. Verify whether a generated Lua reference has a preferred schema notation before editing the Haddock.
- **Remaining uncertainty:** Another human-facing reference may already enumerate these keys, and a full inline schema can become another list that drifts. The processor should choose a durable documentation owner and may combine this with a schema-contract test rather than simply expanding one comment.
