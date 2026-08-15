# Project Review Findings: PRs #1018–#991

These entries record focused evidence from the senior review of the twelve merged PRs from #1018 through #991 for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

## Status

- [ ] PRR-1. Fall regressions call sub-minute survival “well over a minute”
- [ ] PRR-2. Definition-free landless worlds lose the explicit `NoLand` outcome
- [ ] PRR-3. The envelope framing fingerprint changes on non-wire pragma edits
- [ ] PRR-4. The tutorial toggle caption remains clipped at the right edge

## 1. Shallow-fall calibration

### PRR-1. Fall regressions call sub-minute survival “well over a minute”

> **Captured note:** Make the shallow-fall regression's description and threshold agree. PR #1018's average and extreme profiles currently survive about 36.7 and 45.7 seconds by the test's own naive exsanguination measure, while their examples claim “well over a minute.”

**Verification:** Verified — evaluating the current test fixture produces two sub-minute results, and all three examples accept anything above 30 seconds despite claiming a substantially stronger bound.

**Evidence:**

- `test-headless/Test/Headless/Unit/Fall.hs:156` — the average-profile example says the result leaves “well over a minute,” but `:161` asserts only `blood / bleed > 30`.
- `test-headless/Test/Headless/Unit/Fall.hs:175` — the extreme-profile example repeats the minute claim and `:179` repeats the 30-second threshold; the frail case has the same shape at `:196-200`.
- Evaluating those exact helpers against the current shipped acolyte YAML produced `(frail, average, extreme) = (133.34822, 36.714085, 45.690052)` seconds. The values still match PR #1018's merge-time calibration table in commit `113e339f` (133 s, 36.7 s, and 45.7 s).
- The focused `--match "2-z fall"` group passed with 8 examples during this review, demonstrating that the current gate accepts the two sub-minute results under the stronger prose label.
- Tracker searches for the minute/30-second mismatch found no exact owner. Open issues #1218 and #1221 concern scenario control and treatment throughput rather than this deterministic regression contract.

**Handoff context:**

- **Current behavior:** The physics and test agree on the measured numbers, but the example names communicate a guarantee the assertions and two shipped profiles do not meet. A future change from 45 seconds to 31 seconds remains green while still reporting “well over a minute.”
- **Expected behavior:** The test either enforces the intended minute-scale survival bound with wording precise enough for the chosen threshold, or describes the actual greater-than-30-second contract without overstating it.
- **Scope and constraints:** Surfaced in PR #1018 / issue #998. Preserve the deterministic shipped-topology fixture, per-profile independence, aggregate-bleed calculation, and the issue's permission for profile-specific calibration rather than assuming “frail” is always most vulnerable.
- **Remaining uncertainty:** The implementation issue required an aggregate bleed bound relative to blood volume but did not settle “well over a minute” as the numeric threshold. The processor should decide whether the defect is the threshold, the example names, or both; no fall-tuning change is implied by this finding alone.

## 2. Location-placement outcomes

### PRR-2. Definition-free landless worlds lose the explicit `NoLand` outcome

> **Captured note:** Define which placement outcome wins when a world has no land and also has no placeable location definitions. `computeLocationPlacement` currently short-circuits to `NoPlaceableDefinitions`, even though `NoLand` is documented as the explicit result whenever the world holds no land at all.

**Verification:** Partially verified — the overlapping condition deterministically returns `NoPlaceableDefinitions`, but normal Create World loads a placeable ruin definition and the no-definition short-circuit is deliberately kept cheap for headless dumps. The unresolved defect is the public outcome contract at that edge, not the normal player path.

**Evidence:**

- `src/Location/Overlay.hs:105-115` — `NoPlaceableDefinitions` is documented as “nothing to place,” while `NoLand` says the world holds no land at all and callers surface that explicit result.
- `src/Location/Overlay.hs:165-167` — the `null placeable` guard runs before `null landMetrics`, so a definition-free all-ocean world is classified only as `NoPlaceableDefinitions`.
- `test-headless/Test/Headless/WorldGen.hs:444-456` — the definition-free and `max_count: 0` cases run on a world with land; `:458-464` tests `NoLand` only with a placeable definition. No case combines the two conditions or states their precedence.
- `src/World/Thread/Command/Init.hs:245-260` — the world thread logs only the `NoLand` constructor as the explicit landless result. `scripts/create_world/generation.lua:210-233` independently infers landlessness from an empty overlay plus the presence of a placeable definition, so the interactive path intentionally cannot surface a definition-free landless world as such.
- The focused `--match "Location overlay"` group passed with 32 examples during this review; the missing overlap case explains why the current branch order is not challenged.
- Tracker searches for the `NoLand`/`NoPlaceableDefinitions` precedence found only the closed parent issue #997 and no live owner.

**Handoff context:**

- **Current behavior:** When both facts are true, callers are told only that content supplied nothing placeable; the stronger physical fact that the generated world has no land is masked. This is presently visible mainly to the placement API and headless/content-light callers, not the shipped Create World flow.
- **Expected behavior:** The outcome contract explicitly settles overlapping causes and the implementation/tests follow it. If preserving the no-definition fast path is authoritative, the documentation should state that `NoPlaceableDefinitions` takes precedence without inspecting land; if “no land” must always be explicit, the implementation needs a cheap or deliberately paid classification path.
- **Scope and constraints:** Surfaced in PR #1015 / issue #997. Preserve the guarantee's no-fire rule for absent or `max_count: 0` definitions and the tracked headless worldgen baselines; do not turn a content configuration with nothing placeable into a guaranteed placement.
- **Remaining uncertainty:** This may be an intentional precedence rule left implicit rather than a wrong result. The processor should verify whether any supported caller relies on distinguishing landlessness when definitions are absent before choosing documentation-only or behavior work.

## 3. Save-compatibility audit precision

### PRR-3. The envelope framing fingerprint changes on non-wire pragma edits

> **Captured note:** Restrict the envelope framing fingerprint to wire-relevant code. PR #1001 removed a redundant `UnicodeSyntax` pragma from `Codec.hs` and had to update the compatibility manifest even though the PR changed no executable behavior or on-disk bytes.

**Verification:** Verified — the fingerprint hashes the entire codec module, and PR #1001 demonstrates a module-header-only edit changing the recorded compatibility fingerprint.

**Evidence:**

- `tools/save_compat_audit.py:503-523` — `envelope_framing_fingerprint` says there is no non-wire content in `Codec.hs` and therefore covers the entire module.
- `tools/save_compat_audit.py:524-530` — the implementation reads all of `Codec.hs` and sends it through the normalizer with the selected wire bindings from `Types.hs`; pragmas and imports are not excluded by construction.
- The first-parent diff for PR #1001 / merge `6fcedabc` changes `Codec.hs` only from `{-# LANGUAGE Strict, UnicodeSyntax #-}` to `{-# LANGUAGE Strict #-}`, while changing `docs/save_compat/manifest.json`'s framing hash from `582c54fe…` to `4c040818…`.
- `docs/save_compat/manifest.json:7-9` — the durable manifest describes this hash as reacting to actual byte-layout changes. PR #1001 is a concrete counterexample to that stated precision.
- `python3 tools/save_compat_audit.py` passed against the current 14 baselines and 20 fixtures, and `python3 tools/test_save_compat_audit.py` completed successfully during this review. Those checks prove internal agreement, not that the fingerprint excludes non-wire source.
- Tracker searches for non-wire envelope-fingerprint churn found only the closed pragma-cleanup issue #969, whose implementation had to amend the manifest; no live issue owns the audit precision gap.

**Handoff context:**

- **Current behavior:** Any normalized pragma, import, module declaration, helper, or other non-wire edit in `Codec.hs` requires maintainers to bless a new framing fingerprint exactly like a real format change. Repeated false positives train the workflow to update the expected hash without establishing whether bytes changed, weakening the gate's signal.
- **Expected behavior:** The fingerprint changes for encoder/decoder structure that determines header, manifest, payload, and checksum bytes, but remains stable for module scaffolding with no effect on the wire format; its self-tests include a representative non-wire edit and a representative framing edit.
- **Scope and constraints:** Surfaced in PR #1001 / issue #969. Preserve coverage of the hand-rolled framing construction that `ENVELOPE_FRAMING_WIRE_BINDINGS` alone cannot see, and do not replace the structural guard with only the manually bumped `envelopeFramingVersion`.
- **Remaining uncertainty:** A source extractor for the relevant codec bindings may itself be brittle. The processor should compare extraction, AST-based normalization, and a golden-byte/fixture-derived fingerprint before choosing the enforcement boundary.

## 4. Tutorial HUD layout

### PRR-4. The tutorial toggle caption remains clipped at the right edge

> **Captured note:** Fit the tutorial toggle caption inside its right-anchored box. PR #991's own graphical verification measured `> Objectives` through the framebuffer edge as `> Objecti`, reported it out of scope, and no later change repaired or tracked it.

**Verification:** Verified from the merge-time graphical reproduction plus current static layout — the responsible constants and unbounded text placement are unchanged, and the current gates still inspect the box rather than the caption's rendered bounds.

**Evidence:**

- PR #991's merged description records the 1280×720, UI-scale-1.0 reproduction: caption glyphs occupied x=1143 through x=1279 while the 132-pixel toggle box ended at x=1272, visibly reading `> Objecti`.
- `scripts/tutorial_hud.lua:77-86` — the current base layout still uses `toggleW = 132`, `fontSize = 14`, and `indent = 12`.
- `scripts/tutorial_hud.lua:165-173` — responsive layout clamps the toggle rectangle to panel/frame width but does not reserve or fit the caption's measured width.
- `scripts/tutorial_hud.lua:365-374` — the full `> Objectives` / `v Objectives` string is still rendered at the unmodified layout font size from `t.x + indent/2`, with no truncation, clipping, or fit-scale step.
- `tools/tutorial_hud_probe.py:185-206` — the graphical probe checks that a label exists and that the toggle rectangle is in frame; it never checks the label pixels or bounds. The current headless resize group passed 4 examples during this review, but its bare backend cannot supply the real font measurement needed to catch this defect.
- `git log --follow -- scripts/tutorial_hud.lua` contains only the three #960 implementation/review commits; no post-#991 repair touched the module. Tracker searches for the clipped Objectives caption found no open or closed issue.

**Handoff context:**

- **Current behavior:** At a formally supported 1280×720 / scale-1.0 configuration, the persistent tutorial control's box is valid and clickable but its label paints beyond both the box and framebuffer, hiding its last characters.
- **Expected behavior:** The caption remains legible and contained across the supported responsive envelope, with the font and box fitted together or another explicit text-fit policy; a graphical assertion covers the rendered label rather than only its containing rectangle.
- **Scope and constraints:** Surfaced and explicitly left out of scope by PR #991 / issue #922; the responsible HUD originated in #960. Follow the responsive contract that fixed-size controls fit against their real reserved width and preserve the right anchor and minimum-frame degradation behavior.
- **Remaining uncertainty:** The merge-time screenshot is the direct visual reproduction; this review did not rerun the slow offscreen probe because its current assertions cannot detect the clipping. The exact fit mechanism should be verified with the real font atlas before implementation.
