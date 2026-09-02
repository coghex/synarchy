# Coordinated-test assessment findings — playtest, persistence, and HUD behavior

This report preserves six confirmed concerns from the approved 2026-08-31 coordinated-test assessment. It covers playtest input limitations, probe robustness and fixtures, save migration correctness, retained diagnostics, and a rendered HUD regression.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The source was approved assessment
`20260831T171111Z-ui-farming-toolbar-icon-discoverability--704621`, which
correlated thirteen observations from nine coordinated test and playtest runs
at assessed commit
`92b9866ed5b278f12a82afa18112837fd894dc23`.

The assessment inspected the source reports, primary logs, migration evidence,
and four-facing rendered artifacts; correlated observations with shared causes;
and classified twelve findings. For this report, the six concerns assigned to
the durable report lane were rechecked against current master at
`2e5eff7db33f1de843ecedbe491882d691ab6c84`. None of the relevant
implementation, harness, or probe paths changed after the assessment.

No scenario was rerun while drafting this report. The retained reports,
artifacts, and current source remain the evidence. The report-drafting workflow
did not perform a new tracker search; tracker deduplication and final
disposition remain the responsibility of `process-report`.

A prior report, `docs/assessed_test_findings_2026-08.md`, recorded and later
rejected an earlier HUD-glyph observation because pixel inspection of that
run's captures showed invariant glyphs. The later 2026-08-30 full-frame
captures assessed here visibly differ: complete glyphs appear at south and
north, while west and east show narrow slivers or blank glyph interiors.
CTA2-4 is therefore retained as new evidence that must be reconciled with the
older disposition rather than silently inheriting it.

The assessment's other six findings are not copied into this report: one
Create World defect was fixed after its run; one graphical-readiness result
came from violating a one-off driver's documented setup boundary; two transfer
session disappearances need focused diagnostic tests before classification;
the farming-wheel failure is already owned by issue #1980; and one
near-threshold physiology sample belongs to the de-flake work tracked by
#1426. No implementation, test, tracker item, or remote state was changed while
drafting this report.

## Status

- [x] CTA2-1. Screenshot players cannot perform a pointer-only hover — [#2050]
- [x] CTA2-2. `input_check.py` crashes after an early click miss — [#2052]
- [x] CTA2-3. Migrated sparse v1 unit-AI rows cannot execute a live tick — [#2055]
- [x] CTA2-4. Later captures show fixed HUD bitmap glyphs collapsing at east/west facings — [no-issue]
- [x] CTA2-5. The chop probe relies on generated wood flora absent from its fixed fixture — [#2058]
- [x] CTA2-6. The broad persistence sweep drops failed-check identity from retained output — [#2060]

---

## Interactive test apparatus

### [#2050] CTA2-1. Screenshot players cannot perform a pointer-only hover

The screenshot-only playtest harness cannot deliberately move the pointer
without also clicking, dragging, or scrolling. Consequently, a player cannot
reveal hover-only information before committing an action. In the retained
Unit Info playtest, this prevented the player from discovering that an
uncaptioned build-picker sprite was the Acolyte Portal and contributed to an
incorrect click and abandoned flow.

**Evidence:**

- `tools/playtest/engine.py:46-48` — defines the accepted action vocabulary as
  click, drag, scroll, key, hold, type, wait, and done, with no pointer-move or
  hover action.
- `tools/playtest/engine.py:293-314` — emits `input.moveMouse` only while
  translating a drag or an aimed scroll; there is no pointer-only translation.
- `tools/playtest/agent.py:115-128` — gives the screenshot player the same
  action list and no way to request hover.
- `scripts/build_tool.lua:360-381` — renders picker entries as clickable
  sprites and places their display names and descriptions in rich tooltips.
- `.git/codex-test/reports/20260830T200331Z-playtest-naive-unit-info-comprehension-253e93.test-result.md`
  — records the uncaptioned portal sprite, the player's conflicting
  interpretations, and the accepted click that armed placement rather than
  reaching Unit Info.

**Handoff context:**

- **Current behavior:** A screenshot player must click, drag, or scroll to move
  the pointer, so hover-only names and hints cannot be inspected safely.
- **Expected direction:** The playtest vocabulary should support one bounded
  pointer-only action whose result is represented in replay and action-outcome
  evidence.
- **Scope and constraints:** Preserve screenshot-pixel coordinates, framebuffer
  clamping, naive-player isolation, deterministic replay translation, and the
  one-action-per-turn contract. Cover invalid and out-of-range input with an
  offline harness self-test.
- **Remaining uncertainty:** This evidence establishes a harness limitation,
  not a product discoverability defect for human pointer users.

### [#2052] CTA2-2. `input_check.py` crashes after an early click miss

The manual input checker records a failed click assertion and then
unconditionally reads callback state that does not exist when the click missed.
That secondary `KeyError` aborts the checker before its remaining assertions
and obscures the primary setup or routing failure.

**Evidence:**

- `tools/input_check.py:151-157` — checks whether the click count reached one,
  then immediately indexes `st["shiftAtClick"]` without first establishing that
  the callback populated the field.
- `.git/codex-test/reports/20260830T203046Z-manual-input-injection-windowed-d95fae.test-result.md`
  — records an early fixture click miss followed by the `shiftAtClick`
  traceback, preventing later checks from executing.
- The file is unchanged from tested commit
  `b70ce762effb0bd1376781cc369d36989254218f` through current master.

**Handoff context:**

- **Current behavior:** Any missed initial fixture click can turn a normal failed
  assertion into an unhandled exception and truncate the diagnostic sequence.
- **Expected direction:** A missed setup or callback precondition should produce
  an explicit terminal diagnostic or guarded dependent checks without a Python
  traceback.
- **Scope and constraints:** Preserve the original click failure as the primary
  result, do not report dependent assertions as meaningful passes, and retain
  cleanup of the attached instance.
- **Remaining uncertainty:** The retained run's click miss was caused by
  attaching before graphical readiness; the secondary crash is independent of
  that cause.

---

## Product correctness

### [#2055] CTA2-3. Migrated sparse v1 unit-AI rows cannot execute a live tick

A valid sparse unit-AI row migrated from save schema v1 survives decoding,
canonical comparison, resaving, restart, and reload, but lacks the transient
`nextActionAt` field required by the live AI tick. Two independent migration
matrix runs reproduced the same post-restart warning.

**Evidence:**

- `scripts/unit_ai_core.lua:84-96` — supplies `nextActionAt = 0` only when
  `ensureState` creates an entirely absent row; it does not normalize an
  existing sparse row.
- `scripts/lib/save_modules.lua:852-876` — clears live state and installs each
  migrated row directly as `live[id] = row`.
- `scripts/unit_ai_save.lua:294-297` — applies migrated unit-AI data through
  that direct entity-row replacement path.
- `scripts/unit_ai.lua:358` — compares `engine.gameTime()` with
  `s.nextActionAt`, requiring the missing field before the first decision.
- `.git/codex-test/reports/20260830T205713Z-probe-save-compat-migration-84b219.test-result.md`
  and
  `.git/codex-test/reports/20260830T232406Z-probe-save-compat-migration-002eca.test-result.md`
  — independently record the same failure for
  `b3-lua-versioned-session-v1` after a fresh-process reload.

**Handoff context:**

- **Current behavior:** The persistence representation accepts a legacy sparse
  row that the live AI runtime cannot safely consume.
- **Expected direction:** Reconciliation should establish all required
  transient runtime defaults before a migrated row becomes live.
- **Scope and constraints:** Preserve legacy-schema acceptance, entity filtering,
  canonical migration comparisons, in-place table identity, and fresh-process
  behavior. The focused oracle must unpause and tick the migrated unit rather
  than stopping after codec and resave checks.
- **Remaining uncertainty:** The evidence identifies the missing
  `nextActionAt` field; other transient unit-AI fields may require the same
  reconciliation audit.

### [no-issue] CTA2-4. Later captures show fixed HUD bitmap glyphs collapsing at east/west facings

> **Disposition:** No issue — the later captures show the same invariance the
> rejected earlier set did. Per-pixel inspection of `b0cfcb`'s four
> `wall_face*.png` frames finds identical glyph-ink counts and zero differing
> interior pixels (>6 px from the box edge) for the hamburger and all six
> toolbar glyphs between any two facings; all differences fall in the
> rounded-corner margins where the rotated world shows through, and the
> `Objectives` label interior is likewise stable. The four frames differ by
> 260k–450k pixels overall, so the rotation occurred. This run's
> `toolbar_face*.png` crops frame the wall, not the toolbar, leaving only
> full-frame eyeballing — which misread the per-facing corner-margin changes
> as glyph collapse, exactly as in the earlier rejected observation. `src/UI/`
> still contains no camera-facing reference and both UI vertex paths apply
> only `ubo.uiProj`.

A later real-Vulkan offscreen session shows the hamburger and six fixed
left-toolbar glyphs at full size when the camera faces south or north, but as
blank interiors or narrow vertical slivers at west and east. Their rounded
button backgrounds and the `Objectives` label remain stable, isolating the
visible change to bitmap glyph content rather than the entire HUD layout.

**Evidence:**

- `.git/codex-test/artifacts/20260830T213614Z-probe-structure-rotation-b0cfcb/wall_facesouth.png`
  and `wall_facenorth.png` — show complete hamburger and toolbar glyphs.
- `.git/codex-test/artifacts/20260830T213614Z-probe-structure-rotation-b0cfcb/wall_facewest.png`
  and `wall_faceeast.png` — show the same controls with full backgrounds but
  slivered or blank glyph content.
- `.git/codex-test/reports/20260830T213614Z-probe-structure-rotation-b0cfcb.test-result.md`
  — records that all four images came from one paused session, settled after
  each rotation, with distinct frame digests and successful camera/scene
  read-backs.
- `scripts/hud.lua:119-132` and `scripts/hud.lua:494-544` — load the bitmap
  assets and construct the six-item `toolbarTool` stack.
- `scripts/ui/toggle.lua:107-122` — creates every toolbar item as a square
  `UI.newSprite`.
- `docs/assessed_test_findings_2026-08.md:174-186` — documents why an older
  observation was rejected after its own earlier captures showed invariant
  glyph pixels; those are not the later captures cited here.

**Handoff context:**

- **Current behavior:** The later captured HUD glyph content changes with world
  camera facing while its control backgrounds and nearby text remain stable.
- **Expected direction:** Fixed screen-space bitmap geometry, UVs, legibility,
  and interactive bounds should remain invariant under world-camera rotation.
- **Scope and constraints:** Reconcile the later evidence with the prior
  no-issue record before filing work. Keep world sprites, directional
  structure art, camera rotation, and lighting behavior out of scope. A
  focused check should inspect representative fixed bitmap controls at all four
  facings and separately verify clickability.
- **Remaining uncertainty:** The responsible renderer or texture path is not
  localized, click bounds were not measured, and the reason the later frames
  differ from the previously rejected capture set remains unknown.

---

## Probe fixtures and retained evidence

### [#2058] CTA2-5. The chop probe relies on generated wood flora absent from its fixed fixture

The chop probe defaults to one generated world fixture and searches its loaded
region for naturally placed wood-harvestable flora. Seed 42 currently supplies
none there, so the probe exits before testing designation, worker behavior,
yield, persistence, or construction consumption. The same setup failure
recurred in the parallel sweep and its solo retry.

**Evidence:**

- `tools/chop_probe.py:31-32` and `tools/chop_probe.py:94-100` — document and
  select seed 42, size 64, and three plates as the default fixture.
- `tools/chop_probe.py:72-82` — searches sampled points in the generated loaded
  region for wood-tagged harvestable flora.
- `tools/chop_probe.py:117-130` — initializes that fixed world and exits
  immediately when the scan finds no wood flora.
- `.git/codex-test/reports/20260831T001852Z-probe-persistence-contract-sweep-296b3d.test-result.md`
  — records the same missing-tree precondition in both the parallel attempt and
  the required solo retry.

**Handoff context:**

- **Current behavior:** A deterministic absence in generated input prevents the
  probe from reaching any chop behavior while presenting as a probe failure.
- **Expected direction:** The probe should establish a deterministic,
  behavior-relevant wood-flora precondition before grading the chop pipeline.
- **Scope and constraints:** Keep the probe manual and opt-in. Do not promote it
  into CI or a required gate. Preserve the real designation, AI, inventory,
  save/load, and material-consumption path after fixture setup.
- **Remaining uncertainty:** A different pinned seed may work temporarily, but
  the evidence does not establish that natural world generation is a durable
  fixture contract for this probe.

### [#2060] CTA2-6. The broad persistence sweep drops failed-check identity from retained output

The broad persistence sweep prints each check as it runs but ends with only a
failure count. The aggregate probe runner retains a default 25-line tail plus
limited progress attribution. When enough output follows an early failure, the
durable result therefore says that multiple checks failed without naming every
failed check.

**Evidence:**

- `tools/persistence_contract_sweep.py:222-229` — prints individual check
  labels when they execute but stores only a numeric failure count.
- `tools/persistence_contract_sweep.py:768-769` — emits only
  `FAIL: N check(s) failed` at completion and does not repeat failed labels.
- `tools/run_probes.py:1551-1552` — defaults failure presentation to the final
  25 captured lines.
- `tools/run_probes.py:1704-1713` and `tools/run_probes.py:1918-1931` — show
  that sequential and parallel failure presentations print attribution plus
  that tail, not the complete child output.
- `.git/codex-test/reports/20260831T001852Z-probe-persistence-contract-sweep-296b3d.test-result.md`
  — retains the chop-derived failure but reports a second failed check whose
  identity is no longer present.

**Handoff context:**

- **Current behavior:** A completed broad sweep can retain its total failure
  count while losing the names of earlier failed assertions.
- **Expected direction:** Every failed check should remain identifiable at the
  sweep's durable output boundary, independent of runner tail length or job
  mode.
- **Scope and constraints:** Preserve concise successful output and the
  runner's bounded default presentation. Cover both sequential and parallel
  runner paths, and distinguish assertion identity from nested-probe progress
  attribution.
- **Remaining uncertainty:** The missing assertion from the retained run cannot
  be reconstructed, so it cannot yet be classified as a product, fixture, or
  oracle failure.
