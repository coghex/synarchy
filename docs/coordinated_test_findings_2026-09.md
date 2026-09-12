# Coordinated test findings — Agent-system reliability and UI containment

This report records six current concerns retained from the first coordinated
test batch produced through the new agent system: four defects in playtest,
probe, or coordinator reliability, and two player-visible UI containment
defects.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The source was approved assessment
`20260911T034256Z-ui-gameplay-time-controls-discoverabilit-51d289`
(SHA-256
`bb01798792a31b81426636af3e611ddaf1771d8d98a4914a70fb119b2be261a1`).
It correlated twenty-two observations into seventeen findings and assessed
the repository at commit
`13bd01bdeb3043e0087b93adc90d67352a253bf0`.

Because these were the first reports produced through the new agent setup,
agent narration and interpretation were treated as hypotheses rather than
ground truth. A concern was retained only when current source, captured
actions, logs, screenshots, or other artifacts independently supported it.
The implicated repository paths were rechecked against current `master`,
which remains at the assessed commit. The installed `$test` coordinator was
checked separately because it is local skill code and is not contained in
the Synarchy revision.

No scenario was rerun while drafting this report. The retained evidence is
therefore sufficient to establish the listed apparatus or product concern,
but not to claim that excluded gameplay paths were exercised successfully or
unsuccessfully. This drafting pass did not perform tracker deduplication or
choose issue dispositions; those decisions remain for `process-report`.

Observations omitted from the durable queue include:

- findings already fixed or already owned elsewhere, including the
  power-workshop deadline and obsolete arena save-test prohibition;
- text-selection claims contradicted by the rendered end caret and the
  established focus-at-end editing behavior;
- portal and combat shortfalls downstream of wasted editing or no-op scroll
  turns rather than evidence about those gameplay systems;
- expedition and mental-efficiency results that remain inconclusive and need
  focused reruns;
- deferred-probe selection commentary that did not establish a current product
  or harness defect.

No implementation, test, tracker item, or report file was changed while
preparing this draft.

## Status

- [ ] CT9-1. Playtest scroll actions silently accept a missing vertical delta
- [ ] CT9-2. River-naming phase synchronization can observe the wrong page
- [ ] CT9-3. Blueprint screenshot grading saturates on whole-frame luma noise
- [ ] CT9-4. Coordinator cancellation closes its log before output draining completes
- [ ] CT9-5. Randbox text escapes into the adjacent randomize control
- [ ] CT9-6. The main-menu title is clipped at the supported 800×600 minimum

---

## Test-system reliability

### CT9-1. Playtest scroll actions silently accept a missing vertical delta

The playtest action contract permits a provider to describe a zoom attempt
using horizontal `dx` while supplying `null` for vertical `dy`. Normalization
removes the null field, after which translation silently substitutes zero and
injects an accepted horizontal or no-op scroll. Two independent playtests
repeated this mistake for four and eight turns respectively while narrating
zoom-in attempts, consuming their budgets without exercising game zoom.

This is an action-contract defect in the playtest apparatus, not evidence that
the game's camera zoom is broken or that the prompt states the wrong polarity.

**Evidence:**

- `tools/playtest/agent.py:82-119` — the shared strict-output schema exposes
  both `dx` and nullable `dy`; every action property is required, so irrelevant
  fields are represented as null.
- `tools/playtest/engine.py:490-515` — translation converts absent or null
  `dy` to zero, forwards `dx`, and injects `input.scroll` without a corrective
  note.
- `tools/playtest/selftest_player.py:330-348` — current self-tests explicitly
  accept absent `dy` as a zero vertical scroll and separately preserve
  horizontal-only scrolling.
- `.git/codex-test/reports/20260907T224503Z-playtest-naive-onboarding-2f1811.test-result.md`
  — turns 8–11 each supplied `dx=-5`, `dy=null`, producing four accepted
  `input.scroll(-5.0, 0.0)` calls while the player reported trying to zoom in.
- `.git/codex-test/reports/20260911T021107Z-playtest-naive-first-combat-1d3be8.test-result.md`
  — turns 5–12 repeated the same confusion with `dx=-5` and `dx=-10`; the map
  framing did not change and the intended combat path was never reached.

**Handoff context:**

- **Current behavior:** Provider-shaped scroll actions with null or absent
  vertical input are accepted and injected even when they cannot perform the
  narrated wheel/zoom intent.
- **Expected direction:** A scroll decision that lacks usable vertical input
  should be rejected or should return an explicit corrective memory note
  without silently injecting a no-op.
- **Scope and constraints:** Preserve valid bounded and fractional `dy`,
  cursor-aimed pre-movement, atomic rejection, and any intentionally supported
  horizontal-scroll behavior. Extend the playtest self-test with the exact
  provider-shaped `dx` plus null `dy` case.
- **Remaining uncertainty:** The best boundary may be an action-specific schema,
  translator rejection, or a corrective retry contract. The evidence does not
  select among those designs.

### CT9-2. River-naming phase synchronization can observe the wrong page

The river-naming probe initializes a requested page, waits for whichever page
is currently active, and only then requests that the new page be shown. During
the second-world phase, the already-complete named world can satisfy the wait.
The probe then switches pages asynchronously and immediately queries rivers,
so its no-language assertions are not reliably bound to the intended unnamed
world.

The observed failure therefore does not establish that language provenance
changes river identities or that unnamed worlds receive names.

**Evidence:**

- `tools/river_naming_probe.py:95-106` — `gen_world` calls `world.init`,
  page-agnostic `world.waitForInit`, and then `world.show(page)` in that order.
- `tools/river_naming_probe.py:215-238` — phase 3 creates the unnamed page and
  immediately queries and grades its river payload after `gen_world` returns.
- `src/Engine/Scripting/Lua/API/World/Lifecycle.hs:725-734` — `world.show`
  enqueues a selection change rather than synchronously applying it.
- `src/Engine/Scripting/Lua/API/World/Lifecycle.hs:794-821` —
  `world.waitForInit` repeatedly reads the active world rather than accepting
  a page identifier.
- `src/Engine/Scripting/Lua/API/WorldQuery/Lookup.hs:38-44` — world-generation
  query data is likewise resolved through the active world.
- `tools/multiworld_save_probe.py:172-200,268-288` — another multiworld probe
  already uses page registration and active-world polling before the
  active-page completion wait, and documents the same wrong-page failure mode.
- `.git/codex-test/reports/20260901T205041Z-probe-river-naming-d308a0.test-result.md`
  — retained logs prove both pages initialized, but the failing assertion did
  not retain the active page ID alongside its river payload.

**Handoff context:**

- **Current behavior:** An already-active completed page can satisfy phase 3's
  readiness wait before the requested page is active, allowing subsequent
  assertions to observe the prior page or an incompletely applied page.
- **Expected direction:** Every phase must prove that its requested page is
  registered, active, and fully initialized before querying page-dependent
  river state.
- **Scope and constraints:** Preserve the probe's generated-name,
  no-provenance, stable-identity, save/load, and regeneration checks. A setup
  synchronization failure should be reported as apparatus setup failure rather
  than as a river-naming regression.
- **Remaining uncertainty:** The failing run did not capture the active page at
  the exact query instant. The unsafe ordering is verified, but the retained
  payload cannot affirmatively identify which page answered that particular
  query.

### CT9-3. Blueprint screenshot grading saturates on whole-frame luma noise

The construction-blueprint footprint probe treats every nonzero luminance
difference between frames as meaningful. Its live scene is not paused between
captures, so low-level changes spread across the framebuffer and turn all
computed bounding boxes into the full 1024×768 image. This makes the sprite-box
equality pass vacuously while causing the smaller-shape and no-jump checks to
fail for unrelated noise.

The retained images appear consistent with localized 1×1 and 2×3 ghosts and no
significant stake transition. They do not prove exact pixel equivalence, but
they contradict interpreting the raw numeric failures as a full-screen ghost
or visible placement jump.

**Evidence:**

- `tools/construction_blueprint_footprint_probe.py:355-401` —
  `png_diff_bbox` and `png_diff_count` operate on every nonzero luminance
  difference without thresholding or segmentation.
- `tools/construction_blueprint_footprint_probe.py:806-883` — those
  unthresholded results directly grade ghost visibility, sprite-box equality,
  the stake transition, and relative 1×1/2×3 size.
- `.git/codex-test/reports/20260911T013919Z-probe-construction-blueprint-footprint-8cd084.test-result.md`
  — the run produced full-frame bounding boxes for the 1×1 ghost, 2×3 ghost,
  and staked building. Raw changed-pixel counts were 347,157, 366,457, and
  508,762.
- `.git/codex-test/artifacts/20260911T013919Z-probe-construction-blueprint-footprint-8cd084/`
  — recomputing the retained pairs at luminance ≥8 yielded 388 pixels for the
  1×1 designation, 3,595 for the 2×3 designation, and zero for the stake step.
  The frames show correspondingly localized building art.
- The source report records that the simulation was not paused between the
  offscreen captures.

**Handoff context:**

- **Current behavior:** Insignificant whole-frame changes dominate the bounding
  box and pixel-count oracles, making both passing and failing shape judgments
  unreliable.
- **Expected direction:** Screenshot comparison should isolate meaningful
  building-sprite changes so box equality, relative size, and transition
  checks grade the rendered feature rather than background noise.
- **Scope and constraints:** Retain the real offscreen render path, own-art
  designation, one-job and anchor-only assertions, 1×1 control, staked
  comparison, and no-visible-jump claim. Add a focused image-oracle self-test
  containing known localized signal plus low-level whole-frame noise, then
  rerun the real probe.
- **Remaining uncertainty:** The source of the 1–7-luma scene variation is not
  established. Pausing, thresholding, spatial segmentation, or a combination
  may be appropriate; the retained run does not choose the robust design.

### CT9-4. Coordinator cancellation closes its log before output draining completes

The installed `$test` coordinator starts a background output reader inside an
open-log context, but joins that reader only after normal process completion.
A keyboard interrupt leaves the context first, closing the log, and only then
terminates the child. Buffered child output can consequently reach the reader
after its destination has closed.

The observed cancellation still recorded terminal registry state and removed
the owned processes, but it lost the final buffered line from the primary log
and emitted an uncaptured thread exception.

**Evidence:**

- `/Users/vincentcoghlan/.codex/skills/test/scripts/test_coordinator.py:863-906`
  — the log context owns the child and output-reader lifetime, with
  `reader.join` reached only on the normal path.
- `/Users/vincentcoghlan/.codex/skills/test/scripts/test_coordinator.py:907-911`
  — `KeyboardInterrupt` is handled after the log context has unwound and then
  terminates the child.
- `/Users/vincentcoghlan/.codex/skills/test/scripts/test_coordinator.py:884-890`
  — the reader writes every drained line directly to the context-owned log.
- `.git/codex-test/reports/20260908T005556Z-probe-construction-20763e.test-result.md`
  — cancellation while the child waited for `cabal-build` produced
  `ValueError: I/O operation on closed file` at `log.write(line)`. The delayed
  lock-wait line and traceback were absent from the primary log.
- `/Users/vincentcoghlan/.codex/skills/test/scripts/test_test_coordinator.py`
  — the current focused coordinator tests contain no matching interrupt/output
  drain regression case.

**Handoff context:**

- **Current behavior:** SIGINT can close the primary log before the child is
  terminated and its output reader has drained, losing final evidence and
  raising on the daemon thread.
- **Expected direction:** Cancellation should terminate the owned child,
  drain and join its reader, close the log afterward, and retain terminal
  registry state without a background exception.
- **Scope and constraints:** Preserve process-group cleanup, cancellation exit
  classification, heartbeat/registry updates, and normal-completion behavior.
  Cover a child that emits one final buffered line during interrupt teardown.
- **Remaining uncertainty:** Only one scheduling instance was observed, though
  the source ordering makes the race explicit. A deterministic regression
  fixture will need to control when the child's final output becomes readable.

## Player-visible UI containment

### CT9-5. Randbox text escapes into the adjacent randomize control

The Create World randbox renders its complete value from fixed left padding
without clipping, elision, or horizontal scrolling. When a generated or typed
name exceeds the input width—or when the seed is wider than its fitted field—
the text and caret continue into the independent dice-button region.

This is a visible containment defect. The retained evidence does not indicate
that the underlying name or seed value is corrupted.

**Evidence:**

- `scripts/ui/randbox.lua:175-190` — the input and randomize button receive
  adjacent fixed regions, with the button beginning at `inputWidth`.
- `scripts/ui/randbox.lua:238-306` — the display text and cursor are children
  of the input box while the dice control is a separate adjacent sprite.
- `scripts/ui/randbox.lua:509-530` — `updateDisplay` renders the full value at
  fixed left padding and positions the caret from the full prefix width; it
  performs no width containment or visible-substring calculation.
- `.git/codex-test/reports/20260907T224503Z-playtest-naive-onboarding-2f1811.test-result.md`
  — at 1920×1080, the eight-digit seed and subsequently lengthened world name
  visibly extended beneath or into their randomize buttons. Captured geometry
  placed the text region immediately before each button.
- `.git/codex-test/artifacts/playtest-naive-onboarding.hh_h2a7u/` — retained
  pre/post frames show the overlap for both the seed and long name.
- `test-headless/Test/Headless/UI/ResponsiveMenus.hs:707-720` — existing
  coverage verifies that the overall World Name control stays in-frame, but
  does not verify containment of its rendered value within the input portion.

**Handoff context:**

- **Current behavior:** Long text and its caret can paint across the randbox's
  randomize button, reducing legibility and obscuring the control boundary.
- **Expected direction:** The visible value and caret should remain within the
  input region, using clipping or cursor-aware horizontal scrolling while the
  complete stored value remains available for editing and generation.
- **Scope and constraints:** Preserve the dice button's independent hit area,
  focus and resize restoration, full underlying value, and UTF-8 code-point
  cursor semantics. Validate both a generated seed and a long multibyte world
  name, including real rendered pixels.
- **Remaining uncertainty:** The evidence does not choose between clipping,
  elision while unfocused, or horizontal scrolling while editing, nor does it
  establish the complete width/scale range affected.

### CT9-6. The main-menu title is clipped at the supported 800×600 minimum

At the formally supported 800×600 framebuffer, the main menu places the
`Ecce Homo` title at approximately the top-edge margin but does not account for
the text's upward glyph extent. Only the bottom slivers of its white glyphs are
visible, making the title unreadable while the menu buttons remain usable.

The evidence comes from the untouched menu around a passing scene-primitives
fixture, so this is a main-menu layout defect rather than a failure of the
scene-primitives feature being tested.

**Evidence:**

- `scripts/ui/responsive.lua:15-24` — 800×600 is the formal minimum supported
  framebuffer, including UI scale 1.0.
- `scripts/main_menu.lua:170-206` — compact fitting reserves `titleOffset` and
  clamps `menuY` so the calculated title position is at least four pixels,
  without using the title's rendered vertical bounds.
- `scripts/main_menu.lua:232-248` — the title is created at the base title font
  size, horizontally measured, and positioned at
  `menuY - s.titleOffset`.
- `.git/codex-test/reports/20260908T001540Z-probe-scene-primitives-ca5be9.test-result.md`
  — both the untouched baseline and restored final 800×600 screenshots show
  only bottom glyph slivers at the framebuffer's top edge.
- `.git/codex-test/artifacts/20260908T001540Z-probe-scene-primitives-ca5be9/screenshots/baseline.png`
  and `text_destroyed.png` — retained pixel evidence before scene creation and
  after its removal isolates the clipping from the probe's custom primitives.
- `test-headless/Test/Headless/UI/ResponsiveMenus.hs:722-781` — current compact
  menu checks assert only a nonnegative title element `y` at high-scale
  fixtures; they do not assert rendered glyph bounds at 800×600.

**Handoff context:**

- **Current behavior:** The title element's nominal position passes the
  nonnegative-origin rule while its rendered glyphs extend above the
  framebuffer at the supported minimum.
- **Expected direction:** Position and bound the title using its actual rendered
  extent so the complete title remains visible throughout the supported
  responsive envelope.
- **Scope and constraints:** Preserve menu-panel fitting, centered title
  placement, supported-scale classification, and existing button reachability.
  Add a focused 800×600 layout assertion and real-pixel evidence for the glyph
  extent.
- **Remaining uncertainty:** Only one framebuffer and scale combination
  directly demonstrated clipping. The full affected responsive range and the
  best ownership boundary between label metrics and main-menu placement remain
  to be measured.
