# Coordinated test findings — Create World state and harness reliability

This report records five current concerns retained from an approved coordinated-test assessment: one player-visible Create World state defect and four weaknesses in playtest or probe reliability.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The source was approved assessment
`20260830T193853Z-ui-gameplay-escape-discoverability-ui-it-430146`, which
correlated fifteen observations from eleven coordinated test and playtest runs
at assessed commit `e0c93888deb3e15023759eb8aabc6d807bafdab8`.

The source reports, retained logs and artifacts, owning implementation, focused
tests, and existing findings-report corpus were rechecked against current
master at `cd34c956eb2073f98faf3ae1f5855d515d0fa596`. None of the five
implicated implementation or harness paths changed after the assessment. No
scenario was rerun while drafting this report. Tracker deduplication and final
disposition remain intentionally deferred to `process-report`.

Four assessed findings are not copied into the durable queue:

- The foraging fixture defect was fixed by closed issue #1766 before the
  assessment.
- The two transfer-session disappearances remain inconclusive and require the
  focused diagnostic prescribed by the assessment before either can be filed
  as a defect.
- The randbox replacement-selection claim is omitted because report-level
  re-verification disproves it. `docs/coordinated_test_findings.md` already
  processed the same concern as `[no-issue]`: the focused and unfocused
  textbox center textures are byte-identical, only the border becomes a yellow
  focus ring, the cursor visibly remains at the end, and focus-at-end is the
  project-wide text-editing convention. The observed append behavior is
  therefore consistent with the rendered control rather than a product defect.

No implementation, test, tracker item, or remote state was changed while
drafting this report.

## Status

- [x] CTA-1. Create World restores blank pending values over visible suggestions — [#1978]
- [x] CTA-2. Playtest wheel actions omit usable camera-zoom semantics — [#1980]
- [x] CTA-3. Location-embark failures can discard the failed assertion — [#1982]
- [x] CTA-4. Power-workshop grades a variable AI walk against a fixed deadline — [#1426]
- [x] CTA-5. Etymology probe rebuilds the HUD with incorrect resource handles — [#1983]

---

## Create World state

### [#1978] CTA-1. Create World restores blank pending values over visible suggestions

On initial menu construction, the Create World screen generates and displays a
random World Name and Seed, then restores a snapshot taken while both pending
values were still blank. Pressing Generate without editing either control
therefore creates an unnamed seed-0 world rather than the world represented by
the visible controls.

**Evidence:**

- `scripts/create_world_menu.lua:90-94` — initializes
  `createWorldMenu.pending.worldName` and `.seed` as empty strings.
- `scripts/create_world_menu.lua:324-344` — initial menu setup calls
  `createUI()` with its default state-preservation behavior.
- `scripts/create_world_menu.lua:360-409` — `createUI()` defaults
  `preserveState` to true and snapshots the still-blank name/seed tuple before
  destroying or constructing any controls.
- `scripts/create_world_menu.lua:517-519` and `:675-798` — the same build calls
  `createLeftPanel`, which reaches `settingsTab.create`.
- `scripts/create_world/settings_tab.lua:139-158` — settings construction rolls
  an empty seed and generates the initial language-derived world name directly
  into the pending table.
- `scripts/create_world_menu.lua:598-605` and
  `scripts/create_world/name_suggest.lua:231-263` — the end of `createUI()`
  restores the earlier snapshot, writing its blank `seed` and `worldName` back
  over those generated values.
- `scripts/create_world/generation.lua:54-65` and `:123-130` — generation reads
  the pending table, normalizes a blank seed to zero, logs the blank name/seed,
  and publishes those values to the world-view generation path.
- `.git/codex-test/reports/20260830T190317Z-playtest-naive-first-day-survival-follow-6703a3.test-result.md:78-100`
  — preserves a pre-click frame with visible Name and Seed values followed by
  `Generating world: name= seed=0x`, runtime seed `0`, and an unnamed world.

**Handoff context:**

- **Current behavior:** A player can accept the initial visible name and seed
  by immediately pressing Generate, but the generated world silently receives
  blank/zero values instead.
- **Expected direction:** The initially displayed World Name and Seed should be
  the authoritative pending values consumed by Generate without requiring a
  focus, edit, or reroll.
- **Scope and constraints:** Preserve responsive rebuild state, raw
  in-progress edits, deliberately cleared names, Defaults behavior, UTF-8
  cursor handling, the name suggestion’s gloss/expression/language provenance,
  and its seed-specific reroll sequence. Cover initial entry separately from a
  genuine rebuild that has prior controls to preserve.
- **Remaining uncertainty:** None at draft time.

## Playtest action semantics

### [#1980] CTA-2. Playtest wheel actions omit usable camera-zoom semantics

The screenshot-driven playtest action contract exposes scrolling as an
unbounded numeric `dy` and describes one sign only as “away/up.” It does not
state which sign moves the camera toward ground-level gameplay, what magnitude
represents an ordinary physical wheel step, or how to express a short
multi-notch correction without spending one model turn per notch. Five
coordinated playtest observations consequently failed before reaching their
actual gameplay goals.

**Evidence:**

- `tools/playtest/agent.py:58-80` — the structured action schema accepts any
  numeric `dy` and supplies no bounds, unit, or semantic zoom action.
- `tools/playtest/agent.py:115-128` — the player prompt documents
  `{"do":"scroll","dy":N}` only as “negative = away/up”; it does not define
  camera-zoom polarity or a normal gesture size.
- `.git/codex-test/reports/20260830T170612Z-playtest-naive-starter-toolbox-e779d1.test-result.md:102-130`
  — records six accepted `dy=5` gestures that the player described as zooming
  in while moving away from the intended gameplay path.
- `.git/codex-test/reports/20260830T192918Z-playtest-naive-first-construction-07e21b.test-result.md:78-109`
  — records four accepted `dy=600` gestures, demonstrating that a wildly
  oversized delta remains valid under the action contract.
- `.git/codex-test/reports/20260830T190317Z-playtest-naive-first-day-survival-follow-6703a3.test-result.md:108-132`
  — a follow-up supplied the missing sign and limited the player to one
  `dy=-1` notch at a time; nine separate scroll turns then consumed the
  available decision budget before the gameplay goal was reached.
- The approved assessment correlates these with the earlier Escape and
  first-day-survival runs and establishes one shared harness cause across all
  five observations.

**Handoff context:**

- **Current behavior:** A compliant screenshot player may choose the wrong sign,
  an unrealistic magnitude, or a correct but prohibitively turn-expensive
  sequence before it can interact with ordinary ground-level gameplay.
- **Expected direction:** The action vocabulary should make camera-relative
  zoom intent and ordinary gesture scale unambiguous, while allowing a bounded
  corrective sequence to be expressed efficiently.
- **Scope and constraints:** Preserve cursor-aimed scrolling, horizontal/vertical
  wheel delivery where needed, one observable action boundary per playtest
  turn, and the distinction between synthetic action semantics and physical
  input behavior. This finding does not establish a defect in the game’s real
  wheel routing or zoom implementation.
- **Remaining uncertainty:** Whether the best contract uses bounded raw
  notches, semantic zoom actions, or another compact representation is a
  design-time choice.

## Probe evidence and timing

### [#1982] CTA-3. Location-embark failures can discard the failed assertion

A coordinated location-embark run completed late save/reload and icon-state
coverage, exited nonzero, and reported one failed check, but its retained
primary evidence does not identify that check. The probe prints checks to
buffered stdout, repeats failures on stderr, and the runner merges those
streams while retaining only their final 25 lines. The failure can therefore
appear before later buffered output and fall outside the retained tail. The
probe then deletes its invocation-owned logs, screenshots, and saves unless
the caller opted into retention before knowing it would fail.

**Evidence:**

- `tools/location_embark_probe.py:141-150` — each check prints its immediate
  result to stdout and records a failed description in the process-global
  failure list.
- `tools/location_embark_probe.py:241-283` — the invocation directory is
  recursively removed unless `--keep-artifacts` was supplied.
- `tools/location_embark_probe.py:1464-1474` — the final report repeats failed
  descriptions to stderr before printing its stdout summary and advises a
  failed caller to rerun with artifact retention.
- `tools/run_probes.py:1380-1443` — the runner captures the child through a pipe
  and redirects stderr into stdout.
- `tools/run_probes.py:980-997` — durable progress attribution contributes
  nothing when the child emitted no `#probe-progress#` records; the
  location-embark probe currently emits none.
- `tools/run_probes.py:1549-1550`, `:1702-1711`, and `:1916-1929` — both
  sequential and parallel failure presentation retain only the default
  25-line tail after any progress attribution.
- `.git/codex-test/reports/20260830T153514Z-probe-location-embark-85af89.test-result.md:51-74`
  — records exit 1, twenty-two visible terminal passes, a one-failure summary,
  and the absence of the failed assertion and invocation artifacts.

**Handoff context:**

- **Current behavior:** A meaningful four-and-a-half-minute offscreen run can
  be retained as “one check failed” without enough evidence to classify the
  failure as product, fixture, oracle, or infrastructure behavior.
- **Expected direction:** Every coordinated failure should durably retain the
  exact failed check, its phase, and the minimum decisive diagnostic context
  without requiring a rerun.
- **Scope and constraints:** Preserve invocation isolation and automatic cleanup
  for successful runs, keep normal runner failure output concise, avoid
  permanently retaining large success artifacts, and keep sequential and
  parallel runner behavior equivalent. A repair may use ordered/flushed
  summaries, durable progress/result records, or failure-conditional artifact
  retention; the finding does not prescribe one mechanism.
- **Remaining uncertainty:** The missing assertion and its underlying product
  or harness interpretation cannot be recovered from this run.

### [#1426] CTA-4. Power-workshop grades a variable AI walk against a fixed deadline

The power-workshop probe requires a real `craft_job` worker to claim a bill,
fetch material, walk to the station, and enter `working` within two fixed
20-second polls. Its own census classification records that this AI-timed leg
failed the same working/drain pair in half of six measured attempts. In the
assessed run, the new diagnostics showed the live job still walking when the
working deadline expired; the same bill later completed and passed its output
and power checks.

**Evidence:**

- `tools/power_workshop_probe.py:289-299` — the shared poll helper fails once a
  fixed wall-clock deadline expires, with no progress-based extension.
- `tools/power_workshop_probe.py:643-678` — the AI scenario allows 20 seconds
  for claim and another 20 seconds for `working`, despite the latter transition
  including real fetch, pathing, and AI arbitration.
- `tools/power_workshop_probe.py:321-376` — the diagnostics added by #1758
  capture the bill, network, AI action/job phase, and unit position when a poll
  expires.
- `tools/ci_probes.py:238-256` — the authoritative manual-only reason records
  that three of six measured runs failed exactly `AI reaches the working
  phase` and its dependent drain check while all non-AI checks passed.
- `.git/codex-test/reports/20260830T173310Z-probe-power-workshop-3d6f77.test-result.md:59-75`
  — the timeout bundle shows the claimed bill still in `craft_job` phase
  `walking`; after noon the same bill completed, produced its output, and the
  deterministic battery charge/discharge checks passed.

**Handoff context:**

- **Current behavior:** The probe can report a power-workshop failure while its
  real worker is still making valid progress toward the station and eventually
  completes the same bill.
- **Expected direction:** The fixture and deadline should distinguish a
  progressing fetch/walk from a genuinely stalled craft job while retaining a
  bounded failure condition.
- **Scope and constraints:** Preserve the real AI claim/fetch/walk/working path,
  the claimed-versus-working demand distinction, midnight brownout, noon
  recovery, output placement, and deterministic battery checks. Do not promote
  the probe to CI as part of this concern; its broader scenario-heavy
  classification remains valid.
- **Remaining uncertainty:** The evidence does not choose between staging a
  deterministically shorter route, tracking monotonic progress, or changing
  the bounded deadline, and it does not explain every source of run-to-run
  path duration.

### [#1983] CTA-5. Etymology probe rebuilds the HUD with incorrect resource handles

Both manual HUD rebuilds in the etymology probe pass the world-selection texture
as the box texture set and the existing box texture set as the menu font. The
probe still exits successfully, but the affected scrolling, unavailable-state,
resize, and teardown phases execute amid roughly 1,400 missing-font and
missing-box warnings and therefore do not establish the render-valid lifecycle
coverage they claim.

**Evidence:**

- `scripts/hud.lua:94-100` — declares the contract as
  `hud.init(boxTexSet, menuFont, width, height)` and assigns those resources in
  that order.
- `tools/etymology_probe.py:803-808` — the forced-scroll rebuild instead calls
  `hud.init(hud.texWorldSelect or 1, hud.boxTexSet or 2, ...)`.
- `tools/etymology_probe.py:896-904` — the resize phase repeats the same
  incorrect argument order.
- `.git/codex-test/reports/20260828T145350Z-probe-etymology-fced6d.test-result.md:50-60`
  — records 884 missing-font and 528 missing-box warnings after the first
  rebuild while the probe reported PASS.
- `.git/codex-test/reports/20260830T161919Z-probe-etymology-2d6471.test-result.md:59-75`
  — independently reproduces the unchanged call sites and 1,410 corresponding
  warnings in a later run.

**Handoff context:**

- **Current behavior:** The later etymology phases can pass their state and
  input assertions while their boxes and text refer to invalid render
  resources.
- **Expected direction:** Probe-owned HUD rebuilds should preserve the live box
  texture set and menu font in the declared order and fail when the rebuild
  introduces missing UI-resource warnings.
- **Scope and constraints:** Preserve the real generated world/location/river
  entry points, decomposition checks, forced overflow and wheel routing,
  unavailable custom-name state, resize survival, close, and teardown. This is
  a probe repair; production HUD initialization and etymology behavior are not
  implicated.
- **Remaining uncertainty:** Neither retained run includes screenshots after
  the bad rebuild, so the exact pixel-level impact is unknown even though the
  invalid-resource lookups are established.
