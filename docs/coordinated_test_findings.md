# Coordinated test findings — AI, persistence, and onboarding

This report records eight current concerns from approved coordinated-test
assessments that had not yet entered a durable findings workflow. They cover
runtime AI state, save/load probe validity, and first-use UI feedback.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

Seven completed local `assess-tests` assessments were inventoried, covering
nineteen observations:

- `20260813T011157Z-ui-transfer-combat-blood-gameplay-expedi-5fa13c`;
- `20260813T020840Z-combat-animation-bddd95`;
- `20260813T021424Z-ux-action-feedback-52da40`;
- `20260818T160612Z-onboarding-discoverability-persistence-l-794241`;
- `20260819T145517Z-ui-gameplay-unified-transfer-persistence-8213cd`; and
- `20260819T152510Z-gameplay-first-aid-survival-cd8036`; and
- `20260821T135443Z-combat-retaliation-tick-continuity-gamep-3d7769`.

The first three assessments were already incorporated into
`docs/coordinated_playtest_findings.md`. That report retained seven confirmed
concerns, all since processed as issues, and explicitly accounted for the two
remaining observations as one fixed transfer-probe failure and one inconclusive
recovered-radio scenario. Those nine observations are not duplicated here.

This report retains all seven confirmed findings from the two later
assessments. Their approved evidence was compared with the owning implementation,
focused probes, supporting documentation, relevant history, and the existing
findings-report corpus. The implementation was rechecked at
`bc51346e1c236a21d6e037f8dfa9e2a7a12552e5`; every premise below remains present.
No equivalent entry was found in an existing findings report. Tracker
deduplication and issue disposition remain intentionally deferred to
`process-report`.

The sixth assessment covered one first-aid scenario harness observation at
`4bc2811df65344f13f831d7eb619e8b87120e08e`. It confirmed that the runner polls
only hard-coded acolyte 2 even though production ranks every eligible allied
medic; the preserved run instead showed the kit move to acolyte 3 while its
bandages were consumed and all nine wounds were dressed. That assessment's sole
finding was disposed as a duplicate of open issue #1221, whose approved review
already requires the scenario to discover and report the actual claimant. It is
recorded here for completeness but does not add an eighth concern to the status
queue.

The seventh assessment covered two independent manual-probe harness defects at
`4fb9eb8d30920718da558502fd542e61f01391ae`. This report records the
retaliation-swap fixture failure that suppresses its stale-window oracle. The
assessment's separate position-hold inventory mismatch belongs to the
documentation/code-health reporting lane and is not duplicated here. The
retaliation premise was rechecked at
`4eb63002b427118e569f3391f5c751b0504cdb1a`; the relevant source is unchanged.

No scenario was rerun while drafting. The report relies on the approved
assessments’ preserved execution evidence and a fresh current-code check. No
implementation, test, tracker item, or remote state was changed.

## Status

- [x] TEST-1. Combat retaliation reads a constant outside its Lua module boundary — [#1483]
- [x] TEST-2. Destroyed construction targets remain in persisted unit-AI state — [#1484]
- [x] TEST-3. The save-migration probe omits required location registries — [#1485]
- [x] TEST-4. The save-migration probe continues after a prerequisite load failure — [#1486]
- [x] TEST-5. The unified-transfer probe ignores persistence integrity diagnostics — [#1487]
- [x] TEST-6. Fresh-world entry hides the portal-placement toolbar — [#1488]
- [x] TEST-7. Randbox focus styling falsely signals replacement selection — [no-issue]
- [ ] TEST-8. Retaliation-swap fixture carries injury state into its stale-window case

---

## Runtime AI state

### [#1483] TEST-1. Combat retaliation reads a constant outside its Lua module boundary

The mid-fight retaliation path compares an attacker timestamp against
`RETALIATE_WINDOW_SEC`, but that constant is local to a different Lua module.
When an eligible recent attacker triggers the target-swap branch, the comparison
uses `nil` and aborts that unit’s AI update. The approved mental-state assessment
preserved three real-engine update errors from this path.

**Evidence:**

- `scripts/unit_ai_combat.lua:236-240` — the three-second
  `RETALIATE_WINDOW_SEC` value is declared `local`, so it is visible only inside
  this module.
- `scripts/unit_ai_combat_attack.lua:9-22` — the attack module imports three
  named helpers from `unit_ai_combat`, but no retaliation-window value.
- `scripts/unit_ai_combat_attack.lua:301-320` — the target-swap branch reads the
  undeclared `RETALIATE_WINDOW_SEC` while comparing it with elapsed game time.
- `tools/mental_state_probe.py:746-765` — the focused scenario samples behavior
  during the same documented three-second retaliation window.

**Handoff context:**

- **Current behavior:** A recent attacker different from the current target can
  drive the combat action into a number-to-`nil` comparison, terminating that
  unit’s Lua AI update instead of completing the target-swap decision.
- **Expected direction:** The retaliation-window value should cross the Lua
  module boundary explicitly, and an eligible target swap should complete
  without an update error.
- **Scope and constraints:** Preserve the existing three-second duration,
  initial-engagement behavior, attack-mode helpers, and the exclusions for dead,
  collapsed, and technomule targets. This is a module-boundary repair, not combat
  retuning.
- **Remaining uncertainty:** The captured probe establishes the executable
  failure but not its frequency during ordinary gameplay.

### [#1484] TEST-2. Destroyed construction targets remain in persisted unit-AI state

When no unbuilt construction target resolves, both the utility and execution
paths return without clearing the previously cached `buildTarget`. A destroyed
building can therefore remain referenced indefinitely, cross a save boundary,
produce dangling-reference diagnostics, and survive until load reconciliation
scrubs it.

The approved unified-transfer assessment recorded three surviving units
referencing the destroyed fixture at save and load. The load succeeded and
reconciliation removed the references, so the impact is stale state and noisy
integrity boundaries rather than save corruption.

**Evidence:**

- `scripts/unit_ai_logistics.lua:232-241` — `buildNearbyUtility` returns when no
  target resolves before replacing or clearing the cached `s.buildTarget`.
- `scripts/unit_ai_logistics.lua:264-271` — `buildNearbyExecute` repeats the same
  early-return shape.
- `scripts/unit_ai_save_refs.lua:18-23` — `buildTarget` is explicitly classified
  as a persisted building reference.
- `scripts/unit_ai.lua:432-464` — load reconciliation scans restored AI state and
  scrubs references whose targets did not survive, masking the stale value only
  after a load.

**Handoff context:**

- **Current behavior:** A unit that once targeted a construction site can keep
  that building ID after the site is destroyed and after subsequent AI
  decisions find no replacement. Saves tolerate the reference but emit an
  integrity diagnostic.
- **Expected direction:** Once a construction target no longer resolves, the
  surviving unit’s cached target should be cleared before the next save
  boundary.
- **Scope and constraints:** Preserve the persistence layer’s ability to tolerate
  a legitimately dead reference captured before an AI cleanup tick. Do not turn
  dangling optional targets into load-blocking corruption. Cover both target
  loss during utility scoring and during action execution.
- **Remaining uncertainty:** The same structural pattern may affect
  `storeTarget`, but the assessed runtime evidence establishes only
  `buildTarget`; broader cleanup should be verified separately.

---

## Persistence probe validity

### [#1485] TEST-3. The save-migration probe omits required location registries

The headless save-compatibility migration probe loads several content registries
but omits loot tables and location definitions. Seven tracked complete-session
fixtures contain the `ruin_small` location definition, so content validation
rejects them before their migration behavior is exercised.

Normal game startup supplies these registries. The observed failures therefore
weaken the migration harness rather than establish a player-facing save/load
regression.

**Evidence:**

- `tools/save_compat_migration_probe.py:105-121` — the headless bootstrap loads
  substances, items, equipment, materials, units, buildings, and recipes, but
  neither loot tables nor locations.
- `scripts/startup_loader.lua:145-165` — production startup loads loot tables and
  then locations after their dependent registries.
- `tools/location_content_probe.py:108-124` — another headless probe explicitly
  registers loot tables before loading `data/locations/ruin_small.yaml`.
- `docs/save_compat/manifest.json:377-459` — tracked complete-session compatibility
  fixtures include persisted location-instance state whose definitions must
  resolve during validation.

**Handoff context:**

- **Current behavior:** A valid tracked fixture can fail at content validation
  on unknown `ruin_small`, preventing its migration, resave, restart, and reload
  assertions from running.
- **Expected direction:** The probe should provision every content registry
  required by its declared complete-session fixtures in dependency-safe order.
- **Scope and constraints:** Keep the boot headless and isolated. Mirror the
  authoritative production ordering where dependencies matter without turning
  the fixture bootstrap into an unrelated full application startup.
- **Remaining uncertainty:** Whether the probe should always load the complete
  production registry set or derive a minimal set from fixture metadata remains
  a repair-time decision.

### [#1486] TEST-4. The save-migration probe continues after a prerequisite load failure

A fixture’s acceptance and publication checks do not guard the remainder of its
scenario. After either prerequisite fails, the probe still queries an absent
active page, attempts to resave nonexistent state, boots a second engine, and
tries to load a slot that could not have been produced. One root setup failure
therefore becomes a cascade of misleading passes and secondary failures.

The missing registries in TEST-3 triggered the captured cascades, but the
control-flow weakness is independent: any future fixture rejection would
produce the same noise.

**Evidence:**

- `tools/save_compat_migration_probe.py:316-326` — the probe records whether
  engine A accepted and published the fixture but does not return when either
  check fails.
- `tools/save_compat_migration_probe.py:328-365` — active-page, pause, Lua-state,
  resave, and canonical-summary checks execute unconditionally afterward.
- `tools/save_compat_migration_probe.py:370-416` — engine B is then booted and
  asked to load and resave the first engine’s output regardless of whether that
  output exists.
- `tools/save_compat_migration_probe.py:428-433` — cleanup already has a
  structured `finally` boundary, so terminating a failed fixture early need not
  strand either engine or its temporary root.

**Handoff context:**

- **Current behavior:** A prerequisite load rejection expands into unrelated
  empty-state assertions, missing-file failures, and invalid fresh-process
  follow-ups, obscuring the first actionable cause.
- **Expected direction:** A fixture should stop after the first failed
  acceptance or publication prerequisite, preserving that root result and
  leaving dependent stages explicitly unexecuted.
- **Scope and constraints:** Retain per-fixture cleanup, continued execution of
  later independent fixtures, and useful diagnostics for the root load status.
  Do not convert one fixture failure into termination of the entire manifest
  sweep unless the shared harness itself is unusable.
- **Remaining uncertainty:** The probe’s check recorder has no established
  “skipped due to prerequisite” vocabulary; the repair must decide whether to
  report dependent checks as skipped or simply omit them.

### [#1487] TEST-5. The unified-transfer probe ignores persistence integrity diagnostics

The unified-transfer probe boots its save and load processes with dedicated log
files but never reads those files. Its approved run exited successfully with
333 passing checks while the logs contained three dangling-reference warnings at
save and the same three again at load.

The positive transfer assertions remain useful, but unexpected integrity
diagnostics produced by the scenario cannot affect its mechanical verdict.

**Evidence:**

- `tools/unified_transfer_probe.py:160-161` — the probe defines separate engine-A
  and engine-B log paths.
- `tools/unified_transfer_probe.py:2651-2694` — both processes write to those
  paths, but the save/load orchestration performs no log validation before
  reporting its result.
- `tools/transfer_order_probe.py:954-959` — the adjacent transfer persistence
  probe provides a focused helper for reading matching log lines.
- `tools/transfer_order_probe.py:1011-1019` — that probe rejects unexpected
  integrity diagnostics at its save boundary.
- `tools/transfer_order_probe.py:1054-1064` — it also requires the post-load,
  post-completion save boundary to remain free of integrity diagnostics.

**Handoff context:**

- **Current behavior:** Transfer identities, mode behavior, and fresh-process
  completion can all pass while the saved session emits integrity warnings that
  the probe never reports.
- **Expected direction:** The probe should inspect both engine logs after their
  relevant boundaries and fail on unexpected persistence integrity diagnostics.
- **Scope and constraints:** Preserve the existing 333 positive transfer checks
  and eight stage reports. Diagnostics intentionally created by a fixture should
  require a narrow, documented expectation rather than a broad warning
  allowlist. Ensure logs are flushed before evaluation.
- **Remaining uncertainty:** The assessment establishes that integrity
  diagnostics must be covered; whether other warning classes should also be
  mechanically fatal is not decided here.

---

## First-use UI feedback

### [#1488] TEST-6. Fresh-world entry hides the portal-placement toolbar

A new world enters gameplay at zoom `64.0`, which selects the zoom-map HUD page
and hides the world toolbar. The player manual instead says the player arrives
with the bottom-left toolbar visible and immediately directs them to place the
Acolyte Portal. In the assessed naive-player session, this mismatch blocked the
first required action until the player discovered the map-mode transition.

**Evidence:**

- `scripts/world_view.lua:350-365` — every newly generated world resets the
  camera to zoom `64.0`; loaded saves are the only exception.
- `scripts/hud.lua:718-727` — zoom above the fade end is classified as
  `zoomed_out`.
- `scripts/hud.lua:895-901` — the zoomed-out state shows `hud.zoom_page` rather
  than `hud.world_page`, which owns the build-tool controls.
- `scripts/hud.lua:1080-1113` — only crossing into the zoomed-in band swaps the
  visible page back to the world toolbar.
- `docs/player_manual.md:15-24` — first-session guidance says the bottom-left
  toolbar is already present and immediately instructs portal placement.
- `scripts/tutorial_hud.lua:48-49,150` — the tutorial checklist begins closed,
  so it does not guarantee an expanded first-entry explanation.

**Handoff context:**

- **Current behavior:** The initial camera presents the strategic zoom map while
  the documented portal-placement controls live on a hidden HUD page. A new
  player can see no direct route from entering the world to the required first
  colony action.
- **Expected direction:** Fresh-world entry should make the portal-placement
  path discoverable and keep in-game guidance, HUD state, and the player manual
  consistent.
- **Scope and constraints:** Preserve the usefulness of the zoom map, normal
  zoom-band page ownership, loaded-save camera behavior, and responsive HUD
  contracts. The finding does not prescribe an initial zoom, tutorial cue, or
  toolbar redesign.
- **Remaining uncertainty:** One offscreen naive-player session establishes the
  mismatch but not its prevalence or the best presentation remedy.

### [no-issue] TEST-7. Randbox focus styling falsely signals replacement selection

> **Disposition:** No issue — the focused appearance is a border-only focus
> ring, not a selection highlight. `boxTextures.load` maps `<prefix>.png` to
> the 9-slice CENTER tile that covers the field interior, and
> `assets/textures/ui/textboxselected/textbox.png` is byte-identical to the
> unfocused set's; only the eight border tiles differ, adding a yellow
> `(249,255,12)` ring. Nothing renders as replacement selection, so a focus
> ring plus a blinking end cursor already is this finding's own second
> acceptable outcome. Focus-at-end is the project-wide convention shared by
> `scripts/ui/textbox.lua:384-393` and `scripts/ui/dropdown.lua:41`, pinned by
> `test-headless/Test/Headless/UI/UnicodeTextEditing.hs:40`, and the engine has
> no text-selection concept at all. The `texSetSelected` variable name, not the
> pixels, is what signals selection.

Focusing the generated world-name control applies its selected texture to the
whole field but places the text cursor at the end of the existing value. The
field therefore looks replacement-selected while typed characters append to the
generated name.

The assessed session confirmed that input delivery itself worked: the attempted
replacement produced a composite generated-plus-typed name.

**Evidence:**

- `scripts/ui/randbox.lua:412-427` — focus applies `texSetSelected` to the whole
  box.
- `scripts/ui/randbox.lua:428-438` — the same focus operation places the cursor
  at the Unicode code-point end of the existing value rather than selecting or
  clearing it.
- `scripts/ui/randbox.lua:627-645` — character input inserts at the current
  cursor position.
- `scripts/ui/randbox.lua:509-530` — display rendering shows an ordinary cursor
  computed from the text prefix before that position; no replacement selection
  state is represented.

**Handoff context:**

- **Current behavior:** A click gives the field a visually selected treatment,
  but the next character is appended to the generated value.
- **Expected direction:** Visual focus and editing semantics should agree:
  either initial focus should select the generated value for replacement, or
  the focused appearance should clearly communicate an end cursor without
  selection.
- **Scope and constraints:** Preserve UTF-8 code-point cursor handling,
  randomization, resize restoration, validation, maximum length, and
  `onUserEdit` behavior. Avoid silently changing programmatic focus or restored
  in-progress edits unless the interaction contract calls for it.
- **Remaining uncertainty:** The preferred choice between select-on-first-focus
  and focus-only restyling is a UX decision; the assessment did not test other
  randbox consumers or keyboard Select All behavior.

---

## Behavior probe validity

### TEST-8. Retaliation-swap fixture carries injury state into its stale-window case

> **Captured note:** Shared combat fixture collapses before the stale-window oracle

**Verification:** Verified — the probe reuses one genuinely injured subject for
its fresh and stale cases, but its claimed restore step does not clear wounds or
restore blood volume. The approved run completed every fresh-window check, then
stopped before any stale-window assertion because the subject was collapsed.

**Evidence:**

- `tools/retaliation_swap_probe.py:219-256` — `stanch` only dresses bleeding
  wounds, while `restore` queues `unit.revive`, dresses wounds, and tops up
  survival resources; it does not establish a clean injury or blood state.
- `tools/retaliation_swap_probe.py:464-500` — each case relies on that restore,
  lets natural ticks run, and checks the subject pose only after the window.
- `tools/retaliation_swap_probe.py:657-687` — the fresh and stale windows receive
  the same subject, and the stale no-swap and tick-completion checks occur only
  after `run_case` returns successfully.
- `src/Engine/Scripting/Lua/API/Units/Spawn.hs:376-390` — `unit.revive` is an
  asynchronous queued transition and is a no-op unless the unit is collapsed.
- `scripts/unit_resource_injury.lua:95-149` and
  `scripts/unit_resource_tick.lua:240-272` — residual injury, consciousness,
  locomotion, and blood state can keep or return a unit to collapse after a
  revive request.
- `tools/README.md:460` — the documented probe contract requires a stale hit to
  trigger no swap while still completing the subject's tick.

**Handoff context:**

- **Current behavior:** A real staging hit leaves persistent physiology on the
  shared subject. In the assessed run, the fresh case passed, but the subject
  was collapsed when the stale case reached its captured preconditions, so the
  probe emitted neither stale behavioral result.
- **Expected behavior:** Both retaliation windows should begin from an
  independently established live, non-collapsed fixture and always emit their
  declared behavioral checks.
- **Scope and constraints:** Preserve the genuine-hit setup, fresh-window target
  swap, same-invocation sentinel, log-error oracle, stale no-swap assertion, and
  the product's real injury rules. A harness repair must not weaken or skip the
  stale assertions.
- **Remaining uncertainty:** The failed run did not record wound details, blood
  fraction, or the pose-transition timeline, so the relative contributions of
  residual wounds, low blood, and revive synchronization were not isolated. The
  scenario was not rerun during assessment or capture.
