# Non-CI test audit findings

This audit reviews tests outside GitHub CI's blocking headless suite and
path-selected checks: the graphical Hspec suite, manual-only behavior probes,
and opt-in full tiers. It examines correctness, usefulness, coverage, and
disproportionate cost without treating intentional CI exclusions as defects by
themselves.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

Started with the graphical Hspec suite registered in `test/Spec.hs`, then will
review manual-only probe categories and opt-in tiers. Each finding is verified
against the owning test and implementation where relevant, checked against
existing repository findings documents, and recorded only after explicit
approval.

## Status

- [x] NCT-1. Graphical GLFW “set time” test does not test setting time — [#1400]
- [x] NCT-2. Graphical Vulkan lifecycle tests never destroy their raw instances — [#1401]
- [x] NCT-3. Vulkan extension tests require optional, platform-specific capabilities — [#1402]
- [x] NCT-4. Most graphical GLFW tests validate the upstream binding, not game behavior — [#1573]
- [x] NCT-5. Graphical device test bypasses the engine's GPU-selection contract — [#1576]
- [x] NCT-6. Injury-log probe accepts a `unit.injure` event for the wrong unit — [#1579]
- [x] NCT-7. Injury-log probe never gates a real fall's event emission — [#1583]
- [x] NCT-8. Listing movement courses unnecessarily boots the engine — [#1586]
- [x] NCT-9. Legacy river diagnostics present anomalies as tests but never fail — [#1590]
- [x] NCT-10. Legacy water diagnostics present anomalies as tests but never fail — [#1594]
- [x] NCT-11. Baseline capture accepts variation in its strict worldgen invariants — [#1598]
- [x] NCT-12. Thermo probe treats a failed ice dump as absent evidence — [no-issue]
- [x] NCT-13. Etymology UI probe skips required location and river entry points — [#1604]
- [x] NCT-14. Etymology UI probe skips its real scrolling interaction phase — [#1608]
- [x] NCT-15. Tilling probe skips the required fluid-exclusion behavior — [#1609]
- [x] NCT-16. Item-temperature probe skips its required cooling-rate behavior — [#1611]
- [x] NCT-17. Core queue tests omit the custom timeout contract and real contention — [#1612]
- [x] NCT-18. Item-temperature persistence phase can load a stale save — [#1613]
- [x] NCT-19. Vegetation/farming persistence probes can load stale saves — [#1616]
- [x] NCT-20. Item-identity persistence phase can load a stale save — [#1617]
- [x] NCT-21. Foraging persistence phase can load a stale save — [#1618]
- [x] NCT-22. Location persistence probes can load stale fixtures — [#1620]
- [x] NCT-23. Position-hold inventory overstates the executable fixture by one acolyte — [#1621]

---

## Graphical Hspec suite

### [#1400] NCT-1. Graphical GLFW “set time” test does not test setting time

The graphical GLFW test calls `GLFW.setTime 0`, then checks only that
`GLFW.getTime` returns a nonnegative value. Time is naturally nonnegative, so
a no-op, failed, or wrong-value setter remains green as long as the getter
works. The test's stated behavior is therefore unprobed.

This is assertion-quality coverage within the non-CI graphical suite, separate
from the existing concern that CI does not execute that suite.

**Evidence:**

- `test/Test/Engine/Graphics/Window/GLFW.hs:52-57` already tests the getter's
  nonnegative result independently.
- `GLFW.hs:59-64` calls `GLFW.setTime 0` and repeats only the same
  nonnegative-getter check; it never compares the result with the requested
  value.
- Repository findings searches found no existing finding specifically about
  this setter assertion.

**Handoff context:**

- **Current behavior:** The named setter test passes if `setTime` does
  nothing, sets an incorrect nonnegative value, or fails to update the GLFW
  clock while `getTime` remains usable.
- **Expected direction:** Set a distinctive value such as `12.5` and require
  the following read to equal it, using a small explicit floating-point
  tolerance only if the GLFW binding warrants one.
- **Scope and constraints:** Retain the existing separate getter test. Do not
  make the setter test depend on elapsed wall-clock time or a display-specific
  window condition.
- **Remaining uncertainty:** None material; GLFW's time setter is process
  global, so restore it to a neutral value after the assertion if subsequent
  graphical tests rely on its normal origin.

### [#1401] NCT-2. Graphical Vulkan lifecycle tests never destroy their raw instances

The graphical Vulkan specs call `createVulkanInstance` directly and leave the
returned instances alive.  The repeat test is explicitly named “can create and
destroy instance multiple times”, yet it creates two instances and invokes no
destruction operation.  This means the suite neither verifies instance
teardown nor gives Vulkan objects the lifetime discipline used by production.

This is distinct from the managed surface and device objects in the same
tests: `createWindowSurface` and `createVulkanDevice` each use `allocResource`
internally, whereas instance creation deliberately returns a raw handle and
production wraps that operation at its owner.

**Evidence:**

- `test/Test/Engine/Graphics/Vulkan/Instance.hs:31-48,59-70` calls
  `createVulkanInstance` in three examples (four instances total), but never
  calls `destroyVulkanInstance`; the repeated-lifecycle example only checks
  non-null handles.
- `test/Test/Engine/Graphics/Vulkan/Surface.hs:29-34,58-62` and
  `test/Test/Engine/Graphics/Vulkan/Device.hs:47-59` likewise construct raw
  instances without a corresponding destruction scope.
- `src/Engine/Graphics/Vulkan/Instance.hs:47-153` exposes creation and
  `destroyVulkanInstance` as separate operations; creation itself does not
  register cleanup.
- `src/Engine/Graphics/Vulkan/Init.hs:68-76` supplies the production pattern:
  `allocResource destroyVulkanInstance $ createVulkanInstance ...`.
- `src/Engine/Core/Resource.hs:18-28` shows that managed cleanup runs after
  the enclosing continuation; it does not rescue raw instances not placed in
  such a scope.

**Handoff context:**

- **Current behavior:** The test process accumulates the raw instance handles
  until exit, and the test called a create/destroy cycle would still pass if
  destruction were broken or absent.
- **Expected direction:** Give the graphical test helper a bracket/resource
  scope matching production, or explicitly invoke `destroyVulkanInstance` in
  `finally`-protected cleanup.  Exercise several independent
  create--destroy cycles, so cleanup occurs between creations rather than only
  at process termination.
- **Scope and constraints:** Preserve valid Vulkan destruction order: destroy
  surface/device resources before their owning instance.  Do not introduce a
  finalizer-dependent assertion or merely rename the existing test.
- **Remaining uncertainty:** None material; the resource ownership contract is
  explicit in the current implementation.

### [#1402] NCT-3. Vulkan extension tests require optional, platform-specific capabilities

The graphical instance suite declares two portability-related extensions to be
required and fails when the local Vulkan driver does not advertise them.  The
production instance builder deliberately treats those extensions as optional:
it enables each only if discovered, so that a driver that lacks them remains a
supported configuration.  The tests therefore assert a particular developer
machine's driver inventory rather than the engine's cross-platform contract.

**Evidence:**

- `test/Test/Engine/Graphics/Vulkan/Instance.hs:24-28` calls
  `hasRequiredExtensions`, which unconditionally requires both extensions;
  `:50-57` repeats the same requirement as “includes required macOS
  extensions.”
- `Instance.hs:40-48` similarly requires a debug messenger whenever debug
  mode is requested, although that capability is optional too.
- `Instance.hs:92-95` defines the supposedly required set as
  `VK_KHR_portability_enumeration` and
  `VK_KHR_get_physical_device_properties2`.
- `src/Engine/Graphics/Vulkan/Instance.hs:40-46` states that portability
  enumeration and properties2 are optional, including the case where
  MoltenVK-oriented extensions are absent on Linux.
- `Instance.hs:65-83` discovers both capabilities and adds each to the
  created instance only when it is present; the GLFW window extensions are
  the actual hard requirement.
- `Instance.hs:68-69,85-90,136-146` applies the same availability rule to
  `VK_EXT_debug_utils`: debug mode remains usable without a messenger and
  logs the degraded state instead.

**Handoff context:**

- **Current behavior:** A valid Vulkan installation that lacks either optional
  extension fails the graphical suite before the test can confirm that the
  supported fallback creates an instance.
- **Expected direction:** Keep an environment-independent check for the GLFW
  extensions required by `InstanceForWindow`.  Test optional-extension
  selection conditionally, preferably by extracting the pure
  availability-to-configuration decision so both present and absent cases can
  be exercised without depending on one driver's extension list.
- **Scope and constraints:** Do not turn optional extensions into a production
  requirement merely to satisfy the test.  A macOS-specific assertion, if
  retained, must be explicitly platform-gated and should complement rather
  than replace fallback coverage.
- **Remaining uncertainty:** The exact extension inventory varies by Vulkan
  loader/driver, which is precisely why an unconditional test is unsuitable.

### [#1573] NCT-4. Most graphical GLFW tests validate the upstream binding, not game behavior

Most of the graphical GLFW suite unwraps the raw GLFW handle and calls the
upstream `Graphics.UI.GLFW` API directly.  Beyond the one-time suite setup
that obtains the handle, these examples do not exercise Synarchy's window
wrapper, callback routing, configuration ownership, or rendering behavior.
They consequently remain green through changes to nearly all project code;
failures predominantly report a display, monitor, driver, or binding
condition.

NCT-1 remains a separate assertion-strength defect in the clock setter test.
This concern is the usefulness of the surrounding group as regression tests.

**Evidence:**

- `test/Test/Engine/Graphics/Window/GLFW.hs:17-64` imports
  `Graphics.UI.GLFW` directly and every example calls its raw getter,
  monitor, Vulkan-support, or time API.
- The only Synarchy type it uses is `Window(..)`, solely to unwrap the raw
  handle stored by suite setup; it does not call
  `Engine.Graphics.Window.GLFW`'s project-owned operations.
- `test/Spec.hs:29-50` itself performs the direct GLFW initialization and
  raw-window creation before Hspec starts, so the checks largely repeat facts
  already necessary to reach the examples.
- `src/Engine/Graphics/Window/GLFW.hs` contains project behavior that these
  tests do not probe: configuration-to-window creation, callback setup,
  `EngineM` error translation, resource cleanup, and input queue routing.

**Handoff context:**

- **Current behavior:** A functional regression in Synarchy's window layer can
  leave these examples green, while harmless environmental differences such as
  monitor arrangement can fail them.
- **Expected direction:** Decide which minimal display/Vulkan checks are
  intentional manual preflight smoke tests and label them as such.  Replace
  remaining cases with tests of project-owned contracts, for example window
  configuration propagation, GLFW error handling, callback-to-queue routing,
  and resource lifetime.
- **Scope and constraints:** Do not promote driver-inventory checks into CI as
  substitute coverage.  Keep direct GLFW integration smoke tests only where
  they answer an explicit supported-environment question.
- **Remaining uncertainty:** The appropriate retained smoke set is a product
  policy decision, but the current tests' lack of project-owned assertions is
  directly observable.

### [#1576] NCT-5. Graphical device test bypasses the engine's GPU-selection contract

The graphical device smoke test takes the first physical device returned by
the Vulkan driver and attempts to create a logical device from it.  Production
does not use enumeration order: it rates every candidate and selects a device
with the required queue, presentation, extension, and bindless capabilities.
The test can therefore fail on a supported multi-GPU machine when the first
adapter is unsuitable while another adapter would boot the game; it also omits
the selection behavior it ostensibly represents.

**Evidence:**

- `test/Test/Engine/Graphics/Vulkan/Device.hs:47-59` enumerates the devices,
  checks only that the list is nonempty, takes `V.head physDevs`, and passes
  that device directly to `createVulkanDevice`.
- `src/Engine/Graphics/Vulkan/Device.hs:139-174` implements
  `pickPhysicalDevice`, which rates every adapter, rejects zero-score or
  non-bindless candidates, and returns the best usable device.
- `Device.hs:179-219` documents the criteria that enumeration order ignores:
  queue/presentation support, required extensions, bindless capability, and
  the deliberate capable-over-incapable ranking.
- `test-headless/Test/Headless/Graphics/BindlessFeatures.hs:193-220` already
  exercises the pure ranking policy independently, so the graphical test need
  not reinvent it with a driver-order assumption.

**Handoff context:**

- **Current behavior:** The test can produce a false failure on hybrid or
  multi-adapter systems and supplies no end-to-end check that boot chooses a
  usable adapter.
- **Expected direction:** Use `pickPhysicalDevice inst (Just surface)` in the
  graphical integration smoke test before `createVulkanDevice`.  Keep the
  headless ranking tests as the deterministic policy coverage; the graphical
  test should only prove that the policy-selected live adapter can create the
  device.
- **Scope and constraints:** Preserve the existing valid object lifetime order
  from NCT-2.  Do not encode a vendor, device type, or enumeration position as
  a test prerequisite.
- **Remaining uncertainty:** None material; the production selection boundary
  and its existing pure coverage are explicit.

---

## Manual-only targeted probes

### [#1579] NCT-6. Injury-log probe accepts a `unit.injure` event for the wrong unit

The injury-log probe's live `unit.injure` phase reads an event's target, kind,
and wound kind, but tests only that the returned string contains
`|injure|stab`.  The expected target is the freshly spawned test unit, yet it
is explicitly left unchecked.  A stale, sentinel, or other unit id can thus
be attached to an otherwise correctly shaped event and the probe stays green.

**Evidence:**

- `tools/injury_log_probe.py:89-99` injures `_TU`, serializes
  `e[1].target|e[1].kind|e[1].payload.woundKind`, then accepts any non-`NONE`
  result containing `|injure|stab`.
- The adjacent comment says “target should equal `_TU`; we only assert
  kind/woundKind shape here,” making the missing behavior deliberate and
  visible rather than inferred.
- The later `engine.emitEventForUnit` check proves that a different producer
  can tag an event with `4242`; it does not prove that `unit.injure` sends its
  own event to the injured unit.

**Handoff context:**

- **Current behavior:** A correctly named stab injury event attributed to the
  wrong unit, a stale id, or a sentinel target passes the only live
  `unit.injure` assertion.
- **Expected direction:** Require the exact target (`_TU`) along with
  `injure` and `stab`.  If the public event contract includes them, also
  assert the injured body part and severity/value rather than only the broad
  event shape.
- **Scope and constraints:** Keep the separate generic
  `emitEventForUnit` attribution test; it covers a different entry point.
  Preserve the probe's atomic emit-and-drain round trip so background Lua
  panel ticks cannot consume the event between operations.
- **Remaining uncertainty:** Whether severity must be represented exactly or
  within a serialization tolerance depends on the event payload contract;
  target attribution itself is unambiguous.

### [#1583] NCT-7. Injury-log probe never gates a real fall's event emission

The injury-log probe describes its fourth phase as proving that a real fall
emits a `fall` injury event, but deliberately reports a missing event only as
information and still exits successfully.  Its gating phases cover synthetic
`injury.emit` and the separate `unit.injure` producer, neither of which proves
that the live movement/fall path writes to the shared injury-event stream.

**Evidence:**

- `tools/injury_log_probe.py:7-12` lists “A real fall emits a `fall` injury
  event” as one of the probe's stated checks.
- `injury_log_probe.py:107-139` labels that phase `INFORMATIONAL
  (non-gating)`; when no event arrives it prints an `[INFO]` message and does
  not modify `passed`.
- `src/Unit/Thread/Movement.hs:138-192` is the actual producer boundary: a
  landed fall is converted to injuries and, when damaging, emits one `fall`
  event via `pushInjuryEvent`.
- `test-headless/Test/Headless/Unit/Fall.hs` covers the pure fall-injury
  model, but repository searches found no other test that verifies this live
  movement-thread producer reaches `injury.drainEvents`.

**Handoff context:**

- **Current behavior:** A regression that removes, misroutes, or malformedly
  emits the movement-thread's fall event leaves the targeted probe green as
  long as its unrelated synthetic and `unit.injure` phases pass.
- **Expected direction:** Build a deterministic damaging-fall fixture that
  does not rely on a planner deciding to walk off a plateau, then gate on an
  event with the fallen unit's id, kind `fall`, and the documented payload
  shape (`detail`, count, and severity as applicable).
- **Scope and constraints:** Retain the pure fall-model tests and the generic
  stream round trip; they cover different layers.  Keep drain operations
  atomic with the observed action or otherwise isolate the panel consumer so
  it cannot race the assertion.
- **Remaining uncertainty:** The existing movement setup is intentionally
  pathing-dependent; selecting a deterministic trigger/API requires design
  work, but the absent gating coverage is certain.

### [#1586] NCT-8. Listing movement courses unnecessarily boots the engine

`movement_probe.py --list` is documented as a listing operation, but the
command starts a headless engine and performs the normal probe bootstrap before
it checks the list flag.  A metadata operation therefore pays an engine boot,
content/script loads, and an arena setup, and can fail for those unrelated
runtime reasons without exercising any movement behavior.

**Evidence:**

- `tools/movement_probe.py:20-24,515-520` documents `--list` as “list
  courses + exit.”
- `movement_probe.py:533-538` calls `boot(args.port)` and enters the protected
  run before branching on `args.list`.
- `movement_probe.py:543-548` calls `bootstrap` — loading definitions and
  scripts — before it queries `movement_arena.listCourses()` for the list
  result.
- `tools/movement_probe.py:35-91` shows that bootstrap is deliberately a full
  real-engine setup, appropriate for movement execution but not for listing.

**Handoff context:**

- **Current behavior:** `--list` consumes a headless engine startup and may
  report a boot/resource/port failure instead of simply listing the available
  courses.
- **Expected direction:** Handle listing before `boot`, from a static
  course-definition source or a deliberately lightweight metadata command.
  If the Lua module remains authoritative, provide a no-engine way to expose
  its names rather than treating a full behavior-probe boot as introspection.
- **Scope and constraints:** Do not alter normal course execution, which
  correctly needs the engine and arena.  Preserve one authoritative course
  inventory so the cheap listing cannot drift from runnable courses.
- **Remaining uncertainty:** The best inventory representation is a design
  choice; the unnecessary boot order is direct.

---

## Standalone river-analysis utilities

### [#1590] NCT-9. Legacy river diagnostics present anomalies as tests but never fail

Five standalone scripts are named `test_river_*` and identify concrete river
or lake defects, but they only print measurements and examples.  They have no
threshold assertion and their `main` paths return normally regardless of how
many anomalies they find.  Consequently a command that reports `ISSUE`,
fillable gaps, sinking rivers, or mouth cliffs still exits zero.  This makes
the scripts useful exploratory diagnostics, but not regression tests as their
names imply, and makes any automation that invokes them incapable of detecting
a regression.

Renamed under #1590: the five now carry the repository's diagnostic-report
naming (`tools/river_cutoff_report.py`, `river_lake_depth_report.py`,
`river_lake_gaps_report.py`, `river_mouth_cliff_report.py`,
`river_mouth_gap_report.py`) and each docstring names the actual river gates.
The paragraph above describes the pre-rename tree; the analyses themselves are
unchanged and still report without a verdict, which is the deliberate outcome —
the misleading name was the defect, not the missing threshold.

**Evidence:**

- `tools/river_cutoff_report.py:62-106` counts dry gap tiles and prints their
  severity and examples; its `__main__` only calls `main`.
- `tools/river_lake_depth_report.py:17-47` reports river tiles sinking beside a
  lake but has no pass/fail decision.
- `tools/river_lake_gaps_report.py:28-109` labels river-lake cliffs and parallel
  chains as `ISSUE 1` and `ISSUE 2`, then returns normally.
- `tools/river_mouth_cliff_report.py:18-49` counts surface cliffs at river mouths
  without a threshold or failure exit.
- `tools/river_mouth_gap_report.py:43-125` distinguishes fillable and blocked
  mouth gaps, including the statement that fillable gaps mean water “should
  exist,” but likewise ends after reporting them.
- This is distinct from `tools/test_river_pour.py` and
  `tools/test_river_stress.py`, whose shared `river_thresholds.py` contract
  explicitly makes them pass/fail regression gates.

**Handoff context:**

- **Current behavior:** These scripts can show an arbitrarily large number of
  their named anomalies while producing an exit status of zero.
- **Expected direction:** Decide which remain exploratory analysis and rename
  or relocate them accordingly.  For behavior intended to regress, consolidate
  it into a maintained pass/fail checker with explicit, documented thresholds
  (preferably extending the existing shared river-threshold contract rather
  than creating competing criteria).
- **Scope and constraints:** Do not turn every visual diagnostic into a brittle
  zero-tolerance gate.  Keep the tools' detailed examples available when a
  deliberate threshold fails.
- **Remaining uncertainty:** The acceptable count for each anomaly is a domain
  decision; the current lack of any test verdict is unambiguous.

### [#1594] NCT-10. Legacy water diagnostics present anomalies as tests but never fail

The standalone water diagnostics repeat the river tools' misleading test
shape in a separate subsystem: both identify conditions their descriptions
call visible defects or overflow failures, print `ISSUE` totals and examples,
then exit normally for every observed count.  They are valuable for exploring a
dump, but a green process does not distinguish a healthy world from one with
floating water, water cliffs, or dry banks below a water surface.

Renamed under #1594: the two now carry the diagnostic-report naming #1590
established for the river scripts (`tools/water_above_land_report.py`,
`tools/water_anomalies_report.py`) and each docstring names
`tools/world_audit.py` / `tools/world_check.py` as the maintained worldgen
gate, listing the audit categories that overlap the anomalies it prints.  The
paragraph above describes the pre-rename tree; the analyses themselves are
unchanged and still report without a verdict, which is the deliberate outcome —
the misleading name was the defect, not the missing threshold.

**Evidence:**

- `tools/water_above_land_report.py:53-119` prints two `ISSUE` categories and
  examples without comparing them to a limit or exiting nonzero; before the
  rename its `test_*` name and its docstring both framed the “water floating
  above grass” detection as a test.
- `tools/water_anomalies_report.py:50-144` labels the detected floating-water,
  water-cliff, dry-bank, and isolated-water counts as issues, but its
  `__main__` merely calls `main`; before the rename its `test_*` name and its
  “comprehensive test” docstring both said otherwise.
- Neither script contains a threshold, boolean test result, or failure exit,
  so the process status remains zero after any reported anomaly count.
- NCT-9 covers the analogous river/lake diagnostic scripts; this entry is
  deliberately limited to the water-analysis tools and their own criteria.

**Handoff context:**

- **Current behavior:** Automation can run either script, print a substantial
  failure-looking report, and still receive success.
- **Expected direction:** Keep them as clearly named analysis/report commands,
  or define the subset of anomalies that is a regression and enforce explicit
  thresholds in a maintained pass/fail check.
- **Scope and constraints:** The expected limits are design decisions.  Retain
  the detailed breakdowns for diagnosis instead of replacing them with an
  opaque pass/fail result.
- **Remaining uncertainty:** None about the current success-only control flow;
  only the future acceptance policy is open.

---

## Opt-in worldgen baseline capture

### [#1598] NCT-11. Baseline capture accepts variation in its strict worldgen invariants

The baseline-capture tool says tile count and elevation statistics should be
deterministic, but on variation across its capture runs it only writes a
warning.  It then records the first run's values as the baseline and returns
success.  Later `world_check.py` treats those very fields as strict per-run
equalities, so this can publish an arbitrary first sample that makes future
checks flaky or reports ordinary variation as a regression.

This is distinct from the existing finding that the regression checker does
not compare canonical content hashes to its baseline: this entry concerns the
capture operation accepting a violation of its own strict-invariant contract.

**Evidence:**

- `tools/world_baseline.py:99-108` states that tile count and elevation stats
  “should be deterministic,” but prints only `WARNING` when either varies
  across the configured capture runs.
- `world_baseline.py:76-78,110-130` writes the first audit result's
  `tileCount` and `elevationStats` into the new baseline regardless of those
  warnings.
- `world_baseline.py:163-176` counts only dump-command exceptions as capture
  failures, writes the baseline in the normal path, and returns zero when
  none of those exceptions occurred.
- `tools/world_check.py:251-269` compares every current run's tile count and
  elevation minimum, maximum, median, and count exactly with the captured
  values; it has no corresponding variability envelope for them.

**Handoff context:**

- **Current behavior:** A rebaseline can look successful after observing
  non-deterministic strict invariants, while embedding one arbitrary sample as
  their durable reference.
- **Expected direction:** Fail capture before writing any affected baseline
  whenever an invariant designated deterministic varies.  If a field is
  intentionally variable, model it as an explicit baseline envelope and make
  `world_check` enforce that same policy instead of keeping a warning-only
  capture path and an exact-comparison check path.
- **Scope and constraints:** Preserve the existing fluid/quality envelopes,
  which already deliberately accommodate identified variability.  Do not
  suppress a strict-invariant warning merely to let a baseline refresh land.
- **Remaining uncertainty:** Whether such variation is currently reproducible
  is not needed to establish the inconsistent tool contracts; the handling of
  an observed variation is direct.

---

## Worldgen-derived behavior probes

### [no-issue] NCT-12. Thermo probe treats a failed ice dump as absent evidence

> **Disposition:** No issue — fixed by commit `b6d67ff0` (2026-08-21) before this
> finding was processed. The `except json.JSONDecodeError: tiles = []` path the
> finding cites is gone; `run_ice_dump` now raises `DumpFailure` on a nonzero
> exit, undecodable stdout, or a non-list payload
> (`tools/thermo_altitude_probe.py:152-164`), and the caller turns that into
> `rep.abort` and a non-zero exit (`:355-358`). The informational skip survives
> only for a decoded dump with no interior ice, and ends MISSING rather than
> passing. `tools/test_probe_flake.py:2124-2148` drives all three failure shapes
> engine-free and asserts each aborts rather than skips.

The thermo-altitude probe's ice-agreement phase launches a second
`--dump=terrain,ice` process, but ignores its exit status and maps any JSON
parse failure to an empty tile list.  Empty tiles then take the same
informational “no interior ice tiles … skipped” path as a genuine absence of
nearby ice.  A broken executable invocation, dump mode, or malformed output
therefore silently removes the phase that compares live ambient temperature
with worldgen's ice placement.

**Evidence:**

- `tools/thermo_altitude_probe.py:113-120` defines phase 4 as the
  worldgen-ice agreement check and runs a local dump for the selected peak
  region, but does not use `dump.returncode` or request `check=True`.
- `thermo_altitude_probe.py:121-124` catches every `JSONDecodeError` and
  substitutes `tiles = []`, discarding the dump's diagnostic output.
- `thermo_altitude_probe.py:125-128` treats that indistinguishably from a
  valid dump containing no interior ice and prints an informational skip;
  no failure is added to the probe's final verdict.
- The remaining phases can therefore all pass while phase 4 never inspected
  the requested worldgen evidence.

**Handoff context:**

- **Current behavior:** A failed or malformed ice dump appears as an
  unremarkable no-ice sample, leaving the probe green if its unrelated ambient
  and arena checks pass.
- **Expected direction:** Fail with the dump exit status and a bounded stderr
  excerpt when the command or JSON decoding fails.  Preserve the current
  informational skip only for a successfully parsed dump that genuinely has
  no eligible ice tiles in the selected region.
- **Scope and constraints:** Do not require every seed/region to contain ice;
  the defect is failure-to-observe being conflated with a valid empty result.
- **Remaining uncertainty:** None material about the error path; whether the
  canonical tuple normally contains ice is separate from whether its dump can
  be trusted.

---

## Offscreen Etymology UI probe

### [#1604] NCT-13. Etymology UI probe skips required location and river entry points

The offscreen Etymology probe declares a discovered location and a selected
named river as dedicated real-UI phases, yet both phases return successfully
without an assertion when the generated fixture lacks the required entity.
Consequently a worldgen or identity regression that produces no locations or
no named rivers removes the corresponding plate-selection and panel-retarget
coverage while the overall probe can still report success.

This entry intentionally excludes bound-form, recurrence, and long-scroll
subcases.  Those are documented as legitimately data-dependent observations;
the location and river entry points are instead named as required probe phases
and the default fixture is selected specifically to exercise them.

**Evidence:**

- `tools/etymology_probe.py:20-31` lists a discovered-location entry point
  (phase 3) and visible named-river entry point (phase 4) among the behavior
  the real offscreen boot is meant to cover.
- `etymology_probe.py:239-248` returns from phase 3 after printing `SKIP` if
  `world.listPlacedLocations` is non-list or empty, without calling `check`.
- `etymology_probe.py:293-300` does the same for phase 4 when
  `world.getRivers` contains no named, segmented river.
- `etymology_probe.py:587-591` says the default world size is 64 because a
  smaller world places no locations and would otherwise silently skip the
  location entry point and much recurrence coverage.
- `etymology_probe.py:607-613` returns zero whenever no explicit `check`
  failed, so either skipped phase leaves no failure in the final verdict.

**Handoff context:**

- **Current behavior:** The probe can pass without exercising the actual UI
  route from a discovered location or a named river.
- **Expected direction:** Choose and pin a fixture tuple known to provide at
  least one location and named river, then fail with a clear fixture/setup
  diagnostic when either required source is absent.  Continue to use the
  real selection flow when residency permits; the existing panel-entry fallback
  can remain a secondary diagnostic path.
- **Scope and constraints:** Do not make the optional language-shape cases
  mandatory merely because this probe shares their world.  The required
  entities should be selected or constructed deliberately rather than assuming
  every arbitrary generated world contains them.
- **Remaining uncertainty:** The exact durable fixture tuple or construction
  mechanism is a design choice; the current successful skip is direct.

### [#1608] NCT-14. Etymology UI probe skips its real scrolling interaction phase

The Etymology UI probe declares arrow and wheel scrolling to be a real input
routing phase.  It deliberately shrinks the framebuffer and increases UI
scale to create an overflowing name list, but then succeeds when no inspected
name actually overflows.  That makes a regression in row population, panel
sizing, or scrollbar creation remove every arrow and wheel assertion while
leaving the probe green.

This is separate from NCT-13's missing world entities: it concerns the UI
control and scroll-capture behavior after the probe has populated its own
targets.

**Evidence:**

- `tools/etymology_probe.py:444-460` describes phase 6 as exercising real
  scroll arrows and wheel routing, then forces an 800x600 framebuffer and
  UI-scale 4 specifically to create overflow.
- `etymology_probe.py:461-478` selects the world target and up to eight rivers
  and looks for a panel where `rowCount > visibleRows`.
- `etymology_probe.py:479-483` prints `SKIP` and returns successfully when no
  target overflows, restoring the scale without recording a failed check.
- `etymology_probe.py:485-539` contains the actual arrow and wheel assertions;
  none run on that successful skip path.
- `etymology_probe.py:607-613` reports zero whenever no explicit check has
  failed, so a skipped phase does not affect the final verdict.

**Handoff context:**

- **Current behavior:** A change that prevents the deliberately configured
  panel from overflowing can silently remove the only real scroll-routing
  coverage.
- **Expected direction:** Make the probe's forced configuration produce a
  known scrollable fixture, and fail with a clear setup diagnostic if it does
  not.  Then retain the existing real arrow and wheel interactions as the
  behavior checks.
- **Scope and constraints:** Do not require arbitrary generated names to
  overflow at normal scale.  The expectation belongs only to this probe's
  intentional reduced-framebuffer/high-scale configuration.
- **Remaining uncertainty:** The durable way to provide enough rows (a pinned
  generated tuple or controlled UI fixture) is a design choice; the current
  non-exercise success path is direct.

---

## Tilling behavior probe

### [#1609] NCT-15. Tilling probe skips the required fluid-exclusion behavior

The tilling probe declares that a fluid-covered tile must never receive a till
designation, but it searches its generated region for a suitable tile and
silently skips this phase when none is found.  The designation, persistence,
and autonomous-AI phases may all pass, leaving a regression in the
fluid-exclusion rule unexercised.

**Evidence:**

- `tools/till_probe.py:8-23` lists "a tile under fluid is never designated"
  as behavior 2 of the probe, alongside its required designation, save/load,
  and AI contracts.
- `till_probe.py:93-100` searches the loaded generated region for a
  fluid-covered tile rather than supplying a known one.
- `till_probe.py:167-178` performs the exclusion assertion only when that
  search succeeds; `:179-181` instead prints `[SKIP]` and adds no failed
  result when no tile is found.
- The final verdict continues from that branch through the other independent
  phases, so their success can return zero without this rule being exercised.

**Handoff context:**

- **Current behavior:** A fixture change that removes local fluid, or a
  regression that makes the scan unable to observe it, leaves the fluid
  exclusion contract without probe coverage while preserving a green result.
- **Expected direction:** Use a pinned fixture known to include a reachable
  fluid tile, or establish a controlled fluid-covered tile through the real
  supported world API, then fail with a fixture/setup diagnostic if that
  prerequisite cannot be obtained.
- **Scope and constraints:** Keep the real designation call and its negative
  assertion; do not convert this into a pure implementation-detail test.
  The other dry-tile, persistence, and AI checks are separate useful behavior
  coverage and should remain independent.
- **Remaining uncertainty:** The durable fixture mechanism is a design choice;
  the successful omission of the stated behavior is direct.

---

## Item-temperature behavior probe

### [#1611] NCT-16. Item-temperature probe skips its required cooling-rate behavior

The item-temperature probe declares a Newtonian-rate check: an item farther
from ambient should close more degrees over the same interval.  It instead
omits that check whenever ambient is at least 45°C, even though its fixed 100°C
and 60°C items remain on the same cooling side of ambient until 60°C.  A warm
fixture can therefore produce a passing probe that never checks its stated
cooling-rate behavior.

**Evidence:**

- `tools/item_temp_probe.py:7-18` names the hot/cold direction and the
  farther-from-ambient Newtonian-rate relation as separate behavior 2 and 3
  of the end-to-end probe.
- `item_temp_probe.py:89-105` creates the two rate-comparison items at fixed
  100°C and 60°C on the same tile, records their initial values, and samples
  both over the same elapsed interval.
- `item_temp_probe.py:115-124` evaluates `drop_hot > drop_warm > 0` only
  under `amb < 45`; at every warmer ambient it prints `[SKIP]` and records no
  failed result.
- For `45 ≤ amb < 60`, both fixed initial temperatures are still above ambient
  and the same positive-drop comparison is valid.  For any ambient, the probe
  could instead choose two explicitly distinct temperatures on one side of
  ambient to retain the intended relation.

**Handoff context:**

- **Current behavior:** A world/climate fixture with ambient at or above 45°C
  loses the probe's only relative-rate assertion while retaining a successful
  final verdict from its direction, pause, held-item, and persistence checks.
- **Expected direction:** Choose the two item temperatures relative to the
  observed ambient (with a safe separation and headroom), then require the
  farther item to close more distance.  If an extreme ambient cannot support
  the configured range, fail clearly or select a known suitable tile rather
  than silently omitting the phase.
- **Scope and constraints:** Keep the existing monotonic direction checks;
  they test a different property.  Avoid asserting exact decay amounts, which
  would make the real-time probe unnecessarily brittle.
- **Remaining uncertainty:** None material about the skipped range; the best
  fixture-selection mechanism is an implementation choice.

---

## Core queue Hspec tests

### [#1612] NCT-17. Core queue tests omit the custom timeout contract and real contention

The core queue suite covers only immediate `tryReadQueue`, `writeQueue`, and
`flushQueue` behavior.  It never calls the blocking `readQueue` operation or
the project's custom `readQueueTimeout` implementation, whose reason for
existing is to avoid losing a dequeued message to an external timeout race.
Its two examples labelled concurrent also create no contention: one is wholly
sequential, and the other makes the reader wait until the sole writer has
finished all writes.

This is a separate core-test finding, not a proposal to stress-test STM or
revalidate `TQueue` itself.

**Evidence:**

- `src/Engine/Core/Queue.hs:19-35` defines the untested blocking read and the
  project-owned timeout race inside one STM transaction; the comment at
  `:25-28` identifies message loss as the behavior it prevents.
- `test/Test/Engine/Core/Queue.hs:16-38` exercises only immediate
  write/try-read/flush behavior.
- `Queue.hs:40-49` labels an example “multiple writers and readers” but writes
  three values and then reads them sequentially in the same thread.
- `Queue.hs:51-69` uses one writer and one reader, but the reader blocks on a
  separate barrier that the writer sets only after all three writes have
  completed; no read can overlap the writes.
- Repository findings searches found no existing entry owning this queue
  timeout/concurrency coverage gap.

**Handoff context:**

- **Current behavior:** A regression in the blocking or timeout path can leave
  the full queue suite green, while the nominal concurrency examples mostly
  reconfirm upstream `TQueue`'s already-completed sequential operations.
- **Expected direction:** Add bounded, deterministic tests for an empty
  timeout, delivery before a timeout, and a writer that becomes eligible while
  a timed read is waiting.  Use barriers to establish the intended ordering,
  then permit the relevant read/write overlap; retain simple FIFO tests as
  smoke coverage rather than calling them concurrent.
- **Scope and constraints:** Do not introduce a probabilistic load/stress test
  or make the suite depend on scheduler luck.  The objective is the wrapper's
  own atomic timeout contract, not benchmarking STM.
- **Remaining uncertainty:** None material; the only design choice is the
  preferred deterministic orchestration of the timing cases.

---

## Item-temperature persistence probe

### [#1613] NCT-18. Item-temperature persistence phase can load a stale save

The item-temperature probe queues a save, sleeps for three seconds, then
loads a fixed slot without observing whether the save was accepted or durably
completed.  Because it also uses the ordinary resource root, a previous
`item_temp_v68_check` artifact can be selected when the current save fails or
has not yet published.  The final temperature comparison may consequently
claim a current round trip while observing stale state.

This is separate from NCT-16's skipped cooling-rate phase: it is a
save/load-oracle correctness problem in the persistence check.

**Evidence:**

- `tools/item_temp_probe.py:67-75` boots against the normal working resource
  root; it does not create a probe-owned root or save directory.
- `item_temp_probe.py:172-180` queues `engine.saveWorld` to the fixed
  `item_temp_v68_check` name, discards its Boolean result by returning a
  literal `ok`, waits a fixed three seconds, then requests a load of that
  name.
- `item_temp_probe.py:181-192` waits for load publication but never connects
  that load to a successfully completed save request from this invocation.
- `src/Engine/Scripting/Lua/API/Save.hs:241-255` documents that `saveWorld`
  returns only synchronous validation/queue acceptance and that disk-write
  failures are asynchronous.
- `tools/probelib.py:279-331` provides load-status waiting, and its adjacent
  save-status/request helpers establish the repository's request-specific
  completion pattern; this probe imports none of that save completion support.

**Handoff context:**

- **Current behavior:** A failed, interrupted, or still-writing current save
  can be confused with a prior fixed-slot generation, so the persistence phase
  does not prove the state it loads was saved by this run.
- **Expected direction:** Give the probe an isolated resource root and a
  per-run save identity, check the immediate acceptance result, capture the
  request id, and wait for its terminal successful save status before loading.
  Capture and match the following load request id as well.
- **Scope and constraints:** Preserve a real save/load transaction and the
  current paused temperature comparison.  Do not substitute a direct file
  read or a sleep-based longer timeout for request-specific completion.
- **Remaining uncertainty:** None material about the asynchronous boundary;
  the exact helper composition and slot naming are implementation choices.

---

## Vegetation and farming persistence probes

### [#1616] NCT-19. Vegetation/farming persistence probes can load stale saves

The farm-AI and flora-growth probes each queue a save into a fixed slot in the
ordinary resource root, wait an arbitrary three seconds, and load it without
proving their own save reached durable completion.  An old generation from a
previous invocation can therefore satisfy the final post-load assertions when
the current save is rejected, interrupted, or still writing.

This is the same oracle class as NCT-18 but belongs to the separate
vegetation/farming probe region.  It deliberately does not combine unrelated
manual probes that happen to use a fixed sleep.

**Evidence:**

- `tools/farm_ai_probe.py:191-199` boots using the ordinary resource root;
  `:553-559` queues `farm_ai_v79_check`, returns a literal `ok`, sleeps three
  seconds, then starts the load.
- `tools/flora_growth_probe.py:250-259` likewise boots without an isolated
  resource root; `:366-371` does the same fixed-sleep sequence with
  `flora_growth_check`.
- Both probes call `wait_load_published` only after the load request.  Neither
  imports or calls a request-specific save-completion helper.
- `src/Engine/Scripting/Lua/API/Save.hs:241-255` defines the asynchronous
  boundary: a `saveWorld` true result means queued, while disk-write outcomes
  occur later.
- `tools/probelib.py:279-331` documents the matching load lifecycle and
  contains the shared request/status support these callers bypass.

**Handoff context:**

- **Current behavior:** A green farm/flora persistence phase need not have
  loaded output produced by the run that issued the assertion.
- **Expected direction:** Give each probe an isolated root and invocation-owned
  slot; assert queue acceptance, capture the save request identity, and wait
  for successful completion before starting a matching load transaction.
- **Scope and constraints:** Retain the real save/load coverage and their
  respective crop/flora post-load assertions.  Do not solve the race by merely
  increasing the fixed delay.
- **Remaining uncertainty:** None material about the race; helper factoring
  across this probe family is an implementation choice.

---

## Item-identity persistence probe

### [#1617] NCT-20. Item-identity persistence phase can load a stale save

The default item-instance probe queues a save into the ordinary resource root
and immediately loads the shared `issue67_probe` slot.  It neither checks the
current save's Boolean acceptance nor waits for its durable completion.  A
previous slot generation can therefore be loaded and made to satisfy the
instance-ID and allocator assertions, even if the inventory state created by
this invocation was not saved.

This is separate from NCT-18's item-temperature persistence phase: the probes
cover different inventory contracts and have independent lifecycle setup.

**Evidence:**

- `tools/item_instance_probe.py:139-146` boots against the normal resource
  root, with no invocation-owned save directory.
- `item_instance_probe.py:297-306` enters the default persistence phase,
  queues `engine.saveWorld('arena', 'issue67_probe')` while discarding its
  result, and immediately queues the load; it waits only for load
  publication.
- `item_instance_probe.py:311-322` treats the loaded inventory IDs and a fresh
  post-load allocation as evidence for this run's save without establishing
  that causal link.
- `src/Engine/Scripting/Lua/API/Save.hs:241-255` specifies that queue
  acceptance is the synchronous result and storage success/failure is
  asynchronous.
- `tools/probelib.py:279-331` supplies request/status lifecycle support, but
  this probe imports only `wait_load_published` and no save completion helper.

**Handoff context:**

- **Current behavior:** The default persistence test can pass against a stale
  shared slot rather than a newly committed snapshot, so it does not reliably
  prove either ID preservation or allocator continuation for the current run.
- **Expected direction:** Use an isolated root and invocation-owned slot;
  require current save acceptance and request-specific completion before a
  matched load request.  Keep the existing ID and post-load allocation
  assertions after that reliable boundary.
- **Scope and constraints:** The explicit `--no-save` mode may remain a
  deliberate opt-out.  Do not convert the persistence test into a direct
  serialization inspection or replace completion with an arbitrary delay.
- **Remaining uncertainty:** None material about the current false-green path;
  shared helper adoption is a design choice.

---

## Foraging persistence probe

### [#1618] NCT-21. Foraging persistence phase can load a stale save

The foraging probe saves a freshly harvested tile's regrowth state to a fixed
slot in the normal resource root, sleeps for three seconds, and then loads
that name.  It does not establish acceptance or durable completion for the
save request it just made.  A prior `foraging_v66_check` generation can thus
be loaded and make the regrowth-timer round trip appear to pass.

This remains separate from NCT-19's farm/flora group.  The nearby chop probe
also uses a fixed delay but uses an isolated temporary root, so it does not
share the stale player-slot false-green path recorded here.

**Evidence:**

- `tools/foraging_probe.py:72-79` boots without a `--resource-root` override,
  so ordinary `saves/` slots are in scope.
- `foraging_probe.py:120-128` harvests the timer fixture, queues a save to
  `foraging_v66_check` while returning a literal `ok`, sleeps three seconds,
  then starts the load and waits only for load publication.
- `foraging_probe.py:139-143` accepts the loaded tile's live
  `regrowthRemaining` as proof of the current harvest's persistence despite
  not correlating it with the current save request.
- `src/Engine/Scripting/Lua/API/Save.hs:241-255` documents that queue
  acceptance is synchronous while disk completion is asynchronous.
- `tools/probelib.py:279-331` provides the shared request/status lifecycle
  infrastructure this caller does not use for saving.

**Handoff context:**

- **Current behavior:** A green save/load check can reflect an older shared
  slot rather than this run's just-harvested flora state.
- **Expected direction:** Use an isolated per-run root and slot, then require
  the current save request's successful terminal status before a matched load
  and the existing regrowth assertion.
- **Scope and constraints:** Preserve the real harvest, save/load, and
  reloaded-tile observation.  Do not replace asynchronous synchronization with
  a longer sleep or a direct persistence-file assertion.
- **Remaining uncertainty:** None material about the causal gap; naming and
  helper factoring are implementation decisions.

---

## Location persistence probes

### [#1620] NCT-22. Location persistence probes can load stale fixtures

Four location-oriented manual probes save their fixture to fixed slots in the
ordinary resource root, sleep briefly, then restart or load without proving
that the current save completed.  Their later location-content, overlay,
stamping, or visual assertions can consequently inspect an older fixture
generation rather than the state created by the invocation under test.

This is one coherent location-probe group.  It deliberately excludes
`location_embark_probe.py`, whose fixed-slot and save-completion defects are
already captured separately in `docs/project_review_859-848.md`.

**Evidence:**

- `tools/location_content_probe.py:818-820,869-875` saves the fixed
  `loc_content_probe` slot, sleeps one second, then starts a fresh-process
  load; `:1404-1419` repeats that pattern for `loc_naming_probe`.
- `tools/location_overlay_probe.py:315-316,375-381` does the same for
  `loc_overlay_probe`; `:449-464` repeats it for `loc_centre_probe`.
- `tools/location_stamp_idempotent_probe.py:210-228,285-299` uses one-second
  sleeps around the fixed `stamp_idempotent_probe` and
  `stamp_idempotent_probe_fresh` saves before fresh-process loads.
- `tools/portal_ghost_probe.py:85-90,201-209` sleeps one second after writing
  the fixed `portal_ghost_probe` fixture, then has the offscreen session load
  that name.
- None of the four scripts supplies a probe-owned `--resource-root` or a
  request-specific successful-save wait at these boundaries.  `saveWorld` is
  asynchronous after acceptance by `src/Engine/Scripting/Lua/API/Save.hs:241-255`.
- `tools/probelib.py:279-331` provides shared lifecycle support for a real
  load transaction, but not one of these callers establishes a corresponding
  current save-completion boundary first.

**Handoff context:**

- **Current behavior:** The test's post-restart evidence is not causally tied
  to the fixture it just generated; a prior shared-slot generation can mask a
  failed or interrupted current save.
- **Expected direction:** Give each probe invocation an isolated root and
  unique slots, require current save acceptance plus request-specific durable
  completion, and then wait for the matching load publication before existing
  behavior assertions run.
- **Scope and constraints:** Retain the multi-process, real persistence
  scenarios and their current location/UI observations.  Do not replace
  lifecycle synchronization with longer sleeps or collapse the four distinct
  feature assertions into one generic smoke test.
- **Remaining uncertainty:** None material about the shared asynchronous
  boundary; the exact common helper and artifact ownership design is open.

---

## Manual-only probe inventory

### [#1621] NCT-23. Position-hold inventory overstates the executable fixture by one acolyte

> **Captured note:** Position-hold apparatus overstates its unit count

**Verification:** Verified — the probe docstring, manual inventory, and CI
classifier all say four acolytes share the arena, but the executable fixture
spawns exactly three and its declared checks use only those three roles.

**Evidence:**

- `tools/position_hold_probe.py:17-27` — the module description claims four
  units while naming only `held`, `control`, and `internal`.
- `tools/position_hold_probe.py:113-140` — all 12 declared checks concern the
  held unit, the never-commanded control, or the internal-move unit; no fourth
  role appears in the probe contract.
- `tools/position_hold_probe.py:203-218,370-378` — `_run` spawns `held` and
  `control` initially and `internal` later, with no other `spawn_acolyte` call.
- `tools/README.md:450` — the manual inventory repeats that four acolytes share
  the arena.
- `tools/ci_probes.py:187-191` — the manual-only classifier rationale likewise
  attributes the scenario's cost to four acolytes.
- `git blame` attributes the three descriptions and all three spawn sites to
  the same introducing commit, `87dafde2f`, so no later fixture reduction
  explains the disagreement.

**Handoff context:**

- **Current behavior:** Three maintained descriptions overstate the live
  fixture's cardinality and describe a nonexistent fourth role, while the
  executable probe and its checks consistently operate on three acolytes.
- **Expected behavior:** The probe docstring, manual inventory, classifier
  rationale, and executable fixture should agree on the number and purpose of
  participating units.
- **Scope and constraints:** Preserve the three existing oracle roles, all 12
  behavior checks, and the justified `scenario-heavy` manual-only
  classification. Do not add an otherwise unused fourth unit merely to make
  stale prose true.
- **Remaining uncertainty:** No fourth oracle role is present or documented.
  If one is intended, its independent behavioral purpose would need to be
  established rather than inferred from the current count claim.
