# Holistic Project Audit Findings

This report records correctness, lifecycle, distribution, and rendering concerns found during a repository-wide audit of Synarchy at commit `b70ce762effb0bd1376781cc369d36989254218f`. It is intended for one-at-a-time verification and disposition through `process-report`.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The audit divided the repository into three parallel subsystem reviews—engine core and persistence, world generation and simulation, and frontend/gameplay/rendering—plus a cross-cutting inspection of builds, packaging, tests, tools, assets, and CI governance.

The review inspected the owning implementation, focused tests, relevant engine contracts, and local findings/project-review documents. Tracker deduplication and issue disposition remain deliberately deferred to later `process-report` runs.

Validation included:

- Warning-clean builds of the library, executable, graphical test executable, and headless test executable.
- 315 focused headless examples covering worker lifecycle, fluid seams, scene batching, unit atlases, UI input ownership, container stacking, responsive gameplay, and Lua random-stream ownership.
- 131 unit-atlas packer self-tests.
- Capability, persistence, module-budget, Unicode-operator, Lua-decoding, material, texture, and asset-reference audits.
- Pure GHCi reproductions for the fluid underflow and unlike-fluid behavior.
- Targeted tests run from an unpacked Cabal source distribution.

The full headless suite, full world-generation tier, baseline regeneration, full behavior-probe sweep, graphical test execution, offscreen rendering, and `make ci` were not run. No implementation or documentation files were changed during the audit.

A follow-up static architecture pass at commit
`4960d4d9976d572e664c4c8860ae95f22455ba36` traced settings state,
content-startup readiness, interval timing, worker acquisition, configuration
persistence, and Lua callback scheduling. That pass used source and document
inspection only: it ran no project code, builds, tests, probes, launches, or
external mutations. HPA-12 through HPA-19 record its retained findings.

A second follow-up static architecture pass at commit
`30a764fcbb9b1d9dfb4dfc39c0b10939f5badbea` traced content identity,
load-reference validation, save-barrier exclusion, filesystem durability,
world-page incarnation, simulation admission, public numeric APIs, debug
command completion, fail-stop behavior, and diagnostic retention. It also
cross-checked the ready chunk-residency and designation designs so concerns
already owned by those arcs were not duplicated. This pass used source and
document inspection only: it ran no project code, builds, tests, probes,
scripts, launches, or external mutations. HPA-20 through HPA-32 record its
retained findings.

A third follow-up static architecture pass at commit
`78dbbba78bb568badeca7d03adcc73335cee2b7a` traced configuration failure
semantics, numeric ingress from Lua, fresh-session ownership, construction and
inventory transactions, medical spatial authority, deferred UI callbacks,
exact-instance inventory identity, save compatibility under content evolution,
projected-page chunk work, and flora schema validation. The review
independently cross-checked each retained concern against the existing holistic
report, local findings reports, project-review documents, persistence
contracts, and active design documents. This pass used source and document
inspection only: it ran no project code, builds, tests, probes, GHCi sessions,
scripts, launches, or external mutations. HPA-33 through HPA-45 record its
retained findings.

A fourth follow-up static architecture pass at commit
`275c2fe24990ff841d753cec912abf514b614dae` divided deeper inspection
across engine/persistence, gameplay/AI, UI/render/Lua, content/world-schema,
and world-generation/simulation specialists. Two opposite-domain reviews then
attempted to falsify the retained transaction, lifecycle, and reachability
claims. The pass traced page-local identity, asynchronous admission and commit,
action suspension, save enumeration and publication, spatial and temporal
numeric ingress, content-graph validation, zoom-fluid classification, and
generated spillway ownership. It deduplicated candidates against the holistic
report, local findings and project-review reports, persistence contracts, and
active design documents. This pass used source and document inspection only:
it ran no project code, builds, tests, probes, GHCi sessions, scripts, launches,
or external mutations. HPA-46 through HPA-62 record its new concerns; the same
pass broadened HPA-4, HPA-7, and HPA-35 with independently verified evidence.

## Status

- [x] HPA-1. Lateral fluid equalization can underflow its source volume — [#2042]
- [x] HPA-2. Runtime fluid ignores the cylindrical world seam — [#2044]
- [ ] HPA-3. Transfers between unlike fluids silently change fluid identity — [deferred]: reaction epic pending via /design-epic
- [x] HPA-4. Session replacement retains independent history and transient UI — [#2156]
- [x] HPA-5. Legacy save read exceptions leave the load transaction active — [#2162]
- [x] HPA-6. Forced worker shutdown returns before the worker has exited — [#2165]
- [x] HPA-7. Debug-console trust, admission, and listener lifetime are unbounded — [#2170]
- [x] HPA-8. The Cabal source distribution omits required resources — [#2175]
- [x] HPA-9. Stale review-approval removal fails open — [#2184]
- [x] HPA-10. Grouped combat and injury histories bypass their retention caps — [#2189]
- [x] HPA-11. Public scene text and UI-layer sprites are omitted from rendering — [#2192]
- [x] HPA-12. Settings Back cannot restore most applied-but-unsaved video values — [#2194]
- [x] HPA-15. Video configuration validation stops at the intended UI — [#2198]
- [x] HPA-19. Local configuration persistence has no common durable-write contract — [#2202]
- [x] HPA-13. Startup completion measures dispatch instead of successful readiness — [#2203]
- [x] HPA-18. Worker startup has no general partial-acquisition rollback — [no-issue]
- [x] HPA-14. Runtime intervals use discontinuous wall time without a valid first sample — [#2204]
- [x] HPA-16. A Lua update that changes its interval is scheduled twice — [#2205]
- [x] HPA-17. Lua callback isolation cannot contain nontermination or chronic failure — [no-issue]
- [x] HPA-20. Persisted flora species identity is an OS-order-dependent ordinal — [#2236]
- [x] HPA-21. Plant-designation species bypass the flora load gate — [#2243]
- [x] HPA-22. Save-barrier acknowledgements do not establish a closed quiescence boundary — [#2221]
- [x] HPA-23. Unknown-component preservation fails open when a generation cannot be read — [#2227]
- [x] HPA-24. Slot creation and autosave rotation omit parent-directory durability — [#2229]
- [x] HPA-25. A fresh world's center chunk is never admitted to fluid simulation — [#2232]
- [ ] HPA-26. Reusing a world-page name can adopt the previous incarnation's state — [deferred]: page incarnation epic pending via /process-design-doc
- [x] HPA-27. Normal world initialization discards runtime material registrations — [#2278]
- [x] HPA-28. A non-finite time scale crashes the world worker — [#2280]
- [x] HPA-29. A timed-out debug command can still execute later — [#2282]
- [x] HPA-30. Fallible crash logging can suppress the engine's fail-stop transition — [#2283]
- [x] HPA-31. The action-outcome “ring” is unbounded and survives ordinary session exit — [#2284]
- [x] HPA-32. Popup-enabled events accumulate in a write-only queue — [#2285]
- [x] HPA-33. A malformed world-generation configuration silently becomes the complete default configuration — [#2286]
- [x] HPA-34. Non-finite world-generation parameters can crash asynchronous world initialization — [#2288]
- [x] HPA-35. Unit motion accepts invalid numeric state from Lua and YAML — [#2290]
- [x] HPA-36. Exit to Menu has no authoritative fresh-session reset transaction — [#2291]
- [x] HPA-37. Multi-item construction payment can consume a partial cost and then charge the whole cost again — [no-issue]
- [x] HPA-38. Autonomous harvesting and foraging bypass carrying-capacity admission — [#2293]
- [x] HPA-39. Player-issued medical treatment bypasses page and proximity logistics — [#2297]
- [x] HPA-40. Deferred ground-item Info can install a stale selection and erase valid selections — [#2300]
- [x] HPA-41. Medical supply discovery aliases same-definition containers to the first instance — [#2302]
- [x] HPA-42. Saved immunity keys bypass infection-reference validation — [#2305]
- [x] HPA-43. Saved equipment slot keys bypass the current equipment-class schema — [#2307]
- [x] HPA-44. Projected page selection does not bind bulk chunk work to its intended page — [#2310]
- [x] HPA-45. Flora YAML silently defaults or drops misspelled semantic enums — [#2315]
- [x] HPA-46. Zoom coastal fill promotes non-ocean fluids through a scan-order-dependent ocean cascade — [#2316]
- [x] HPA-47. Shared lake spillways collapse two source identities into one — [#2323]
- [x] HPA-48. Craft-bill mutations can cross page-local identity domains — [#2325]
- [x] HPA-49. Building placement validates an unreserved snapshot and commits trusted coordinates — [#2326]
- [x] HPA-50. Combat resolution does not authoritatively revalidate mutable attack conditions — [#2328]
- [x] HPA-51. Page suspension leaves action clocks and working leases running — [#2332]
- [x] HPA-52. One unreadable save entry aborts the complete save listing — [#2333]
- [x] HPA-53. Dump fast-settle can wait forever after a worker failure — [#2334]
- [x] HPA-54. Modern and legacy saves can occupy and expose the same logical name — [#2335]
- [x] HPA-55. A failed candidate flush leaks its still-owned file handle — [no-issue]
- [x] HPA-56. World-space item and decal spawns admit non-finite coordinates into rendering — [#2336]
- [x] HPA-57. Camera APIs admit non-finite state into partial loop and load arithmetic — [#2337]
- [x] HPA-58. `world.digTile` can kill the world worker or durably poison a designation — [#2338]
- [x] HPA-59. `world.setDate` installs and persists a noncanonical date — [#2339]
- [x] HPA-60. Infection YAML has no semantic validation for bands, weights, or multipliers — [#2346]
- [x] HPA-61. Building animation FPS can reach partial main-render arithmetic — [#2347]
- [x] HPA-62. Duplicate unit body-part IDs split targeting from damage authority — [#2348]

---

## Fluid simulation integrity

### [#2042] HPA-1. Lateral fluid equalization can underflow its source volume

**Severity:** High

Lateral equalization calculates every requested transfer from a frozen source volume, then applies those requests sequentially without bounding them by the source’s remaining volume. Because active volume is stored as `Word16`, over-draining wraps the source to a value near 65,535 and manufactures a large quantity of fluid.

**Evidence:**

- `src/Sim/Fluid/Active.hs:318-343` — `phaseLateral` freezes the input grid and calculates every neighbor transfer from the snapshot’s `srcVol`.
- `src/Sim/Fluid/Active.hs:344-351` — each transfer subtracts the original requested amount from the mutable source without checking its current volume.
- `src/Sim/Fluid/Types.hs:13-23` — `afcVolume` is `Word16`, and one terrain level contains seven volume units.
- `src/Sim/Fluid/Active.hs:278-309` and `:401-437` — the gravity and waterfall phases already demonstrate the required conservation shape by tracking total availability and capping each transfer against the remaining source.

A pure fixture placed volume 3 in a flat center cell and volume 1 in each of its four cardinal neighbors. One `simulateActiveTick` changed total volume from 7 to 65,543 and left the center at `Just 65535`.

**Handoff context:**

- **Current behavior:** An ordinary low-volume cell can wrap below zero during four-way equalization and publish tens of thousands of nonexistent volume units into authoritative simulation state.
- **Expected direction:** Every fluid phase conserves total volume and no transfer exceeds the source volume remaining at the moment it is applied.
- **Scope and constraints:** Preserve the existing integer-quarter equalization behavior and minimum-one-unit progress rule. Add a low-volume four-neighbor regression and randomized conservation checks over valid active grids.
- **Remaining uncertainty:** None at draft time; the numerical failure was reproduced directly.

### [#2044] HPA-2. Runtime fluid ignores the cylindrical world seam

**Severity:** Medium–High

World storage uses a cylindrical U-axis topology, but runtime simulation neither retains the owning world’s size nor canonicalizes neighboring chunk coordinates. Fluid activation and cross-chunk exchange therefore treat the physical U seam as a hard boundary.

**Evidence:**

- `src/World/Chunk/Types.hs:36-55` — `wrapChunkCoordU` is the canonical storage and lookup transformation for cylindrical worlds.
- `src/Sim/State/Types.hs:31-40` — `SimWorldState` contains chunks, dirtiness, and activation state but no world size or topology information.
- `src/Sim/Thread.hs:251-260` — an edited chunk activates four raw cardinal neighbors using `cx ± 1` and `cy ± 1`.
- `src/Sim/Fluid/Active.hs:195-238` — seam reconciliation likewise probes only raw positive-X and positive-Y neighbor coordinates.
- `test-headless/Test/Headless/Sim/Seam.hs:79-115` — current seam tests cover ordinary `(0,0)` to `(1,0)` adjacency but not opposite canonical edges of a wrapped world.

For a 64-chunk cylindrical world, a raw neighbor across the U boundary canonicalizes to a different stored coordinate with both components changed. The simulation’s direct hash-map lookup cannot find that physically adjacent chunk.

**Handoff context:**

- **Current behavior:** Water or lava edited near the cylindrical boundary cannot activate or exchange with the physically adjacent chunk across the seam, so fluid can pile up against an artificial wall.
- **Expected direction:** Runtime simulation uses the same canonical topology as chunk storage, rendering, loading, and generated hydrology.
- **Scope and constraints:** The simulation needs enough per-world topology context to canonicalize activation and reconciliation while processing each shared edge exactly once. Arena and zero-size worlds must retain identity behavior.
- **Remaining uncertainty:** The coordinate mismatch is direct, but an end-to-end active-fluid fixture at the physical wrap boundary was not run.

### [deferred] HPA-3. Transfers between unlike fluids silently change fluid identity

> **Deferred:** Owner chose a lava+water→stone reaction feature, which is epic-sized — precondition: the unlike-fluid reaction design document is processed through `/process-design-doc` and its epic number exists to link here.

**Severity:** Medium

Every active-fluid transfer copies the source type only when the destination is empty. If a destination already contains a different fluid, its existing type is retained while the source volume is added, silently converting all transferred material.

**Evidence:**

- `src/Sim/Fluid/Active.hs:163-181` — cross-chunk transfer adds volume to an occupied destination without considering the source and destination types.
- `src/Sim/Fluid/Active.hs:298-308` — gravity has the same occupied-destination behavior.
- `src/Sim/Fluid/Active.hs:338-351` — lateral equalization likewise retains the destination type.
- `src/Sim/Fluid/Active.hs:421-431` — waterfall transfer repeats the same rule.
- `src/Unit/Pathing/Cost.hs:269-277` — the retained type materially changes gameplay: lava is impassable while lakes and rivers are wadeable.

A pure tick with `Lava 7` above an occupied `Lake 1` cell produced an empty source and `Lake 8` at the destination.

**Handoff context:**

- **Current behavior:** Lava becomes water, or water becomes lava, according only to which type occupied the destination first.
- **Expected direction:** Unlike-fluid contact follows one explicit and consistent gameplay policy rather than implicitly allowing the destination type to win.
- **Scope and constraints:** Apply the chosen policy consistently to gravity, lateral, waterfall, and cross-chunk transfers. Cover both type orderings and their pathing/rendering consequences.
- **Remaining uncertainty:** The intended product policy—blocking, reaction, precedence, or typed composition—is not currently stated.

---

## Session and runtime lifecycle

### [#2156] HPA-4. Session replacement retains independent history and transient UI

**Severity:** Medium–High

The Haskell publication path clears much of its transient session state before
broadcasting `onSaveLoaded`, but the common Lua callback only rebinds the HUD
and closes the container stack. Independently owned histories, modal log pages,
selections, popup cards, and the engine-owned locked-tooltip surface remain
attached to the replacement session.

**Evidence:**

- `src/World/Load/Publish.hs:352-372` — publication clears engine-owned combat, injury, thought, action, event-store, popup-queue, input, focus, and hover state.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:302-339` — the first Lua-side post-publication boundary broadcasts `onSaveLoaded`.
- `scripts/ui_manager_menu.lua:171-200` — the common callback rebinds the active world and HUD and closes the container window, but does not run a general session teardown.
- `scripts/combat_log.lua:61-83,477-498,954-1007` — combat history and grouped battles persist in module-owned tables and the module defines no `onSaveLoaded`.
- `scripts/injury_log_panel.lua:54-68,408-423,803-847` — injury history and per-unit groups have the same ownership and no load callback.
- `scripts/thought_log.lua:12-63` — per-unit thought rings persist by numeric UID and have no load callback.
- `scripts/event_log.lua:615-648,722-725` and `scripts/unit_log.lua:565-618` — both own modal pages that can remain visible; the unit log also retains its selected UID.
- `scripts/popup.lua:1061-1090` — a suitable `dismissAll` operation exists, but load replacement does not call it.
- `scripts/ui/view_teardown.lua:251-267,328-342` — the existing registry knows how to hide these independent surfaces during HUD transitions but defines no load/session-replacement transition.
- `src/UI/Tooltip/Lock.hs:59-74` and `src/UI/Tooltip/State.hs:76-80,127-144`
  — locking snapshots the tooltip content and makes later ticks ignore hover or
  source validity.
- `src/UI/Manager/Page.hs:45-54` — deleting the source page removes its
  elements without reconciling the separately owned tooltip state.
- `src/UI/Tooltip/State.hs:188-195` and
  `src/World/Load/Publish.hs:311-372` — tooltip visuals live on an independent
  page, and replacement publication clears hover/focus but not the locked
  tooltip.

A bare production-module Lua harness inserted a thought for UID 7, drained the event, simulated a replacement with an empty engine queue, and confirmed that the module had no `onSaveLoaded` callback and still returned the old text from `unitEntries(7)`.

**Handoff context:**

- **Current behavior:** Reused integer UIDs can inherit another session’s combat,
  injury, or thought history. A debug-console load during gameplay can also
  leave old modal panels or a locked tooltip blocking input, and stale cards can
  continue to refer to entities and coordinates from the replaced session.
- **Expected direction:** Session replacement has one explicit cross-owner
  teardown transition that clears session-bound data and dismisses independent
  transient surfaces before they can operate on the new session.
- **Scope and constraints:** Preserve the existing survivor reconciliation and HUD/world rebinding. Clear selected UID/tab and scroll state without re-firing ordinary user callbacks. Test a reused UID and initially visible modal/card surfaces.
- **Remaining uncertainty:** Thought-history retention was reproduced directly; visible modal persistence was established structurally but not exercised in an offscreen session.

### [#2162] HPA-5. Legacy save read exceptions leave the load transaction active

**Severity:** Medium

The supported legacy flat-save path reads its file after `beginLoad` and pause have already established a transaction. A synchronous filesystem exception escapes instead of returning the normal `Left` result that terminalizes the load.

**Evidence:**

- `src/World/Save/Serialize.hs:144-170` — `loadWorld` selects the legacy flat-file path after an existence check.
- `src/World/Save/Serialize.hs:248-260` — `decodeLegacyFile` calls `BS.readFile` without catching `IOException`.
- `src/Engine/Scripting/Lua/API/Save.hs:651-665` — `loadSaveFn` begins the transaction and imposes the load pause before continuing.
- `src/Engine/Scripting/Lua/API/Save.hs:689-701` — `failLoad` runs only when `loadWorld` returns `Left`.
- `src/Engine/Scripting/Lua/API/Internal.hs:28-41` — unexpected synchronous exceptions are converted into Lua errors, keeping the Lua thread alive but performing no load-status cleanup.

**Handoff context:**

- **Current behavior:** A permission error or existence-check/read race on `saves/<name>.synworld` leaves the load non-terminal. Every later save or load rejects as “transaction already active” until the process restarts.
- **Expected direction:** Every synchronous failure after `beginLoad` reaches a terminal failed-load state with an actionable diagnostic.
- **Scope and constraints:** Preserve the deliberate rule that a failed load leaves simulation paused. Asynchronous exceptions must continue to propagate rather than being converted into ordinary load failures.
- **Remaining uncertainty:** The exception path is direct but was not induced through a live Lua API call during this audit.

### [#2165] HPA-6. Forced worker shutdown returns before the worker has exited

**Severity:** Medium

The shared worker loop catches `SomeException`, which includes asynchronous `ThreadKilled`. After its normal join times out, `shutdownThread` sends that exception but does not perform a second join, so it can return while the crash callback and fork finalizer are still running.

**Evidence:**

- `src/Engine/Core/Thread.hs:148-150` — `tsDone` is filled only by the fork’s `finally`, after the worker loop has actually exited.
- `src/Engine/Core/Thread.hs:180-193` — the running-tick guard catches every `SomeException`, invokes `wsOnCrash`, and returns normally; this also consumes `ThreadKilled`.
- `src/Engine/Core/Thread.hs:195-208` — the normal path waits for `tsDone`, but the timeout path only calls `killThread`.
- `test-headless/Test/Headless/Harness.hs:232-246` — the harness explicitly documents the timeout/kill path as having no subsequent join and being outside its guarantee.
- `src/Engine/Scripting/Lua/Thread.hs:85-92` — Lua crash cleanup logs, drains commands, closes the Lua state, and mutates lifecycle state after the exception is caught.

**Handoff context:**

- **Current behavior:** After a ten-second shutdown timeout, teardown can proceed while worker crash cleanup or Lua-state closing still runs. A second `shutdownThread` call observes `ThreadStopped` and cannot repair the missing join.
- **Expected direction:** Shutdown does not proceed past a worker until its termination is confirmed or an explicit fatal condition is reported. Asynchronous termination is not treated as an ordinary worker crash.
- **Scope and constraints:** Preserve fail-stop handling for synchronous tick failures and idempotent normal shutdown. Add focused coverage for asynchronous termination and the post-timeout completion signal.
- **Remaining uncertainty:** The race requires a worker to exceed the ten-second graceful timeout and was not deliberately induced.

### [#2170] HPA-7. Debug-console trust, admission, and listener lifetime are unbounded

**Severity:** Medium

The loopback debug server exposes the full Lua evaluator to every local process
without authentication, and owns neither connection admission nor listener
supervision. Clients can execute arbitrary in-process Lua, retain unlimited
threads and buffers, and permanently kill the discarded listener through an
`accept` exception after the process has already announced readiness.

**Evidence:**

- `src/Engine/Scripting/Lua/DebugServer.hs:193-200` — the server prints `READY`, forks the accept loop, and discards its thread handle.
- `src/Engine/Scripting/Lua/DebugServer.hs:205-209` — every accepted connection receives an unmanaged thread, with no connection cap or listener recovery.
- `src/Engine/Scripting/Lua/DebugServer.hs:217-231` — a client that never sends a newline grows `leftover <> chunk` without a command-size limit or read timeout.
- `src/Engine/Scripting/Lua/DebugServer.hs:247-259` — complete commands enter an unbounded queue before the client waits up to 30 seconds for a reply.
- `src/Engine/Scripting/Lua/DebugServer.hs:193-200,247-259` — the listener
  accepts commands based only on loopback reachability and forwards every
  non-built-in command to the Lua command queue; there is no credential,
  capability negotiation, or per-client trust decision.
- `test-headless/Test/Headless/Core/DebugListener.hs:1-121` — existing tests cover boot-mode listener policy and diagnostics, not socket admission, buffer limits, or post-`READY` listener death.

**Handoff context:**

- **Current behavior:** Any process in the host network namespace can execute
  the console's full Lua authority and can consume unbounded memory,
  descriptors, and threads. Descriptor exhaustion or another `accept` failure
  can leave a headless/offscreen process running after its sole interactive
  control surface has disappeared.
- **Expected direction:** The console's trust model is explicit and appropriate
  to its deployment modes, its command and connection resources are bounded,
  listener/client lifetimes have named owners, and terminal listener failure is
  observable.
- **Scope and constraints:** Loopback is a transport boundary, not an
  authentication boundary. Preserve concurrent long-running built-ins and the
  command-response timeout while distinguishing transient accept errors from
  permanent loss of the console. If full evaluator authority is intentionally
  retained, document and enforce the operational assumption that the host is
  the security principal.
- **Remaining uncertainty:** Resource exhaustion was not executed because doing so would be intentionally disruptive.

---

## Distribution and governance

### [#2175] HPA-8. The Cabal source distribution omits required resources

**Severity:** Medium

The Cabal manifest says the source distribution contains runtime and test resources, but it omits required JSON assets, the save-compatibility corpus, and a Python file read by the headless suite. The resulting tarball builds but cannot satisfy the runtime and test contracts described beside the manifest.

**Evidence:**

- `synarchy.cabal:28-51` — `extra-source-files` claims to ship runtime/test resources but includes asset PNG and TTF files without the unit-atlas JSON indexes.
- `src/Unit/Atlas/Load.hs:6-16,118-162` — an atlas index is authoritative; animated units with missing indexes reject and have no per-frame fallback.
- `test-headless/Test/Headless/Preview/UnitAnimation.hs:710-736,765-775` — shipped animated units are loaded through the production atlas path, and fixture sanity requires every compiled index.
- `test-headless/Test/Headless/World/Save/Compat.hs:336-360` — compatibility tests read `docs/save_compat/manifest.json` and its declared fixture files at runtime.
- `test-headless/Test/Headless/UI/InteractiveBounds.hs:434-440` — a headless UI contract reads `tools/playtest/critic.py`.
- `assets/textures/units/*/atlas/index.json` — eight tracked runtime indexes are absent from `cabal sdist --list-only`.
- `test-headless/data/save-compat/` — the tracked binary and expected-summary fixture corpus is absent from the distribution.

Comparison against tracked resources found 59 omitted files across the atlas indexes, save-compatibility documentation and fixtures, and the playtest critic. In an unpacked source distribution, targeted tests produced four failures across six examples: the playtest-oracle source check, two save-compatibility cases, and the unit-index fixture sanity check.

**Handoff context:**

- **Current behavior:** An unpacked source distribution lacks the only supported animation metadata for shipped units and cannot run several headless contracts because their declared resources are missing.
- **Expected direction:** The source distribution contains every runtime and test resource its Cabal comments and packaged test suites require.
- **Scope and constraints:** Continue excluding gitignored local configuration state. Add resource patterns narrowly enough to avoid leaking runtime-local files, and give the source manifest an automated completeness check.
- **Remaining uncertainty:** The package declares that it is not published to Hackage, but the repository still maintains and documents `sdist` as a supported validation artifact.

### [#2184] HPA-9. Stale review-approval removal fails open

**Severity:** Medium

The review workflow describes its stale-approval policy as fail-closed, but suppresses every failure from the command that actually removes `reviewed:approve`. A synchronize event can therefore retain a stale label and pass its required review check when the GitHub mutation fails.

**Evidence:**

- `.github/workflows/review-gate.yml:29-39` — the required check passes when the synchronize event’s pull-request payload contains `reviewed:approve`.
- `.github/workflows/review-gate.yml:41-65` — changed PR content is intended to remove that label and trigger a new failing `unlabeled` run.
- `.github/workflows/review-gate.yml:96-107` — the decision’s strip branch executes `gh pr edit ... --remove-label reviewed:approve || true`, masking permission, API, authentication, and transport failures.
- `tools/review_gate_decision.py:49-64` — the self-tested logic is fail-closed only through the decision to strip; its own documentation explicitly excludes the best-effort mutation from that guarantee.

**Handoff context:**

- **Current behavior:** If label removal fails, the synchronize run can remain green with the stale approval label still present, and no `unlabeled` event is generated to close the gate.
- **Expected direction:** A content-changing update cannot leave the gate open unless the workflow positively verifies that the approval label is absent.
- **Scope and constraints:** Preserve approval across proven content-identical branch updates. Distinguish “label was already absent” from failure to remove it, and verify mutation state after the attempted write.
- **Remaining uncertainty:** No external GitHub failure was induced; the fail-open shell control flow is direct, while the frequency of real mutation failures is unknown.

---

## Frontend state and rendering APIs

### [#2189] HPA-10. Grouped combat and injury histories bypass their retention caps

**Severity:** Medium

Combat and injury panels cap only their flat “All” rings. Their grouped battle/unit histories, per-group events, identifiers, and tab metadata are never pruned, even though ingestion and rendering repeatedly scan the entire retained group set.

**Evidence:**

- `scripts/combat_log.lua:61-83` — the 200-entry limit applies only to `allEvents`; `battles` and `nextBattleId` are independent persistent module state.
- `scripts/combat_log.lua:400-413,441-497` — battle lookup and collision naming scan all retained battles, and every event is also inserted into an uncapped per-battle list.
- `scripts/combat_log.lua:675-714` — rendering measures and considers every retained battle tab.
- `scripts/injury_log_panel.lua:54-68` — the injury panel has the same split between a capped flat ring and uncapped grouped state.
- `scripts/injury_log_panel.lua:366-423` — unit-log lookup and naming scan all retained logs, and grouped events are never capped.
- `scripts/injury_log_panel.lua:550-609` — rendering walks the full retained unit-log list.

A bare Lua harness injected 1,000 encounters separated beyond the rejoin window. Combat retained `200 allEvents / 1,000 battle tabs / 1,000 grouped events`; injury retained `200 allEvents / 1,000 unit tabs / 1,000 grouped events`.

**Handoff context:**

- **Current behavior:** Long sessions accumulate unbounded Lua memory and increasingly expensive event ingestion and tab-strip rebuilding. Many distinct groups produce cumulative quadratic lookup and naming work.
- **Expected direction:** The documented retention policy bounds both flat and grouped history while keeping active-tab and scroll state valid when old groups are removed.
- **Scope and constraints:** Cover both one very large group and many expired groups. Preserve the rejoin-window protection against numeric UID reuse.
- **Remaining uncertainty:** None at draft time; the uncapped growth was reproduced directly.

### [#2192] HPA-11. Public scene text and UI-layer sprites are omitted from rendering

**Severity:** Low–Medium

The Lua API accepts scene text and sprites on general layers, and scene update builds corresponding nodes and text batches. Final frame assembly consumes only world-layer scene sprites and independently rendered UI pages, leaving scene text and UI-layer scene sprites without a production draw path.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Register/Engine.hs:140-149,188-192` — the public engine table registers `spawnSprite`, its mutation operations, `spawnText`, and `setText`.
- `src/Engine/Scripting/Lua/Message/Scene.hs:33-86` — handlers create visible text and sprite scene nodes using the requested `LayerId`.
- `src/Engine/Scene/Render.hs:60-78` — scene update collects text batches and stores them in the scene batch manager.
- `src/Engine/Scene/Types/Batch.hs:223-240` and `src/Engine/Scene/Batch/Update.hs:30-35` — `bmTextBatches` is a first-class render-item collection populated by update.
- `src/Engine/Scene/Render.hs:214-221` — the only scene extraction used by frame assembly filters sprites to layers below `uiLayerThreshold`.
- `src/Engine/Loop/Frame.hs:250-292` — final assembly merges those world scene sprites with `renderUIPages`; it never reads `bmTextBatches` or UI-layer scene sprites.

Repository-wide reference inspection found no production consumer of `bmTextBatches`. Current shipped Lua scripts do not call these APIs, so the defect is dormant rather than a regression in a presently visible screen.

**Handoff context:**

- **Current behavior:** `spawnText` can report success and update internal scene state while producing no pixels. `spawnSprite` works on world layers but silently disappears when assigned a UI layer.
- **Expected direction:** Every supported public scene primitive reaches frame assembly with its declared layer ordering, or the unsupported API surface is explicitly removed or rejected.
- **Scope and constraints:** Preserve world-tile interleaving and the existing independent UI-page renderer. Add a GPU-free assembly test and an offscreen pixel-level check if the APIs remain supported.
- **Remaining uncertainty:** No shipped script currently exercises the missing paths, and graphical/offscreen rendering was not run.

---

## Settings state and persistence

### [#2194] HPA-12. Settings Back cannot restore most applied-but-unsaved video values

**Severity:** High

The Settings Apply path writes most video values into the live Haskell
`VideoConfig`, while Back later asks that same live object for what it describes
as the on-disk baseline. After Apply, the durable and applied states are
therefore indistinguishable and Back has no old value to restore. Brightness
and the two tooltip delays avoid this only through dedicated Lua-side saved
snapshots; the other video settings do not have them.

**Evidence:**

- `scripts/settings/data.lua:426-559` — Apply copies pending resolution, window mode, VSync, MSAA, pixel snap, texture filter, UI scale, and frame limit into `data.current` and calls engine setters that update the live engine configuration.
- `src/Engine/Scripting/Lua/API/Config.hs:37-51` — `engine.getVideoConfig` reads `rvVideoConfigRef`, the same live `IORef` those setters mutate; it does not reread `config/video.local.yaml`.
- `scripts/settings/data.lua:588-640` — Back calls that live getter and pushes its returned values back into the engine as though they were the last saved values.
- `scripts/settings_menu.lua:1017-1032` — the caller explicitly describes Back as reverting an applied-but-unsaved change to the on-disk config and expects UI-scale fan-out when that happens.
- `test-headless/Test/Headless/UI/SettingsRevert.hs:20-27,122-204` — the durable regression suite covers the exceptional snapshot-backed fields: tooltip dwell, tooltip hint delay, save-then-revert ordering, and defined brightness.
- `scripts/settings/data.lua:206-210` — autosave Back demonstrates the missing model by rereading its effective durable configuration before notifying its live consumer.

**Handoff context:**

- **Current behavior:** Apply followed by Back leaves unsaved resolution, mode, UI scale, VSync, MSAA, frame limit, pixel snap, and texture filter active for the rest of the process, despite the menu promising to restore disk state.
- **Expected direction:** Settings maintain distinct persisted, applied, and pending values, and Back restores the persisted baseline for every setting family.
- **Scope and constraints:** Preserve live application, Save-as-Apply-then-persist, brightness and tooltip live preview, autosave's separate storage, and responsive UI-scale notifications. Add coverage for every video field rather than only the existing snapshot-backed exceptions.
- **Remaining uncertainty:** The state contradiction is direct. No rendered Settings interaction was run during the static follow-up.

### [#2198] HPA-15. Video configuration validation stops at the intended UI

**Severity:** Medium–High

The Settings widgets constrain the values they generate, but neither the YAML
decoder nor the Haskell Lua API enforces the same domain. A hand-edited local
file, debug-console call, or future alternate caller can store dimensions,
scale, sampling, brightness, and frame-limit values that the ordinary screen
would never offer, immediately before those values reach window, swapchain,
timing, or layout code.

**Evidence:**

- `src/Engine/Graphics/Config.hs:163-203` — `Resolution` and `VideoConfigFile` decode plain `Int` and `Float` fields without domain checks.
- `src/Engine/Graphics/Config.hs:231-253` — a successfully shaped file is copied directly into `VideoConfig`; only syntax/schema failure falls back to defaults.
- `src/Engine/Scripting/Lua/API/Config.hs:53-88` — `setVideoConfig` accepts unrestricted dimensions, UI scale, MSAA, and brightness; an invalid window-mode token silently becomes `Windowed` while an invalid texture filter preserves the old filter.
- `src/Engine/Scripting/Lua/API/Config.hs:124-166,196-220` — the individual UI-scale, resolution, MSAA, and brightness setters likewise lack the Settings screen's ranges.
- `src/Engine/Graphics/Window/GLFW.hs:51-83` — persisted dimensions reach `GLFW.createWindow` directly and an invalid pair can make window creation fail.
- `scripts/settings/data.lua:481-521,648-674` — range checks and clamps live in the intended Lua UI path rather than an authoritative domain boundary.

**Handoff context:**

- **Current behavior:** Out-of-domain but correctly typed configuration can enter authoritative live state, with inconsistent rejection/default/preserve-old behavior across fields.
- **Expected direction:** One authoritative video-config validation boundary is shared by YAML decoding, Lua mutation, persistence, and side-effect application; UI validation remains a feedback layer rather than the correctness layer.
- **Scope and constraints:** Preserve backward-compatible optional YAML fields and the documented default fallback for malformed files. Define policy for unsupported resolutions, legal MSAA samples, finite/ranged scales, brightness, and frame limits before choosing clamping versus rejection.
- **Remaining uncertainty:** No deliberately malformed local configuration was launched; the unvalidated data flow is direct.

### [#2202] HPA-19. Local configuration persistence has no common durable-write contract

**Severity:** Medium–Low

Video, keybinding, notification, and autosave settings use separate persistence
paths with different error and live-state semantics. The ordinary writers call
`Yaml.encodeFile` directly rather than a shared same-directory temporary-write,
flush, validate, and atomic-replace protocol. Some APIs return no outcome or an
unconditional success, while autosave exposes an explicit failure result.

**Evidence:**

- `src/Engine/Graphics/Config.hs:255-275` — video settings encode directly and return `IO ()`.
- `src/Engine/Input/Bindings.hs:115-120` and `src/Engine/Scripting/Lua/API/Keybinds.hs:215-224` — keybindings encode directly and the Lua API returns `true` after the call rather than owning a recoverable result contract.
- `src/Engine/Asset/YamlNotifications.hs:123-138` — notification overrides use the same direct write.
- `src/Engine/Scripting/Lua/API/PlayerEvent.hs:318-325` — notification state is updated live before persistence, and the source explicitly says write errors do not roll it back.
- `src/Engine/Save/Config.hs:205-262` — autosave is more explicit: it clamps write-side values, catches filesystem failure, logs it, and returns `Either Text ()`, but still has its own direct encode path.

**Handoff context:**

- **Current behavior:** A local-settings write has family-specific failure behavior and no repository-level guarantee against a crash or interruption leaving a partial file. Live and durable state can diverge differently depending on the setting.
- **Expected direction:** Local configuration uses one durable replacement mechanism and one explicit result vocabulary, with each setting family declaring whether a failed persistence attempt rolls back, remains live-only, or prompts a retry.
- **Scope and constraints:** Keep sparse autosave overrides, tracked-default inheritance, gitignored local paths, and notification live-preview semantics. Do not conflate local configuration with the heavier world-save format.
- **Remaining uncertainty:** No filesystem failure or interrupted write was induced. The absence of an explicit atomic-replace layer and the inconsistent API contracts are direct; actual filesystem failure frequency is unknown.

---

## Startup readiness and acquisition

### [#2203] HPA-13. Startup completion measures dispatch instead of successful readiness

**Severity:** High

The startup loader counts one unit when a loader function has been invoked,
regardless of whether it parsed any definitions, merely queued asynchronous GPU
work, or later produced a terminal asset failure. Missing directories add no
required work at all. The loading screen nevertheless treats the exhausted
function list as complete and enters the menu, so its percentage is a dispatch
metric rather than a readiness metric.

**Evidence:**

- `scripts/startup_loader.lua:19-29,61-91` — YAML and texture work items wrap loader calls but retain neither their return values nor handles/outcomes.
- `scripts/startup_loader.lua:188-224` — normal boot queues core material, item, unit, building, and related content with no required/optional classification.
- `scripts/startup_loader.lua:269-298` — Tick invokes `item.fn()`, increments `processed` unconditionally, and marks the loader done solely when the function list is exhausted.
- `src/Engine/Asset/YamlList.hs:25-45` — malformed keyed YAML warns and returns an empty list rather than raising, so the startup item still counts as processed.
- `src/Engine/Scripting/Lua/API/YamlTextures.hs:57-130` — a content loader returns the number of texture requests queued, including zero on an empty/failed definition list, but startup discards that result.
- `src/Engine/Scripting/Lua/API/Graphics.hs:41-58` — texture loading publishes `AssetLoading` and enqueues later work; invocation is not readiness.
- `scripts/loading_screen.lua:405-435` and `scripts/ui_manager_boot.lua:292-298` — exhausting the dispatch list reports "Startup loader complete" and initializes the main menu.

**Handoff context:**

- **Current behavior:** Boot can display 100% and continue with missing or empty core registries, while asynchronous asset outcomes are still pending or have failed outside the loader's accounting.
- **Expected direction:** Startup has an aggregate terminal state over declared required and optional dependencies, and progress represents completed work rather than admitted work.
- **Scope and constraints:** Preserve incremental per-frame dispatch and existing texture callbacks. Required YAML/content absence needs a fail-fast or explicit degraded-mode decision; optional visuals may continue through fallback behavior without blocking all startup.
- **Remaining uncertainty:** No shipped file was damaged or removed during the static follow-up. The false-success control flow is direct; which individual assets should be required remains a product decision.

### [no-issue] HPA-18. Worker startup has no general partial-acquisition rollback

> **Disposition:** No issue — the window is real but empty: the five non-refusable workers' pre-fork startup is `getPOSIXTime`/`newIORef`/`pure`/a capability read (`src/World/Thread.hs:48`, `src/Unit/Thread.hs:45-50`, `src/Sim/Thread.hs:52`, `src/Combat/Thread.hs:56`, `src/Engine/Input/Thread.hs:54`), so only resource exhaustion can throw after an earlier fork; the one worker with real startup failure modes (Lua) already unwinds the exact partial set (#1190, `App.Boot.luaThreadOrAbort`) and treats a failed `init.lua` as an `Either`; no diagnostic is lost because `startWorkerThreadEither` logs before rethrowing and the `LogToHandle` backend flushes every line synchronously (`src/Engine/Core/Log/Format.hs:31`); and nothing acquired by then outlives process exit (workers start before any window, device, or save). #1147 requirement 7 deliberately kept cross-worker rollback out of the boot modules; a future fallible startup takes Lua's refusal path. Verified on master `5500e9771`.

**Severity:** Medium–Low

Each boot mode starts workers sequentially but constructs its `EngineWorkers`
cleanup authority only after the final start succeeds. Apart from the Lua
listener's bespoke refusal path, a later worker-start exception escapes before
the shared boot-result cleanup tail exists, relying on process termination to
reclaim earlier workers and potentially losing orderly cleanup and buffered
diagnostics.

**Evidence:**

- `app/App/Graphical.hs:41-60` — six workers are acquired sequentially before the complete cleanup record exists.
- `app/App/Headless.hs:38-53` — the headless boot has the same shape for its five workers.
- `src/Engine/Core/Thread.hs:119-136` — startup exceptions are deliberately rethrown outside `runEngineM` and its `Either EngineException` boot channel.
- `app/App/Boot.hs:86-105` — normal error cleanup requires a fully constructed `EngineWorkers` value.
- `app/App/Boot.hs:107-152` — Lua listener refusal has a special partial-worker list and cleanup tail precisely because no half-constructed worker record is available.

**Handoff context:**

- **Current behavior:** If world, unit, simulation, or combat startup throws after earlier workers were forked, the process exits without the ordinary worker stop/join and logger-shutdown sequence.
- **Expected direction:** Worker acquisition owns a cleanup stack from the first successful start and unwinds acquired workers in dependency-safe order on every later refusal or exception.
- **Scope and constraints:** Preserve the established worker order, boot-mode-specific listener policy, typed startup failures, and process-fatal outcome where recovery is not supported. The goal is deterministic teardown, not necessarily continuing after failure.
- **Remaining uncertainty:** Because the executable terminates and the OS reclaims process resources, the practical impact is diagnostic loss and architectural fragility rather than a proven persistent leak. No startup failure was induced.

---

## Runtime timing and scripting

### [#2204] HPA-14. Runtime intervals use discontinuous wall time without a valid first sample

**Severity:** High

The render loop, world thread, unit thread, and Lua scheduler all derive elapsed
time or deadlines from POSIX/UTC wall time. The render timing record additionally
starts at zero, so its first update stores roughly epoch-seconds as `deltaTime`;
camera integration consumes that value on the next loop. None of these interval
consumers defines a clamp or a host-suspend/clock-correction policy.

**Evidence:**

- `src/Engine/Core/Defaults.hs:74-81` — `lastFrameTime` starts at `0.0` rather than an initial clock sample.
- `src/Engine/Loop/Timing.hs:47-90` — frame pacing and `actualDt` subtract `lastFrameTime` from `getPOSIXTime` with no first-sample branch or delta bound.
- `src/Engine/Loop.hs:41-55,120-126` — camera integration runs before the end-of-tick timing update, so the first enormous stored delta is used on the following loop.
- `src/Engine/Loop/Camera.hs:149-190,285-305` — pan position and zoom integrate directly with `deltaTime`.
- `src/World/Thread.hs:39-67` and `src/Unit/Thread.hs:36-74` — simulation deltas also subtract POSIX wall-clock readings, and unit game time and movement accept the result directly.
- `src/World/Time/Types.hs:57-70` — negative elapsed time can wrap time-of-day backward while the calendar date changes only when `daysRolled > 0`.
- `src/Engine/Scripting/Lua/Util.hs:56-61` and `src/Engine/Scripting/Lua/TickPolicy.hs:160-176` — Lua deadlines use UTC/POSIX seconds and compare them directly to stored next-tick values.

**Handoff context:**

- **Current behavior:** A held camera input near startup can integrate against an epoch-sized delta. Host sleep or a forward/backward wall-clock adjustment can create very large or negative world/unit deltas, inconsistent calendar movement, delayed Lua ticks, or a catch-up burst.
- **Expected direction:** Elapsed-time consumers use a monotonic source, initialize their first sample explicitly, reject non-finite/negative deltas, and apply a documented maximum-step and catch-up policy.
- **Scope and constraints:** Preserve real UTC timestamps where they are actually displayed or persisted. Decide separately whether simulation should pause across host sleep, skip elapsed time, or catch up in bounded steps.
- **Remaining uncertainty:** No system-clock discontinuity or held-input startup was executed. The zero-baseline and wall-clock data flows are direct.

### [#2205] HPA-16. A Lua update that changes its interval is scheduled twice

**Severity:** Medium

`engine.setTickInterval` resets a script's deadline to current time plus the new
rate. When the call occurs inside that script's own `update`, the scheduler
returns from the callback and unconditionally advances the current map entry
again, adding the newly stored rate a second time. The next callback therefore
arrives roughly two new intervals later.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Core.hs:364-383` — an accepted interval writes both `scriptTickRate = rate` and `scriptNextTick = currentSecs + rate` into the live scripts map.
- `src/Engine/Scripting/Lua/Thread.hs:395-403` — `runDueScripts` snapshots due scripts, invokes `update`, then applies `advanceTick` to the map entry currently stored under that id.
- `src/Engine/Scripting/Lua/TickPolicy.hs:166-170` — `advanceTick` adds the entry's current `scriptTickRate`, so it observes and adds the rate written by the callback.
- `test-headless/Test/Headless/Lua/TickInterval.hs:415-433` — existing scheduling coverage verifies ordinary advancement when the callback does not mutate its schedule; it does not exercise reentrant interval change.
- `scripts/` — repository-wide search found no current shipped call to `engine.setTickInterval`, so the defect is dormant in today's scripts but remains part of the public API.

**Handoff context:**

- **Current behavior:** A positive dynamic interval change from inside `update` waits two intervals. Changing to event-only zero happens to avoid the extra delay because adding zero is inert.
- **Expected direction:** The scheduler has an explicit reentrancy rule and never overwrites or double-applies a scheduling decision made by callback code.
- **Scope and constraints:** Preserve tick-rate validation, event-only semantics, pause/resume, kill-during-callback behavior, and the accepted interval passed as `dt`. Add direct coverage for positive, zero, pause, and kill mutations from within `update`.
- **Remaining uncertainty:** None in the control flow; no shipped script currently reaches it.

### [no-issue] HPA-17. Lua callback isolation cannot contain nontermination or chronic failure

> **Disposition:** No issue — scripts are trusted shipped code (as the finding states), so a nonterminating callback is a development bug with an immediate symptom and a chronically throwing one already logs its `file:line` every interval, which is the fail-loud signal wanted; a quarantine would hide it and would pause modules that legitimately throw until a precondition arrives. A wall-clock budget conflicts with the console's blocking built-ins (`world.waitForInit` polls with `threadDelay` for up to 300 s inside the Lua call, `src/Engine/Scripting/Lua/API/World/Lifecycle.hs:795-815`), and an instruction budget is new infrastructure — hslua-core 2.3.2 exposes no `sethook`, so it needs a C count hook in `cbits/lua_debug.c`, a budget value, and a console exemption — i.e. an epic-sized design decision, not an issue. Per-module error isolation and transaction-visible save/load callback failures (#1204) already hold. Verified on master `5500e9771`.

**Severity:** Medium

Module callbacks are protected with `pcall`, which isolates raised Lua errors
from the worker thread, but all callbacks still execute synchronously and
serially on the one Lua state. A callback that never returns or simply takes too
long monopolizes the Lua owner; a callback that raises every interval remains
scheduled and logs again forever. There is no instruction budget, cooperative
yield contract, repeated-error backoff, or quarantine state.

**Evidence:**

- `src/Engine/Scripting/Lua/Script.hs:45-90` — callbacks run synchronously under `Lua.pcall`; the wrapper handles only a returned success or error status.
- `src/Engine/Scripting/Lua/Util.hs:80-89` — broadcasts call modules serially and can reach the next module only after the current callback returns.
- `src/Engine/Scripting/Lua/Thread.hs:273-381` — queued messages, debug commands, and timed scripts all share the single Lua-thread tick.
- `src/Engine/Scripting/Lua/Thread.hs:395-403` — a throwing timed callback remains present and is rescheduled after every invocation.
- `src/Engine/Scripting/Lua/Thread/Console.hs:141-164` — debug-console Lua execution is likewise a synchronous `pcall` on the same state.

**Handoff context:**

- **Current behavior:** One infinite or pathologically slow shipped callback can halt all Lua UI, AI, debug commands, asset callbacks, and save/load cooperation. A persistently throwing callback can produce recurring log noise indefinitely.
- **Expected direction:** Script execution has a bounded cooperative or instruction-count budget, and repeated callback failures have an observable backoff/quarantine policy that preserves debuggability.
- **Scope and constraints:** Scripts are trusted shipped game code rather than an adversarial sandbox. Preserve traceback-rich diagnostics, independent error isolation between modules, and the save/load callbacks whose failure must remain transaction-visible.
- **Remaining uncertainty:** No nonterminating script was executed. The absence of a containment boundary is direct; an acceptable per-callback budget is a product/performance decision.

---

## Content identity and validation

### [#2236] HPA-20. Persisted flora species identity is an OS-order-dependent ordinal

**Severity:** High

Flora definitions receive sequential numeric `FloraId` values in filesystem
enumeration order. That order is explicitly OS-dependent, yet the numeric value
is persisted in world edits, crop plots, and plant designations and also salts
procedural placement. Load validation checks only whether today's catalog has an
entry at that number. If definitions are reordered while the catalog retains
the same number of entries, the load passes and silently interprets an old
species as a different one.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Core.hs:551-577` — `engine.listFiles` returns raw
  `listDirectory` order and explicitly documents that it is OS-dependent.
- `scripts/startup_loader.lua:23-29,61-69,188-198` — flat flora loading preserves
  that order; unlike recursive item loading, it deliberately applies no
  canonical sort.
- `src/Engine/Scripting/Lua/API/YamlTextures.hs:230-275` — flora registration
  obtains the next sequential `FloraId` as each definition is encountered.
- `src/World/Flora/Placement.hs:95-119` — species enumeration order contributes
  to generated placement identity.
- `src/World/Save/Component/Page.hs:347-390,489-512` — flora placement edits,
  crop plots, and plant designations persist `FloraId`.
- `src/World/Save/Types.hs:1128-1174` — compatibility validation asks only
  whether the numeric id currently resolves; it cannot detect a semantic
  remapping at the same ordinal.
- `docs/persistence_state_inventory.md:553` — the catalog is rebuilt at boot,
  while world state persists its numeric ids without persisting the catalog
  that gave those numbers meaning.

**Handoff context:**

- **Current behavior:** Host enumeration differences or adding, removing, or
  renaming a definition can change generated flora and reinterpret existing
  crops and designations without a load error.
- **Expected direction:** Persist an authored stable species key or a versioned
  content-manifest identity. Runtime ordinals may remain an optimization but
  must be resolved from the stable key rather than serving as durable identity.
- **Scope and constraints:** Preserve deterministic world generation and define
  an explicit migration for existing numeric references. Sorting filenames
  alone improves host repeatability but still makes inserting a definition a
  save-compatibility event.
- **Remaining uncertainty:** Issue #1854 is adjacent because it introduces
  stable `FloraInstanceId` values, but its current scope does not explicitly
  migrate every persisted species reference, particularly groundcover crop
  plots and outstanding plant designations.

### [#2243] HPA-21. Plant-designation species bypass the flora load gate

**Severity:** Medium–High

The flora compatibility gate claims to enumerate saved species references but
omits `wpsPlantDesignations`. Those designations persist a `FloraId` and are
restored verbatim. An unresolved id therefore passes load, becomes a visible and
claimable job with an empty crop name/category, and can later be removed by the
farm AI after an unsuccessful planting attempt.

**Evidence:**

- `src/World/Plant/Types.hs:29-44` — every `PlantDesignation` owns a selected
  `FloraId`.
- `src/World/Save/Component/Page.hs:489-499` — the id is preserved in
  `PlantDesignationDTO`.
- `src/World/Save/Types.hs:1152-1174` — `missingFloraReferences` scans only
  `WePlaceFlora` edits and crop plots despite describing every saved flora
  reference.
- `src/Engine/Scripting/Lua/API/Save.hs:770-815` — this function is the load
  path's flora-content gate.
- `src/World/Load/Stage.hs:245-259` — plant designations are restored unchanged.
- `src/Engine/Scripting/Lua/API/Plant.hs:80-123` — an unresolved designation
  remains present but is exposed with empty `crop` and `category` fields.
- `scripts/unit_ai_farm.lua:274-337,379-411` — the designation remains eligible
  for scanning and claiming; completion dispatches planting and then cancels
  the designation without observing authoritative success.

**Handoff context:**

- **Current behavior:** A malformed or no-longer-resolvable plant job survives
  compatibility validation, then degrades into blank content and can silently
  lose the player's intent.
- **Expected direction:** Load validation enumerates every schema field that
  carries a flora reference, including `wpsPlantDesignations`.
- **Scope and constraints:** Prefer a single exhaustive reference traversal over
  another hand-maintained partial list. Coordinate with any stable-species-id
  migration from HPA-20.
- **Remaining uncertainty:** No save was deliberately corrupted; the omission
  and subsequent blank-value behavior are direct in the call chain.

---

## Transaction integrity and durability

### [#2221] HPA-22. Save-barrier acknowledgements do not establish a closed quiescence boundary

**Severity:** High

An owner's acknowledgement says that its current tick finished, but it does not
park that owner. The final acknowledgement leaves the barrier in
`SaveWaitingOwners`; the transaction driver later wakes and performs a separate
`reachSnapshot` operation. In that interval, an acknowledged owner can begin
another tick while `captureLocked` still reports false, then resume its already
authorized work after snapshot or load publication has begun.

**Evidence:**

- `src/Engine/Save/Barrier.hs:78-101` — the final acknowledgement records the
  complete set and `SaveWaitingOwners`; it does not enter
  `SaveSnapshotBoundary`.
- `src/Engine/Save/Barrier.hs:103-123` — `waitForOwners` observes the set, while
  `reachSnapshot` is a later independent STM transaction.
- `src/Engine/Save/Barrier.hs:164-170` — workers are gated only while the phase
  is exactly `SaveSnapshotBoundary`.
- `src/Engine/Scripting/Lua/API/Save.hs:339-372` — the Lua initiator waits and
  then calls `reachSnapshot` separately.
- `src/Engine/Loop/Mode.hs:196-216` — the renderer's contract explicitly
  acknowledges that owner participation establishes a wait, not mutual
  exclusion, and that a new unlocked tick can start between acknowledgement
  and the boundary.
- `src/Engine/Input/Thread.hs:89-97`,
  `src/Unit/Thread.hs:69-104`, and
  `src/Sim/Thread.hs:84-135` — owners check the point-in-time lock, do work,
  acknowledge, and continue their loops without waiting for release.

**Handoff context:**

- **Current behavior:** Save capture can observe post-acknowledgement mutations.
  During load, a pre-publication input or engine message can pass its initial
  gate and complete against the replacement session after the one-time stale
  queue flush.
- **Expected direction:** The final required acknowledgement atomically closes
  the boundary, and acknowledged owners remain parked until capture is
  released. This is the standard safepoint shape: acknowledge-and-park, not
  acknowledge-and-immediately-reenter.
- **Scope and constraints:** Preserve multi-pass causal draining, owner
  diagnostics, timeout behavior, and early release during encoding. Extra drain
  passes alone do not close the post-final-acknowledgement window.
- **Remaining uncertainty:** No scheduler interleaving was forced. The protocol
  explicitly documents and permits the unsafe interval.

### [#2227] HPA-23. Unknown-component preservation fails open when a generation cannot be read

**Severity:** High

Before publishing, the storage layer is supposed to refuse overwriting an
existing generation that contains an unknown optional component. Its scanner
catches any `BS.readFile` exception and treats it as an empty component list.
The later topology classifier correctly calls the same unreadable generation
corrupt, after which publication can replace it or stage and remove it. A
temporary permission or I/O failure can therefore destroy the data that this
guard exists to preserve.

**Evidence:**

- `src/World/Save/Storage.hs:410-450` — the guard promises to inspect both the
  authoritative and previous generations before overwriting either.
- `src/World/Save/Storage.hs:453-459` — any `IOException` from reading a present
  generation becomes `[]`, indistinguishable from “contains no foreign
  component.”
- `src/World/Save/Storage.hs:602-639` — publication may replace the authoritative
  generation or stage and later clean up an old previous generation.
- `src/World/Save/Storage.hs:641-696` — unreadable authoritative data is treated
  as recovery topology and may be replaced.
- `src/World/Save/Storage.hs:896-925` — the ordinary decoder classifies the same
  read failure as `GenerationCorrupt`, confirming that it is not equivalent to
  absence.
- On POSIX filesystems, permission to rename a directory entry is independent
  of permission to read the file, so a read refusal does not imply that the
  later replacement will also fail.

**Handoff context:**

- **Current behavior:** A present but unreadable generation passes the
  foreign-data guard and may be permanently removed by the ensuing successful
  publication.
- **Expected direction:** Preservation checks fail closed whenever a present
  generation cannot be inspected, with the path and underlying read error
  reported before creating a candidate.
- **Scope and constraints:** Preserve the distinction between storage corruption
  and content incompatibility, and retain both existing generations on a
  preflight failure.
- **Remaining uncertainty:** A permission failure was not induced. The
  fail-open branch and destructive publication topology are direct.

### [#2229] HPA-24. Slot creation and autosave rotation omit parent-directory durability

**Severity:** Medium

Save publication flushes files and directory entries inside a slot directory,
but it does not flush the parent directory that owns the slot itself. A
first-ever `saves/<slot>` creation can therefore report success without making
that new directory entry durable. Autosave rotation renames and removes entire
slot directories under `saves/` without synchronizing `saves/` at all.

**Evidence:**

- `src/World/Save/Serialize.hs:106-115` — a manual save publishes into
  `saves/<slot>`.
- `src/World/Save/Storage.hs:364-398` — publication creates that directory with
  `createDirectoryIfMissing True dir`.
- `src/World/Save/Storage.hs:602-639,732-735` — every directory sync targets
  only `dir`, the slot itself; no publication path synchronizes
  `takeDirectory dir`.
- `src/World/Save/Autosave.hs:400-433` — autosave rotation renames and removes
  directories whose owning directory is `saves/`, with no directory-sync
  operation.
- `src/World/Save/Storage.hs:69-86` — the module claims Linux crash and
  power-loss durability once its sync boundary returns.

**Handoff context:**

- **Current behavior:** A newly reported successful save can disappear after
  power loss, and a reported successful autosave rotation can recover with an
  older or mixed directory-name layout.
- **Expected direction:** Synchronize the owning parent after creating a slot
  and after autosave directory rename/removal transactions, propagating failure
  before reporting durability.
- **Scope and constraints:** This is separate from the documented macOS
  `fsync` versus `F_FULLFSYNC` trade-off; it concerns which directory is being
  synchronized at all.
- **Remaining uncertainty:** Power-loss behavior was not simulated. The missing
  parent synchronization is direct.

---

## World lifecycle and simulation

### [#2232] HPA-25. A fresh world's center chunk is never admitted to fluid simulation

**Severity:** Medium–High

Normal world creation generates and inserts `(0,0)` synchronously, then queues
only the surrounding chunks. Simulation admission occurs only for chunks
generated by those later queue consumers. Because every loader recognizes the
center as already resident, no later path sends its missing
`SimChunkLoaded`.

**Evidence:**

- `src/World/Thread/Command/Init.hs:344-381` — normal initialization generates
  and inserts the center chunk synchronously.
- `src/World/Chunk/Queue.hs:93-118` — `initialChunkQueue` explicitly excludes
  that center.
- `src/World/Thread/Command/Init.hs:383-393` — initialization dispatches location
  stamps and queues only `remainingCoords`; it sends no simulation seed.
- `src/World/Thread/ChunkLoading.hs:278-346` — the init-queue consumer emits
  `SimChunkLoaded` only for newly generated chunks and skips already resident
  coordinates.
- `src/World/Thread/ChunkLoading.hs:85-109,145-159` — camera loading has the same
  already-resident behavior.
- `src/Sim/Thread.hs:147-155` — activating a world changes only its active flag;
  it does not discover or seed missing resident chunks.
- Whole-session load publication has an explicit deferred simulation-seed list,
  confirming that simulation membership is not inferred automatically from
  `wsTilesRef`.

**Handoff context:**

- **Current behavior:** Rivers, lakes, ocean, or other fluid in the initial
  center chunk do not tick or reconcile across their chunk boundary. If the
  center later evicts and reloads, it suddenly joins simulation, making behavior
  depend on cache history.
- **Expected direction:** Every resident chunk enters simulation exactly once,
  including synchronously generated bootstrap chunks, before initial settling
  can be considered complete.
- **Scope and constraints:** Preserve FIFO ordering ahead of fast-settle
  commands and avoid duplicate seeds during load or camera admission.
- **Remaining uncertainty:** No live center-fluid world was observed; the sole
  producer inventory confirms the missing message.

### [deferred] HPA-26. Reusing a world-page name can adopt the previous incarnation's state

> **Deferred:** Owner chose the epic path (queue-ordered per-page unit and building teardown on single-page destroy and same-id re-init, and fencing in-flight fluid writebacks with the page's existing `ChunkGeneration` epoch; design authority `docs/page_incarnation_design.md`) — precondition: that design document's EPIC entry is processed through `/process-design-doc` and its epic number exists to link here.

**Severity:** High

`WorldPageId` acts as both a logical name and the lifetime identity of a page.
Single-page destruction removes the `WorldState` and requests simulation
teardown but leaves page-owned units and buildings in global managers.
Reinitialization deliberately replaces a page under the same id and similarly
has no complete incarnation fence. Old entities then match the replacement by
name, while old or in-flight simulation writebacks also carry only that reusable
name.

**Evidence:**

- `src/World/Thread/Command/Basic.hs:43-79` — single-page destroy removes the
  page and queues `SimDropWorld` but does not remove its units or buildings.
- `src/World/Thread/Command/Basic.hs:81-117` — only `WorldDestroyAll` sends
  `UnitClearAll` and `BuildingClearAll`, demonstrating the missing ownership
  step on the single-page path.
- `src/World/Thread/Command/Init.hs:108-133,452-469` — normal and arena
  initialization explicitly replace an existing `WorldState` under the same id.
- `src/Unit/Types/Instance.hs:22-33` and `src/Building/Types.hs:172-179` —
  entity ownership stores only `WorldPageId`, with no page-incarnation token.
- `src/Unit/Types/Manager.hs:57-68` and
  `src/Unit/Thread/Movement/PathAdvance.hs:69-85` — queries and movement resolve
  those retained entities against whatever page currently owns that id.
- `src/Sim/State/Types.hs:20-39` — simulation state is likewise keyed only by
  `WorldPageId`.
- `src/World/Thread/Command.hs:263-307` — fluid writebacks identify a page by id;
  a replacement's absent edit generation reads as zero, allowing an old
  generation-zero batch to pass.
- `scripts/world_manager.lua:206-225`,
  `scripts/create_world/generation.lua:67-70`, and
  `scripts/movement_arena.lua:94-101` — destroy/recreate is a shipped lifecycle,
  not merely a testing-only theoretical API.

**Handoff context:**

- **Current behavior:** Old units and buildings can disappear while the page is
  absent and reappear in unrelated replacement terrain, carrying inventories,
  jobs, power relationships, and other dependent state. In-flight simulation
  can also target the replacement.
- **Expected direction:** Give each page incarnation an epoch/token carried by
  asynchronous commands and owned entities. Destruction performs page-scoped,
  ordered teardown, and name reuse cannot resolve an older incarnation.
- **Scope and constraints:** Coordinate with `CRS-1`'s proposed page/generation
  epoch, but include unit, building, simulation, transfer, power, knowledge,
  selection, and queued-command ownership rather than limiting the fix to chunk
  residency.
- **Remaining uncertainty:** No destroy/recreate session was run. The retained
  manager rows and reusable-id resolution are direct; exact dependent-state
  cleanup needs an ownership inventory.

### [#2278] HPA-27. Normal world initialization discards runtime material registrations

**Severity:** Medium

The public material-loading API overlays definitions into the process-global
material registry. Creating any normal world later rebuilds only the shipped
`data/materials` registry and replaces the global value wholesale. The
save/load path already identifies this as unsafe and merges live registrations
over its rebuilt base, but fresh initialization does not use that policy.

**Evidence:**

- `src/Engine/Scripting/Lua/API/YamlTextures.hs:57-122` —
  `engine.loadMaterialYaml` loads a caller-selected file and overlays its
  physical definitions into the live registry.
- `src/World/Thread/Command/Init.hs:135-148` — every normal `world.init` loads
  only `data/materials` and overwrites `wsMaterialRegistryRef`.
- `src/World/Material.hs:267-283` — `mergeMaterialRegistry` exists specifically
  to preserve runtime/custom definitions, with live registrations winning
  collisions.
- `src/Engine/Scripting/Lua/API/Save.hs:751-769` — load validation and
  publication correctly use that merge because replacing the registry would
  reject or discard valid custom materials.
- The registry is global while multiple pages may remain live, so one page's
  initialization can change material interpretation for every other page.

**Handoff context:**

- **Current behavior:** A custom material's texture registrations can survive
  while its name, physical properties, and known-id membership disappear.
  Existing pages then fall back to default physics, and later persistence can
  reject the now-unknown id.
- **Expected direction:** Treat boot content catalogs as immutable/versioned
  snapshots, or merge page initialization against the live catalog using the
  same documented precedence as load.
- **Scope and constraints:** Preserve deterministic shipped-material loading and
  explicit collision policy. Avoid letting one page's creation reinterpret
  another live page.
- **Remaining uncertainty:** No shipped normal flow currently loads an
  out-of-tree custom material before world creation; the public runtime API and
  multi-page lifecycle make the contract reachable.

### [#2280] HPA-28. A non-finite time scale crashes the world worker

**Severity:** High

`world.setTimeScale` accepts every Lua number, including infinity and NaN, and
queues its conversion to `Float` without validation. The world clock later calls
`floor` on calculations derived from that value. `floor` is partial for
non-finite input, so the exception escapes the tick and triggers world-worker
crash handling.

**Evidence:**

- `src/Engine/Scripting/Lua/API/World/Clock.hs:190-214` — any
  `Lua.Number` is accepted and queued as `WorldSetTimeScale`.
- `src/World/Thread/Command/Time.hs:55-89` — the command stores the supplied
  value verbatim, including in pause/resume state.
- `src/World/Thread/Time.hs:39-52` — the stored scale feeds the clock on the next
  unpaused tick.
- `src/World/Time/Types.hs:57-65` — `advanceWorldClock` applies `floor` to the
  derived day and minute values.
- `src/World/Thread.hs:35-56` — an exception from that tick terminates the world
  worker and initiates engine cleanup.

**Handoff context:**

- **Current behavior:** A public gameplay/debug call such as
  `world.setTimeScale(page, math.huge)` kills the active world worker. If issued
  while paused, it arms the failure for resume.
- **Expected direction:** Reject non-finite values at the Lua boundary and
  defensively validate inside the authoritative clock primitive. This follows
  the same finite-domain rule already applied to other public numeric systems.
- **Scope and constraints:** Define whether finite negative scales are supported;
  today's documented vocabulary describes zero as paused and positive values as
  forward speed.
- **Remaining uncertainty:** No malformed call was executed. The partial
  arithmetic path is direct.

---

## Debugging and fail-stop semantics

### [#2282] HPA-29. A timed-out debug command can still execute later

**Severity:** Medium

The TCP debug server reports failure after waiting thirty seconds for a command,
but it neither removes the queued command nor marks it cancelled. The Lua thread
later executes every queued command. A command delayed behind a long-running
operation can therefore mutate state after its client has been told it timed
out, and an ordinary retry can apply the mutation twice.

**Evidence:**

- `src/Engine/Scripting/Lua/DebugServer.hs:247-260` — a command is queued with an
  `MVar`; timeout only substitutes an error string.
- `src/Engine/Scripting/Lua/Thread/Console.hs:24-35` — the Lua thread recursively
  drains and executes every queued command, with no deadline or cancellation
  check.
- The response `MVar` remains valid after the client stops observing it, so late
  completion does not prevent execution or surface its outcome.
- A command queued behind another callback can exceed the timeout without any
  worker crash or external fault.

**Handoff context:**

- **Current behavior:** “Command timed out” means neither “cancelled” nor
  “failed”; the command can still run and mutate the session.
- **Expected direction:** Give commands identities and cancellation/deadline
  state. Cancel atomically when execution has not started; if it has started,
  report “execution outcome unknown” rather than implying failure.
- **Scope and constraints:** Preserve long-blocking built-ins that deliberately
  execute on client threads and distinguish cancellation-before-claim from an
  already-running Lua callback.
- **Remaining uncertainty:** No client retry was issued. The queue semantics are
  direct.

### [#2283] HPA-30. Fallible crash logging can suppress the engine's fail-stop transition

**Severity:** Medium

Every critical worker's crash callback logs the exception before setting the
engine lifecycle to `CleaningUp`. Logger output performs unguarded handle or
callback I/O. If logging itself throws—for example because the output handle is
closed—the callback exits before the lifecycle write, while the worker's
`finally` still marks only that worker as finished. The main loop can therefore
continue in `EngineRunning` with a dead critical subsystem.

**Evidence:**

- `src/Engine/Core/Thread.hs:168-193` — the shared loop invokes `wsOnCrash`
  inside its exception handler; an exception from that callback escapes and the
  worker exits.
- `src/Engine/Input/Thread.hs:67-70`,
  `src/World/Thread.hs:53-56`, and the unit, simulation, combat, and Lua worker
  specifications all log before writing `CleaningUp`.
- `src/Engine/Core/Thread.hs:148-150` — the fork finalizer fills only the worker
  completion signal; it does not enforce global lifecycle failure.
- `src/Engine/Core/Log.hs:144-160` and
  `src/Engine/Core/Log/Format.hs:29-32` — enabled logging calls an unguarded
  handle write/flush or arbitrary backend callback.
- `src/Engine/Loop/Mode.hs:249-263` — the main loop continues recursively while
  the lifecycle still reads `EngineRunning`.

**Handoff context:**

- **Current behavior:** A logger failure during crash reporting can leave a
  partially alive session rather than enforcing the critical worker's
  fail-stop policy. Lua can additionally skip queue draining and state closing.
- **Expected direction:** Signal `CleaningUp` before any reporting, then perform
  logging and cleanup as independent best-effort actions so no one failure
  suppresses the mandatory state transition.
- **Scope and constraints:** Preserve rich crash diagnostics and worker
  completion signaling. Startup failure reporting needs the same ordering
  review but is not required to share the runtime callback.
- **Remaining uncertainty:** A failing log handle was not injected. The ordering
  vulnerability is direct.

---

## Diagnostic retention

### [#2284] HPA-31. The action-outcome “ring” is unbounded and survives ordinary session exit

**Severity:** Medium

The playtest action-outcome buffer is described as a ring but is implemented as
an unbounded `Seq`. Keyboard, character, mouse, scroll, and gameplay-action
producers append detailed records continuously. Its only consumer is an opt-in
debug drain. Load publication clears it, but ordinary Exit to Menu does not, so
records can accumulate across successive new games in one process.

**Evidence:**

- `src/Engine/ActionOutcome.hs:1-20,60-70` — the buffer is called a ring, but
  `pushActionOutcome` performs an unrestricted `Seq.|>`.
- `src/Engine/Scripting/Lua/API/ActionOutcome.hs:87-105` — the Lua producer
  reimplements the unrestricted append instead of using the shared helper.
- `src/Engine/Scripting/Lua/API/ActionOutcome.hs:155-165` — the only consumer is
  `debug.drainActionOutcomes`, which drains only when explicitly called.
- Input routing and cursor-designation modules produce records for routine
  interaction.
- `src/World/Load/Publish.hs:366` — load replacement resets the buffer.
- `src/World/Thread/Command/Basic.hs:88-118` — Exit to Menu clears worlds and
  entity managers but not this diagnostic state.

**Handoff context:**

- **Current behavior:** Input-heavy play retains every structured outcome and
  increases heap and GC pressure indefinitely, including across ordinary
  Exit-to-Menu → New Game cycles.
- **Expected direction:** Use one bounded append helper for every producer,
  retaining newest-first diagnostic usefulness while dropping the oldest
  entries, and reset it at every session boundary.
- **Scope and constraints:** Size the ring for the playtest critic's polling
  cadence and preserve stable oldest-first drain ordering.
- **Remaining uncertainty:** No long-session heap profile was collected. The
  missing cap and ordinary-session reset are direct.

### [#2285] HPA-32. Popup-enabled events accumulate in a write-only queue

**Severity:** Medium

Every popup-enabled player event is appended to a `TVar (Seq PlayerEvent)` and
also independently delivered through `LuaShowPopup`. The capability and state
documentation explicitly say that nothing reads the stored queue. Unlike the
real event store, it has no cap, and like the action-outcome buffer it is reset
by load publication but not ordinary Exit to Menu.

**Evidence:**

- `src/Engine/PlayerEvent/Emit.hs:171-209` — one event is appended to
  `ecPopupQueueRef` and then separately sent through the actual Lua delivery
  queue.
- `src/Engine/Core/Capability/Events.hs:108-115` and
  `src/Engine/Core/State.hs:516-523` — the queue is explicitly documented as
  write-only today.
- `src/Engine/PlayerEvent/Emit.hs:281-295` — the actual event store uses a
  bounded retention policy, demonstrating the missing policy on the duplicate
  queue.
- `src/World/Load/Publish.hs:372` — load replacement resets the popup queue.
- `src/World/Thread/Command/Basic.hs:88-118` — ordinary session destruction does
  not reset it.

**Handoff context:**

- **Current behavior:** Arbitrary popup text and metadata remain retained after
  delivery throughout long combat/survival play and across successive new-game
  sessions.
- **Expected direction:** Remove the speculative queue until it has a consumer,
  or give it an explicit bounded/cursor-backed retention contract and reset it
  at every session boundary.
- **Scope and constraints:** Preserve immediate `LuaShowPopup` delivery and the
  independently bounded event log. Do not make the popup renderer drain two
  competing authorities.
- **Remaining uncertainty:** No memory profile was run. The write-only ownership
  and unbounded append are explicit.

---

## Configuration and public numeric integrity

### [#2286] HPA-33. A malformed world-generation configuration silently becomes the complete default configuration

**Severity:** Medium

The world-generation configuration loader treats a malformed configuration
exactly like an absent optional file: it discards the decoder error and returns
the complete compiled-in default configuration. A single syntax error or
wrongly typed field can therefore cause every otherwise valid authored setting
in the tracked configuration to be ignored, while startup continues without
identifying the file, parser failure, or fallback.

**Evidence:**

- `src/World/Generate/Config/IO.hs:12-22` — a missing file returns
  `defaultWorldGenConfig`, and a YAML decoder `Left` also returns the same
  default without retaining or logging the diagnostic.
- `src/Engine/Core/Init.hs:241-244` — engine initialization loads this
  configuration as part of the normal startup path.
- `src/World/Generate/Config/Types.hs:174-179` — the configuration module's own
  commentary records that one parse failure silently discards every setting.
- `config/world_gen_default.yaml` is a tracked operational resource rather than
  an ephemeral user preference whose corruption can safely be ignored.
- `src/Unit/Pathing/Config.hs:160-175` — the pathing configuration establishes a
  safer local precedent: malformed content emits the parser diagnostic before
  falling back.
- The focused world-generation configuration tests inspect selected values and
  normalization behavior but do not establish malformed-file failure
  semantics.

**Handoff context:**

- **Current behavior:** An author can change several world-generation settings,
  introduce one malformed value, and receive a valid but entirely default world
  with no indication that the intended configuration was rejected.
- **Expected direction:** Distinguish absence from invalidity. A required tracked
  configuration should normally fail startup; if fallback is deliberately
  retained, it must emit a prominent diagnostic containing the path and decoder
  error.
- **Scope and constraints:** Preserve a documented default for a genuinely
  absent optional override, if that remains supported. Do not partially apply a
  malformed document unless field-by-field recovery becomes an explicit schema
  policy.
- **Remaining uncertainty:** The intended product policy for a missing tracked
  default file is not stated. The loss of the malformed-file diagnostic is
  direct.

### [#2288] HPA-34. Non-finite world-generation parameters can crash asynchronous world initialization

**Severity:** High

The public Lua world-generation configuration API converts arbitrary Lua
numbers to `Float` and stores them without checking finiteness. Configuration
normalization constrains world size and plate count but does not validate the
floating-point generation parameters. At least volcanic activity later reaches
`round`, which is partial for NaN and infinity. Because generation executes on
the world worker after the Lua command has returned, the originating call
cannot contain or accurately report the resulting exception.

**Evidence:**

- `src/Engine/Scripting/Lua/API/World/GenConfig.hs:131-138,152-163` — numeric
  fields accept any `Lua.Number` and narrow it through `realToFrac`.
- `src/Engine/Scripting/Lua/API/World/GenConfig.hs:176-235` — values including
  `volcanic_activity` are installed in the shared generation configuration
  before normalization.
- `src/World/Generate/Config/Normalize.hs:40-44` — normalization adjusts only
  size and plate-count relationships; it supplies no finite-number or domain
  validation for the floating-point fields.
- `src/World/Geology/Timeline/Volcanism.hs:25-50` — volcanic activity feeds
  `round (fromIntegral n * va)`. `round` raises on NaN or infinity.
- `src/World/Thread/Command/Init.hs:151-169` — normal initialization passes the
  configured activity into geological generation and forces the result on the
  world worker.
- `src/World/Thread.hs:39-56` — an uncaught initialization exception terminates
  the worker and transitions the engine toward cleanup.
- `src/Unit/Pathing/Config.hs:84-142` — the pathing loader already demonstrates
  a reusable `finiteOr`-style boundary for rejecting non-finite configuration.
- HPA-28 owns the equivalent time-scale ingress, but not the independent
  world-generation configuration path.

**Handoff context:**

- **Current behavior:** A public call can successfully install a value such as
  `math.huge`; a later `world.init` then fails asynchronously in a partial
  numeric conversion and brings down the world subsystem.
- **Expected direction:** Every floating-point world-generation setting has a
  named finite and domain constraint applied consistently to YAML and Lua
  input. Invalid updates fail without mutating the last valid configuration.
- **Scope and constraints:** Centralize validation so future fields cannot
  bypass it. Retain defensive checks immediately before expensive generation
  where corruption would otherwise reach partial arithmetic.
- **Remaining uncertainty:** Volcanic activity supplies a direct partial path.
  The complete valid domains of the other floating-point knobs require a
  field-by-field design decision.

### [#2290] HPA-35. Unit motion accepts invalid numeric state from Lua and YAML

**Severity:** High

The public unit movement APIs accept arbitrary Lua numbers for teleport
coordinates, movement targets, and movement speed. `unit.setPos` additionally
turns missing or nonnumeric coordinates into zero rather than rejecting an
invalid call. The unit-definition loader independently accepts an unrestricted
`max_speed` and copies it into runtime definitions. The unit worker stores or
derives motion from these values and later applies `floor` to coordinates
derived from them. Most ordinary movement uses the default fall-permitted path,
which lacks the isolated NaN guard present in one fall-prohibited branch.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Units/Spawn.hs:289-315` — `unit.setPos` converts
  numeric arguments directly to `Float`; missing or nonnumeric X/Y arguments
  silently become zero before `UnitTeleport` is queued.
- `src/Engine/Scripting/Lua/API/Units/Spawn.hs:317-399` — `unit.moveTo` and
  `unit.setMoveSpeed` accept arbitrary numeric values without finite or domain
  validation.
- `src/Engine/Asset/YamlUnits.hs:340-350,408-418` — `max_speed` is decoded as
  an unrestricted `Float`, with no positive-finite semantic validation.
- `src/Engine/Scripting/Lua/API/Units/Yaml.hs:261-272` and
  `src/Unit/Thread/Command/Motion.hs:37-53` — the authored value is copied into
  `UnitDef` and participates in runtime gait thresholds without another
  validity boundary.
- `src/Unit/Thread/Command/Lifecycle.hs:58-77` — teleport with an omitted height
  floors the supplied horizontal position while resolving terrain height.
- `src/Unit/Thread/Command/Motion.hs:24-113` — movement targets and speed are
  installed without authoritative validation.
- `src/Unit/Thread/Movement/PathAdvance.hs:181-230,246-261,346-365` — movement
  progression floors current, target, or newly derived coordinates. Only the
  fall-prohibited calculation contains a localized NaN check; ordinary
  fall-permitted movement remains exposed.
- `src/Unit/Thread.hs:36-59` — an escaping exception terminates the critical unit
  worker and initiates global cleanup.
- `src/Engine/Scripting/Lua/API/Register/Unit.hs:22-31` — these are registered
  public unit APIs rather than private trusted primitives.

**Handoff context:**

- **Current behavior:** An invalid public movement call can either teleport a
  unit toward an unintended zero coordinate or insert NaN/infinity into
  authoritative motion state, where a later `floor` can kill the unit worker.
  A malformed `max_speed` can also install nonsensical gait and speed policy for
  every instance of an otherwise successfully registered unit definition.
- **Expected direction:** Required numeric arguments reject missing,
  nonnumeric, non-finite, and out-of-domain values before queue admission.
  Authoritative worker handlers retain a defensive validity check rather than
  trusting every producer forever.
- **Scope and constraints:** Define whether zero or negative movement speed has
  supported semantics and apply the same domain to authored definition values.
  Validate after `Double`-to-`Float` narrowing because a finite `Double` can
  overflow the narrower representation.
- **Remaining uncertainty:** The first partial operation depends on the command
  shape and movement state. No malformed command was executed.

---

## Fresh-session lifecycle

### [#2291] HPA-36. Exit to Menu has no authoritative fresh-session reset transaction

**Severity:** Medium

Save replacement has a centralized publication transaction that resets
session-owned transient state, but Exit to Menu is assembled from Lua teardown,
`WorldDestroyAll`, and an explicit unpause. That path clears world and entity
managers without resetting all process-global state whose own documentation
classifies it as per-session. In particular, the event store survives, and the
process game clock resumes advancing before or between fresh worlds.

**Evidence:**

- `src/Engine/Core/State.hs:487-501` — the event store is explicitly described
  as session-owned state.
- `docs/persistence_contract.md:237` — a loaded session begins with an empty
  event ring rather than inheriting the previous session's rows.
- `src/World/Load/Publish.hs:352-372` — load publication centrally resets event
  rows, popup events, action outcomes, combat/injury transients, input, focus,
  and other session state.
- `src/World/Load/Publish.hs:115` — load publication restores the new session's
  authoritative game time.
- `src/World/Thread/Command/Basic.hs:81-118` — `WorldDestroyAll` clears worlds
  and entity managers but does not perform the equivalent session-state or
  clock transition.
- `scripts/pause_menu.lua:316-356` — Exit to Menu performs Lua teardown, invokes
  destruction, and then explicitly unpauses the process.
- `src/Engine/Core/Init.hs:253` — game time is initialized once at process boot.
- `src/Unit/Thread.hs:61-74` — the process game clock advances while unpaused
  without first requiring an active world.
- `src/World/Thread/Command/Init.hs:73-134` — fresh world initialization does not
  establish a new-session game time or clear the event store.
- `scripts/event_log.lua:435-448,627-640` — the surviving event store has a
  shipped user-visible consumer.
- HPA-4 owns Lua state retained across save replacement, while HPA-31 and HPA-32
  own two independently unbounded diagnostic queues. None establishes the
  missing fresh-session transaction.

**Handoff context:**

- **Current behavior:** Exit-to-menu and subsequent new-world creation can
  inherit prior event history and a continuously advancing process game time.
  Reused page names can make surviving page-qualified records appear to belong
  to the new incarnation.
- **Expected direction:** Define one authoritative fresh-session transition
  with explicit quiescence and ownership ordering. It resets all session-bound
  stores and establishes the new clock epoch before gameplay resumes.
- **Scope and constraints:** Do not reset the game clock on every page
  initialization in a legitimate multi-page session. Preserve process-monotonic
  identifiers where they deliberately prevent stale identity reuse, and clear
  event rows without resetting any globally monotonic diagnostic sequence that
  external consumers rely on.
- **Remaining uncertainty:** The desired clock value for a fresh world is a
  product decision. The mismatch between the documented per-session ownership
  and the exit path is direct.

---

## Gameplay transaction and physical-authority integrity

### [no-issue] HPA-37. Multi-item construction payment can consume a partial cost and then charge the whole cost again

> **Disposition:** No issue — fixed by #1844 (`22b6caf39`, 2026-09-01), after
> the `78dbbba78` audit commit. The per-item `unit.removeItem` loop is gone:
> `construction.payMaterials` pops every material inside one
> `atomicModifyIORef'` of the unit manager and commits nothing on a shortfall,
> then writes the receipt as a CAS guarded on the attempt, splicing the exact
> popped instances back at their original indices if that CAS loses
> (`src/Engine/Scripting/Lua/API/Construct/Payment.hs:113-180`). Gated by
> hspec `--match "payment"` in `Test/Headless/Construct/AttemptIdentity.hs`,
> including "removes NOTHING when the unit cannot cover the cost" and
> "restores the EXACT instances when a cancellation wins the race".

**Severity:** Medium

Construction verifies material availability before walking to a site, but
payment at arrival is implemented as a sequence of individually destructive
`unit.removeItem` calls. Inventory can legitimately change while the worker is
walking. If a later removal fails, earlier removals are not restored and the
job is re-pended without recording the partial payment. A future claimant then
owes the complete original cost.

**Evidence:**

- `scripts/unit_ai_construct.lua:383-400` — construction performs an initial
  availability preflight before movement rather than reserving or consuming the
  materials.
- `scripts/unit_ai_construct.lua:403-439` — arrival loops through material
  requirements and invokes `unit.removeItem` one item at a time.
- The same failure path releases the claim and returns the job to pending
  without rolling back earlier successful removals or durably recording them.
- `scripts/unit_ai_construct.lua:440-443` — the durable paid/consumed state is
  set only after the complete loop succeeds.
- `scripts/unit_ai.lua:365-405` and
  `scripts/unit_ai_construct.lua:485-494` — higher-priority work and transfer
  behavior can preempt a walking construction unit while preserving the job,
  so the preflight is not an exclusion boundary.
- `scripts/unit_info_v2_context_menu.lua:201-221` and
  `scripts/transfer_gestures.lua:100-118,185-197` — shipped exact-item transfer
  gestures can remove a required final item during that interval.
- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:219-243` —
  `unit.removeItem` is a synchronous, destructive, one-item mutation; it offers
  no consume-many transaction or rollback token.

**Handoff context:**

- **Current behavior:** Earlier materials vanish, the building is not started,
  and the re-pended job charges the entire recipe again. The failure therefore
  converts a normal concurrent inventory change into permanent material loss.
- **Expected direction:** Construction payment is one authoritative
  all-or-nothing transaction: either the complete requirement is resolved and
  removed from one current inventory snapshot, or nothing changes.
- **Scope and constraints:** Support requirements spanning multiple stacks or
  exact instances. Do not implement rollback by blindly re-adding definitions
  after failure, because that can lose instance metadata and reorder
  inventories.
- **Remaining uncertainty:** No live construction/transfer interleaving was
  run. The partial-commit sequence and reachable intervening mutation are
  direct.

### [#2293] HPA-38. Autonomous harvesting and foraging bypass carrying-capacity admission

**Severity:** Medium

Player-issued ground pickup checks carrying capacity both when the order is
created and again when the unit arrives. Autonomous harvesting and needs-driven
foraging instead call the low-level ground-item pickup operation directly. That
operation moves an item into inventory without any weight or capacity
validation, despite comments in the AI code and movement model describing
capacity as a hard pickup gate.

**Evidence:**

- `scripts/unit_ai_harvest.lua:154-169` — autonomous collection removes a
  queued ground-item id and invokes `item.pickupGround` without a live capacity
  check.
- `scripts/unit_ai_needs.lua:364-377` — the foraging path comments as though the
  pickup operation can refuse for capacity, but supplies no capacity
  information or preceding check.
- `scripts/unit_ai_needs.lua:399-417` — another direct forage pickup ignores the
  operation's return value.
- `src/Engine/Scripting/Lua/API/Items/Ground.hs:438-497` —
  `item.pickupGround` validates page, item, and unit existence, then appends the
  item to inventory; it does not consult current weight or carrying capacity.
- `scripts/unit_ai_pickup.lua:1-18,96-110,198-214,241-264` — the shipped
  player-order path demonstrates the intended two-phase contract by checking
  capacity at command time and rechecking it at arrival.
- `scripts/movement_speed.lua:23-30` — the movement model describes capacity as
  a hard pickup gate.
- `scripts/unit_ai_tunables.lua:393-408` — autonomous harvesting is recurring
  shipped behavior over a substantial scan radius, not an unused helper.
- The repository deliberately permits some unit-to-unit transfer behaviors to
  exceed nominal capacity; this finding is limited to ground collection paths
  that claim the stricter pickup contract.

**Handoff context:**

- **Current behavior:** Autonomous workers can exceed capacity indefinitely
  through repeated harvest or forage cycles, while the equivalent player order
  is refused.
- **Expected direction:** Ground-to-inventory commitment has one capacity-aware
  authoritative operation, or every autonomous caller performs the same
  last-moment same-instance check immediately before mutation.
- **Scope and constraints:** If harvested yield has already been materialized on
  the ground, capacity refusal should leave the excess there rather than
  deleting it or creating a partial invisible inventory mutation.
- **Remaining uncertainty:** The desired behavior for emergency food
  acquisition at full capacity is not documented. The present inconsistency is
  direct.

### [#2297] HPA-39. Player-issued medical treatment bypasses page and proximity logistics

**Severity:** Medium

The context-menu medical actions search selected units for knowledge and
supplies, then invoke authoritative treatment immediately. Neither the UI path
nor the treatment primitive checks that the medic, patient, and supply owner
are on the same page or within treatment range. The autonomous medic workflow,
by contrast, fetches a remote kit and walks to the patient using an explicit
arrival radius.

**Evidence:**

- `scripts/init_context_menu.lua:103-116,228-276,302-351` — the player action
  selects a knowledgeable medic and supply owner, then calls treatment directly
  without a page, distance, or arrival check.
- `src/Engine/Scripting/Lua/API/Units/Medical.hs:66-115,150-169` —
  bleeding treatment checks entity existence, medical knowledge, wound state,
  and supplies before committing, but not relative page or position.
- `src/Engine/Scripting/Lua/API/Units/Medical.hs:397-448` — infection treatment
  has the same omission.
- `scripts/unit_ai_tunables.lua:467-488` — autonomous medicine defines a
  `treat_arrival` distance of 1.5.
- `scripts/unit_ai_medic.lua:278-324` — the AI fetches supplies when necessary
  and walks to the patient before invoking treatment.
- The normal UI can therefore treat distant units on the current page
  instantaneously; direct public Lua calls can additionally combine units from
  different pages.

**Handoff context:**

- **Current behavior:** Medical supplies can be consumed from a remote holder
  and a patient treated without travel, despite the autonomous workflow
  modeling both supply and patient logistics.
- **Expected direction:** The authoritative treatment commit enforces same-page
  and proximity rules, or the player gesture creates the same durable
  fetch-and-walk order used by autonomous medicine.
- **Scope and constraints:** Decide whether the medic must physically hold the
  kit, whether a nearby separate supply owner is valid, and whether debug/admin
  treatment needs an explicitly named bypass rather than weakening the
  gameplay primitive.
- **Remaining uncertainty:** The AI arrival contract strongly implies physical
  treatment, but the intended immediacy of the player context-menu gesture is
  not stated in a design document.

---

## Deferred UI and exact-instance identity

### [#2300] HPA-40. Deferred ground-item Info can install a stale selection and erase valid selections

**Severity:** Medium

Opening a ground item's Info action captures its numeric ground-item id, but
the actual selection change is deferred until a modal callback. Simulation
continues while the menu is open, so the exact item can be collected or removed
before that callback. Unlike unit selection, `item.select` does not verify that
the id still identifies an item on the active page and cannot report refusal.
The callback then unconditionally clears valid unit and building selections.

**Evidence:**

- `scripts/init_context_menu.lua:377-400` — the Info callback captures the
  hit-tested ground-item id, later invokes `item.select`, and unconditionally
  deselects units and the selected building.
- `src/Engine/Scripting/Lua/API/Items/Render.hs:55-70` — `item.select` blindly
  writes the supplied numeric id into the active page's selection cursor. It
  performs no ground-item lookup and returns no success flag.
- `scripts/init_context_menu.lua:118-133` — the corresponding unit Info path has
  already adopted the safer contract: `unit.select` can refuse, and the
  callback clears other selection domains only after success.
- `scripts/item_info_panel.lua:28-49,77-110` — the panel initially fails to
  resolve a newly installed invalid id; its later same-id update path then
  clears it, producing a transient stale selection rather than preventing it.
- `test-headless/Test/Headless/UI/UnitInfoRowSelection.hs:131-145` — an existing
  fixture can seed an invalid item id precisely because the public item API
  permits that state.
- The previously corrected unit-selection path is adjacent precedent, not
  ownership of the remaining item-selection defect.

**Handoff context:**

- **Current behavior:** A delayed Info action can replace a valid selection with
  an invalid item id, clear unrelated valid selections, and then make the item
  panel disappear after it discovers the stale reference.
- **Expected direction:** Item selection validates the exact instance against
  the intended active page and returns success. Deferred callbacks clear other
  domains only after that commit succeeds.
- **Scope and constraints:** Validation and mutation must share one
  authoritative boundary; a Lua precheck alone would retain the same
  time-of-check/time-of-use window.
- **Remaining uncertainty:** No interactive modal timing scenario was run. The
  stale-id admission and unconditional clearing are direct.

### [#2302] HPA-41. Medical supply discovery aliases same-definition containers to the first instance

**Severity:** Medium

Inventory exposes exact instance ids, but the UI and autonomous medic supply
searches query container contents by definition name alone. The underlying API
interprets an omitted instance id as “the first held item with this
definition.” When a unit owns two medical kits of the same definition, every
iteration can therefore inspect the first kit rather than the specific
container represented by the current inventory row. Transfer-by-definition has
the same ambiguity.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:470-517` —
  `unit.getItemContents` accepts an optional instance id; when omitted, it
  resolves the first held item with the requested definition.
- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:529-586` —
  `unit.getInventory` supplies each row's exact `instanceId`, so callers already
  possess the identity required to avoid ambiguity.
- `scripts/init_context_menu.lua:242-254,306-318` — medical supply discovery
  loops through inventory rows but calls `getItemContents` without the row's
  instance id.
- `scripts/unit_ai_medic.lua:217-255` — autonomous supply discovery similarly
  reduces a kit to its definition name.
- `scripts/unit_ai_medic.lua:287-297` — remote acquisition requests transfer by
  definition rather than the stocked kit's exact identity.
- `src/Engine/Scripting/Lua/API/Units/Cargo.hs:161-179` — an omitted instance id
  in transfer resolves and removes the first same-definition item.
- `src/Engine/Scripting/Lua/API/Units/Medical.hs:111-115,266-292` — the eventual
  treatment commit scans the actual containers, so its authoritative result can
  disagree with the UI or AI's aliased discovery.
- Shipped loot and transfer mechanics can consolidate multiple same-definition
  kits in one inventory; this does not require malformed state.

**Handoff context:**

- **Current behavior:** If kit A is empty and same-definition kit B is stocked,
  the context menu can disable valid treatment, the AI can miss its own supply,
  or a fetch can transfer the wrong empty kit.
- **Expected direction:** Internal medical planning carries
  `{definition, instanceId}` throughout discovery, transfer, and treatment.
  Mutable containers are never resolved by definition when exact identity is
  available.
- **Scope and constraints:** Preserve definition-only helpers for genuinely
  fungible stack items if needed, but do not let them silently stand in for
  stateful container instances.
- **Remaining uncertainty:** The exact ordering of same-definition containers
  determines which symptom appears. No live duplicate-kit scenario was run.

---

## Save compatibility under content evolution

### [#2305] HPA-42. Saved immunity keys bypass infection-reference validation

**Severity:** Medium

A unit's immunity state is a map keyed by infection definition id. Saves
round-trip that map exactly, but the content-reference validation sweep checks
only infections attached to current wounds. An immunity entry can therefore
refer to a removed or renamed infection and still pass staged load validation.
The orphan remains latent in future saves, and if the same id is later
reintroduced it silently resumes influencing infection behavior.

**Evidence:**

- `src/Unit/Types/Instance.hs:123-134` — unit immunity is stored as a map keyed
  by infection id.
- `src/World/Save/Types.hs:620-629,649-675,719-758` — immunity keys and values
  are serialized and reconstructed without semantic reconciliation.
- `src/Combat/Wounds/Tick.hs:254-278` — the map is consulted by infection id
  during later wound progression, so the key has gameplay semantics.
- `src/World/Save/Types.hs:1270-1312` — `missingInfectionReferences` inventories
  infection ids present on wounds but does not inspect immunity-map keys.
- `src/Engine/Scripting/Lua/API/Save.hs:760-815` — the staged content gate relies
  on that incomplete inventory before publication.
- `src/Engine/Scripting/Lua/API/Units/Combat.hs:232-265` — unknown infection
  keys can fall back to their raw identifier for display rather than being
  rejected or reconciled.
- HPA-23 concerns preservation of unreadable optional generations, not known
  save data carrying a missing content reference.

**Handoff context:**

- **Current behavior:** Content removal leaves invisible orphan immunity state
  that can persist indefinitely and unexpectedly reactivate if an identifier is
  reused.
- **Expected direction:** Include immunity-map keys in the prepublication
  content-reference inventory, or define an explicit prune-and-warn migration
  policy. Unknown keys must not pass silently.
- **Scope and constraints:** Preserve the load transaction's all-before-publish
  guarantee. Add a fixture with an unknown immunity key and no wound carrying
  the same infection id.
- **Remaining uncertainty:** Whether missing immunity should reject the save or
  be deliberately pruned is a compatibility-policy decision.

### [#2307] HPA-43. Saved equipment slot keys bypass the current equipment-class schema

**Severity:** Medium

Saved equipment is restored as a map from slot id to item instance. Load
validation confirms that the item definitions still exist, but it does not
confirm that the unit's current equipment class still declares each slot or
that the equipped item's kind remains valid for it. UI rendering enumerates the
current class's slots, while combat, insulation, and carrying-weight code
consume every value in the restored map. Content evolution can therefore
produce invisible but mechanically active equipment.

**Evidence:**

- `src/Unit/Types/Instance.hs:91-95` — unit equipment is keyed by textual slot
  id.
- `src/World/Save/Types.hs:649-669,719-749` — the equipment map is reconstructed
  with its saved slot keys intact.
- `src/World/Save/Types.hs:941-971` — load validation checks referenced item
  definitions but does not resolve the current unit definition, equipment
  class, declared slot ids, or slot-kind compatibility.
- `scripts/unit_info_v2_inventory_data.lua:52-89` — the equipment UI enumerates
  slots from the current class and looks up only those ids, making stale map
  entries invisible.
- `src/Combat/Resolution/Damage.hs:165-174` — damage protection considers the
  complete equipment map rather than only currently valid slots.
- `src/Engine/Scripting/Lua/API/Units/Combat.hs:267-289` — insulation likewise
  aggregates all restored equipment values.
- `src/Engine/Scripting/Lua/API/Units/Inventory.hs:121` — carrying weight counts
  all equipped entries, including ones absent from the current schema.
- `src/Engine/Scripting/Lua/API/Equipment/Slot.hs:125-148` — a stale item can be
  unequipped only if a caller already knows its obsolete slot id.

**Handoff context:**

- **Current behavior:** Renaming/removing a slot or changing a unit's equipment
  class can leave an item hidden from the inventory UI while it continues to
  affect armor, insulation, and weight.
- **Expected direction:** Staged load reconciles every saved slot against the
  current unit and equipment-class schema before publication. Invalid equipment
  is either a load error or is migrated, with an explicit diagnostic, into a
  visible recoverable inventory location.
- **Scope and constraints:** Preserve exact item instances and their contents
  during migration. Define behavior when the destination inventory is nominally
  over capacity; never silently delete the item.
- **Remaining uncertainty:** Current shipped definitions and fixtures appear
  internally consistent. This is a latent content-evolution failure rather than
  a presently broken shipped save.

---

## Page-bound bulk work

### [#2310] HPA-44. Projected page selection does not bind bulk chunk work to its intended page

**Severity:** Medium–High

`world.show` updates a projected visible-page selection and queues the applied
page switch to the world worker. The immediately following bulk-loading API
does not use that projection or accept an explicit page. Instead, it resolves
the currently applied visible world. A single Lua evaluation that calls
`world.show("B")` and then `world.loadChunksInRegion(...)` can therefore enqueue
the region on old page A or new page B depending on worker scheduling.
`world.waitForChunks` independently re-resolves the active page on every poll,
so it can then report completion for B while the mistakenly queued work
continues on hidden A.

**Evidence:**

- `src/Engine/Scripting/Lua/API/World/Lifecycle.hs:559-570,650-661` —
  `world.show` updates projected selection and queues `WorldShow`; the applied
  `wmVisible` state changes only when the world worker handles that command.
- `src/Engine/Scripting/Lua/API/WorldQuery/Chunk.hs:87-123` —
  `loadChunksInRegion` resolves `activeWorldStateFrom` at admission and appends
  work to whichever page is applied at that instant.
- `src/Engine/Core/State.hs:802-838` — active-world resolution reads applied
  world-manager visibility rather than the projected selection established by
  the preceding API call.
- `src/Engine/Scripting/Lua/API/WorldQuery/Chunk.hs:129-156` —
  `waitForChunks` resolves the active page anew on each poll rather than
  following the page or request that received the bulk work.
- `src/World/Thread/ChunkLoading.hs:218-240` — chunk queues for every page are
  drained, so mistakenly queued work on hidden A remains live rather than being
  cancelled by the subsequent visibility switch.
- The planned chunk-residency work introduces page-qualified chunk identity but
  does not presently own this projected-versus-applied API admission race.

**Handoff context:**

- **Current behavior:** Ordered Lua source does not establish ordered page
  ownership for bulk chunk work. The wrong page can be populated, and the
  apparent wait can finish before the requested work has occurred on the
  intended page.
- **Expected direction:** Bulk work is admitted against an explicit or
  projected `WorldPageId`, and admission returns a request/page identity.
  Waiting follows that same identity rather than mutable global visibility.
- **Scope and constraints:** Preserve asynchronous worker ownership and
  multi-page background loading. Integrate with page-incarnation fencing from
  HPA-26 and future page-qualified chunk residency so a destroyed and recreated
  name cannot adopt an older request.
- **Remaining uncertainty:** The failure requires an ordinary cross-thread
  scheduling interleaving but was not executed. The absence of an ordering or
  request-identity boundary is direct.

---

## Content-schema validation

### [#2315] HPA-45. Flora YAML silently defaults or drops misspelled semantic enums

**Severity:** Medium

Flora YAML decodes lifecycle, growth-phase, annual-cycle, and override selectors
as unrestricted text. Registration later interprets the strings through
partial parsers: an unknown lifecycle silently becomes `Evergreen`, while
unknown phase, cycle-stage, and override selectors are discarded. The loader
still reports the species file as successfully loaded. A normal authoring typo
can therefore change gameplay or remove visual and growth states without
producing a schema error.

**Evidence:**

- `src/Engine/Asset/YamlFlora.hs:30-64,227-268` — lifecycle and the semantic tag
  fields are decoded as unrestricted `Text` rather than closed enums or
  validated tokens.
- `src/Engine/Asset/YamlFlora.hs:286-304` — the tag parsers return `Nothing` for
  unknown text.
- `src/Engine/Scripting/Lua/API/YamlTextures.hs:289-297` — an unknown lifecycle
  token falls through to `Evergreen`.
- `src/Engine/Scripting/Lua/API/YamlTextures.hs:299-350` — unknown growth
  phases, annual stages, and override references are removed through
  `mapMaybe`-style registration without a warning.
- `src/World/Flora/Growth.hs:181-191` — harvesting is season-gated only when
  the registered runtime cycle contains `CycleFruiting`. A misspelled fruiting
  stage can be dropped and leave the species harvestable outside its intended
  season.
- `src/Engine/Scripting/Lua/API/YamlTextures.hs:230-258` — after these defaults
  and omissions, the loader reports the file/species as loaded successfully.
- Shipped tags inspected during this pass appear valid. The defect is therefore
  a latent authoring and content-evolution hazard.
- HPA-20 owns persisted ordinal species identity, and HPA-21 owns missing flora
  references in plant designations. Neither covers semantic schema tokens
  accepted with changed meaning.

**Handoff context:**

- **Current behavior:** Misspelled semantic fields can make an annual or
  deciduous species behave as evergreen, remove a phase's art/state, discard
  an override, or disable seasonal harvest gating while startup reports
  success.
- **Expected direction:** Validate every closed flora vocabulary during decoding
  or in one named semantic-validation pass, rejecting the species with
  file/species/field context before runtime registration.
- **Scope and constraints:** Validate override references against the resolved
  phase/cycle set as well as validating token spelling. Do not replace malformed
  required semantics with a plausible gameplay default.
- **Remaining uncertainty:** No currently shipped species was found with an
  invalid tag. Malformed lifecycle, phase, cycle, and override fixtures are
  needed to lock the rejection contract.

---

## Zoom and generated hydrology integrity

### [#2316] HPA-46. Zoom coastal fill promotes non-ocean fluids through a scan-order-dependent ocean cascade

**Severity:** Medium

The zoom-cache coastal-fill pass is described as an ocean-boundary extension,
but its in-chunk seed test accepts every fluid kind. It also reads and writes
the same pixel vector during a row-major traversal, allowing an Ocean pixel
synthesized earlier in the pass to seed another one later. Presentation can
therefore promote dry low terrain beside a lake, river, or lava cell to Ocean,
with an extent that depends on traversal direction rather than authoritative
world topology.

**Evidence:**

- `src/World/ZoomMap/Cache/BuildPixels.hs:229-245` — the pass documents an
  ocean-only boundary extension, and its cross-chunk lookup correctly requires
  `FluidCell Ocean`.
- `src/World/ZoomMap/Cache/BuildPixels.hs:249-272` — the in-chunk lookup tests
  only whether a fluid is present, and the row-major loop mutates the same
  vector it consults for later cells.
- `src/World/ZoomMap/Cache/BuildPixels.hs:70-94,151-153` and
  `src/World/Generate/Chunk/Zoom.hs:200` — the input already comes from the
  detail-world composition that distinguishes Ocean, River, Lake, and Lava.
- `src/World/ZoomMap/Cache/Pixels.hs:64-79` — Ocean has its own paint path; a
  synthesized cell at `seaLevel` can visibly ocean-paint otherwise dry terrain.
- `src/World/Thread/Command/Init.hs:303` and
  `src/World/Load/Stage.hs:361` — the cache is built for fresh worlds and during
  non-arena save staging, so the defect is part of ordinary world publication.

**Handoff context:**

- **Current behavior:** A dry sub-sea basin adjacent to a non-ocean fluid can be
  rendered as ocean, and a connected low run can grow farther in increasing
  row-major order than in the opposite direction while the detailed world
  retains a different classification.
- **Expected direction:** Derive the presentation mask deterministically from
  one authoritative snapshot. Seed only from Ocean, and use either one bounded
  immutable dilation or an explicit order-independent fixed point.
- **Scope and constraints:** Preserve the intended visual closure of small
  coast gaps without changing generated hydrology. Confirm whether the composed
  ocean mask already makes the mutating fill unnecessary.
- **Remaining uncertainty:** The invalid seed and traversal dependency are
  direct. The frequency and maximum visible extent in shipped world seeds were
  not measured under this static-only pass.

### [#2323] HPA-47. Shared lake spillways collapse two source identities into one

**Severity:** Medium

River identification permits two lakes to choose the same external spillway
tile, then inverts spillway ownership into a scalar last-write-wins map. Both
lakes still inject their complete flow through that tile, but only the retained
owner controls the tile's descent exclusions and downstream source metadata.
The overwritten lake can route back into itself or lose its intended outlet,
making generated topology depend on lake iteration order.

**Evidence:**

- `src/World/Fluid/River/Identify/Flow.hs:115-176` — spillways are selected per
  lake, and the source explicitly acknowledges that one tile can serve two
  lakes before retaining only one inverse owner.
- `src/World/Fluid/River/Identify/Flow.hs:188-203` — the shared tile receives one
  descent direction whose neighbor exclusions are derived from the retained
  scalar owner.
- `src/World/Fluid/River/Identify/Flow.hs:328-375` — each lake injects its full
  flow independently, while `walkInject` stops and reabsorbs an injection when
  it enters a lake; a direction chosen for the other owner can therefore send
  the overwritten source back into its own basin.
- `src/World/Fluid/River/Identify/Components.hs:401-418` — river metadata also
  derives a single source-lake identity for a spillway.
- `src/World/Fluid/River/Identify.hs:129` — the flow construction participates
  in every ordinary river-identification pass.

**Handoff context:**

- **Current behavior:** In the admitted shared-spillway case, one basin can lose
  or misroute its generated outlet, and `rivSourceLake` can depend on traversal
  order instead of physical source ownership.
- **Expected direction:** Preserve every source identity in the one-to-many
  relation, or derive routing per `(lakeId, spillwayTile)` while excluding that
  path's own source basin. Define merged-outlet metadata explicitly.
- **Scope and constraints:** Keep deterministic generated output and account for
  save compatibility if the correction changes world-generation results.
- **Remaining uncertainty:** The mechanism is explicit in the source, which
  calls the collision rare; its occurrence rate was not measured.

---

## Asynchronous gameplay authority

### [#2325] HPA-48. Craft-bill mutations can cross page-local identity domains

**Severity:** Medium–High

Craft bills are stored per world page and every page allocates bill IDs from
one, but the Lua-facing bill verbs carry only the numeric ID and resolve the
currently active page afresh. A page selection change between related calls can
therefore redirect an old page's job to a same-number bill on another page, or
make the old job disappear locally without releasing its original claim.

**Evidence:**

- `src/Craft/Bills.hs:148-157` — each page-local bill store starts its allocator
  at the same numeric ID.
- `src/Engine/Scripting/Lua/API/Craft/Bill.hs:214-311` — bill lookup, claim,
  progress, completion, and release accept only `billId` and independently
  resolve the active page for each operation.
- `scripts/unit_ai_page.lua:1-18` — active page selection can change while Lua
  orchestration continues across separately scoped engine calls.
- `scripts/unit_ai_craft.lua:174-186,258-292,354-384` — an actor can retain a
  local job containing only the bill ID, then refresh, progress, complete, or
  clear it after another active-page lookup. The refreshed result is not bound
  back to an owning page identity.
- `src/Engine/Scripting/Lua/API/Units/List.hs:42-74` and
  `scripts/unit_ai_craft.lua:278-290` — active-page unit enumeration and current
  actor/station checks narrow the ordinary settled-page case, but do not make
  the multi-call transaction atomic against a mid-update page switch.

**Handoff context:**

- **Current behavior:** A page switch during craft orchestration can progress or
  finish another page's colliding bill, or abandon the old page's working claim
  while the Lua job appears cleared.
- **Expected direction:** Every bill identity and mutation carries its owning
  page and page incarnation, and a multi-step craft attempt refuses to cross
  that identity boundary.
- **Scope and constraints:** Preserve ordinary hidden-page suspension and
  asynchronous worker ownership. A destroyed and recreated page name must not
  adopt an older job or claim.
- **Remaining uncertainty:** Normal settled switches are substantially guarded;
  the retained failure requires a mid-update interleaving and was not executed.

### [#2326] HPA-49. Building placement validates an unreserved snapshot and commits trusted coordinates

**Severity:** Medium

`building.spawn` validates terrain and occupancy against a manager snapshot,
allocates an ID, and queues the placement. The authoritative building-thread
commit later inserts the building without revalidating that the footprint is
still free. Multiple requests admitted before the first commit can therefore
all approve the same location and create overlapping buildings.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Buildings/Spawn.hs:80-121` — admission reads the
  manager, validates the candidate footprint, allocates an ID, and queues the
  command without reserving the accepted tiles.
- `src/Building/Thread/Command.hs:108-144` — commit rechecks world existence but
  inserts the supplied building without rerunning terrain or occupancy
  validation.
- `src/Building/Thread/Command/BoundSpawn.hs:57-70` — bound placement rechecks
  selection generation, not footprint exclusivity.
- `scripts/locations.lua:566-572` — ordinary Lua orchestration can issue several
  spawn requests during one callback, so duplicate authored offsets can admit
  against the same pre-commit snapshot without rapid manual input.

**Handoff context:**

- **Current behavior:** Two independently accepted requests can occupy the same
  footprint because serialized command consumption occurs only after both have
  passed the non-reserving admission check.
- **Expected direction:** Revalidate all mutable exclusivity conditions in the
  same authoritative transaction that commits placement, or establish a
  reservation whose ownership is checked at commit.
- **Scope and constraints:** Retain early Lua-side diagnostics and page binding,
  but treat them as advisory. Define deterministic failure/refund behavior for
  a request that loses the commit race.
- **Remaining uncertainty:** The TOCTOU is direct; no overlapping pair was
  spawned during the static review.

### [#2328] HPA-50. Combat resolution does not authoritatively revalidate mutable attack conditions

**Severity:** Medium

`combat.attack` accepts two IDs and queues a resolution request. The combat
authority correctly rechecks entity existence, definitions, and death, but not
same-page ownership, current distance or reach, faction permission, collapsed
pose, or sufficient current stance. Those mutable conditions are checked only
by Lua callers before queueing and can change before the combat thread commits
the strike.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Combat.hs:45-73` — the public API queues attacker
  and target IDs without encoding the admission facts under which the request
  was accepted.
- `src/Combat/Resolution.hs:117-152` — resolution rechecks missing definitions,
  missing instances, and death, but not page, distance, faction, pose, or
  current stance sufficiency.
- `scripts/unit_ai_combat_attack.lua:229-282` — range and stance admission live
  in Lua orchestration before the asynchronous call.
- `scripts/init_context_menu.lua:135-175` — faction permission is checked while
  creating the player order, not at authoritative effect commit.
- `src/Combat/Thread.hs:107-126` — requests drain on a separate worker, leaving
  time for motion, collapse, another attack, or a page transition to invalidate
  the earlier conditions.

**Handoff context:**

- **Current behavior:** A queued strike can land after its participants move
  apart, change pages, collapse, exhaust or change stance, or cease to satisfy
  the Lua-side faction policy.
- **Expected direction:** The combat authority revalidates every mutable safety
  and gameplay precondition immediately before applying effects, returning a
  named rejected outcome when one no longer holds.
- **Scope and constraints:** Preserve the existing liveness checks and
  asynchronous worker. Decide which permission rules are authoritative game
  rules versus UI policy, and centralize only the former.
- **Remaining uncertainty:** The exact product rule for player-forced attacks
  across faction states needs clarification; range, page, pose, and spendable
  stance are direct mutable commit conditions.

### [#2332] HPA-51. Page suspension leaves action clocks and working leases running

**Severity:** Medium–High

Hiding a page removes its units from Lua AI iteration, but global game time
continues. A craft job records its last-work timestamp and holds a persisted
bill claim before the page disappears; neither clock nor lease is suspended.
When the page becomes active again, hidden duration is credited as immediate
craft progress even though page-local power and coupled resources did not tick.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Units/List.hs:42-74` and
  `scripts/unit_ai.lua:460-489` — AI enumeration covers only active-page units,
  so hidden-page actions stop receiving ordinary update callbacks.
- `src/Unit/Thread.hs:61-74` — the game clock continues advancing while the
  overall simulation is unpaused.
- `scripts/unit_ai_craft.lua:318-364` — entering work records `lastCraftAt` and
  marks the bill working; later progress is `now - lastCraftAt` with no page
  activation epoch or elapsed cap.
- `src/Craft/Bills.hs:254-282` — the same holder can refresh its claim after the
  nominal timeout, so hiding does not force lease loss or reconciliation.
- `src/World/Thread/Command/UI.hs:88-146` — hiding a page has no craft/action
  suspension hook.
- `src/World/Thread/Time.hs:35-77` — page-local power advances only for visible
  pages, so the credited work can lack matching energy passage.

**Handoff context:**

- **Current behavior:** Reactivating a page can grant a burst of off-page work
  and preserve a stale working claim while its required power and other
  page-local systems were suspended.
- **Expected direction:** Suspension stops action elapsed clocks and explicitly
  releases or freezes their leases, or background simulation advances all
  coupled resources under one consistent policy.
- **Scope and constraints:** Apply the rule to every timestamp-based action, not
  only crafting, and distinguish global pause from page suspension. Persist any
  new action epoch or suspension state deliberately.
- **Remaining uncertainty:** Crafting supplies the traced complete example; the
  breadth of other Lua actions using the same timestamp pattern still needs an
  inventory during implementation.

---

## Save discovery and failure containment

### [#2333] HPA-52. One unreadable save entry aborts the complete save listing

**Severity:** Medium

Save discovery intends to skip semantically corrupt entries and return every
healthy save, but the filesystem reads underneath each candidate have no
per-entry exception boundary. One permission failure, concurrent removal, or
other `IOException` escapes the traversal and prevents the public API from
constructing any listing at all.

**Evidence:**

- `src/World/Save/Serialize.hs:312-318` — `listSaves` enumerates the directory
  and traverses every entry with plain `mapM tryEntry`.
- `src/World/Save/Serialize.hs:320-329` — entry classification itself performs
  unchecked filesystem I/O.
- `src/World/Save/Serialize.hs:331-417` — semantic corruption is deliberately
  logged and skipped, but authoritative and previous-generation `BS.readFile`
  calls are not caught.
- `src/World/Save/Serialize.hs:419-437` — the legacy-file read is likewise
  outside an entry-local exception boundary.
- `src/Engine/Scripting/Lua/API/Save.hs:106-128` — the public best-effort list
  API calls `listSaves` directly, so one propagated exception suppresses every
  healthy row.
- `src/World/Save/Autosave.hs:226-240` — autosave ownership inspection depends
  on the same complete listing and inherits its all-or-nothing failure.

**Handoff context:**

- **Current behavior:** One unreadable, concurrently removed, or otherwise
  I/O-failing entry prevents every healthy save from appearing and can abort
  autosave's ownership scan instead of representing the slot as unclassifiable
  but occupied.
- **Expected direction:** Contain unpredictable I/O per entry, emit one
  path-specific diagnostic, and continue with healthy entries. Distinguish a
  failure to enumerate the `saves/` root, where no collection exists to recover.
- **Scope and constraints:** Do not reinterpret semantic corruption as an empty
  or safe slot. Autosave must remain conservative about any path whose ownership
  cannot be proved.
- **Remaining uncertainty:** The uncaught calls are direct; the platform-specific
  filesystem failures were not induced.

### [#2334] HPA-53. Dump fast-settle can wait forever after a worker failure

**Severity:** High

Dump mode queues a global fast-settle request and waits on an unbounded
completion `MVar`. The sim fills it only after every world writeback has
acknowledged, while those acknowledgements are emitted only on successful
handler completion. A sim or world-worker failure can therefore strand the
dump forever even though the shared worker lifecycle has already recorded the
failure.

**Evidence:**

- `app/App/Dump.hs:168-177` — dump queues `SimFastSettleAll` and calls
  unbounded `takeMVar`; unlike its earlier initialization and chunk waits, this
  path has no timeout or lifecycle race.
- `src/Sim/Thread.hs:275-302` — the sim waits for every world writeback
  acknowledgement and fills the outer completion only after all worlds settle.
- `src/World/Thread/Command.hs:278-293` — the world acknowledgement is emitted
  only after the handler body finishes, not from a finalizer.
- `src/Engine/Core/Thread.hs:168-193` — a tick exception invokes the worker's
  crash callback and terminates it without signalling operation-specific
  completion variables.
- `src/Sim/Thread.hs:72-76` and `src/World/Thread.hs:53-57` — worker crashes set
  lifecycle to `CleaningUp`, but the blocked dump does not observe it.
- `src/Engine/Core/Thread.hs:43-51,148-161` — workers already expose reliable
  `tsDone` completion, including crash exit, but fast-settle does not race its
  wait against those signals.

**Handoff context:**

- **Current behavior:** A sim failure before the outer completion, or a world
  failure before writeback acknowledgement, leaves automation with neither
  dump JSON nor a terminal nonzero error and prevents ordinary boot-result
  cleanup.
- **Expected direction:** Make fast-settle completion outcome-bearing and
  bounded: race it against relevant worker completion/lifecycle failure and a
  deadline, and ensure an exceptional writeback cannot abandon its waiter.
- **Scope and constraints:** Success must continue to mean that every requested
  writeback reached world-thread application. Do not convert a timeout or worker
  death into partial-success output.
- **Remaining uncertainty:** The wait graph is direct; a worker was not
  deliberately crashed during dump mode.

### [#2335] HPA-54. Modern and legacy saves can occupy and expose the same logical name

**Severity:** Medium

Modern directory slots and retained legacy flat files use different physical
paths but the same extension-free logical name. Ordinary save publication
checks only the modern directory, while discovery lists both representations
independently and loading always prefers the modern one. Creating a modern save
beside a legacy namesake can therefore shadow the legacy generation and expose
two indistinguishable rows that both load the modern slot.

**Evidence:**

- `src/World/Save/Serialize.hs:68-78` — `saves/<name>/` and
  `saves/<name>.synworld` represent the modern and legacy forms of one name.
- `src/World/Save/Serialize.hs:109-121` — normal publication considers only the
  modern directory and does not reject a same-name legacy flat file.
- `src/World/Save/Serialize.hs:312-370,419-443` — listing visits both entries
  independently and produces rows with the same `slName`.
- `src/World/Save/Serialize.hs:445-464` — sorting does not coalesce or diagnose
  duplicate logical names.
- `src/World/Save/Serialize.hs:148-174` — logical-name loading prefers the
  modern directory and consults the legacy file only if the directory is absent.
- `scripts/main_menu.lua:60-83` and `scripts/save_browser.lua:164-168` — UI
  consumers retain the returned rows verbatim.
- `src/World/Save/Autosave.hs:226-255` — autosave already detects and refuses a
  same-name legacy collision, demonstrating an invariant absent from ordinary
  manual publication.

**Handoff context:**

- **Current behavior:** A modern save can silently shadow a legacy generation;
  the browser can display two rows with one apparent identity and differing
  metadata, but selecting either reaches the modern save.
- **Expected direction:** Enforce one logical-name occupancy rule across both
  representations. Refuse the collision or require an explicit migration or
  replacement decision, and publish at most one unambiguous listing row.
- **Scope and constraints:** Preserve legacy readability and case/path safety.
  Collision handling must not silently delete either representation.
- **Remaining uncertainty:** Existing user save directories were not inspected;
  the namespace collision is structurally reachable.

### [no-issue] HPA-55. A failed candidate flush leaks its still-owned file handle

> **Disposition:** No issue — fixed after the audit by `d23cc5b7c` (#2024, 2026-09-01): `durableFlush` now closes the handle via `onException` at both the flush and the `handleToFd` hand-over (`src/World/Save/Storage/Durable.hs:112-116`), pinned by the "durableFlush closes the handle when the flush itself fails" spec in `test-headless/Test/Headless/World/GeneratedLibrary.hs`.

**Severity:** Medium

Durable save publication transfers a fresh Haskell `Handle` into the candidate
write/validate/publish routine. Its write-failure branch closes the handle, but
its flush-failure branch returns without closing. Because ownership transfers
to a raw file descriptor only after `hFlush` succeeds, a flush exception leaves
the original handle open while outer cleanup attempts only to remove the path.

**Evidence:**

- `src/World/Save/Storage.hs:392-400` — candidate creation gives a fresh
  `Handle` to `writeValidateAndPublish`; the outer finalizer removes only the
  temporary path.
- `src/World/Save/Storage.hs:463-479` — candidate-write failure calls
  `closeQuietly`, but candidate-flush failure returns immediately without it.
- `src/World/Save/Storage.hs:496-506` — `durableFlush` calls `hFlush` before
  `handleToFd`; descriptor ownership exists only after that transfer.
- `src/World/Save/Storage.hs:508-511` — a quiet-close helper already exists but
  is not used on the flush failure branch.
- `src/World/Save/Storage.hs:782-794` — path cleanup only attempts
  `removeFile`, swallows its failure, and cannot close the still-owned handle.

**Handoff context:**

- **Current behavior:** A recoverable `PhaseCandidateFlush` result can leave an
  open descriptor. Repeated retries during persistent storage failure can
  accumulate descriptors, and an unlinked candidate can retain disk space until
  finalization or process exit.
- **Expected direction:** Bracket candidate ownership explicitly and close the
  Haskell handle on every failure before successful `handleToFd` transfer,
  while closing the post-transfer descriptor exactly once.
- **Scope and constraints:** Preserve the current phase-specific diagnostic and
  temporary-file cleanup. Account for asynchronous exceptions during each
  ownership transition.
- **Remaining uncertainty:** No flush error was injected; the missing close on
  the pre-transfer branch is direct.

---

## Public spatial and temporal numeric integrity

### [#2336] HPA-56. World-space item and decal spawns admit non-finite coordinates into rendering

**Severity:** High

The public ground-item and blood-decal spawn APIs accept arbitrary Lua numbers
for world coordinates and narrow them to `Float` without a finite check. Both
render paths later apply partial integral conversion to those coordinates.
Ground-item coordinates are additionally part of authoritative saved page
state, so one malformed call can either fail the current render loop or persist
the same failure into a later load.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Items/Ground.hs:59-165` —
  `item.spawnGround` accepts numeric X/Y and inserts their narrowed values
  directly into the selected page's ground-item store; its detailed condition
  validation does not cover coordinates.
- `src/Item/Ground.hs:30-50` — ground-item world position is stored as two
  unconstrained strict `Float` fields.
- `src/World/Render/GroundItemQuads.hs:93-126,216-243` — geometry and rendering
  call `floor` on the stored coordinates before canonicalizing the tile frame.
- `src/World/Save/Component/Page.hs:687-729,1463-1499` — the DTO round-trips the
  coordinates exactly, while page validation checks ground-item allocator
  integrity rather than spatial finiteness.
- `src/Engine/Scripting/Lua/API/Blood.hs:61-175` — `blood.spawn` accepts X/Y and
  additional geometry numbers without finite validation; integer properties
  such as `seed`, `surfaceZ`, and `sourceUnit` are also produced with partial
  `round`.
- `src/World/Render/BloodQuads.hs:308-365` — blood rendering calls `floor` on
  decal X/Y, and the remaining unvalidated geometry flows into quad vertices
  and tint.
- `src/Blood/Types.hs:18-23` — blood is deliberately transient, limiting that
  half of the defect to the current session rather than removing its main-loop
  failure.

**Handoff context:**

- **Current behavior:** A non-finite ground-item or blood coordinate can raise
  from the main rendering path. A ground item can preserve the bad coordinate
  across save/load; malformed blood geometry can also publish non-finite vertex
  or tint values.
- **Expected direction:** Validate finite world coordinates after narrowing and
  validate all decal geometry domains before model insertion. Recheck persisted
  ground positions during staged load before publication.
- **Scope and constraints:** Require positive finite scale and bounded finite
  opacity/wetness. Reject invalid calls before allocating IDs, instances, random
  draws, or texture work so failure is side-effect-free.
- **Remaining uncertainty:** Coordinate-to-`floor` reachability is direct. The
  downstream behavior of non-finite vertex values inside the graphics backend
  was not exercised.

### [#2337] HPA-57. Camera APIs admit non-finite state into partial loop and load arithmetic

**Severity:** High

`camera.move` and `camera.setPosition` accept arbitrary Lua numbers and publish
their narrowed values directly into the live camera. The main loop always wraps
an unlocked camera position with a `floor`-based algorithm, and chunk selection
uses additional partial coordinate conversions. Camera state is also saved
without validation and processed through world-to-grid arithmetic during load
staging.

**Evidence:**

- `src/Engine/Scripting/Lua/API/Camera.hs:42-76` — both public APIs store or add
  arbitrary numeric inputs without checking finiteness after `Double`-to-`Float`
  narrowing.
- `src/Engine/Scripting/Lua/API/Register/Camera.hs:13-20` — the functions are
  registered public Lua APIs.
- `src/Engine/Loop.hs:120-126` and `src/Engine/Loop/Camera.hs:149-190` — every
  unlocked graphical/offscreen tick invokes camera panning and wrapping even
  when no movement key is held.
- `src/Engine/Loop/Camera.hs:82-87` — coordinate wrapping applies `floor` to a
  quotient derived from the current position.
- `src/World/Grid.hs:238-243`,
  `src/World/Generate/Coordinates.hs:49-53`, and
  `src/World/Thread/ChunkLoading.hs:48-63` — camera-driven chunk identity has
  another path through partial world-coordinate rounding.
- `src/World/Thread/Command/Save/WriteWorld.hs:231-237` and
  `src/World/Save/Component/Session.hs:53-82` — live camera floats are captured,
  and the session component declares no value validation.
- `src/World/Load/Stage.hs:428-435` — load staging calls `worldToGrid` on the
  restored camera before publication.

**Handoff context:**

- **Current behavior:** One public call can take down the main loop on its next
  unlocked tick. A captured or externally malformed save can reproduce partial
  spatial arithmetic during staging.
- **Expected direction:** Camera position is finite and normalized at every
  mutation boundary, with defensive validation at save decode before any
  coordinate conversion.
- **Scope and constraints:** Validate after narrowing because a finite `Double`
  can overflow `Float`. Define finite domains for zoom and velocity in the same
  camera-state validator without weakening existing clamps.
- **Remaining uncertainty:** The partial conversions are direct; no malformed
  camera was installed or loaded.

### [#2338] HPA-58. `world.digTile` can kill the world worker or durably poison a designation

**Severity:** High

`world.digTile` accepts arbitrary Lua numbers for coordinates, work amount,
skill, perception, and unit position. Some inputs reach partial rounding before
or during worker execution; others enter the mine-progress state as non-finite
floats. A bad skill can terminate the critical world worker, while a NaN work
amount can create an unfinishable designation whose poisoned progress survives
save/load.

**Evidence:**

- `src/Engine/Scripting/Lua/API/World/Edit.hs:301-334` — the API immediately
  rounds coordinates and narrows the remaining numeric arguments without
  finite or domain validation before queueing.
- `src/Engine/Scripting/Lua/API/Register/World.hs:81-88` and
  `scripts/unit_ai_dig.lua:300-316` — the verb is public and has a shipped AI
  caller.
- `src/World/Thread/Command/Edit/Dig.hs:163-181` — progress incorporates the
  supplied skill and applies `floor`; NaN or infinity reaches that partial
  conversion once a diggable material produces positive progress.
- `data/materials/igneous_intrusive.yaml:16` and
  `data/materials/ores.yaml:12,57,69` — shipped materials provide the relevant
  `dig_chunk` behavior.
- `src/World/Mine/Types.hs:86-121` — the work allocator rejects only
  `amount <= 0`; NaN passes that guard, propagates through corner progress, and
  prevents completion comparisons from succeeding.
- `src/World/Save/Component/Page.hs:402-415,1479-1497` — mine progress floats
  round-trip in page state, while validation covers ground-item allocation and
  not mine-state finiteness or bounds.

**Handoff context:**

- **Current behavior:** A malformed call can fail-stop the world worker or leave
  a visible mine designation that can never complete and remains poisoned after
  reload.
- **Expected direction:** Require finite, nonnegative work deltas and finite,
  named domains for skill/perception at admission and authoritative commit;
  validate persisted mine progress as finite and bounded.
- **Scope and constraints:** Reject invalid input before queueing or mutating
  progress. Preserve valid partial-dig behavior and make legacy bad-state policy
  explicit rather than silently clamping corruption.
- **Remaining uncertainty:** The numeric paths are direct; neither worker death
  nor a poisoned save fixture was executed.

### [#2339] HPA-59. `world.setDate` installs and persists a noncanonical date

**Severity:** Medium

`world.setDate` accepts arbitrary integer year, month, and day components. The
world worker stores them verbatim even though `WorldDate` documents narrower
calendar ranges. Derived queries clamp invalid components, so the same state
can report raw and ordinal dates that disagree, then jump to a different
canonical representation when time next crosses midnight. The invalid raw
components survive save/load.

**Evidence:**

- `src/Engine/Scripting/Lua/API/World/Clock.hs:111-127` — the public setter
  accepts arbitrary integer components without calendar validation.
- `src/World/Thread/Command/Time.hs:32-50` — the worker constructs and writes
  `WorldDate year month day` verbatim.
- `src/World/Time/Types.hs:75-92` — the type documents month and day ranges but
  does not enforce them in its representation.
- `src/World/Time/Types.hs:101-144` — ordinal conversion clamps invalid
  components, and positive day rollover later canonicalizes through that
  clamped ordinal.
- `src/Engine/Scripting/Lua/API/World/Clock.hs:129-162` — `world.getDate`
  returns raw components beside derived `dayOfYear` and `absoluteDay`, exposing
  the contradiction.
- `src/World/Thread/Command/Save/WriteWorld.hs:147-148`,
  `src/World/Save/Component/Page.hs:1057-1080`, and
  `src/World/Load/Stage.hs:235-243` — save captures the raw date, page
  validation omits it, and staging reinstalls it unchanged.

**Handoff context:**

- **Current behavior:** Public date state can contradict its own derived fields,
  calendar-dependent gameplay observes the clamped interpretation, and a later
  midnight rollover changes the representation discontinuously.
- **Expected direction:** Dates are canonical at every mutation and decode
  boundary: reject invalid components or normalize once and expose only that
  canonical result.
- **Scope and constraints:** Define supported year bounds and leap/calendar
  semantics centrally. Preserve compatibility through an explicit migration or
  load rejection diagnostic for already malformed saves.
- **Remaining uncertainty:** The repository documents the current missing range
  check, but the desired reject-versus-normalize policy remains a product choice.

---

## Content-schema relational and numeric integrity

### [#2346] HPA-60. Infection YAML has no semantic validation for bands, weights, or multipliers

**Severity:** Medium–High

Infection definitions decode all gameplay numbers as unrestricted floats and
accept climate bands as arbitrary-length lists. Registration silently rewrites
list shapes and installs values without checking finiteness, ordering,
probability domains, or weight sign. Syntactically valid authoring mistakes can
therefore distort selection, growth, or treatment while startup reports the
definition as successfully loaded.

**Evidence:**

- `src/Engine/Asset/YamlInfection.hs:16-50` — weights, multipliers, and climate
  values are raw `Float`s, while bands are arbitrary lists.
- `src/Engine/Asset/YamlInfection.hs:77-80` — the `pair` helper truncates lists
  longer than two, combines a singleton with a default upper bound, and maps an
  empty list to the full default band without validating order or range.
- `src/Engine/Scripting/Lua/API/Infection.hs:32-76` — registration copies the
  decoded values into the live definition and logs it as loaded without a
  semantic-validation phase.
- `src/Infection/Types.hs:21-42` — the runtime type documents selection-weight
  and moisture domains but represents them as unconstrained floats.
- `src/Combat/Wounds/Infection.hs:151-194` — climate filters and weighted
  selection assume ordered bands and nonnegative finite weights when summing a
  total, constructing a random range, and subtracting cumulative weights.
- `src/Combat/Wounds/Tick.hs:258-285` and
  `src/Engine/Scripting/Lua/API/Units/Medical.hs:424-449` — aggressiveness,
  infectability, and cure rate feed progression and treatment arithmetic
  directly.

**Handoff context:**

- **Current behavior:** A malformed band can be silently reinterpreted, and a
  negative or non-finite weight/multiplier can bias or violate selection,
  infection growth, or treatment assumptions without rejecting the content.
- **Expected direction:** One semantic pass rejects wrong cardinality,
  non-finite values, unordered or out-of-range bands, negative weights, and
  out-of-domain multipliers with file/definition/field context.
- **Scope and constraints:** State every domain explicitly rather than choosing
  plausible defaults. Validate the complete definition before mutating the live
  registry.
- **Remaining uncertainty:** Shipped definitions inspected in the pass were
  valid. Exact library behavior for every non-finite random range was not
  asserted; the violated weighted-selection precondition is sufficient.

### [#2347] HPA-61. Building animation FPS can reach partial main-render arithmetic

**Severity:** Medium–High

Building YAML accepts animation `fps` as an unrestricted float and registration
copies it into the runtime definition. Time-driven building rendering later
applies `floor` to elapsed time multiplied by that value. A NaN or infinite FPS
in an otherwise loadable definition can therefore raise on the main render
path.

**Evidence:**

- `src/Engine/Asset/YamlBuildings.hs:23-33` — building animation FPS decodes as
  a raw `Float` without positive-finite validation.
- `src/Engine/Scripting/Lua/API/Buildings/Yaml.hs:54-82` — registration copies
  the value verbatim into `banFps`.
- `src/Building/Render.hs:81-91` — a referenced time-driven animation selects
  its frame with `floor (elapsed * banFps)`.
- `src/Engine/Asset/YamlBuildings.hs:45-95` — related building numeric fields,
  including work, dimensions, storage capacity, power drain, and material
  counts, are also decoded without one named semantic-validation boundary.
- `src/Building/Types.hs:277-283,331-347` and
  `src/Engine/Scripting/Lua/API/Units/Transfer.hs:695-721` — negative material
  counts, non-finite work, and non-finite capacity can violate their downstream
  completion or admission assumptions even when they do not reach the same
  partial render conversion.

**Handoff context:**

- **Current behavior:** One malformed animation rate can terminate rendering;
  other malformed building numbers can create cost-free, permanently
  appearing, or effectively unbounded-capacity definitions while load reports
  success.
- **Expected direction:** Register buildings only after semantic validation
  requires positive finite playback rates and dimensions, finite nonnegative
  work/capacity/drain, and positive material counts.
- **Scope and constraints:** Keep the direct FPS-to-`floor` failure as the
  primary defect while using one schema validator for the closely coupled
  numeric fields. Reject the whole definition with precise authoring context.
- **Remaining uncertainty:** Shipped building YAML inspected in this pass was
  valid; this is a latent content-authoring failure.

### [#2348] HPA-62. Duplicate unit body-part IDs split targeting from damage authority

**Severity:** Medium

Unit body parts decode as an unconstrained list with free-text IDs and parents.
Combat target selection preserves every list entry and weights duplicates
independently, but damage resolution later collapses the same list into a map
keyed by ID. A selected identity can therefore be chosen using one duplicate's
area or tactical fields and resolved using another duplicate's tissue, vital,
or structural definition.

**Evidence:**

- `src/Engine/Asset/YamlUnits.hs:185-227,408-433` — body parts are decoded with
  unrestricted IDs and parent references, and the complete unit decoder runs no
  uniqueness or graph validation.
- `src/Engine/Scripting/Lua/API/Units/Yaml.hs:217-220,261-287` — registration
  preserves the authored list exactly in `udBodyParts`.
- `src/Combat/Resolution.hs:173-184` and
  `src/Combat/Resolution/Strike.hs:169-186` — target selection retains and
  scores every targetable list entry, so duplicate IDs receive independent
  selection weight.
- `src/Combat/Resolution/Common.hs:134-135` — later resolution builds a
  last-value map with `HM.fromList`, silently collapsing duplicate IDs.
- `src/Combat/Resolution/Damage.hs:147-153` — actual tissue-layer damage resolves
  the selected ID through that collapsed map.

**Handoff context:**

- **Current behavior:** Selection probability and tactical metadata can come
  from one duplicate while tissue, vital, and damage behavior comes from
  another. Missing or cyclic parent references can independently strand parts
  outside coherent macro/subpart allocation.
- **Expected direction:** Validate unique primary keys, existing parent foreign
  keys, acyclic parentage, and coherent targetable/subpart roles before
  publishing a unit definition.
- **Scope and constraints:** Report every conflicting path and ID in one load
  error. Preserve authored list order only after the relational graph is proven
  valid.
- **Remaining uncertainty:** No duplicate ID was found in shipped definitions;
  the split between list-based selection and map-based resolution is direct.
