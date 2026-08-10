# Save/load transaction findings

This report records defects verified in the save/load transaction and its
recovery boundaries. It is being drafted one concern chapter at a time so each
finding can be discussed before the audit expands.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

Save/load was selected for this targeted audit because a recovery failure can
leave persistent owners on different session generations or make an intact
generation unreachable. The first pass read the persistence contract and
state inventory, traced the Lua prepare/apply/rollback path into the Haskell
publication barrier, inspected the focused Lua persistence tests, and ran an
isolated reproduction against the real `scripts.lib.save_modules` registry.
The follow-up pass traced atomic storage publication and load-source selection,
inspected their focused storage tests, and reproduced the vulnerable durable
filesystem topology against the production generation selector using a
tracked compatibility fixture. The third pass traced the post-publication
`LuaSaveLoaded` lifecycle through its callback error boundary, inspected the
shipped reconciliation callbacks and focused dispatcher coverage, and
reproduced the terminal status transition with an intentionally failing module
registered on a real Lua backend.

The focused rollback regression was run with
`cabal test synarchy-test-headless --test-options='--match "rolls a failed
apply back VERBATIM"'`; its successful-rollback case passed. The complete
`atomic save storage` describe was also run; all 39 examples passed, confirming
that its existing cases do not cover the composed recovery/interruption state.
The `LuaSaveLoaded stale debug-command cancellation` describe passed all three
examples; its scriptless backend does not cover callback failure.
`python3 tools/persistence_inventory_audit.py` and
`python3 tools/save_compat_audit.py` both passed. No standalone game process,
graphical or preview mode, full test suite, world check, probe sweep, or
`make ci` was run; the targeted dispatcher test did initialize its standard
headless engine fixture. No GitHub duplicate search was performed; that belongs
to `process-report`. The report currently contains the three
transaction-boundary concerns discussed and approved so far.

## Status

- [x] SAVE-1. Lua load rollback failures are silently ignored — [#1200]
- [x] SAVE-2. Interrupted recovery-state publication can leave no selectable generation — [#1203]
- [x] SAVE-3. Failing post-load callbacks still report `LoadSucceeded` — [#1204]

---

## Transactional publication and rollback

### [#1200] SAVE-1. Lua load rollback failures are silently ignored

`saveModules.applyAll()` promises to leave the old Lua session intact when a
component apply or reset hook fails, but every rollback callback is invoked
through an unchecked `pcall`. If any rollback callback also throws, the
registry discards the error, clears its transaction bookkeeping, and reports
that every component was rolled back even though live Lua state can still
contain values from the rejected session. The Haskell publication is correctly
withheld, so this produces the exact mixed-generation state the rollback is
meant to prevent: old Haskell state paired with partially new Lua state.

**Evidence:**

- `docs/persistence_contract.md:35` — the authoritative contract requires a
  partway load failure not to leave the live session half-replaced.
- `scripts/lib/save_modules.lua:852` — the implementation explicitly says a
  failed apply must restore the live Lua session exactly as it was.
- `scripts/lib/save_modules.lua:928` — rollback of components that applied
  earlier discards the `(ok, error)` result from each `pcall`.
- `scripts/lib/save_modules.lua:948` — rollback of the component whose forward
  apply failed also discards its `pcall` result.
- `scripts/lib/save_modules.lua:954` — after those unchecked calls, the raised
  diagnostic unconditionally claims every component, including the partially
  mutated failing component, was rolled back.
- `scripts/lib/save_modules.lua:968` — reset-hook failure uses the same
  unchecked rollback helper and therefore has the same failure mode.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:432` — an `applyAll` failure
  prevents `WorldLoadPublish`, marks the load failed, and clears the staged
  Haskell session; no corresponding check can detect that Lua rollback itself
  failed.
- `src/Engine/Save/Barrier.hs:149` — marking the barrier failed is terminal;
  `captureLocked` then becomes false at line 168, so owner threads are no longer
  held at the transaction boundary while the mixed Lua state remains live.
- `test-headless/Test/Headless/Lua/SaveModules.hs:715` and
  `test-headless/Test/Headless/Lua/SaveModules.hs:1114` — current crash and
  rollback regressions arrange for rollback-time `apply()` to succeed; neither
  covers a rollback callback that throws.

The isolated reproduction registered two valid components through the public
registry API. Component `a` applied `new-a`; component `b` then applied
`new-b` and threw. During unwind, `b` restored successfully while `a` threw
before restoring. `applyAll()` returned an error claiming it had "rolled back
every already-applied component", but the final live values were
`a = new-a` and `b = old-b`.

**Handoff context:**

- **Current behavior:** A forward-apply/reset-hook failure followed by any
  rollback-apply failure is reported as a cleanly aborted load, while the
  process continues paused with old Haskell state and partially replaced Lua
  singleton state.
- **Expected direction:** A failed rollback must never be treated or reported
  as a restored old session. The load boundary should either make restoration
  reliable without depending on fallible live-state callbacks, or detect the
  failed recovery and keep the engine in an explicit fail-closed state that
  cannot be mistaken for a usable session.
- **Scope and constraints:** Cover rollback of the currently failing
  component, previously applied components in reverse dependency order, and
  the reset-hook failure path. Preserve the original forward failure while
  surfacing all rollback failures, and add a focused double-fault regression
  that verifies the resulting session disposition rather than only the error
  string or registry bookkeeping.
- **Remaining uncertainty:** The reproduction requires two faults: a forward
  apply/reset failure and a rollback callback that also throws. The currently
  registered callbacks are intended to be total for validated snapshots, and
  this audit did not demonstrate an ordinary gameplay save that triggers the
  double fault. The framework nevertheless explicitly handles callback bugs,
  exposes registration to future components, and currently overstates the
  recovery guarantee when that handling itself fails.

### [#1203] SAVE-2. Interrupted recovery-state publication can leave no selectable generation

When a slot's authoritative generation is corrupt but its previous generation
is valid, the loader correctly recovers from the previous generation. A
subsequent save does not preserve that recovery topology: publication checks
only whether the authoritative path exists, stages the valid previous
generation under an internal stale filename, rotates the corrupt authoritative
file into the previous-generation path, and only then publishes the new
candidate.

An interruption after the rotation but before the candidate publication leaves
no generation the loader will select. The authoritative path is missing and the
previous-generation path is corrupt. The valid recovered generation remains
intact under its staged filename, and the validated candidate may remain under
its temporary filename, but both names are deliberately ignored by the loader.
The slot is therefore reported as unloadable despite complete save data still
being present on disk.

**Verification:** Verified — the production publication sequence can create
this state, and the production generation selector rejects a deterministic
reproduction of it.

**Evidence:**

- `src/World/Save/Storage.hs:85` — the storage contract promises that an
  interruption at any publication phase always leaves at least one complete
  generation selectable by `selectLoadGeneration`.
- `src/World/Save/Storage.hs:105` — staged previous-generation and unpublished
  candidate files are explicitly excluded from generation selection.
- `src/World/Save/Storage.hs:556` — `publishValidated` decides whether to rotate
  solely from the existence of the authoritative file; it does not distinguish
  a valid authoritative generation from a corrupt one whose previous
  generation is the currently selected recovery source.
- `src/World/Save/Storage.hs:560` — when the corrupt authoritative path exists,
  the valid previous generation is first moved to a `world-synworld-stale*`
  path.
- `src/World/Save/Storage.hs:575` — the corrupt authoritative generation is
  then renamed into `world.synworld.prev` and the directory is durably synced
  before candidate publication begins.
- `src/World/Save/Storage.hs:581` — only after that durable intermediate state
  does the transaction rename the validated candidate into the authoritative
  path.
- `src/World/Save/Storage.hs:729` — `selectLoadGeneration` consults only
  `world.synworld` and `world.synworld.prev`; it has no recovery path for the
  staged valid generation.
- `test-headless/Test/Headless/World/Save/Storage.hs:580` — existing coverage
  verifies corrupt-authoritative fallback to a valid previous generation.
- `test-headless/Test/Headless/World/Save/Storage.hs:696` — existing
  interruption coverage starts from a valid authoritative generation, so that
  generation remains selectable after staging; it does not compose the
  interruption with the corrupt-authoritative recovery state.
- `test-headless/Test/Headless/World/Save/Storage.hs:469` — the previous-only
  publication regression protects the valid previous generation when the
  authoritative path is absent, but not when that path exists and is corrupt.

The deterministic reproduction first placed a current compatibility fixture at
`world.synworld.prev` with no authoritative file; the selector returned
`Right FromPrevious`. It then reproduced the durable state immediately after
the vulnerable rotation: the same valid fixture under
`world-synworld-stale77777`, a truncated file at `world.synworld.prev`, and no
authoritative file. The selector returned an error saying the authoritative
generation was missing and the previous generation had a truncated header.
The focused `atomic save storage` suite passed all 39 examples, confirming that
the composed recovery/interruption case is not currently covered.

**Handoff context:**

- **Current behavior:** Saving a slot while its valid session is being recovered
  from `.prev` can move that session outside the loader's recognized generation
  names before the new candidate becomes authoritative. A crash or publication
  failure in that window makes the slot unloadable.
- **Expected direction:** Throughout publication, the generation from which the
  slot currently recovers should remain under a recognized, selectable name
  until the new authoritative generation has crossed its durability boundary.
  Every interruption point should satisfy the existing “at least one complete
  and selectable generation” invariant.
- **Scope and constraints:** Preserve the distinction between storage-corrupt
  and semantically incompatible authoritative generations, the refusal to
  discard unknown optional components, symlink containment, atomic same-
  filesystem publication, and stale-artifact cleanup. Add a regression that
  composes corrupt-authoritative fallback with interruption after staging and
  rotation, ideally using the same generation classification as the loader so
  publication and selection cannot drift.
- **Remaining uncertainty:** The reproduction constructed the exact durable
  filesystem topology rather than forcibly terminating a publisher at the
  instruction boundary. The valid bytes are initially stranded rather than
  erased and could be recovered manually, but a later successful publication
  may sweep the staged copy as an owned stale artifact.

### [#1204] SAVE-3. Failing post-load callbacks still report `LoadSucceeded`

The load transaction deliberately postpones its successful terminal status
until Lua's `onSaveLoaded` reconciliation broadcast has run. Callback failures,
however, are caught and reduced to warning logs by the generic broadcast
machinery. The broadcast returns no outcome, and the dispatcher unconditionally
calls `finishLoad`, recording `LoadPublished` with `LoadSucceeded` even when a
callback partially mutated its singleton state and then failed.

This is not merely presentation cleanup. Shipped `onSaveLoaded` callbacks prune
orphaned persistent rows, scrub typed references, reconstruct derived unit
state, normalize the loaded world's pause state, and bind Lua's HUD/world state
to the newly published Haskell session. A failure can therefore leave the
already-published session incompletely reconciled while callers are told the
load completed successfully.

**Verification:** Verified — the production dispatcher reports success after a
real registered module's `onSaveLoaded` callback throws.

**Evidence:**

- `src/World/Thread/Command/Save.hs:97` — Haskell publication deliberately does
  not call `finishLoad`; successful status is deferred until the Lua
  reconciliation broadcast completes.
- `src/World/Thread/Helpers.hs:24` — `LuaSaveLoaded` exists so per-entity Lua
  singleton state can reconcile after every live Haskell reference has already
  been replaced.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:287` — the final load handoff
  broadcasts `onSaveLoaded` to every registered Lua module.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:294` — the dispatcher relies on
  the broadcast never throwing because each callback is `pcall`-guarded.
- `src/Engine/Scripting/Lua/Thread/Dispatch.hs:297` — `finishLoad` runs
  unconditionally after that error-swallowing broadcast.
- `src/Engine/Scripting/Lua/Util.hs:67` — `broadcastToModules` returns `IO ()`
  and has no channel through which any callback failure can reach its caller.
- `src/Engine/Scripting/Lua/Script.hs:45` — `callModuleFunction` catches callback
  errors, logs a warning, and returns `IO ()`; the module and callback identity
  are not returned to the transaction.
- `src/Engine/Load/Status.hs:127` — `finishLoad` records `LoadPublished` and
  `LoadSucceeded`, ending the in-progress transaction.
- `scripts/unit_ai.lua:390` — `unitAi.onSaveLoaded` performs the orphan prune
  and nested-reference scrub that apply-time persistence ownership cannot do.
- `scripts/building_spawn.lua:498` — `buildingSpawn.onSaveLoaded` similarly
  prunes restored rows and removes stale unit references.
- `scripts/unit_resources.lua:164` — `unitResources.onSaveLoaded` reconstructs
  derived body statistics needed by strength and starvation behavior.
- `scripts/ui_manager_menu.lua:146` — `uiManager.onSaveLoaded` is the common
  path that rebinds Lua's world and HUD identifiers after every kind of load;
  without it, later gameplay actions can target a page from the replaced
  session.
- `test-headless/Test/Headless/Lua/DebugQueue.hs:35` — the focused
  `LuaSaveLoaded` dispatcher coverage intentionally registers zero scripts, so
  its reconciliation broadcast is a no-op and cannot exercise callback
  failure.

The isolated reproduction created a real Lua backend with the full API,
registered one module whose `onSaveLoaded` callback set a marker and then
raised an intentional error, began a load status transaction, and delivered
`LuaSaveLoaded` through the production `processLuaMsg` dispatcher. The warning
was logged, the marker proved the failing callback ran, and the final status
was:

`LoadPublished`, `Just LoadSucceeded`, `lsFailedAtPhase = Nothing`.

The focused `LuaSaveLoaded stale debug-command cancellation` describe also
passed all three existing examples; those tests cover queue quarantine but use
the scriptless backend described above.

**Handoff context:**

- **Current behavior:** Once Haskell publication has occurred, every
  `onSaveLoaded` callback is attempted under `pcall`. Any failure is logged and
  discarded, remaining modules continue, and the transaction becomes a clean
  `LoadSucceeded` with no durable indication that reconciliation was incomplete.
- **Expected direction:** The terminal result should distinguish a fully
  reconciled successful load from a published session whose required post-load
  reconciliation failed. A callback failure must remain observable after the
  warning log and must not be represented as an unqualified success.
- **Scope and constraints:** Preserve callback isolation so one Lua error does
  not crash the Lua thread or prevent independent callbacks from being
  attempted. Aggregate enough callback identity and error information for the
  load lifecycle to choose an explicit degraded or fail-closed result. Add a
  dispatcher-level regression with a module that mutates and then throws, and
  define whether subsequent callbacks still run.
- **Remaining uncertainty:** No ordinary gameplay save was found that makes a
  shipped callback throw; they are intended to be total. Haskell publication
  and barrier release have already occurred when these callbacks run, so
  rolling back to the old session may no longer be possible. The appropriate
  terminal status and recovery behavior therefore require a design decision,
  especially because the current broadcast does not distinguish
  correctness-critical reconciliation from presentation-only cleanup.
