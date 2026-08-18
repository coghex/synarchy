# Lua script audit findings

This report records defects and maintainability risks verified in the Lua
scripts under `scripts/`. It is an audit and handoff document only; it does not
change implementation or create tracker issues.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The audit inventoried all 194 Lua files under `scripts/`—67,312 lines—and
reviewed the project's Lua module-loading, UI lifecycle, UTF-8, persistence,
save-reconciliation, utility-AI, and randomness contracts. Higher-risk source
paths were traced into their Haskell-facing APIs, current content data, focused
tests, and behavior probes.

Every Lua source passed `luac -p`. `tools/lua_duplicate_function_audit.py`
passed, as did `tools/lua_module_budget.py`. A focused run of

`cabal test synarchy-test-headless --test-options='--match "shell debug console adopts the shared resize/scale contract"'`

passed all six examples. That suite is relevant to LUA-1 because its fixture
loads the shell through `require` before loading the settings menu, which is the
reverse of production and therefore masks the split module identity.

Isolated Lua reproductions additionally confirmed:

- `dofile("scripts/shell.lua")` followed by `require("scripts.shell")` returns
  two distinct tables.
- A capacity-limited worker repeatedly releases the same bronze-smelting bill
  after retaining only part of its ingredients; three attempts produced no
  craft progress.
- `unitAi.onSaveLoaded({}, {})` leaves a pre-existing repair-priority flag
  intact.
- Generating one eight-digit RandBox seed advances the same RNG stream used by
  gameplay.
- Fifty fresh Lua processes produced all six permutations of a three-part
  sever narration supplied in one fixed encounter order.

No graphical or offscreen session, full test suite, complete probe sweep,
world check, or `make ci` was run. The capacity and save-reconciliation
reproductions used production Lua modules with controlled API stubs rather
than a standalone game process. No GitHub duplicate search was performed;
that belongs to `process-report`.

Previously resolved UTF-8 wrapping defects and unrestricted module-size
concerns were not re-filed: the current shared UTF-8 helpers are present, the
syntax and duplicate-function checks pass, and the repository explicitly
treats Lua module budgets as per-split ratchets rather than a tree-wide size
policy.

## Status

- [x] LUA-1. Settings rescale a disconnected shell module instance in production — [#1325]
- [x] LUA-2. Crafting and structure construction re-claim jobs whose input loads cannot fit — [#1326]
- [x] LUA-3. Unit-AI coordination tables leak across replacement save loads — [#1329]
- [x] LUA-4. The RandBox widget owns and advances gameplay’s global Lua RNG — [#1330]
- [x] LUA-5. Multi-part sever narration has process-dependent ordering — [#1331]

---

## Module identity and lifecycle

### [#1325] LUA-1. Settings rescale a disconnected shell module instance in production

Production first loads `scripts/shell.lua` through `engine.loadScript`, which
executes it with `dofile`. The shell does not register the returned table in
`package.loaded`. Much later, loading `ui_manager.lua` requires
`settings_menu.lua`, which in turn requires `scripts.shell`. Because the first
load never populated `package.loaded`, this executes the shell a second time
and gives settings a disconnected table with its own private UI state.

The engine ticks and broadcasts framebuffer events to the first table, while
Settings Apply, Save, Defaults, and Back call the second table’s resize
handler. A scale-only change therefore updates the disconnected copy rather
than the visible shell.

**Verification:** Verified. A production-order Lua reproduction returned
`false` for equality between the table returned by `dofile("scripts/shell.lua")`
and the subsequent `require("scripts.shell")`. A scan of modules used through
both `loadScript` and `require` found the shell was the only intersecting module
that did not self-register.

**Evidence:**

- `scripts/init_loader.lua:83` — loads the live shell with
  `engine.loadScript("scripts/shell.lua", 0.5)`.
- `scripts/init_loader.lua:274` — loads `ui_manager.lua` last, after the shell.
- `src/Engine/Scripting/Lua/API/Core.hs:359` — `engine.loadScript` calls
  `loadModuleRef`.
- `src/Engine/Scripting/Lua/Script.hs:16-38` — `loadModuleRef` executes the
  file with `Lua.dofileTrace`; it does not populate `package.loaded`.
- `scripts/shell.lua:10` — creates a fresh `local shell = {}` without the
  singleton registration used by other dual-loaded modules.
- `scripts/ui_manager.lua:3-9` — documents this exact `dofile`/`require`
  identity hazard and self-registers its own singleton at lines 27-28.
- `scripts/ui_manager_boot.lua:37` — requires `settings_menu`.
- `scripts/settings_menu.lua:21` — requires `scripts.shell`, creating the
  second table in production.
- `scripts/settings_menu.lua:155-180` and `:963-1020` — Defaults, Apply, Save,
  and Back call that table’s `onFramebufferResize`.
- `scripts/shell.lua:1051-1121` — the resize and rescale methods mutate
  table-private upvalues, so calling them on the second instance cannot update
  the live one.
- `scripts/unit_ai.lua:68-69`, `scripts/debug.lua:43-44`, and
  `scripts/unit_drag_select.lua:24-25` — established self-registration
  precedents for modules reached through both loading mechanisms.
- `test-headless/Test/Headless/UI/ResponsiveMenus.hs:1305-1443` — the current
  regressions require and initialize the shell before requiring settings,
  producing one shared `require` instance and missing production ordering.

**Handoff context:**

- **Current behavior:** Real framebuffer broadcasts reach the live shell, but
  settings-driven scale-only notifications reach a disconnected copy. An open
  shell can retain stale geometry until another real resize or reopen.
- **Expected direction:** Give the shell one canonical module identity across
  `loadScript` and `require`, following the established self-registration
  convention, and add a regression that reproduces production load order.
- **Scope and constraints:** Preserve the deliberate rule that real framebuffer
  resizes reach the shell exactly once and are not also routed through
  `responsive.notifyResize`.
- **Remaining uncertainty:** No graphical session was used to inspect the stale
  geometry visually. The two-table identity and incorrect call routing are
  deterministic.

## Gameplay work coordination

### [#1326] LUA-2. Crafting and structure construction re-claim jobs whose input loads cannot fit

Craft and structure-construction preflight checks establish that every required
material exists somewhere, but do not establish that the worker can carry all
inputs simultaneously. Their shared fetch loops stop at capacity and remove the
remaining request from the work plan. The post-fetch reconciliation then
releases the job, leaving already-fetched ingredients in the worker’s inventory.
On the next decision, the same stock-availability check succeeds again and the
worker can immediately reclaim the same infeasible job.

This is directly exposed by shipped bronze recipes. Bronze smelting needs one
10 kg copper chunk, one 10 kg tin chunk, and 5–15 kg of coal: 25–35 kg before
the worker’s existing equipment. The average acolyte capacity is approximately
23 kg and weak rolls approach 11 kg. The job is therefore impossible for a
substantial part of the normal worker distribution even when every ingredient
is available.

Structure construction has the same control flow. Current structure costs are
lighter, but a weak, already-equipped worker can lack enough remaining
headroom for the shipped 8 kg wooden-post material and repeatedly release and
reclaim that designation.

**Verification:** Verified for crafting with the production
`unit_ai_craft.lua` and `unit_ai_fetch.lua` modules under controlled API stubs.
A 20 kg-capacity worker assigned a bronze recipe repeatedly fetched two 10 kg
ores, failed to fetch coal, released the bill, and reclaimed it. After three
attempts the bill had made zero progress and the worker remained stuck carrying
the partial input load. The construction branch was verified by source tracing
through the same capacity and reconciliation helpers, not through a live-engine
probe.

**Evidence:**

- `scripts/unit_ai_craft.lua:187-210` — `craftMaterialsAvailable` checks
  inventory and source counts per material but never sums required weight or
  compares it with remaining capacity.
- `scripts/unit_ai_craft.lua:214-247` — that incomplete predicate determines
  whether a bill is worth claiming.
- `scripts/unit_ai_craft.lua:303-350` — the worker claims the bill and plans
  every shortfall at once.
- `scripts/unit_ai_fetch.lua:96-145` — ground fetching removes a requested
  material when capacity prevents pickup.
- `scripts/unit_ai_fetch.lua:147-186` — mule fetching likewise clears each
  request even after reaching capacity.
- `scripts/unit_ai_craft.lua:127-185` — cargo fetching has the same behavior.
- `scripts/unit_ai_craft.lua:390-395` — any remaining shortfall releases the
  bill without returning or staging already-fetched ingredients.
- `scripts/unit_ai_construct.lua:133-149` — structure preflight checks material
  counts but not carrying feasibility.
- `scripts/unit_ai_construct.lua:319-345` and `:380-397` — construction plans
  all inputs, uses the same fetch helpers, and releases the designation after a
  capacity shortfall.
- `scripts/unit_ai_repair.lua:225-241` — the repair AI already sums the target
  item and consumable weights before claiming, explicitly to prevent this
  claim/fail/reclaim loop.
- `data/recipes/smelting.yaml:60-98` — the three shipped bronze recipes require
  copper ore, tin ore, and coal.
- `data/items/ore_chunks.yaml:6-23` — copper and tin chunks weigh 10 kg each.
- `data/items/coal_chunks.yaml:7-24` — coal chunks weigh 5 kg each.
- `src/Unit/Thread/Command/Body.hs:182-195` — documents approximately 23 kg
  average and 11 kg weakest-roll capacities.
- `tools/craft_bill_probe.py:76-105` — the current bill probe uses only one
  lightweight input and does not cover combined-input capacity.

**Handoff context:**

- **Current behavior:** A worker can monopolize and churn a bill or
  construction designation it cannot complete, while retaining partial
  ingredients and making no progress.
- **Expected direction:** Reject infeasible candidates before claiming by
  comparing current carried weight plus the aggregate missing-input weight
  against capacity. If multi-trip staging is desired, it needs an explicit
  destination and durable staged-material model instead of relying on worker
  inventory.
- **Scope and constraints:** Count inputs already held by the worker only once;
  include recipe fuel; preserve the existing race revalidation; and cover
  ground, mule, and cargo sources. Add regressions using a shipped multi-input
  bronze recipe and an under-capacity structure worker.
- **Remaining uncertainty:** The live-engine frequency of repeated claims and
  which worker wins when several acolytes compete were not measured. The
  shipped bronze weight incompatibility and the single-worker loop were
  reproduced directly.

## Save/load reconciliation

### [#1329] LUA-3. Unit-AI coordination tables leak across replacement save loads

Several unit-AI modules keep coordination state outside the persisted
`aiState`: dig, chop, construction, and repair claim tables, plus the
player-facing repair-priority table. These are module-local upvalues and survive
an in-process load. `unitAi.onSaveLoaded` replaces and reconciles `aiState`, but
does not clear or reconstruct any of these side tables.

Loading is a complete session replacement and can rewind game time and reuse
unit or item-instance IDs. A stale entry can therefore attach to an unrelated
entity in the loaded session. Because claim expiry uses `now - claim.at`, a
claim timestamp from a later session time can also remain unexpired until the
loaded clock catches up. Repair priority has no timeout at all and can visibly
mark and preferentially select an unrelated degraded item with the same
instance ID.

**Verification:** Partially reproduced and otherwise verified by source
tracing. After setting repair priority for instance 42, calling
`unitAi.onSaveLoaded({}, {})` left `unitAi.isRepairPriority(42)` true. The claim
tables are inaccessible outside their module closures, so their collision
impact was not reproduced through the public API.

**Evidence:**

- `docs/persistence_contract.md:30-34` — loading replaces the entire session
  rather than merging into existing state.
- `scripts/unit_ai_repair.lua:43-57` — `repairClaims` and `repairPriority` are
  module-local tables outside `aiState`.
- `scripts/unit_ai_repair.lua:59-74` — public UI queries read those tables
  directly.
- `scripts/unit_ai_repair.lua:77-84` — claim expiry depends on game time and
  raw unit-ID existence.
- `scripts/unit_ai_repair.lua:158-179` — priority changes candidate selection
  solely by item-instance ID.
- `scripts/unit_ai_dig.lua:24-35`,
  `scripts/unit_ai_chop.lua:25-36`, and
  `scripts/unit_ai_construct.lua:45-56` — equivalent module-local claim tables
  are keyed by coordinates and store raw unit IDs and timestamps.
- `scripts/unit_ai_construct.lua:14-19` — the implementation assumes a loaded
  save arrives with no registry entry, which is true after a fresh Lua process
  but false for an in-process replacement load.
- `scripts/unit_ai.lua:414-465` — `onSaveLoaded` rebuilds `aiState` and scrubs
  its nested references but never reaches the module-local tables.
- `scripts/unit_resources.lua:57-71` and
  `scripts/unit_resource_alerts.lua:148-155` — an existing reset-hook pattern
  clears another transient per-unit upvalue specifically because loads can
  reuse IDs.
- `docs/persistence_state_inventory.md:441-446` — the registry documents
  `aiState` but does not classify the unit-AI claim or repair-priority side
  tables.

**Handoff context:**

- **Current behavior:** In-process loads can inherit coordination and UI intent
  from the replaced session. Reused IDs or coordinates can cause false repair
  badges/priorities, temporary or long-lived claim blocking, and misleading
  claimant identities.
- **Expected direction:** Explicitly classify each side table. Transient claims
  should be cleared or rebuilt from restored jobs at the load boundary.
  Repair priority should either be persisted as deliberate player intent with
  validated item references or reset as transient state.
- **Scope and constraints:** Preserve table identity where closures capture an
  upvalue; account for restored in-progress jobs; and test loading session B
  after session A in the same Lua state with colliding unit, item, coordinate,
  and game-time values.
- **Remaining uncertainty:** Only repair-priority retention was exercised
  directly. The precise duration and gameplay impact of stale claims depend on
  ID collisions, loaded game time, and whether a restored worker refreshes the
  same job.

## Randomness ownership

### [#1330] LUA-4. The RandBox widget owns and advances gameplay’s global Lua RNG

`randbox.init` contains the only production call to `math.randomseed`, so a UI
widget owns initialization of Lua’s process-global random stream. Generating an
eight-digit seed consumes eight draws from that same stream. Gameplay systems
use the stream for AI decision jitter, thoughts, mental breaks, combat lunges,
animal movement, and other simulation choices.

Consequently, opening the normal UI establishes gameplay entropy, and clicking
a seed-randomize control changes later gameplay outcomes. This does not violate
the persistence contract’s explicit rejection of deterministic replay, but it
is hidden cross-domain coupling: a presentation action changes the sequence of
simulation decisions without any gameplay-level relationship between them.

**Verification:** Verified. With a fixed `math.randomseed(123456)`, the next
draw was `0.56515718063893483`. Reseeding identically, calling
`randbox.newHexSeed()` once, and taking the next draw produced seed `96F17EDB`
and value `0.65829285845522778`.

**Evidence:**

- `scripts/ui/randbox.lua:62-69` — a hexadecimal seed consumes eight calls to
  `math.random`.
- `scripts/ui/randbox.lua:144-157` — widget initialization calls
  `math.randomseed(os.time())`.
- `scripts/ui_manager_boot.lua:78-87` — normal UI-manager initialization
  unconditionally initializes RandBox.
- A complete source search found no other production `math.randomseed` call.
- `scripts/unit_ai_core.lua:105-114` — AI decision cadence consumes the global
  stream.
- `scripts/thoughts.lua:120-155` — thought scheduling and weighted selection
  consume it.
- `scripts/mental_state.lua:132-149` and `:281-292` — episode durations,
  behavior selection, breaks, and euphoria consume it.
- `scripts/unit_ai_combat_attack.lua:176-184` — lunge decisions consume it.
- Additional consumers exist in bear, squirrel, needs, sleep, water-search,
  and location-scatter scripts.
- `test-headless/Test/Headless/UI/CreateWorldControls.hs:329-331` — current
  coverage acknowledges hexadecimal seeds use `math.random` but does not test
  stream isolation.

**Handoff context:**

- **Current behavior:** UI initialization seeds gameplay randomness, and UI
  randomization consumes gameplay draws.
- **Expected direction:** Give RandBox an independent random source or
  local PRNG, and seed the gameplay stream at an explicit gameplay-owned
  lifecycle boundary.
- **Scope and constraints:** Preserve the documented non-deterministic replay
  policy. The goal is ownership and stream isolation, not making simulation
  replayable. Add a regression showing that generating a UI seed does not
  change the next gameplay draw.
- **Remaining uncertainty:** No player-visible failure is promised for a
  particular random outcome, and the project does not currently state a formal
  UI/gameplay RNG-isolation contract. This is a verified coupling and
  maintainability risk rather than a deterministic gameplay failure.

## Presentation stability

### [#1331] LUA-5. Multi-part sever narration has process-dependent ordering

The combat injury sentence builder detects severed subparts by inserting them
into a set-like Lua table, then converts that table to display text with
`pairs`. Lua does not define `pairs` iteration order, so a single fixed injury
payload can describe simultaneously severed parts in different orders across
processes. The rest of the function deliberately preserves encounter order for
layer clauses, making the sever branch an inconsistent exception.

**Verification:** Verified. Fifty fresh Lua processes received the same ordered
three-part detail list. The resulting “slicing off…” clause produced all six
possible permutations.

**Evidence:**

- `scripts/injury_log.lua:122-132` — walks the ordered detail array but records
  severed parts only in a keyed table.
- `scripts/injury_log.lua:180-187` — builds the visible sever list with
  `pairs(severed)`.
- `scripts/injury_log.lua:188-199` — explicitly preserves encounter order for
  the following layer clauses.
- `scripts/injury_log.lua:310-334` — the resulting clause is incorporated into
  the player-visible hit sentence.
- `scripts/combat_log.lua:552-557` — combat hit events display that sentence.
- `tools/injury_log_probe.py:1-20` — the current probe covers engine event
  plumbing, not narration ordering.

**Handoff context:**

- **Current behavior:** Equivalent combat events can produce differently
  ordered sever lists between process runs.
- **Expected direction:** Retain a first-seen array alongside the membership
  set, or deliberately sort severed parts by a documented anatomical order.
- **Scope and constraints:** Continue deduplicating repeated structural-layer
  entries and suppressing layers implied by a sever. Add a pure Lua regression
  with multiple simultaneous severed parts.
- **Remaining uncertainty:** The defect affects narration order only; the set
  of reported injuries and simulation state remain unchanged.
