# Project Assessment — July 2026

## Overall assessment

Synarchy is an unusually capable and well-instrumented engine prototype,
but it is not yet a cohesive, trustworthy game platform. Its strongest
engineering — world generation, observability, regression tooling, and
rendering — is ahead of both its architecture and its validated player
experience.

Blunt assessment:

- Engine/world-generation sophistication: strong
- Debuggability and pure-logic testing: very strong
- Runtime architecture and ownership: fragile
- Gameplay integration confidence: weak-to-moderate
- Player-facing completeness: early
- Release readiness: low

The main danger is continuing to add systems. The project needs
consolidation: prove that existing systems form a compelling, reliable
30–60 minute colony loop before expanding simulation depth.

## What is genuinely good

The project has excellent observability. Headless mode, TCP scripting,
deterministic dumps, tracked world baselines, scenario probes, configurable
test tiers, and the screenshot-based playtest harness are better than what
many mature game projects possess.

The renderer also shows sound thinking: stable bindless handles,
static/dynamic quad separation, cached world rendering, GPU-less headless
execution, explicit frame synchronization, and careful sprite-depth handling.
World generation is both ambitious and empirically guarded.

The code is heavily documented, with decisions and historical failure modes
recorded rather than rediscovered. Recent Haskell and Lua file splitting has
materially improved navigability.

Validation run during this assessment was green:

- 776 headless Hspec examples passed, with one conditional pending case.
- All 35 world-audit test groups passed.
- The six-case quick world baseline passed.
- Lua module-budget checks passed.
- The offline naive-player harness self-test passed.

Those are real strengths. The criticism below is about what those tests do
not cover.

## Biggest deficiencies

### 1. Player-facing controls promise behavior that does not exist

The world-creation screen exposes eight detailed climate controls: simulation
iterations, Coriolis, wind drag, thermal inertia, orographic effects,
evaporation, albedo feedback, and thermohaline thresholds. Their tooltips
explicitly promise effects such as gyres and deep-ocean circulation.

Those parameters are parsed and persisted but never consumed by climate
generation. The resulting climate state always leaves deep water, currents,
thermohaline cells, atmospheric circulation, and surface budgets empty.

The same problem affects hours per day, minutes per hour, axial tilt,
day/night ratio, lunar-cycle length, and initial moon phase. The clock still
hardcodes a 24×60-minute day, while the sun and moon configurations have no
runtime consumers.

This undermines player trust and exposes a testing blind spot: baseline tests
prove that defaults are deterministic, but not that configuration knobs are
live.

Immediate action: hide every nonfunctional control or implement it. Add
configuration-liveness tests that generate two worlds differing in one
parameter and assert an appropriate output difference.

### 2. The architecture has file boundaries but weak ownership boundaries

`Engine.Core.State` is effectively a service locator with 73 fields, imported
by 209 modules. More importantly, the nominal engine layer directly imports
units, buildings, crafting, infections, locations, world state, and other
game domains. The engine and game therefore are not actually layered; the
dependency direction runs both ways.

Each world adds another 38 independently mutable references. Lua, world,
unit, combat, render, and save/load code can directly reach many of them.

That means the threads are not truly actors. They have command queues, but
queues are optional because shared state remains globally reachable.
`atomicModifyIORef'` protects individual updates; it cannot provide coherent
transactions across unit state, movement state, buildings, world state, and
Lua state.

The save path demonstrates the result. Its comment says pausing stops the
world and unit threads before serialization, but the unit thread still
processes unit and building commands while paused. Load code explicitly
acknowledges a window where unit instances and simulation state disagree.

Recommended direction:

- Split `EngineEnv` into narrow capability records.
- Give each mutable manager one authoritative owner.
- Route writes through typed commands; publish immutable read snapshots.
- Introduce an acknowledged simulation/save barrier.
- Separate real-time UI scheduling from deterministic simulation-time
  scheduling.
- Enforce that `Engine.*` cannot import gameplay namespaces.

Do this incrementally. Unit/combat ownership and saving are the best first
seam.

### 3. The test inventory overstates gameplay confidence

The Hspec suite is strong for pure logic, world generation, serialization
mechanics, rendering math, power calculations, and pathing. It is much weaker
around the actual Haskell↔Lua↔threaded gameplay loop.

There are 40 registered behavior probes, but only four are CI-eligible. Seven
are explicitly known to fail on current master, and four more are classified
as flaky. The base-failing set includes construction, combat animation,
command priority, location content, Lua save reconciliation, physiology, and
repair AI — important player-facing systems.

The honest statement is: core algorithms are well tested, while integrated
gameplay is not reliably green.

Priorities:

1. Fix or retire every base-failing probe.
2. Make cheap arena probes deterministic.
3. Gate one complete vertical scenario: spawn colony → designate work →
   source materials → build → craft → save/reload.
4. Add isolated Lua tests using a fake engine API. Currently Lua behavior is
   largely tested only by expensive full-engine probes.
5. Run a graphical smoke test in an appropriate GPU environment; CI currently
   builds but does not execute the graphical suite.

### 4. Persistence is structurally brittle

The project is already at save version 81. Saves are positionally encoded,
and the loader rejects any version other than the current one.
`WorldPageSave` is a large cross-domain record, and Lua state is stored as
opaque module blobs.

This is manageable during private pre-alpha development, but it must change
before players invest meaningful time. Almost any gameplay evolution
currently invalidates every save.

The Lua side is especially fragile: entity references are raw IDs, and each
new AI field containing an entity ID must be manually added to stale-reference
scrubbing. That is an easy future corruption bug.

Move toward:

- Tagged, sectioned save components.
- Per-component versions and defaults.
- Explicit migrations with golden old-save fixtures.
- Stable entity references containing world, entity kind, ID, and generation.
- A consistent snapshot barrier across Haskell and Lua state.

### 5. Lua is becoming both the orchestration layer and hot simulation loop

The Lua API implementation is about 21,500 Haskell lines with roughly 500
manual function registrations. Normal startup installs 28 long-lived scripts,
many based on periodic polling. Both physiology and utility AI enumerate
every unit and make repeated fine-grained Haskell calls every 0.1 seconds.

That is acceptable for five acolytes. It is a poor scaling model for a colony
simulation.

Keep Lua for policy, content, and high-level AI, but provide:

- Bulk per-tick unit snapshots.
- Batched commands and stat mutations.
- Event subscriptions instead of panel polling.
- Explicit simulation-time versus real-time script domains.
- Haskell implementations for high-frequency physiology and bookkeeping if
  colony size grows.

The repeated `package.loaded` self-registration pattern is also a loader
design smell. `engine.loadScript` should canonicalize module identity itself
rather than requiring every important Lua module to implement singleton
mechanics.

### 6. The project is systems-rich but game-thin

The repository contains roughly 98,000 Haskell source lines and 51,000 Lua
lines, but currently has four unit definitions, eight building definitions,
24 recipe entries, and one location. The player manual explicitly describes
no victory condition and no autosave.

A sandbox does not require a scripted victory, but it does require a
progression curve, meaningful medium-term decisions, recovery arcs, and
player feedback. The naive-player harness is promising, but it currently
records sessions without the planned critic, and there are no tracked
playtest results to demonstrate a closed UX loop.

Before adding another deep simulation subsystem, define one intended session:

- Player creates a world and settles successfully.
- Water and food pressure become understandable.
- The colony establishes renewable food.
- It builds and powers a workshop.
- It survives a threat or injury.
- The player reaches a clear milestone within 30–60 minutes.

Instrument completion time, confusion points, deaths, idle time, failed
designations, and abandonment. Let those results determine which simulation
work matters.

## Lower-priority release concerns

- The claimed legacy texture path is explicitly unimplemented.
- Runtime resources are listed as `extra-source-files`, not installed
  application data; distribution still assumes a source checkout/resource
  root.
- There is no frozen dependency plan, while most dependencies have no upper
  bounds.
- The changelog and package version do not reflect the current project.
- The all-exposed, single-library structure and broad `World.Types` re-export
  weaken dependency control and increase recompilation.

## Recommended order of work

1. Restore player-facing truth: hide or implement dead controls; add liveness
   tests.
2. Make gameplay master green: eliminate the seven base-failing probes and
   gate a vertical colony scenario.
3. Establish state ownership: narrow capabilities, typed writers, immutable
   snapshots, deterministic clock domains, and a save barrier.
4. Run UX-driven vertical-slice work: finish the playtest critic, onboarding,
   autosave, progression milestone, and feedback quality.
5. Replace brittle persistence: tagged components and migrations before
   external playtime becomes valuable.
6. Then expand content and simulation.
7. Finally harden packaging, portability, and dependency reproducibility.

The project has an excellent foundation for investigation and iteration. Its
next major gain will not come from another subsystem; it will come from making
the existing ones form one authoritative architecture and one demonstrably
enjoyable game loop.
