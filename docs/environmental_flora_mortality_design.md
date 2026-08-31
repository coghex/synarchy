# Environmental flora mortality and texture fallback design

This design establishes the foundation for flora to retain persistent
conditions such as death from age, drought, frost, fire, disease, or direct
damage, while rendering correctly from whatever subset of art a species has.
It also makes wild-versus-cultivated presentation an explicit part of flora
rendering so a species such as wheat can look bushy in the wild and field-like
when deliberately planted without requiring every possible texture
combination up front.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [ ] EPIC. Make flora condition persistent and texture fallback expressive
- [ ] EFM-1. Publish the canonical flora visual-state and fallback contract
- [ ] EFM-2. Load and audit sparse flora visual and corpse-policy declarations
- [ ] EFM-3. Resolve flora textures through the canonical fallback lattice
- [ ] EFM-4. Give flora occurrences stable identity and explicit render context
- [ ] EFM-5. Persist wild and row-flora condition records across regeneration and saves
- [ ] EFM-6. Persist groundcover-crop condition records through the same model
- [ ] EFM-7. Expose and render persistent flora conditions without hazard producers
- [ ] EFM-10. Apply corpse retention and successor policy
- [ ] EFM-8. Create an approved pilot mortality texture set
- [ ] EFM-9. Demonstrate the complete condition and fallback path on one pilot species

## Epic contract

- **Goal:** Any wild or cultivated flora occurrence can carry a persistent
  semantic condition, including a death cause and its visual state at the time
  of death, and can always resolve a valid texture through one deterministic,
  documented sparse fallback system.
- **Done when:** The canonical selector axes and fallback order are documented;
  optional variants load and validate without requiring a Cartesian product of
  assets; wild flora, planted row flora, and groundcover crop plots retain
  condition state through chunk regeneration and save/load; corpse visibility
  and the successor state follow an explicit content policy; one pilot species
  proves exact and fallback rendering headlessly and in the real preview; and
  a species with only its base texture still renders in every representable
  state.
- **Users and operators:** Players observing a living world, content authors
  adding flora art and YAML, and maintainers adding later weather, fire,
  disease, or damage producers.
- **Arc label:** `flora` proposed. The existing `farming` label is too narrow
  for wild environmental mortality.

## Current state and evidence

### Growth and mortality

- `World.Flora.Types.LifePhaseTag` is one ordered axis containing both
  `PhaseSprout` and `PhaseDead`. A flora occurrence cannot currently be both
  dead and remembered as a sprout.
- `World.Flora.Growth.floraGrowth` is derived from placement age, static
  placement-time habitat health, and the world clock. It writes no per-instance
  state. Habitat health only scales growth between 25% and 100%; even health
  zero does not kill a plant.
- Annuals, biennials, and perennials die only at their deterministic lifespan
  boundary. They display the authored `PhaseDead` texture for a fixed 60-day
  window and then wrap to a new sprout generation. Evergreens never die.
- That 60-day window is currently a universal constant, not species data. It
  couples two distinct behaviors: how long a corpse is visible and the fact
  that the same deterministic site immediately starts another generation.
- `Perennial` stores `lcDeathChance`, and YAML loads `deathChance`, but the
  lifespan calculation currently ignores that value.
- No current drought, frost, fire, disease, or damage producer can create a
  persistent flora death. Repository search found no existing producer path to
  preserve or extend.

### Texture resolution and assets

- `World.Flora.Render.resolveSpeciesTexture` selects one age-driven life phase,
  then one annual stage, with an optional exact `(phase, annualStage)` override.
  The current table has no render context, condition, cause, or phase-at-death
  axis.
- `Engine.Scripting.Lua.API.YamlTextures.registerFloraSpecies` loads every
  explicitly declared phase, annual stage, and cycle override. A declared path
  missing from disk resolves to `unknown_flora.png`; that is an error fallback,
  not a semantic fallback from a specific state to a less-specific valid
  texture.
- `tools/texture_subset_audit.py` reconstructs all explicitly declared flora
  paths and rejects missing files. It does not discover undeclared files by
  filename.
- Thirteen shipped texture families have juvenile art. None has a dead-juvenile
  texture. Nine have one generic `dead.png`; coconut palm, red mangrove, Scots
  pine, and white spruce have no dead art. Juvenile canvases range from 4×4 to
  128×128, so one shared dead-sprout bitmap is not a viable substitute.
- Saguaro issue #1688 and PR #1725 add a 48×48 sprout, mature seasonal variants,
  and one adult dead skeleton, but deliberately do not add dead-juvenile or
  cause-specific art.

### Wild and cultivated flora

- Closed farming issue #334 deliberately made row crops ordinary
  `FloraInstance`s and groundcover crops `CropPlot`s so both reuse the same
  species growth and texture resolver as wild flora.
- `data/flora/crops.yaml` currently gives tomato and wheat density zero and
  points them at the red-raspberry and white-clover texture directories as
  placeholders. There is no cultivated texture override or current
  wild-versus-cultivated render selector.
- A planted groundcover tile is recognizable as cultivated because it exists in
  the tile-keyed `CropPlots` map. A row crop becomes an ordinary
  `FloraInstance` reconstructed from `WePlaceFlora`; its render input carries no
  explicit origin/context field. Using negative age or placement category as an
  implicit context would be brittle and is not a contract.
- The earlier design idea that wild and cultivated plants share one flora model
  did land. Context-sensitive art fallback did not.
- YAML's top-level `type` is decoded but is not retained on `FloraSpecies`.
  `worldGen.category` is retained, but today it controls placement shape and
  crop eligibility through free-text values such as `tree`, `bush`,
  `wildflower`, `row_crop`, and `groundcover_crop`. Neither field is currently
  a validated structural or corpse-retention classification.

### Persistence and regeneration

- Naturally placed flora is deterministic derived chunk content. Chunk
  regeneration recreates it rather than restoring mutable instance records.
- Planted row flora survives through the persisted world-edit log and replay of
  `WePlaceFlora`.
- Groundcover crops live in the separate per-page `CropPlots` sparse map, which
  is persisted in the `world-activity` component.
- Existing persistent flora harvest timers also live in per-page sparse state.
  That establishes a repository precedent for mutable overlays on regenerated
  flora, but harvest state is tile-keyed and cannot uniquely address multiple
  wild flora occurrences on one tile.
- Persistence components use frozen DTOs and independent component versions.
  Any new condition store or changed crop record needs an explicit migration,
  inventory update, save-compat coverage, and deterministic canonical
  serialization. Texture handles must never be saved; semantic selectors are
  saved and handles are resolved again after content loading.

### Tracker overlap

- No open or closed issue search matched environmental flora mortality,
  dead-juvenile flora, cause-specific flora death art, or context-sensitive
  wild/cultivated flora textures.
- The readiness recheck on 2026-08-25 again found no overlapping open issue or
  epic across flora mortality, plant death, corpse persistence, dead sprouts,
  charred flora, texture fallback, or cultivated texture searches.
- Closed #332 owns the current shared derived growth runtime; this arc extends
  it with externally caused persistent condition rather than duplicating it.
- Closed #334 owns the two cultivated growth forms; this arc gives those forms
  explicit render context and persistent condition rather than replacing them.
- Saguaro issue #1688 and PR #1725 remain open. They own the base saguaro
  texture set, while this arc owns mortality state, fallback, retention, and
  the later pilot extensions; they are a prerequisite input, not an overlapping
  epic.

## Desired experience

### Sparse art remains useful

A species author may ship only one base texture. That plant must remain
renderable while alive, dead, burned, drought-killed, frozen, wild,
cultivated, juvenile, mature, in season, or out of season. More specific art
improves the result but is never required merely to make a semantic state safe.

When exact art exists, the game uses it. For example:

- a dead flowering plant prefers a flowering-dead asset;
- a dead sprout prefers a dead-sprout asset;
- a fire-killed plant prefers a charred asset suitable for its phase;
- a cultivated wheat plant prefers cultivated field art;
- if a cultivated override is absent, the corresponding wild art is used;
- if condition-specific art is absent, resolution progressively falls back to
  generic dead art and ultimately the species base texture.

The resolver must explain its choice in diagnostics so authors can tell the
difference between a deliberate sparse fallback and a missing/corrupt declared
file.

### Death and corpse retention are durable but distinct

Every death is durable state while it applies. Chunk eviction, deterministic
regeneration, save/load, and restart must not revive a dead occurrence or reset
its corpse clock. That does not mean every corpse is permanent.

Small or herbaceous plants should normally display a corpse for a finite
period—60 game-days is the initial default—and then stop occupying/rendering
the dead occurrence. Structurally persistent remains such as a dead tree may
stay until an explicit future action clears or replaces them. The policy must
be content-driven rather than inferred from texture availability.

Corpse visibility and successor behavior are separate. When a transient corpse
expires, the game must deliberately choose whether that occurrence reseeds,
becomes an absent/tombstoned site, or awaits replanting. Merely deleting its
mortality overlay would make deterministically regenerated wild flora appear
alive again and is therefore not a valid cleanup implementation.

The initial foundation exposes a safe mutation seam and proves persistence,
but does not decide or implement real hazard formulas. Future systems may
accumulate exposure over time—for example, a juvenile plant outside its
temperature tolerance for long enough may die—but those producers consume the
condition API rather than owning a second mortality representation.

### Wild and cultivated presentation share semantics

Wild and cultivated are render contexts, not separate lifecycle engines. They
share phase, season, health, mortality, harvesting, and fallback behavior. A
cultivated-specific texture is an optional override; missing cultivated art
falls back to the equivalent wild state before losing other state fidelity.

This permits wild wheat to use clumped or bushy art while planted wheat uses a
field-like tile fill, without requiring duplicate growth logic or separate
species solely for presentation.

## Scope

### In scope

- A canonical, documented sparse texture-selector schema.
- Deterministic fallback across render context, life phase, annual stage,
  condition, and condition cause.
- Explicit wild and cultivated render context.
- A persistent flora condition model that records enough semantic information
  to reproduce the intended corpse after regeneration and save/load.
- An explicit corpse-retention and successor policy capable of transient
  60-day remains and persistent structural remains.
- Stable occurrence identity for naturally generated flora and planted row
  flora, including multiple occurrences on one tile.
- Equivalent persistent condition support for groundcover `CropPlot`s.
- A mutation/query seam and headless proof that later hazard systems can call.
- One user-approved pilot texture set and one end-to-end pilot species.
- A documented art maturity ladder and inventory for later per-family art
  issues.

### Out of scope

- Temperature, precipitation, soil-moisture, drought, frost, fire spread,
  disease, or direct-damage formulas and balance tuning.
- Weather simulation or a general environmental exposure engine.
- Multi-stage corpse decomposition art, decay simulation, nutrients, salvage,
  or cleanup jobs performed by units.
- Repainting every flora family inside the foundation implementation.
- Replacing existing crop growth forms or making placeholder tomato/wheat art
  permanent.
- Saving texture handles or deriving gameplay state from which optional image
  files happen to exist.

## Design

### Canonical visual selector

The renderer conceptually requests one semantic selector:

| Axis | Initial vocabulary | Notes |
|---|---|---|
| context | `wild`, `cultivated` | Cultivated is an optional presentation override. |
| life phase | existing `LifePhaseTag` values | For death, this is the phase at death. |
| annual stage | existing `AnnualStageTag` values or none | For death, this freezes under D-10. |
| condition | `alive`, `dead` | The first implementation need not add living stress visuals. |
| cause | `natural`, `drought`, `frost`, `fire`, `disease`, `damage`, `unknown`, or none | Cause is meaningful for death; future producers use this vocabulary. |

The runtime selection type is distinct from the YAML representation and from a
texture filename. Rendering asks for semantics; the content layer returns a
handle and a record of which fallback candidate won.

### Sparse declarations

Existing `phases`, `annualCycle`, and `cycleOverrides` remain valid. At load
time they can be normalized into the new selector table as `wild` + `alive`
entries, preserving every shipped species without YAML churn.

The additive schema is an explicit list of selectors rather than automatic
filesystem discovery:

```yaml
textureVariants:
  - context: cultivated
    phase: matured
    stage: flowering
    condition: alive
    texture: "cultivated_matured_flowering.png"
  - phase: sprout
    condition: dead
    texture: "sprout_dead.png"
  - phase: matured
    stage: flowering
    condition: dead
    cause: fire
    texture: "matured_flowering_charred.png"
  - condition: dead
    cause: fire
    texture: "charred.png"
```

Omitted axes are deliberate wildcards, not defaults accidentally inferred from
the filename. Duplicate selectors are rejected. Unknown vocabulary is rejected
rather than ignored. Every declared path participates in the texture-subset
audit. Filenames follow the readable existing order—context prefix when
present, then phase, stage, and condition/cause suffix—but YAML selectors, not
filenames, are authoritative.

### Fallback invariants

The accepted priority is death condition first, life phase second, cause third,
then annual-stage specificity. At each semantic candidate, an exact cultivated
declaration is tried before the equivalent wild declaration. The resolver must
obey these invariants:

1. Resolution is pure, deterministic, finite, and independently testable.
2. An exact declared selector wins.
3. Cultivated requests try the same wild semantic state before discarding
   phase, stage, condition, or cause information.
4. A dead request exhausts valid dead candidates before falling back to living
   art. Looking dead is more important than preserving any other selector.
5. Within dead candidates, phase-appropriate generic-dead art is preferred over
   an adult-shaped cause-specific asset that would misrepresent a juvenile.
6. Once phase cannot be preserved, cause-specific dead art is preferred over a
   fully generic dead asset. Missing charred, drought, or frost art never makes
   the plant unrenderable.
7. Annual-stage specificity is preserved when an exact asset permits it, but
   may be dropped before condition, phase, or cause.
8. The final species fallback is its loaded base texture. The existing
   `unknown_flora.png` remains only the last error fallback when the declared
   base itself is missing or invalid.
9. Resolution never probes the filesystem at render time and never changes
   because an undeclared file was added to a directory.

### Proposed fallback shape

For a cultivated flowering sprout killed by fire, the accepted shape is:

1. exact cultivated sprout + flowering + fire-dead;
2. exact wild sprout + flowering + fire-dead;
3. cultivated, then wild, sprout + fire-dead without the annual stage;
4. cultivated, then wild, sprout + flowering + generic dead;
5. cultivated, then wild, sprout + generic dead;
6. cultivated, then wild, generic cause-specific dead;
7. cultivated, then wild, stage-specific generic dead;
8. cultivated, then wild, generic dead;
9. the best corresponding living state;
10. the species base texture.

Thus a fire-killed sprout without suitable charred juvenile art displays the
best ordinary dead-sprout art. If no phase-appropriate dead art exists, it may
use generic charred art, then generic dead art, and ultimately the normal base
texture. It never stays visibly alive merely to preserve fire or season.

### Art maturity ladder

The schema deliberately supports incremental art completeness:

- **Tier 0 — base:** one valid species texture. Every state renders through
  final fallback.
- **Tier 1 — generic death:** `dead.png` or an equivalent selector.
- **Tier 2 — phase-aware death:** especially dead-juvenile art such as
  `sprout_dead.png`.
- **Tier 3 — generic cause:** `charred.png`, drought-dead, frost-dead, disease,
  or damage art shared across phases where visually acceptable.
- **Tier 4 — exact state:** context-, phase-, season-, and cause-specific
  combinations such as cultivated flowering charred art.

No tier requires completion of the tier above across every combination. Each
texture family gets its own art issue and PR for a deliberate manifest, and the
owner signs off on every texture. Crops that reuse a texture directory do not
receive duplicate art issues until they gain their own texture family.

### Render context

Render context is an explicit semantic input:

- naturally generated flora is `wild`;
- deliberately planted row flora is `cultivated`;
- `CropPlot` groundcover is `cultivated`;
- future deliberate transplantation must choose rather than infer context.

The context cannot be inferred from current age, health, density, or texture
path. The implementation may persist context directly on an occurrence or in
its authoritative creation record, but it must survive replay and migration
and must not alter existing visuals when no cultivated variants are declared.

### Persistent condition model

A durable condition record conceptually needs:

- a stable occurrence identity or crop-plot key;
- condition (`dead` initially; the representation should allow later
  append-only condition vocabulary);
- cause;
- world day/time at which the condition began;
- render context;
- life phase at the transition;
- annual stage at the transition;
- the selected corpse-retention outcome and any expiry day; and
- any explicit absent/reseed/replacement state future mechanics require.

It stores semantic tags, never texture handles or resolved paths.

The leading ownership proposal is one sparse per-page condition map in the
world-activity persistence component. Wild base instances continue to
regenerate deterministically, then join against that overlay. Row-crop replay
reconstructs its occurrence and joins against the same overlay. Groundcover
crop plots either join through a tile-keyed occurrence key or embed a
component-owned condition value in `CropPlot`; EFM-6 settles this without
forcing the wild-instance representation onto tile fills.

### Corpse retention and successor policy

Corpse retention is authored semantic data, not a side effect of whether dead
art exists. The accepted species-level declaration shape is:

```yaml
corpsePolicy:
  visibility: transient       # transient | persistent
  durationDays: 60            # required only for transient remains
  successor: reseed           # reseed | absent | await_replanting
```

Sparse phase/cause overrides are added only where a real behavior needs
them—for example, a tree sprout is transient even when a mature woody corpse is
persistent. The selected outcome is snapshotted when death occurs so a later
content-pack edit cannot silently reinterpret an existing save.

`persistent` means visible until an explicit clearing, replacement, or revival
action; it is not an immortal in-memory chunk object. The semantic record
remains in page-owned persistence and is joined to regenerated flora.

Repository-owned species explicitly declare their policy. For compatibility,
legacy content without `corpsePolicy` retains the current 60-day visible window
and generational reseed behavior. Policy is never inferred from the placement
category at runtime. Mature trees persist regardless of death cause; tree
sprouts, wildflowers, grasses, ferns, and crops are transient. Individual bush
policies are explicit: the current bracken fern and red raspberry definitions
are both transient for 60 days, while future woody shrubs may opt into
persistent remains.

### Stable occurrence identity

Tile coordinates alone are insufficient because one tile may hold multiple
dandelions or row plants. Float offsets are stable today but are a poor public
identity contract. The preferred proposal is a deterministic durable
occurrence key built at creation:

- wild occurrences derive it from world/page identity, canonical global tile,
  species identity, and deterministic placement ordinal;
- planted row occurrences derive it from a persisted creation identity in the
  world edit rather than from replay order;
- groundcover crops use their canonical tile plus a crop-plot generation or
  replacement identity if replacement history must distinguish old corpses
  from a newly planted crop.

Collisions are invalid and tested. The exact key representation remains an
engineering choice for EFM-4, constrained by canonical serialization,
cylindrical coordinate aliases, and save migration.

### Hazard-producer notes for later arcs

The foundation must accept the full initial cause vocabulary but does not
compute exposure. Later design work may add independent producers that call the
same mutation seam:

- **Temperature exposure:** accumulate time outside species/phase tolerance;
  juvenile susceptibility can use a complex duration-and-severity function so
  one cold hour differs from a sustained cold snap.
- **Drought:** combine precipitation, humidity, soil water availability,
  species tolerance, phase, season, and exposure duration rather than treating
  one dry tick as lethal.
- **Fire:** consume an authoritative fire-contact/burn exposure, record cause
  `fire`, and prefer charred art. Fire spread itself belongs elsewhere.
- **Disease:** record a durable cause and eventual visual variants without
  requiring the mortality store to own epidemiology.
- **Direct damage:** permit tools, units, structures, or scripted events to use
  the same death path.

These producers must be deterministic against their authoritative inputs and
must not tick every unloaded individual merely because the visual foundation
exists. Their cadence, LOD, and unloaded-chunk behavior need separate design.

## Decisions

### D-1. Build the state and rendering foundation before real hazards

The arc wires semantic causes, mutation/query seams, persistence, and texture
fallback. It records temperature, drought, frost, fire, disease, and damage as
supported future causes but does not implement their simulation or balance
functions.

### D-2. Use sparse optional texture variants

Exact art is used when declared. Missing combinations fall through a canonical
resolver; no species is required to ship a Cartesian product of context,
phase, season, condition, and cause.

### D-3. A single valid base texture is sufficient for safe rendering

Every semantic state ultimately resolves to the species base. Missing optional
art is a quality gap, not a runtime failure.

### D-4. Cultivated art overrides wild art rather than defining another lifecycle

Wild and cultivated flora share growth and condition semantics. Cultivated
requests use exact cultivated art when present and otherwise try the equivalent
wild state.

### D-5. Corpse retention is explicit rather than universally permanent

Every death remains authoritative across regeneration and save/load, but the
visible corpse follows authored retention semantics. Small or herbaceous
remains may expire after a finite interval, initially 60 game-days, while
structurally durable remains such as dead mature trees may persist until
cleared or replaced. The earlier proposal that every external death remain
visible indefinitely was rejected. D-12 and D-13 define the class defaults and
successor behavior.

### D-6. All persistent state is semantic and re-resolvable

Save data records condition, cause, context, occurrence identity, and the
chosen phase/stage and retention semantics. It never records GPU handles.
Loading resolves the current content catalogue after registries are ready.

### D-7. The fallback contract is documented and tested as one source of truth

The project will not scatter fallback knowledge across filenames, YAML loader
branches, render code, art prompts, and prose. EFM-1 establishes the canonical
vocabulary and order; later code and audits must match it.

### D-8. Art remains separately tracked and explicitly approved

Each texture-family expansion has its own issue and PR, names every asset and
semantic selector, and requires owner approval. The foundation and pilot do not
silently create placeholders or treat fallback as a reason to avoid desired
art.

### D-9. Existing flora content remains source-compatible

Current phase, annual-cycle, cycle-override, and harvested-texture declarations
continue to load and produce their current visuals. New variant declarations
are additive.

### D-10. A corpse freezes its semantic phase and annual stage at death

The condition record snapshots both life phase and annual stage. A flowering
plant that dies remains semantically flowering-dead rather than continuing to
cycle with the calendar. Missing exact art changes only which fallback texture
is displayed; it never changes the stored death state.

### D-11. Death, phase, and cause are the fallback priorities in that order

The resolver first preserves the fact of death, then the correct life-phase
silhouette, then cause. A fire-killed sprout without suitable charred juvenile
art therefore uses ordinary dead-sprout art before a generic adult-shaped
charred asset. Annual stage is retained by exact art but ranks below those
three concerns.

### D-12. Mature tree remains persist while small and juvenile remains expire

Mature trees remain visible until explicitly cleared or replaced regardless of
whether age, fire, drought, disease, or damage killed them. Tree sprouts,
wildflowers, grasses, ferns, and crops use the 60-day transient policy. Bushes
are intentionally decided per species rather than forced into either class.
Cause changes the preferred art but does not override this structural default.

### D-13. Transient wild flora reseeds; cultivated flora awaits replanting

At the end of its corpse window, transient wild flora advances to a new
generation, preserving the existing ecological behavior and allowing the
condition record to be compacted. Cultivated row and groundcover crops instead
become empty and remain so until deliberately replanted; death never replants a
field for the player.

### D-14. Corpse policy is explicit species data

Every repository-owned species declares `corpsePolicy`, with optional
phase/cause overrides. Runtime code does not infer mortality behavior from the
free-text placement category. Missing policy remains accepted only as a legacy
compatibility path equivalent to the current 60-day/reseed behavior, so older
content continues loading while shipped definitions stay self-documenting.

### D-15. Texture variants are declared explicitly in YAML

Every optional visual variant has an explicit semantic selector and texture
path. Filenames remain a readable convention but never become implicit runtime
registration. This keeps content changes reviewable, lets audits enumerate the
complete declared set, and prevents an undeclared copied file from changing
game behavior.

### D-16. Saguaro is the end-to-end pilot species

Saguaro is the pilot for the first mortality-art and fallback path. Its recent
sprout, seasonal mature, and adult-dead work makes it the relevant continuation
of the initiating flora task. The pilot integration waits for the existing
saguaro asset/species work to be available rather than duplicating it.

### D-17. The pilot adds dead-sprout and generic-charred art

The pilot art manifest is `sprout_dead.png` plus one generic `charred.png`
representing fire-killed saguaro remains. It deliberately omits
`sprout_charred`: a fire-killed sprout must demonstrate that phase-appropriate
ordinary death wins over generic mature-shaped cause art. Drought- and
frost-specific saguaro art remain later texture-family expansions rather than
inflating the pilot.

### D-18. Current bush species use transient remains

Bracken fern and red raspberry both use the 60-day transient/reseed policy.
Their shared `bush` placement category does not imply that every future shrub
must do the same; a genuinely woody shrub can explicitly declare persistent
remains under D-14.

## Open questions

All ten design questions are resolved below; they remain listed to preserve the
decision history and rejected alternatives.

### Q-1. Does indefinite persistence replace natural lifecycle reseeding?

Resolved by D-5. Neither natural nor externally caused death implies universal
permanence. Corpse retention is content-driven, with 60 days retained as the
initial transient interval and persistent remains supported where appropriate.
D-12 and D-13 settle which forms use each policy and what follows expiry.

### Q-2. Does a corpse freeze its annual stage at death?

Resolved by D-10. Both phase and annual stage freeze at death. The rejected
alternative would have let corpse art continue cycling with the calendar,
which would reinterpret a persisted death rather than merely fall back its art.

### Q-3. When exact dead art is missing, which fidelity wins?

Resolved by D-11. Death is primary, then phase, then cause. A
`sprout_dead` asset therefore beats generic adult `charred` art; generic charred
beats fully generic dead only after no phase-appropriate dead candidate exists.

### Q-4. Are variant declarations always explicit YAML?

Resolved by D-15. Every optional variant is declared explicitly in YAML and
filenames remain conventional only. Automatic filename discovery was rejected
because it would make directory contents an unaudited semantic registry.

### Q-5. Which texture family is the pilot?

Resolved by D-16. Saguaro is the pilot. White oak remains a useful later
fallback-matrix expansion but is not the first end-to-end species.

### Q-6. How many art variants define the first acceptable family tier?

Resolved by D-17. The pilot adds `sprout_dead.png` and generic `charred.png`,
deliberately omits `sprout_charred` to prove fallback priority, and defers
drought/frost art.

### Q-7. Which structural forms receive persistent remains by default?

Resolved by D-12. Mature trees persist for every cause. Tree sprouts,
wildflowers, grasses, ferns, and crops are transient for 60 days. Bushes are
authored individually; the current bush manifest remains Q-10.

### Q-8. What succeeds a transient corpse after its 60-day window?

Resolved by D-13. Wild flora reseeds, preserving the current generational
behavior. Cultivated crops become empty and await deliberate replanting. The
rejected uniform-reseed alternative would have made crop death automatically
replant the player's field.

### Q-9. Is corpse policy always declared per species?

Resolved by D-14. Repository-owned species always declare policy. Legacy
content may omit it and receives the current 60-day/reseed behavior. The
rejected inference alternative would have turned free-text placement categories
into an unrelated mortality contract.

### Q-10. What policies do the current bush species declare?

Resolved by D-18. Bracken fern and red raspberry are both transient for 60
days and then reseed. Future woody shrubs can explicitly choose persistent
remains.

## Verification strategy

- Pure table-driven tests enumerate fallback candidates and prove exact-match,
  cultivated-to-wild, stage-drop, cause-drop, phase-drop, and base fallback
  order independently of engine or GPU state.
- Loader tests prove legacy YAML normalizes unchanged, new sparse selectors
  reject duplicate/unknown combinations, every repository-owned species has an
  explicit valid corpse policy, omitted legacy policy retains 60-day/reseed
  behavior, and every declared path is included in the texture-subset audit.
- Growth/render tests use synthetic handles to prove a dead sprout never
  resolves adult art when a juvenile fallback exists and a one-texture species
  always returns its base.
- Persistence tests prove condition, cause, context, phase/stage snapshot, and
  occurrence identity, retention outcome, and expiry survive save/load and
  component migration without saving texture handles.
- Regeneration tests evict and recreate a wild chunk, then prove the same
  occurrence remains dead with the same semantic selector.
- Multiple-occurrence tests kill one of several flora instances on one tile and
  prove siblings remain alive.
- Crop tests cover both row `FloraInstance`s and tile-fill `CropPlot`s,
  including replacement/replanting so an old corpse cannot poison a new crop.
- Boundary tests prove a transient corpse is still dead immediately before its
  expiry, takes its configured successor exactly once at expiry, and does not
  revive merely because its overlay was removed. Persistent remains survive
  the same elapsed time, eviction, and reload.
- Cylindrical seam tests use canonical global coordinates for occurrence keys.
- A focused behavior probe mutates one pilot through exact and fallback states,
  saves, loads, evicts/regenerates, and records the resolved texture names.
- Every new art issue presents native and nearest-neighbor previews for owner
  approval. The owner runs the real flora preview; automated agents do not open
  its real window.
- If worldgen-visible instance identity or output changes, the relevant child
  follows the repository's worldgen-output tier, including full headless tests,
  generated baselines, `world_check`, and required save bookkeeping. Pure
  content/schema children do not claim a world-output change.

## Delivery plan

### EFM-1. Publish the canonical flora visual-state and fallback contract

- **Outcome:** One durable repository contract defines selector axes,
  vocabulary, filename conventions, sparse-declaration rules, fallback order,
  art tiers, and compatibility promises.
- **Scope:** Documentation only, incorporating D-1 through D-18 and the
  existing phase/cycle schema.
- **Phase:** Foundation
- **Depends on:** `none`
- **Ordering:** `critical path`
- **Relevant decisions:** D-1 through D-18
- **Acceptance signals:** One canonical section is referenced by flora YAML,
  loader, resolver, audit, test, and art workflows; no competing prose order
  remains authoritative.
- **Out of scope:** Runtime implementation and art creation.
- **Open questions:** None

### EFM-2. Load and audit sparse flora visual and corpse-policy declarations

- **Outcome:** Existing YAML loads unchanged; optional semantic variants and
  corpse policies load into validated content data; and every declared texture
  path is audited.
- **Scope:** YAML vocabulary, decoder/validation, load-time texture
  registration, corpse-policy validation, legacy normalization, and
  texture-subset coverage.
- **Phase:** Foundation
- **Depends on:** EFM-1
- **Ordering:** `critical path`
- **Relevant decisions:** D-2, D-3, D-5, D-7, D-9 through D-15, D-18
- **Acceptance signals:** Duplicate and unknown selectors fail clearly; a
  one-texture species and every current flora definition still load; invalid
  duration/successor combinations fail clearly; repository-owned definitions
  declare policy while a legacy omission receives the compatibility default.
- **Out of scope:** Choosing variants during rendering or adding assets.
- **Open questions:** None

### EFM-3. Resolve flora textures through the canonical fallback lattice

- **Outcome:** A pure total resolver maps semantic visual requests plus a sparse
  variant table to a valid texture and diagnostic fallback trace.
- **Scope:** Selector types, deterministic candidate order, resolver tests, and
  integration with current phase/cycle selection while current content remains
  visually unchanged.
- **Phase:** Foundation
- **Depends on:** EFM-2
- **Ordering:** `critical path`
- **Relevant decisions:** D-2, D-3, D-4, D-7, D-9 through D-11, D-15
- **Acceptance signals:** Exhaustive table tests cover every fallback boundary
  and prove the species base is total.
- **Out of scope:** Persistent deaths, hazard producers, or art.
- **Open questions:** None

### EFM-4. Give flora occurrences stable identity and explicit render context

- **Outcome:** Wild occurrences, planted row flora, and groundcover crop plots
  provide an unambiguous durable identity and `wild`/`cultivated` context to
  downstream condition and rendering code.
- **Scope:** Identity/context model, creation/replay wiring, canonical
  coordinates, migrations, and no-visual-change coverage.
- **Phase:** Persistence foundation
- **Depends on:** EFM-1
- **Ordering:** `critical path`
- **Relevant decisions:** D-4, D-6, D-9
- **Acceptance signals:** Identity is stable across regeneration/save/load;
  siblings on one tile remain distinct; existing worlds and visuals migrate
  unchanged.
- **Out of scope:** Marking an occurrence dead or selecting cause-specific art.
- **Open questions:** None; representation remains an implementation choice
  bounded by the design.

### EFM-5. Persist wild and row-flora condition records across regeneration and saves

- **Outcome:** A page-scoped sparse condition store can mark one wild or row
  flora occurrence dead, retain cause, frozen phase/stage, and the selected
  retention outcome, and reattach after chunk regeneration and load.
- **Scope:** Runtime owner, frozen DTO/component migration, snapshot/restore,
  integrity validation, inventory documentation, and focused tests.
- **Phase:** Persistence foundation
- **Depends on:** EFM-4
- **Ordering:** `critical path`
- **Relevant decisions:** D-1, D-5, D-6, D-10, D-12 through D-14
- **Acceptance signals:** One of several same-tile occurrences survives a
  kill→evict→regenerate→save→load round trip without affecting siblings.
- **Out of scope:** Groundcover crops, hazard calculations, rendering changes,
  or art.
- **Open questions:** None

### EFM-6. Persist groundcover-crop condition records through the same model

- **Outcome:** A tile-fill `CropPlot` carries equivalent context, death cause,
  phase/stage snapshot, replacement identity, and save/load behavior.
- **Scope:** Crop-plot condition ownership/migration and replanting semantics.
- **Phase:** Persistence foundation
- **Depends on:** EFM-5
- **Ordering:** `critical path`
- **Relevant decisions:** D-4 through D-6, D-10, D-12 through D-14
- **Acceptance signals:** A dead crop persists, a replacement crop does not
  inherit the old death, and row/groundcover queries expose equivalent state.
- **Out of scope:** Crop-specific death balance, cleanup jobs, or dedicated crop
  art.
- **Open questions:** None

### EFM-7. Expose and render persistent flora conditions without hazard producers

- **Outcome:** One authoritative mutation/query seam sets or clears a semantic
  condition, and all flora render forms resolve it through EFM-3.
- **Scope:** Engine/world-thread ownership, inspection API, wild/row/groundcover
  render integration, diagnostic fallback trace, and a GPU-free probe/test
  path.
- **Phase:** Observable foundation
- **Depends on:** EFM-3, EFM-5, EFM-6
- **Ordering:** `critical path`
- **Relevant decisions:** D-1 through D-7, D-9 through D-14
- **Acceptance signals:** Synthetic handles prove exact and fallback choices;
  condition state survives persistence; no real drought/fire/frost producer is
  required.
- **Out of scope:** Player-facing debug UI, hazard simulation, and new art.
- **Open questions:** None

### EFM-10. Apply corpse retention and successor policy

- **Outcome:** Dead occurrences remain visible for their snapshotted retention
  period and then take exactly one explicit successor transition; persistent
  structural remains stay until cleared.
- **Scope:** Retention-policy selection, elapsed-time evaluation, transient
  cleanup, wild reseed/absence behavior, cultivated replanting behavior,
  tombstone or generation handling, and boundary/persistence tests.
- **Phase:** Observable lifecycle
- **Depends on:** EFM-7
- **Ordering:** `critical path`
- **Relevant decisions:** D-5, D-6, D-9, D-10, D-12 through D-14, D-18
- **Acceptance signals:** A 60-day transient corpse crosses its boundary once
  without accidental revival; a persistent corpse survives that boundary,
  eviction, and reload; expired state is compacted when its successor permits.
- **Out of scope:** Hazard simulation, staged decomposition, nutrients,
  salvage, and unit cleanup jobs.
- **Open questions:** None

### EFM-8. Create an approved pilot mortality texture set

- **Outcome:** One texture family gains the minimum approved assets needed to
  demonstrate dead-juvenile and at least one cause-specific fallback branch.
- **Scope:** Saguaro art only: `sprout_dead.png` and generic `charred.png`, with
  native/enlarged owner approval for both textures. `sprout_charred.png` is
  deliberately absent so the resolver's phase-before-cause fallback is tested.
- **Phase:** Pilot
- **Depends on:** EFM-1
- **Ordering:** `independent`
- **Relevant decisions:** D-2, D-8, D-15 through D-17
- **Acceptance signals:** Native/enlarged review passes, declared canvases and
  transparency are valid, the owner approves every image, and the work builds
  on the landed base saguaro assets from #1688 rather than duplicating them.
- **Out of scope:** YAML integration, runtime code, and other texture families.
- **Open questions:** None

### EFM-9. Demonstrate the complete condition and fallback path on one pilot species

- **Outcome:** The pilot species can be observed alive and dead as juvenile and
  mature, can use exact and missing-asset fallbacks, and retains the chosen
  state through regeneration and save/load.
- **Scope:** Pilot YAML declarations, focused tests/probe, owner-run visual
  preview, and documentation of the observed fallback trace.
- **Phase:** Pilot
- **Depends on:** EFM-10, EFM-8
- **Ordering:** `critical path`
- **Relevant decisions:** D-2 through D-17
- **Acceptance signals:** Exact dead-juvenile art wins when present; a deliberately
  omitted combination follows the documented fallback; a base-only fixture
  remains renderable; the selected corpse policy crosses or survives its
  retention boundary correctly; persistence and preview pass.
- **Out of scope:** Real environmental hazard producers and bulk art backfill.
- **Open questions:** None

## Future texture-family backfill candidates

These are inventory entries, not delivery slices yet. After EFM-9 proves the
contract, each selected family becomes its own art issue/PR with a deliberate
tier and manifest:

- bracken fern
- coconut palm (currently lacks any dead art)
- common dandelion
- paper birch
- red mangrove (currently lacks any dead art)
- red raspberry (also temporarily supplies tomato art)
- Scots pine (currently lacks any dead art)
- sugar maple
- weeping willow
- white clover (also temporarily supplies wheat art)
- white oak
- white spruce (currently lacks any dead art)
- saguaro after #1688/PR #1725 and flora integration

Dedicated cultivated crop families should replace placeholder reuse before
crop-specific mortality art is considered complete. A crop species that still
reuses a wild family inherits that family's fallback rather than creating
duplicate files under a second semantic name.

## Source notes

The initiating discussion established these product preferences:

- complex duration/severity functions should eventually let temperature and
  other environmental factors kill juveniles, but those formulas should remain
  design notes rather than being prematurely wired into this foundation;
- exact art should win when present, with reasonable deterministic fallbacks
  when it is absent;
- cause-, lifecycle-, seasonal-, and cultivated-specific textures are desirable
  but intentionally sparse rather than combinatorially mandatory;
- a plant with one texture must always render regardless of state;
- dead state always survives regeneration and save/load, but small/herbaceous
  corpses expire after a finite interval while structurally durable remains
  such as dead trees may persist until cleared;
- 60 game-days is acceptable as the initial transient-corpse interval;
- mature tree remains persist for every death cause, whereas transient wild
  flora reseeds and transient cultivated flora awaits deliberate replanting;
- repository-owned species explicitly declare corpse policy rather than
  deriving it from placement categories;
- bracken fern and red raspberry both use the 60-day transient/reseed policy;
- optional texture variants are explicitly declared in YAML rather than
  discovered from filenames;
- saguaro is the pilot and adds `sprout_dead.png` plus generic `charred.png`,
  while deliberately omitting `sprout_charred.png` to prove fallback;
- exact death art is preferred, while fallback priority is death first, phase
  second, cause third, and finally the normal species base;
- wild and agricultural presentations share the same flora semantics, with
  cultivated art overriding and otherwise falling back to wild art;
- state must survive both chunk regeneration and save/load; and
- the canonical selector/fallback schema must be written down before issues
  implement it.
