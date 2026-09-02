# Procedural Blood Decals

Status: implemented — this is the final documentation/verification gate
for epic #603 (closed once this PR merges, as a repository-management
step). This is the as-built record — see git history and the
referenced issues (their comment threads carry the round-by-round
design/review narrative) for how each decision below was reached.

Visible blood from injuries, without hand-authored blood texture assets:

- A wound creates an immediate blood mark on the ground (`Blood.Impact`,
  #607).
- A bleeding unit leaves drops/smears while moving (`Blood.Trail`,
  #882) or grows a local pool while stationary/collapsed (`Blood.Pool`,
  #883).
- Every visible mark is procedurally generated and deterministic from a
  compact descriptor (`Blood.Texture`, #606) — no stamped repeated
  asset, and near-identical requests reuse an existing texture instead
  of minting a new one.
- Blood ages visually from wet red toward dark dried red, with a floor
  so an old mark never disappears from aging alone (`Blood.Render`,
  #606).
- Runtime texture and decal counts are hard-bounded (FIFO eviction on
  both), so a long fight cannot leak unbounded GPU textures, CPU
  records, or dynamic quads.
- Blood is **transient by design** — see "Transience" below. A loaded
  session always starts with no marks.

## Non-goals

- True liquid simulation.
- Blood on walls or vertical tile faces.
- Gore, severed-part sprites, body overlays, or unit-sprite wound art.
- A general-purpose decal system for scorch marks, grime, footprints,
  or damage.
- Rain washing, fluid dilution, or blood interacting with the world
  fluid system — **deliberately deferred**, not merely unbuilt; see
  "Deferred: rain and fluid integration" below.
- Persisting blood across save/load — **deliberately deferred**; see
  "Transience" below.

## As-built architecture

| Module | Landed in | Role |
|---|---|---|
| `Blood.Types` | #604 | World-scoped data model: `BloodTexturePool` (FIFO of generated-texture descriptors), `BloodDecals` (FIFO of placed marks), the combined per-page `BloodStore`, and the one `spawnDecal` entrypoint that resolves a texture (reuse or create + evict) and places a decal atomically. |
| `Blood.Texture` | #606 | Pure `BloodTextureDescriptor → RGBA8 pixels` generator — no IO, no GPU. |
| `Blood.Render` | #606 | Pure `BloodDecal + now → BloodRenderRecord` — resolved world placement plus an aged tint/alpha. The ONE definition both the headless debug surface and the real renderer consume, so they can't drift apart. |
| `World.Render.BloodQuads` | #606 | GPU half: uploads new descriptors' pixel data and unregisters evicted ones once per frame (`uploadBloodTextures`), and turns visible `BloodRenderRecord`s into world-space `SortableQuad`s (`renderBloodDecalQuads`) through the same bindless/vertex path every other world sprite uses. |
| `World.Blood.Teardown` | #788 | Cross-thread GPU-dispose transport for a page removed/replaced outright (destroy, destroy-all, re-init under a reused id, arena replacement, save-load page replacement) — hands the orphaned page's live blood-texture handle map to the render thread via `EngineEnv.bloodDisposeQueue`. |
| `Blood.Impact` | #607 | One-shot impact marks: maps a fresh wound's kind + severity onto a blood request (or nothing), and `spawnImpactBlood` places it. Integration owners: `Combat.Resolution.runResolution` (combat hits), `Unit.Thread.Movement.tickAllMovement` (fall injuries), `Engine.Scripting.Lua.API.Units.unitInjureFn` (debug). |
| `Unit.Types.Trail` | #882 | `TrailState` — the transient per-unit accumulator BOTH ongoing-bleeding halves share (pending volume, distance/cadence bookkeeping, pool cluster anchor/layer count). Fed by `Combat.Wounds.Tick`'s conserved external-blood-loss accounting; consumed by `Unit.Thread.Movement`. |
| `Blood.Trail` | #882 | The MOVING half of ongoing bleeding: `consumeTrailMarks` pops zero or more drop/smear marks from a unit's accumulator, gated on distance AND cadence; `spawnTrailMark` places them via `Blood.Types.spawnDecal`. |
| `Blood.Pool` | #883 | The STATIONARY half: `classifyOngoing` arbitrates travel-vs-dwell by displacement from a cluster anchor, `consumePoolLayers` pops bounded, cadence/volume-gated pool layers, and `spawnPoolLayer` places them — same `spawnDecal` entrypoint, decal records are never mutated (a pool grows by layering new marks, not by resizing one). |
| `Engine.Scripting.Lua.API.Blood` | #604, #606, #882, #883 | The full debug/headless Lua surface — see "Debug/introspection surface" below. |

`Combat.Wounds.Tick` is the integration owner that feeds `TrailState`
(conserved external blood-loss accounting); `Unit.Thread.Movement` is
the integration owner that consumes it every movement tick and decides,
via `Blood.Pool.classifyOngoing`, whether that tick's emission goes
through `Blood.Trail` or `Blood.Pool`.

## Shape of the system

1. Gameplay code observes an injury or ongoing bleed and calls a blood
   API with parameters such as wound kind, severity, source unit,
   current tile/world position, direction if known, and amount.
2. The blood system turns those parameters into a requested decal
   style: pool, drops, spatter, streak, smear, or no mark.
3. Before generating a new texture, the blood texture pool
   (`Blood.Types.findMatch`) checks whether an existing generated blood
   texture is close enough to the requested style.
4. If a match exists within the accepted threshold, the new decal
   reuses that texture reference with its own transform, tint, age, and
   world placement.
5. If no match exists, the pool synchronously mints a fresh descriptor
   (assigned a stable `BloodTextureId`) and joins it to the blood
   texture FIFO right away — no pixel data is generated at this point.
   On a later frame, `World.Render.BloodQuads.uploadBloodTextures`
   diffs the FIFO against what's already GPU-resident, generates that
   descriptor's pixel data (`Blood.Texture.generateBloodTexture`), and
   uploads it into a bindless slot. That slot is recyclable GPU
   bookkeeping, kept separate from the stable `BloodTextureId` decals
   actually reference.
6. When the FIFO exceeds its configured maximum, the oldest blood
   texture is evicted. Every decal referencing that texture is removed
   at the same time (`removeDecalsForTexture`) so stale placements
   simply disappear instead of pointing at invalid texture data.

Individual injuries can still produce unique-looking marks, but the
system has a hard upper bound on generated texture resources (and,
independently, on live decal records — see "Runtime tuning" below).

## Texture identity and matching

A generated blood texture's descriptor (`BloodTextureDescriptor`)
records the dimensions `Blood.Types.requestDistance` compares when
deciding whether a request can reuse an existing texture:

- style: pool, drops, spatter, streak, smear
- wound kind that requested it
- severity/amount bucket
- approximate footprint size
- directionality/anisotropy bucket
- edge roughness and droplet density bucket

Style and severity bucket are **hard gates** — any difference there
always mints a new descriptor, regardless of the other dimensions.
Wound kind, footprint, anisotropy, and edge are **soft**: a wound-kind
mismatch costs a flat 2, each bucketed dimension costs its ordinal
distance, and a request reuses the closest existing descriptor whose
total cost is ≤ `Blood.Types.matchThreshold` (1) — one near-match bucket
step away still reuses; two or more, or any wound-kind mismatch on its
own, mint a new descriptor.

The descriptor also carries a generation seed (`btdSeed`) — but it
plays NO role in matching (`requestDistance` never reads it). It only
feeds `Blood.Texture.generateBloodTexture`'s pixel data once a
descriptor is actually generated/uploaded, which is exactly why two
descriptors that match on every dimension above reuse ONE texture
rather than each getting their own seed-varied pixels.

The texture reference used by world decals (`BloodTextureId`) is
independent from the live bindless GPU slot — the same separation every
other world sprite's handle keeps from its recyclable slot, so cached
world placements never depend on GPU-side bookkeeping.

## World decal records

Each blood spot on the map is a `Blood.Types.BloodDecal`:

- blood texture reference
- world/page identity
- tile or continuous world position (`bdeX`/`bdeY`, float, sub-tile)
- surface z at placement time
- local offset, rotation, and scale
- creation game time (`bdeCreatedAt`)
- initial wetness (`bdeInitialWetness`) — current age/wetness/dryness
  are derived at read time from this plus elapsed time, never stored
  themselves (`Blood.Types.wetnessAt`)
- source wound kind and severity bucket
- source unit id when known
- amount/opacity

Decal records are **immutable** once placed — nothing "grows" or
rewrites an existing decal (`Blood.Pool`'s layered-spawn design
depends on this). When a texture reference is evicted, every decal
using it is removed (`removeDecalsForTexture`); independently, the
decal store has its own FIFO cap (`defaultBloodDecalCap`) so a request
that keeps reusing an already-live texture — never triggering texture
eviction — still can't grow the decal list without bound.

## Injury behavior (impact marks, `Blood.Impact`, #607)

Immediate impact marks reflect wound kind and severity
(`impactBloodForWound`):

- `stab`: pool style.
- `slash`: streak style, using attack/movement direction when known,
  otherwise a deterministic fallback angle from the event seed
  (`impactFallbackAngle`).
- `blunt`/`concussion`: no external blood below
  `catastrophicBluntThreshold` (0.85, reused from
  `scripts/injury_log.lua`'s own T4 tier boundary — the same point that
  script's narration switches to "crushing"/"pulverizing"/"pulping").
  At or above it, blood.
- `fracture`: no external blood below `Combat.Wounds.destroyThreshold`
  (1.0, the existing structural-destruction cutoff). At or above it,
  blood.
- `arterial`/`severed`: always at least `SeverityModerate` (floored),
  scaling further with severity above that floor.
- `internal`: never draws blood, at any severity — no plausible
  skin-breaking mechanism.

Severity scales the resulting severity bucket, footprint, and opacity
(`impactSeverityBucket`/`impactFootprint`/`impactOpacity`), reusing the
SAME T1..T4 tier boundaries (0.25/0.50/0.85) `scripts/injury_log.lua`
narrates wounds with, so "how strong the mark looks" always agrees with
"how the wound narrates".

A single attack/fall that produces several wounds picks ONE headline
wound to represent the whole event (`pickImpactWound`, ranked by
resulting severity bucket then opacity) — bounded per event, not per
wound. Production call sites: `Combat.Resolution.runResolution`,
`Unit.Thread.Movement.tickAllMovement` (fall injuries), and the debug
surface `unit.injure`. Skips silently (never crashes) if the wounded
unit's page isn't currently loaded.

## Ongoing bleeding behavior (`Blood.Trail` #882, `Blood.Pool` #883)

Every unit with an externally-bleeding wound has a bleed-decal
accumulator (`Unit.Types.Trail.TrailState`), advanced by
`Combat.Wounds.Tick` from the same effective bleed rate the wound
system uses — bandaging, clotting, and healing naturally reduce or stop
new marks. `Unit.Thread.Movement` arbitrates which half of the system
consumes it each tick (`Blood.Pool.classifyOngoing`, by displacement
from a pool cluster anchor, not raw step distance — a unit shuffling in
place keeps feeding one pool rather than restarting a cluster every
tick):

- **Moving (`Blood.Trail`)**: a unit travelling leaves drops or short
  smears. A mark needs BOTH a minimum path distance AND a minimum real
  elapsed time since the last mark (`TrailThresholds`) — neither alone
  gates it, so a fast unit's trail can't flood the decal store, and a
  slow unit's trail still reads as continuous. A mark's weight is the
  FULL volume banked since the last mark (never itself gated), so a
  light trickle produces a lighter mark than a heavy bleed over the
  same distance/time.
- **Stationary/collapsed (`Blood.Pool`)**: a unit that isn't covering
  ground grows a local pool instead — layered bounded spawns, small
  overlapping pool/drop marks placed on a deterministic golden-angle
  spiral around a cluster anchor (`poolLayerOffset`), up to a
  documented per-cluster layer bound (`ptMaxLayers`). At the bound,
  nothing more is added — the layer count is never re-derived from the
  live decal store, so a global FIFO eviction of an old layer can never
  reopen an exhausted budget. Layers are gated on BOTH cadence and a
  volume floor, so a trickle pools slowly and an arterial bleed pools
  fast.

Both halves share the ONE accumulator (`TrailState`), so a
walk-then-stop-then-walk sequence hands conserved blood seamlessly
between them with nothing lost at the seam — a pool layer resets the
same cadence/distance gates a trail mark would. Emission always
considers both elapsed game time and movement distance, which is what
prevents a moving unit from dropping many decals on the same pixel
every tick while still letting a stationary badly-bleeding unit pool.

`TrailState` is entirely runtime/transient — see "Transience" below.

## Aging (`Blood.Render.decalTint`, #606)

Blood ages visually without ever regenerating a texture — purely a
function of the current time and the decal's own stored fields
(creation time, initial wetness):

- fresh blood: wet, saturated dark red, full opacity.
- drying blood: interpolates toward darker, desaturated brown.
- old/dry blood: alpha floors at 0.35 rather than fading to nothing —
  **aging never removes a mark**. Removal only happens via texture-FIFO
  eviction, the decal store's own FIFO cap, page replacement/teardown,
  or an explicit `blood.clear()`.

`bloodDryDuration` (600 seconds of unpaused engine time —
`wsGameTimeRef`, never the world calendar `world.setTimeScale`
advances) is how long a decal takes to linearly dry from its initial
wetness to fully dry.

## Rendering (`World.Render.BloodQuads`, #606)

Blood renders through the same world-space quad / bindless-texture path
every other world sprite uses — no dedicated decal pipeline:

- Sits just above bare terrain and below ground items/units (a fixed
  sort-key nudge above terrain, below the ground-item/unit band — the
  same convention `World.Render.SpoilQuads`/`FloraQuads` use for "sits
  on the ground").
- Rotation is real corner-vertex rotation, not a shader flag.
- Culled to visible chunks/regions via the same chunk-visibility
  culling ground items use, so the per-frame quad count stays bounded
  regardless of world size.
- A decal whose texture hasn't been GPU-uploaded yet (or never will be
  — headless) simply contributes no quad; it appears once
  `uploadBloodTextures` catches up.
- Omitted once its texture reference has been evicted (defensive
  double-check in `Blood.Render.bloodRenderRecord`, on top of eviction
  already having removed the decal itself).

The headless debug surface (`blood.getRenderQuads`) exposes the exact
same resolved `Blood.Render.BloodRenderRecord` data the real renderer
consumes, computed purely from decal + texture-pool state with no GPU
dependency, so a headless probe can assert renderability and aging tint
without a display.

## Runtime tuning

These are compiled production defaults, not dynamically configurable
runtime settings:

| Parameter | Value | Where |
|---|---|---|
| Texture-FIFO cap | 24 | `Blood.Types.defaultBloodTextureCap` |
| Decal-FIFO cap (per page) | 512 | `Blood.Types.defaultBloodDecalCap` |
| Dry duration | 600 s (unpaused engine time) | `Blood.Types.bloodDryDuration` |
| Trail: min distance between marks | 1.0 tile | `Blood.Trail.defaultTrailThresholds` (`ttMinDistance`) |
| Trail: min cadence between marks | 0.5 s | `Blood.Trail.defaultTrailThresholds` (`ttMinCadence`) |
| Trail volume bands (moderate/severe/catastrophic) | 0.05 / 0.15 / 0.4 L | `Blood.Trail.trailModerateVolume`/`trailSevereVolume`/`trailCatastrophicVolume` |
| Pool: cluster radius | 1.0 tile | `Blood.Pool.defaultPoolThresholds` (`ptClusterRadius`) |
| Pool: max layers per cluster | 12 | `Blood.Pool.defaultPoolThresholds` (`ptMaxLayers`) |
| Pool: min cadence between layers | 1.5 s | `Blood.Pool.defaultPoolThresholds` (`ptMinCadence`) |
| Pool: min volume per layer | 0.015 L | `Blood.Pool.defaultPoolThresholds` (`ptMinVolume`) |
| Pool: jitter radius | 0.35 tile | `Blood.Pool.defaultPoolThresholds` (`ptJitterRadius`) |
| Match threshold (soft-distance reuse) | 1 | `Blood.Types.matchThreshold` |
| Max generated texture dimension | 32 px | `Blood.Texture.maxBloodTextureDim` |

## Debug/introspection surface (`Engine.Scripting.Lua.API.Blood`)

The complete registered Lua surface:

- `blood.spawn(gx, gy, woundKind, severity [, props])` — resolves/
  creates a texture and places a decal in one call; the debug entry
  point every headless probe drives.
- `blood.getDecal(decalId)` / `blood.listDecals()` — decal inspection,
  oldest first, with derived `age`/`wetness`/`dryness`.
- `blood.getTexture(textureId)` / `blood.listTextures()` — texture
  descriptor inspection, oldest (FIFO front) first, each reporting its
  0-based FIFO rank plus regenerated `width`/`height`/`pixelHash`.
- `blood.getTextureCap()` — the active world's configured texture-pool
  cap.
- `blood.getRenderQuads([pageId])` — the resolved
  `Blood.Render.BloodRenderRecord` data a real quad would use, without
  a GPU.
- `blood.gpuStats()` — GPU-side resource counts (bindless registrations,
  texture-size cache entries, the active page's live blood handle-map
  size) for teardown/leak probes. The first two are engine-wide totals,
  not blood-only.
- `blood.gpuHandles([handles])` — the blood-OWNED GPU identities those
  totals only count. With no argument it reports the active page's blood
  handle map (`{ id, handle, bindless, texSize }` per row, ascending by
  texture id); with a dense array of integer texture handles it reports
  exactly those, in order and without an `id`, so a probe can re-check
  handles it captured before a teardown once that page is gone. A
  malformed argument returns `nil` — including an element that is not a
  Lua number with an integer value, a numeric string such as `'47'`
  included. `bindless` and `texSize` are
  membership in the two registries `disposeBloodRecord` drops
  SEPARATELY, so a partial leak is visible; both read false with no
  bindless system (headless). Purely observational — it mutates no blood
  or GPU state.
- `blood.clear()` — empties both the decal list and the texture pool on
  the active world.
- `blood.getTrailState(uid)` — the per-unit ongoing-bleeding
  accumulator (pending volume, distance/cadence bookkeeping, pool
  cluster anchor/layer count/at-bound flag), or `nil` for a unit with no
  active accumulator.

## Transience (deliberate design decision, not an omission)

Blood is **transient by design**. `wsBloodStoreRef` (the per-page
`BloodStore` — texture pool + decal list) is deliberately never
persisted, and `Unit.Types.Trail.TrailState` (the per-unit ongoing-
bleeding accumulator) is deliberately never persisted either. A loaded
session always starts with no decals and no active trail/pool
accumulators, even if the session that was saved had plenty of both.

**What actually removes a mark.** Aging alone never does — it floors at
0.35 alpha rather than fading to nothing (see "Aging" above). A mark
disappears only via: texture-FIFO eviction (cascades to every decal
referencing that texture), the decal store's own independent FIFO cap,
a page being torn down or replaced (destroy, destroy-all, re-init under
a reused id, arena replacement, save-load page replacement —
`World.Blood.Teardown` reclaims the GPU side of this), or an explicit
`blood.clear()`.

**Save/load specifically.** `World.Thread.Command.Save.WriteWorld`'s
save capture never reads `wsBloodStoreRef` at all — it isn't part of
`World.Save.Snapshot.PageSnapshot`/`World.Save.Types.WorldPageSave`, so
there is structurally nothing for a save to carry. On load,
`World.Load.Stage` always builds pages from a fresh `WorldState`
(`emptyWorldState`), which starts with an empty `BloodStore` — staging
never writes to `wsBloodStoreRef`. The per-unit trail/pool accumulator
resets independently, through a different path:
`World.Save.Types.fromUnitSnapshot` always reconstructs a loaded unit
with `uiTrailState = Nothing`, regardless of what the live unit's
accumulator held at save time. These are two SEPARATE resets on two
separate save/load surfaces, not one shared mechanism. Note that
`blood.clear()` only empties the active page's `BloodStore`; it does
NOT touch any unit's `uiTrailState` — that resets independently, via
several paths that have nothing to do with save/load: the save/load
reconstruction above, unit death/destroy, and — most often, in normal
play — the instant a unit's external bleed rate reaches zero for ANY
reason (clotting, bandaging, healing, or an explicit treatment), which
`Combat.Wounds.Tick`'s own wound tick clears synchronously, and
`Unit.Thread.Movement` re-checks defensively (a treatment or a healed-
out wound can drop external bleed to zero between wound ticks, and
consuming stale banked volume on that tick would stamp a mark for
blood that stopped flowing) — see `Test.Headless.Blood.Trail`'s
lifecycle coverage.

**Why.** Blood marks are cosmetic and already capacity-bounded — the
texture pool and decal store are both hard-capped (see "Runtime
tuning"), so the worst-case footprint a save component would ever need
to carry is small and fixed, not unbounded. They are NOT self-expiring,
though: aging never removes a mark (see "Aging" above), and FIFO
eviction only fires when NEW activity pushes a store over its cap — an
isolated old mark with nothing further spawned into that page can sit
untouched for the rest of the session. Building and maintaining a full
save component for them — schema, migration, integrity-graph coverage
— is a real ongoing cost for state that is inherently disposable: every
mark is regenerable from live gameplay, and a mark from a fight several
sessions back has no gameplay meaning worth preserving. #884 (persist
blood decals as a versioned save component) was closed as not planned
on this basis.

**The reversal path, if this decision is ever revisited.** Both records
are already persistence-shaped: `BloodTextureDescriptor` and
`BloodDecal` carry every field a save component would need, and
`Blood.Texture.generateBloodTexture` is a PURE, deterministic function
of a descriptor — a saved descriptor regenerates byte-identical pixel
data with no separate texture-data serialization required. Persisting
blood later is a self-contained new save component under the #759/#760
envelope architecture (a `wpsBloodStore`-shaped field, a
`Blood.Types.BloodStore` `Serialize` instance, a schema-version bump,
integrity-graph coverage for `bdeSourceUnit`), not a redesign of the
data model. Closed issue #884 is the specification for that future
work, should it be picked back up.

**What stays correct, unchanged, under this decision.** `Blood.Types`'
module-level "never persisted" note and
`docs/persistence_state_inventory.md`'s `Exclude` classification for
`wsBloodStoreRef` were both already correct — they just used to read as
provisional ("#604 scope", "not yet decided") rather than as the
epic's settled, deliberate contract. Both have been reworded to say so
plainly; neither's underlying behavior changed.

## Deferred: rain and fluid integration

Rain washing, fluid dilution, and blood interacting with the world
fluid system are **deliberately deferred beyond this epic's closure**
(#901, closed as not planned as a fluid-only half-measure — the
decision is that rain and fluid weathering should be specified together
against ONE shared mechanism once a weather system exists, not built
piecemeal now).

**What a future integration would read.** Every `BloodDecal` already
carries the metadata a weather/fluid pass would need: wound kind
(`bdeWoundKind`), severity bucket (`bdeSeverity`), creation time +
wetness (`bdeCreatedAt`/`bdeInitialWetness`, from which current
wetness/dryness derive), position (`bdeX`/`bdeY`/`bdeSurfaceZ`), and
source unit (`bdeSourceUnit`).

**What exists today, precisely — no more, no less.** Three hooks exist,
none of them a weathering primitive on their own:

- `Blood.Types.clearBlood` / `blood.clear()` — clears an ENTIRE page's
  `BloodStore` (every texture and every decal). Not selective.
- `Blood.Types.removeDecalsForTexture` — removes every decal that
  shares a given texture reference, as the texture-FIFO-eviction
  cascade. Not exposed as a standalone Lua verb, and not selective by
  position, age, or unit.
- `Blood.Render.decalTint` — computes an age-derived tint/alpha at
  READ/render time. It doesn't mutate a decal (decal records are
  immutable) and can't be redirected by external state (rain, a nearby
  fluid tile) today.

There is **no per-decal removal primitive** (e.g. "remove decals within
this radius" or "remove this one decal") and **no weather-driven
mutation hook** (e.g. "darken/dilute decals touched by rain this tick")
today. A future rain/fluid integration would need to add such a
primitive — this is a real gap to design against, not an existing
capability this doc is merely under-explaining.

## Testing

See the **Blood decals (#603)** entry under CLAUDE.md's "Domain
contracts" and `docs/engine_contracts.md` §Blood decals: transience for
the turnkey probes, hspec `--match` targets, and the transience contract
restated for anyone writing a new blood test.

## Implementation history

The issues below carry the round-by-round design/review narrative in
their comment threads; this doc states only the resulting contract.

- #604 — decal/texture-pool data model + debug Lua surface.
- #606 — procedural texture generation + world-space rendering.
- #607 — impact blood from fresh wounds.
- #788 — GPU teardown on world page removal/replacement.
- #882 — ongoing bleeding: moving trail marks.
- #883 — ongoing bleeding: stationary/collapsed pooling.
- #884 — blood decal persistence — **closed, not planned** (see
  "Transience" above).
- #901 — rain/fluid weathering — **closed, not planned** (see
  "Deferred: rain and fluid integration" above).
- #885 — this doc; the final gate for epic #603 (closed after this PR
  merges, as a post-merge repository-management step).
