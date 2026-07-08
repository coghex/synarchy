# Procedural Blood Decals

Status: design draft, written 2026-07-07.

## Goals

Add visible blood from injuries without hand-authored blood texture
assets:

- A wound can create an immediate blood mark on the ground.
- A bleeding unit can leave drops, trails, or a local pool over time.
- Each visible mark should look procedurally unique, not like a stamped
  repeated asset.
- Blood should age visually from wet red toward dark dried red.
- Runtime texture and decal counts must be bounded so long fights cannot
  leak unbounded GPU textures, CPU records, or dynamic quads.

## Non-goals for the first pass

- True liquid simulation.
- Rain washing, fluid dilution, or blood flowing through the world fluid
  system.
- Blood on walls or vertical tile faces.
- Gore, severed-part sprites, body overlays, or unit-sprite wound art.
- A general-purpose decal system for scorch marks, grime, footprints, or
  damage.

Rain and fluid integration should be designed as future cleanup and
interaction hooks. Blood should expose enough metadata that later weather
or fluid code can darken, dilute, wash away, cover, or remove decals, but
that behavior should not ride along in the initial implementation.

## Shape of the system

The intended architecture is a bounded procedural decal system, not an
ever-growing collection of permanent texture uploads.

1. Gameplay code observes an injury or ongoing bleed and calls a blood
   API with parameters such as wound kind, severity, source unit, current
   tile/world position, direction if known, and amount.
2. The blood system turns those parameters into a requested decal style:
   pool, drops, spatter, streak, smear, or no mark.
3. Before generating a new texture, the blood texture pool checks whether
   an existing generated blood texture is close enough to the requested
   style.
4. If a match exists within the accepted threshold, the new decal reuses
   that texture reference with its own transform, tint, age, and world
   placement.
5. If no match exists, the system procedurally generates texture data,
   uploads it through the renderer's texture path, assigns it a stable
   blood texture reference, and pushes that reference onto the blood
   texture FIFO.
6. When the FIFO exceeds its configured maximum, the oldest blood texture
   is evicted. All world decal records that reference that texture are
   removed at the same time so stale placements simply disappear instead
   of pointing at invalid texture data.

This is what "bounded procedural decals" means here: individual injuries
can still produce unique-looking marks, but the system has a hard upper
bound on generated texture resources.

## Texture identity and matching

A generated blood texture should have a descriptor that records the
important visual dimensions used for matching:

- style: pool, drops, spatter, streak, smear
- wound kind that requested it
- severity/amount bucket
- approximate footprint size
- directionality/anisotropy bucket
- edge roughness and droplet density bucket
- seed or generation lineage

The exact match metric is an implementation detail, but the observable
contract is:

- nearby wounds should not all stamp the exact same mark when new variety
  is needed;
- visually similar requests may reuse textures to keep the pool bounded;
- repeated visible patterns should be hard to notice in normal play;
- evicting a texture must also clean up decals that reference it.

The texture reference used by world decals should be independent from the
raw GPU slot. Existing texture infrastructure already treats stable
handles and live slots as separate concepts; blood should preserve that
kind of separation so cached world placements do not depend on recyclable
GPU slots.

## World decal records

Each blood spot on the map is a world decal record. It should carry
enough information to render, age, save, and eventually interact with
weather/fluids:

- blood texture reference
- world/page identity
- tile or continuous world position
- surface z at placement time
- local offset, rotation, and scale
- creation game time
- current age/wetness/dryness
- source wound kind and severity bucket
- source unit id when known
- amount/opacity

When a texture reference is evicted, every decal using it is removed.
That cleanup may happen by reverse index, scan, or any other safe bounded
mechanism.

## Injury behavior

Immediate impact marks should reflect wound kind and severity:

- `stab`: small pool and/or clustered drops near the victim.
- `slash`: spatter or directional streaks; use attack or movement
  direction when available, otherwise choose deterministic variation from
  the event seed.
- `blunt`: usually no external blood. Only severe crushing, pulverizing,
  severing, or otherwise catastrophic wounds should create blood.
- `arterial` / `severed`: high amount, strong spatter or rapid pooling.
- `internal`, `fracture`, `concussion`: generally no external ground
  decal unless paired with a catastrophic external injury.

Severity should scale both the chance of creating blood and the size,
count, opacity, and aggressiveness of the mark. A minor scratch should
not paint the ground like a fatal wound.

## Ongoing bleeding behavior

Each unit that is externally bleeding needs a bleed-decal accumulator.
The accumulator should advance from the same effective bleed rate used
by the wound system, so bandaging, clotting, and healing naturally reduce
or stop visible blood.

The accumulator should support two visible outcomes:

- A moving unit leaves a trail of drops or short smears.
- A stationary or collapsed unit grows a local pool over time.

Emission should consider both elapsed game time and movement distance.
That prevents a moving unit from dropping many decals on the same pixel
every tick, while still allowing a stationary badly bleeding unit to pool.

## Aging

Blood should age visually without needing new textures for each age.
The first pass can use tint and alpha:

- fresh blood: wet, saturated dark red;
- drying blood: darker and less glossy/saturated;
- old blood: very dark red/brown or faded.

Aging does not need to force a texture rewrite. Per-decal tint, opacity,
or shader parameters are enough for the first pass.

## Save/load

Persistence can be built on the same texture-reference model:

1. Save the blood texture FIFO descriptors or enough generation
   parameters to recreate the generated textures.
2. Save world decal records with their blood texture reference ids.
3. On load, rebuild or reload the FIFO texture set first.
4. Reconnect each saved decal to the loaded texture reference.
5. Drop any decal whose texture reference cannot be restored.

The first implementation may choose whether save/load is included in its
scope, but the data model should not paint itself into a corner where
save/load requires replacing the whole design.

## Rendering notes

Blood is expected to render through world-space quads at first:

- above terrain/floor surfaces;
- below unit sprites;
- with the same camera-facing and world wrapping rules as other world
  overlays where applicable;
- culled to visible chunks/regions;
- omitted when its texture reference has been evicted.

A dedicated blood/decal pipeline may be useful later, but the first
version should prove the gameplay and visual behavior before adding a
new renderer path.

## Implementation notes (#604)

The first landing (model + debug surface, `Blood.Types` +
`Engine.Scripting.Lua.API.Blood`) pins down some things this design left
open:

- **Match metric.** Style and the severity/amount bucket are hard
  gates — any difference there always mints a new descriptor,
  regardless of the other dimensions (this is what makes "different
  styles or severity buckets create distinct descriptors" a hard
  contract, not just a likely outcome). Wound kind, footprint,
  anisotropy, and edge are soft: a wound-kind mismatch costs a flat 2,
  each bucketed dimension costs its ordinal distance, and a request
  reuses the closest existing descriptor whose total cost is ≤ 1 (one
  near-match bucket step away still reuses; two or more, or any
  wound-kind mismatch on its own, mint a new descriptor).
- **Texture cap.** Defaults to 24 (`Blood.Types.defaultBloodTextureCap`)
  — small on purpose, since this landing is the model and debug
  surface, not final tuning (see "Aging, caps, and cleanup tuning"
  below).
- **Storage shape.** The texture pool and the decal store live in one
  combined `Blood.Types.BloodStore` (one `IORef` per world page,
  `wsBloodStoreRef`) rather than two separate registries. Eviction has
  to cascade into decal removal, and that cascade needs to be atomic
  against a concurrent Lua call — two separate refs would only allow
  evicting the texture and removing its decals as two non-atomic
  steps.
- **Scope held to the issue.** No `Serialize` instance, no
  `WorldPageSave` field, no save-version bump — the store is
  per-session and dies with the `WorldState`, mirroring
  `wsStructureStageRef`. No rendering, no combat/wound hook, and no
  real texture generation (`btdSeed` is stored but unused pending a
  future generator).
- **Debug Lua surface.** `blood.spawn(gx, gy, woundKind, severity[,
  props])` resolves/creates a texture and places a decal in one call;
  `blood.getDecal`/`listDecals`, `blood.getTexture`/`listTextures`
  (oldest-first, each entry carries its 0-based FIFO rank),
  `blood.getTextureCap`, and `blood.clear` round out the inspection +
  reset surface.

## Implementation notes (#606)

The second landing (procedural texture generation + world-space
rendering, `Blood.Texture` + `Blood.Render` + `World.Render.BloodQuads`)
fills in the pieces #604 deliberately left open:

- **Texture generation.** `Blood.Texture.generateBloodTexture` is a
  pure function of a `BloodTextureDescriptor` — no IO, no lookup table.
  It folds every descriptor field into one `StdGen` seed
  (`descriptorSeed`), then composes a handful of soft-edged "splat"
  primitives into the requested style: a pool is 1–2 big overlapping
  blobs, drops/spatter scatter many small splats (count scaled by
  severity), and streak/smear lay a chain of splats along a seeded
  direction (thin+tapered vs. short+wide). Anisotropy is applied as a
  shared rotate/stretch transform over the whole canvas rather than
  per-splat, so it reads as directional regardless of style. Canvas
  size is bounded by footprint bucket (16/24/32 px square,
  `maxBloodTextureDim` = 32) — RGBA8, fully transparent
  (`0,0,0,0`) outside the generated shape. Determinism/distinctness/
  bounds are covered directly in `Test.Headless.Blood.Texture`
  (`cabal test synarchy-test-headless --test-options='--match
  "Blood"'`).
- **Aging stays decal-level, not texture-level**, exactly as the
  design's "Aging" section calls for: `Blood.Render.decalTint` derives
  an RGB tint + alpha from a decal's current `wetnessAt` (fresh =
  saturated dark red at full opacity; dry = desaturated brown at a
  0.35 alpha floor, never vanishing outright — eviction is what
  removes a mark, not aging). No texture is ever regenerated for
  aging.
- **Rendering reuses the existing quad/bindless path**, per
  requirement 9 — no dedicated decal pipeline. `World.Render.BloodQuads`
  splits into two independent halves that only meet through a new
  per-world `wsBloodTextureHandlesRef` map:
  - `uploadBloodTextures` (GPU-touching, runs once per frame from
    `Engine.Scripting.Lua.Message.processLuaMessages`, gated on
    `whenGraphical` — headless has no device) diffs each loaded
    world's texture FIFO against what's already GPU-resident: uploads
    pixel data for anything new (`Engine.Graphics.Vulkan.Texture.
    createTextureFromRGBABytes`, a factored-out version of the
    staging-buffer upload `handleWorldPreview`/`handleZoomAtlasUpload`
    already did inline) and registers it with the bindless system.
    Eviction unregisters the bindless slot, drops the now-stale
    `textureSizeRef` entry, and disposes the GPU image/view — but only
    after a `deviceWaitIdle`, since this runs from `processLuaMessages`
    before `drawFrame`, and `drawFrame` only waits the CURRENT frame's
    own fence, not every frame still in flight (the same reasoning
    `Engine.Scripting.Lua.Message.disposeTransientTexture` documents
    for the shared preview/zoom-atlas texture). The wait only fires
    when a pass actually has something to evict, not every frame.
  - `renderBloodDecalQuads` (pure IO, no GPU calls, same shape as
    `World.Render.GroundItemQuads`) turns each visible decal into a
    `SortableQuad`, sat at a sort-key nudge of 0.0003 above bare
    terrain (0.0) and below ground items/units (0.0006) — the same
    convention `World.Render.SpoilQuads`/`FloraQuads` use for "sits on
    the ground". Rotation is applied as real corner-vertex rotation
    (no shader change); a decal whose texture hasn't been GPU-uploaded
    yet (or never will be, headless) simply contributes no quad.
  - Chunk-visibility culling (`isChunkVisibleWrapped`/
    `computeViewBounds`, the same functions ground items use) keeps
    the VISIBLE quad count bounded regardless of world size.
- **Decal count has its own cap, independent of the texture cap.**
  Texture-FIFO eviction cascades to remove its decals, but a request
  that keeps *reusing* an already-live texture (the near-match design
  is built to encourage exactly that) never triggers texture eviction
  at all — without a bound of its own, decals could accumulate
  forever, and `Blood.Render.bloodRenderRecords` scans every stored
  decal before `renderBloodDecalQuads` gets to cull by visibility, so
  an unbounded decal list means unbounded per-frame work even for an
  empty screen. `BloodDecals` now carries its own FIFO
  (`bdlOrder`/`bdlCap`, `defaultBloodDecalCap` = 512, mirroring
  `BloodTexturePool`'s `btpOrder`/`btpCap`) — `addDecal` evicts the
  oldest decal once over cap, independent of which texture it
  references. `removeDecalsForTexture` (the texture-eviction cascade)
  also prunes `bdlOrder` of the ids it removes, so that queue can't
  grow unboundedly stale either.
- **Debug/headless surface without a GPU.** `blood.getRenderQuads
  ([pageId])` exposes `Blood.Render.bloodRenderRecords` directly —
  the same resolved position/tint/alpha data the real renderer
  consumes, computed purely from the decal + texture-pool state (no
  dependency on `wsBloodTextureHandlesRef`/GPU upload), so a headless
  probe can assert renderability and aging tint without a display.
  `blood.listTextures`/`getTexture` also now report each descriptor's
  generated `width`/`height`/`pixelHash` (regenerated on demand via
  `generateBloodTexture` — never cached alongside the descriptor, so
  it can't drift from what actually gets uploaded).
- **Scope cut: no GPU cleanup on world teardown.** FIFO eviction
  *within* a live world always unregisters + disposes the GPU texture
  (requirement 5). A handful of world-teardown sites (full page
  removal, save-load stale-page cleanup) do NOT — the last few
  registered blood textures for a destroyed world page currently leak
  their GPU resource rather than routing a dispose signal across the
  world→render thread boundary for content that's already explicitly
  out of scope for persistence. Bounded by `defaultBloodTextureCap`
  tiny (≤32×32) textures per destroyed world page; a follow-up if it
  ever matters in practice.

## Implementation notes (#607)

The third landing (`Blood.Impact`) connects fresh wound creation to
one-shot impact blood — the "Injury behavior" section above, made
concrete:

- **Reused existing severity signals, not a new "blunt severity"
  threshold**, per the issue's explicit ask. Two thresholds, both
  already load-bearing elsewhere:
  - `catastrophicBluntThreshold = 0.85` — `scripts/injury_log.lua`'s
    own T4 tier boundary, the point at which that script's narration
    switches to "crushing" (bone), "pulverizing" (nerve — the same
    family concussion's wording draws from), or "pulping" (soft
    tissue). Gates `blunt` and `concussion`: below it, a hit is merely
    "strikes"/"clubs"/"bashes"/"smashes" — no blood; at/above it, blood.
  - `Combat.Wounds.destroyThreshold = 1.0` — the existing structural-
    destruction cutoff (a fracture/severed wound at/above it "destroys"
    the part). Gates `fracture`.
  - `impactSeverityBucket` reuses the SAME T1..T4 boundaries
    (0.25/0.50/0.85) for the generated mark's own severity bucket, so
    "how strong the mark looks" agrees with "how the wound narrates".
- **Wound kind → style** (`defaultStyleForWound`) was already defined
  in `Engine.Scripting.Lua.API.Blood` for the #604 debug `blood.spawn`
  surface's inferred-style default; it now lives in `Blood.Impact` and
  that Lua module imports it, so an automatic impact mark and a
  manually-spawned one for the same wound kind read as the same family
  of mark instead of two mappings that could drift apart.
- **`internal` never draws blood**, at any severity — it's the one
  kind with no "catastrophic" exception, since it names damage with no
  plausible skin-breaking mechanism.
- **`arterial`/`severed` floor at `SeverityModerate`** rather than
  being unconditionally `SeverityCatastrophic` — "always at least a
  strong mark" (the design's "high-volume" framing) while still
  scaling further with actual severity above that floor, so the "wound
  severity must affect amount" requirement holds for every kind that
  draws blood, not just stab/slash.
- **Direction**: real attacker→target angle when one exists (combat
  resolution always has both positions); `impactFallbackAngle` — a
  pure function of a caller-supplied deterministic seed — otherwise
  (falls have no attacker; `unit.injure` is a bare debug call). Feeds
  the decal's own `bspRotation`, not the texture's internal seed.
- **Bounded per event, not per wound.** A single landed hit or fall can
  produce several tissue-layer wounds (a combo "paw", several
  fractured bones); each PRODUCER calls `spawnImpactBlood` at most
  ONCE per event, keyed off the same "headline" (kind, severity) the
  surrounding code already treats as canonical for that event (combat
  resolution's death-check/stance-hit scalar; the fall model's
  worst-injury scalar) — `spawnImpactBlood` itself always places
  exactly one decal per call, so bounding is entirely the caller's
  responsibility, not something the shared function enforces.
- **Three production call sites**, all Haskell (no Lua round-trip):
  `Combat.Resolution.runResolution` (after the wound-append + hit
  event), `Unit.Thread.Movement.tickAllMovement` (after the fall
  injury-log push), and `Engine.Scripting.Lua.API.Units.unitInjureFn`
  (the debug surface requirement 2 calls for, letting headless probes
  exercise every wound kind without staging a real attack). The
  wound-tick's `propagateSevering` cascade (a destroyed part severing
  its children, `Combat.Wounds`) is deliberately NOT a fourth site —
  it's bookkeeping on an ALREADY-bled wound, not a fresh injury landing.
- **Skips safely, never crashes**, per requirement 8:
  `spawnImpactBlood` looks up the wounded unit's page in `wmWorlds`
  and does nothing if it isn't found, after the wound itself has
  already been committed — a missing/unloaded page can never roll back
  or corrupt the wound creation it rode in on.

## Suggested issue split

### 1. Epic: procedural injury blood decals

Track the complete feature arc: impact blood, generated texture pool,
world decal storage, rendering, ongoing bleeding trails, aging, cleanup,
persistence, and future rain/fluid hooks.

### 2. Blood decal model and debug surface

Add the world-scoped blood decal data model, generated texture
descriptor model, FIFO pool policy, and debug/headless commands to spawn,
list, and clear decals. This issue should not require combat or rendering
to be complete.

### 3. Procedural blood texture generation and rendering

Render decals from generated blood textures. The implementation should
create unique-looking texture data, reuse near-matching textures within a
threshold, evict by FIFO when over cap, and remove corresponding world
decals on eviction.

### 4. Impact blood from new wounds

Connect wound creation to blood decal spawning. `stab`, `slash`, severe
`blunt`, `arterial`, and `severed` wounds should produce distinct
severity-scaled requests.

### 5. Ongoing bleeding trails and pools

Add per-unit bleed-decal accumulators driven by effective bleed rate.
Moving units leave trails; stationary units pool; clotting/bandaging/
healing reduces emission.

### 6. Aging, caps, and cleanup tuning

Tune drying/darkening, decal caps, texture caps, eviction behavior, and
test coverage so long-running combat remains bounded.

### 7. Save/load persistence

Persist the blood texture FIFO descriptors and world decal records, then
restore them across save/load.

### 8. Rain/fluid integration follow-up

Later, connect blood decals to rain and fluid behavior: washing, fading,
dilution, covering, and removal.
