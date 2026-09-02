# Designation tool visual language design

Synarchy's standing work designations currently share one generic tile-quad
render path even when they describe different kinds of targets. This design
separates those meanings: excavation may occupy a full three-dimensional tile,
surface work should read as a flat ground treatment, and tree work should mark
the tree the player selected rather than painting the whole ground tile.

Design state: `ready for issue processing`

> **2026-08-28:** returned here from `/process-design-doc` after repository
> evidence contradicted D-11's premise. The construction portion is being
> redesigned around three lifecycle states and a two-alpha ghost model; the
> single DTV-4 slice is retired and replaced by DTV-9, DTV-10 and DTV-11.
> Epic #1837 already exists, its ledger line stays terminal, and its child
> checklist was reconciled to DTV-9 through DTV-11. DTV-12 and DTV-13 were
> added by the later audit and must be synchronized to the epic after fresh
> readiness signoff and before any child is filed. #1780 is closed as
> superseded against the epic (D-23). Chop, Till and crop Plant (DTV-1 to
> DTV-3) and the construction art slices (DTV-5 to DTV-8) are unchanged by
> this pass. Q-10 to Q-12 remain deliberately open owner-authored art
> checkpoints. Q-16 is resolved by D-25's per-tile preview rule, and the owner
> gave fresh readiness signoff after the revised construction design. A second
> code-grounded audit then found three material gaps — structure preview extent,
> Chop's durable target identity, and the structure plan-validity boundary — so
> the document returned to `exploring` under Q-17 to Q-19 before any child was
> filed. The owner resolved all three on 2026-08-28 through D-26 to D-28. Those
> answers materially expanded the persistence and invalidation contracts. The
> owner gave fresh readiness signoff on this revised behavior, scope and
> 12-slice delivery plan on 2026-08-28. The document is ready; epic #1837 still
> needs its DTV-12/DTV-13 checklist synchronization before the first child is
> filed.

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Give each designation tool target-appropriate interaction and visual feedback — [#1837]
- [x] DTV-9. Resolve structure-pack piece art engine-side for unplaced pieces — [#1842]
- [x] DTV-13. Make structure drag planning authoritative and self-clearing — [#1844]
- [x] DTV-10. Ghost planned buildings with their own art in both ghost states — [#1845]
- [x] DTV-11. Ghost planned structure pieces with their own art in both ghost states — [#1846]
- [x] DTV-5. Author Kitchen construction-progress art — [#1848]
- [x] DTV-6. Author Machine Shop construction-progress art — [#1849]
- [x] DTV-7. Author Workbench construction-progress art — [#1850]
- [x] DTV-8. Require progress art for every work-constructed building — [#1853]
- [x] DTV-12. Give every flora instance stable identity and exact mutable state — [#1854]
- [x] DTV-1. Make Chop a drag-box tool with tree-anchored designation markers — [#1856]
- [x] DTV-2. Render Till as a flat top-surface designation — [#1857]
- [x] DTV-3. Render the existing crop Plant job as a light-green tilled surface — [#1858]

## Epic contract

- **Goal:** Make every standing work designation communicate what the player
  actually targeted: a three-dimensional mine tile, one or more trees, a flat
  tilling surface, an existing crop-planting job, or the actual structure or
  building planned for construction.
- **Done when:** Mine retains its full three-face tile reading; Chop uses the
  settled drag gesture and marks only eligible, stably identified tree
  instances through per-instance state which survives chunk eviction and
  save/load; Till and the existing
  crop Plant tool use settled flat-surface rendering; crop Plant designations
  cancel if their soil ceases to be tilled; planned construction shows the
  target's own texture, geometry, and facemap in BOTH the pre-commit preview
  and the committed designation, at the two settled alphas, with
  64-tile anchor-origin preview/commit parity, per-candidate invalid-placement
  feedback on the preview only, no-art diagnostics without a fallback, and
  later invalidation cleanup; structures then remain
  invisible during active work until complete, while every work-constructed
  building uses its own required progress art; appearance-only buildings remain
  a distinct lifecycle; every changed asset has explicit user signoff; and
  focused automated plus UI-capable checks distinguish the settled behaviors.
- **Users and operators:** Players directing colony work; maintainers of world
  tool input, flora selection, designation state, world rendering, and HUD art.
- **Arc label:** None proposed

## Current state and evidence

- `worldCursorToQuad` is the explicit three-face cursor path: it supplies
  `wtIsoFaceMap`, whose red/green/blue channels represent the right, top, and
  left faces. Mine, Chop, Plant, construction, and generic cursor art retain
  that behavior. Till's committed markers and rectangle preview instead use
  the reusable `worldFlatCursorToQuad`, which emits `noFaceMapVertexId`; the
  approved Till texture's own alpha is therefore its complete silhouette.
- Mine already follows the desired three-dimensional model. Its 96×64 marker
  covers the full tile silhouette, and the shared isometric facemap preserves
  distinct top and side faces. The arc should preserve this behavior rather
  than redesign it without evidence of a defect.
- Chop is currently a two-click tile rectangle. The first click stores an
  anchor and the second commits; this is not a press-drag gesture. The world
  handler already filters the rectangle to currently harvestable, wood-tagged
  flora and stores each tree tile at its own surface height, but the renderer
  paints the same ground-aligned tile quad shape used by other designations.
- **Chop is not instance-addressed today.** There IS a separate
  `FloraInstance` record for every rendered plant, with species, local tile,
  sub-tile offset, z, age, health, variant and base width. The missing concept
  is stable mutable identity: generated instances are recreated
  deterministically when chunks reload, rather than saved as an authoritative
  mutable chunk snapshot. `ChopDesignations`,
  `FloraHarvests`, the Lua claim registry, and `world.harvestFlora` are all
  keyed by canonical tile. `FloraInstance` has no stable instance id or Chop
  designation field, and
  `placeTileFlora` independently admits every fitting species before
  concatenating the results, so more than one wood-tagged tree species can
  occupy one tile. The existing tagged harvest then takes the first matching
  instance while the tile-level regrowth timer changes every harvestable
  co-tenant's rendered state. D-27 keeps the per-instance data model the owner
  expected while adding the identity and persistence layer needed to make a
  mutable Boolean survive eviction and save/load.
- Till is also a two-click rectangle. Its handler keeps only level tiles
  (surface slope ID exactly zero) at the anchor's surface z, with no fluid,
  flora, or already-tilled soil. Its approved marker is a flat translucent
  orange top-surface diamond. The completed tilled-soil source preserves its
  existing RGB art but carries the same intrinsic flat-diamond alpha, so a
  facemap fault cannot reveal its otherwise unused canvas.
- Plant is a single-tile tool reached through the crop-selection panel. Both
  Lua and the world command require tilled soil before a designation is
  accepted; the command additionally excludes existing flora and crop plots.
  Its committed marker stores the accepted surface z and renders through the
  same three-face cursor helper. Rendering does not independently re-check the
  soil state each frame.
- Plant queries and the farm AI likewise trust designation existence after
  admission; neither re-reads soil. The clean cancellation signal is therefore
  world-owned removal of the durable designation after a resolvable terrain
  mutation or load/chunk publication proves `isTilledSoil` false. The existing
  AI already releases its local claim/job when that designation disappears on
  its next utility/execute tick. Unloaded terrain is unknown, not invalid, and
  must wait for revalidation rather than being discarded.
- The default face map is a 1×1 pure-green texture meaning 100% top-facing.
  `assets/textures/facemap/vegface.png` is a separate 96×64 top-face-only mask,
  while `isoface.png` includes top, left, and right faces. D-10 selects the 1×1
  default/noface path and makes each authored designation texture's own alpha
  mask the authority for its visible surface shape.
- `construct_designate_structure.png` and `construct_designate_building.png`
  were category-level placeholders from #1780. That proposed solution is
  intentionally abandoned: planned construction should not use replacement
  category marker art at all. #1780 is closed as superseded against epic
  #1837 (D-23), and the stale narrative in `docs/asset_system_findings.md`
  (ASSET-5) now redirects here and to the two replacement children. #1846
  removed the STRUCTURE placeholder, its handle, its HUD wiring and the
  structure branch of `construction.setDesignateTexture`; #1845 (DTV-10) owns
  the building half, so `construct_designate_building.png` is the only
  category marker still shipped.
- A planned `CtBuilding` already names a building definition whose
  `bdSouthTexture`, pixel dimensions, footprint anchor, and `bdSpriteAnchor` drive
  the placement ghost and final renderer. The current committed designation
  instead expands one generic 96×64 marker across every footprint tile. The
  building ghost must be one target-shaped sprite, not a repeated tile marker.
- **The building half is largely existing machinery** (verified 2026-08-28 at
  `dc470999`). `Building.Render.buildingToQuad` already draws the real
  `bdSouthTexture` at 0.6 alpha with correct scale, `bdSpriteAnchor` offset,
  footprint anchor and sort key — its `isGhost` predicate is positive
  `bdBuildWork` plus unsatisfied materials. `renderGhostQuad` does the same
  for the cursor-following placement ghost, and `ghostTint` returns
  `Vec4 1.0 0.4 0.4 0.6` when placement is invalid. `pickBuildingFrame`
  already indexes construction frames by `biBuildProgress / bdBuildWork`
  whenever the activity is `Constructing` (#2080 split that out of the
  overloaded `Appearing`, which now means timed appearance alone).
- **`cdProgress` is dead for buildings.** `World.Construct.Apply` states that
  building designations never accrue progress — they are staked into a real
  building, which owns its own construction visuals — so `cdProgress` is only
  ever nonzero for `CtStructure`. `CursorQuads.hs:247`'s
  `0.45 + 0.55 * cdProgress` therefore pins every building designation at the
  ramp's floor forever. The ramp is not a working feature to preserve.
- **A `CtStructure` designation cannot resolve its own art today, and this
  contradicts D-11's original premise.** It stores only the abstract
  `StructurePiece{spPack, spKind, spEdge}` and deliberately no texture (#95:
  art is "resolved at build time"). The pack/kind → texture+facemap
  translation lives ONLY in `scripts/structures.lua`'s `handles()`, reading
  `data/structure_packs/<pack>.yaml`; the engine first learns a piece's art
  when Lua calls `structure.place`. `StructureWallCatalog` does not close the
  gap — it is walls-only, keyed by an already-placed texture PATH, and exists
  for #1712 camera rotation. `structurePieceSlot` already maps pack/kind/edge
  to a `StructureSlot` engine-side, so the missing piece is the ART lookup
  alone, and it needs new Lua→engine registration plumbing shaped like the
  existing `structure.registerWallFamily` verb.
- `Structure.Render.structurePieceQuads` already takes `tileAlpha` as a
  parameter and owns piece slot, facemap, #1712 camera-facing wall rotation,
  post placement and the #415 front-wall depth strips — so once the art
  resolves, the structure ghost is a call into the existing renderer.
- **The two tool families already diverge at the PREVIEW stage.** A building
  previews with its real `bdSouthTexture` at 0.6 alpha following the cursor, while
  a structure previews through `CursorQuads.hs`'s `constructPreviewQuads`
  using the generic `world_select.png` cursor tile at full alpha, one flat
  quad per rectangle tile (honouring `constructLineMode` for the #359 wire
  path tool). The structure path emits nothing before its first anchor click,
  and only the building path shows invalid-placement feedback. DTV-11 must add
  the unanchored single-piece preview required by D-19 as well as replace the
  anchored rectangle/line art.
- **Structure preview and commit disagree about maximum extent.**
  `CursorQuads.hs` applies `maxMinePreviewSide = 64` to the structure preview,
  while `World.Thread.Command.Cursor.Common.designateRect` lets the commit span
  128 tiles per side. A drag can therefore commit three times as many tiles as
  it showed. The clamp direction differs too: preview bounds outward from the
  first-click anchor, while `designateRect` clamps from the rectangle's
  low-coordinate end and can exclude that anchor on a long negative drag.
  Wire adds a third divergence: preview clamps both deltas before choosing its
  dominant axis, while Lua chooses the axis from the raw localized endpoint and
  the world handler clamps afterward, so a long diagonal can preview one axis
  and commit the other. D-25's exact-set promise requires the one bounded-drag
  rule selected in D-26.
- **The current structure commit is admission, not final placeability.** It
  checks loaded/same-z terrain, target-slot occupancy, and outstanding
  designations, but it accepts a post without a placed floor and accepts an
  abstract pack/kind even when the build AI has no `build:` entry. The AI then
  skips either job indefinitely. Registration failure would add a third dead
  state under the earlier DTV-9 wording: an invisible but still accepted job.
  D-28 makes the shared oracle fail these plans closed and establishes the
  later-invalidation cleanup contract.
- A structure ghost also needs more than art lookup. `cdZ` stores terrain
  surface z, while the real placer uses surface+1 for floors, walls, and wire,
  surface+2 for ceilings, and the existing floor's z for posts. Wall facemaps
  are selected dynamically from same-tile post caps. An exact ghost must share
  those final-grid-z and wall-cap rules with placement rather than calling the
  current renderer at raw `cdZ` with a fixed facemap.
- The new art catalogue is cross-thread, boot-process render handoff state:
  Lua registers it and the world render thread reads it. The existing
  `RenderHandoffCapability` already owns the analogous
  `structureWallCatalogRef`. Registration must not intern unused paths into
  the persisted `TexPalette`; `structure.place` deliberately waits until a
  real validated placement before doing that so rejected/unplaced art cannot
  become permanent save residue. DTV-9 must respect the capability and
  persistence inventories when it chooses the concrete catalogue shape.
- **Today's alpha numbers do not mean what they read as.** The marker PNGs
  bake alpha 150/255 (≈59%) into the texture, so the "45%" designated
  construction marker is ≈26% before the shared world fade. Under the new
  model 25% and 60% are explicit ghost-opacity factors applied to target art;
  source-pixel alpha and the existing `tileAlpha`/depth fade still multiply the
  final framebuffer alpha.
- A building has two storage representations during the one Designated visual
  state. The durable `CtBuilding` entry renders until a worker reaches the site;
  `building.spawn` then removes that designation and creates a real
  `BuildingInstance`, whose pre-delivery renderer is already a 60% finished-art
  ghost until materials are satisfied. DTV-10 must cover both halves so staking
  causes no opacity or geometry discontinuity before progress art begins.
- `applyConstructSlopeToChunk` stamps corner progress into the chunk's
  `ctSlopes`, which `src/Unit/Pathing/Cost.hs` reads — so that stamping is
  pathing state, not decoration. D-18 fences it out of this arc.
- Cargo Hold and Furnace currently provide progress-driven construction frames.
  Kitchen, Machine Shop, and Workbench declare positive `build_work` but no
  construction animation, so they need target-specific progress art authored
  under DTV-5 to DTV-7. The Acolyte Portal has an appearance animation with zero `build_work`;
  that is an appearance lifecycle, not construction progress.
- Open #1692 owns only the 32×32 Till and Plant toolbar-icon redraw. It does not
  own the 96×64 standing designation markers or their render behavior.
- `docs/harvest_tool_design.md` is ready for issue processing and separately
  designs a new Harvest tool. Its HARV-2 slice specifies press-drag selection
  using rendered flora geometry, and HARV-4 owns a designated-plant marker, but
  the design explicitly excludes redesigning Chop, Till, Plant, or Mine. The
  arcs should share or coordinate gesture/projection infrastructure without
  silently merging tracker ownership.
- An open-issue title search, repeated during readiness review on 2026-08-28,
  found no existing designation-tool umbrella. #1780 is the superseded
  construction-marker overlap; #1692 is the adjacent toolbar-icon issue; #1762
  is an unrelated crop-probe soil-gating issue.

## Desired experience

- Mining designation remains a translucent, full three-dimensional tile so the
  player reads excavation as work in volume, not merely a painted floor.
- Chop becomes a mouse press-drag box interaction. Committing the gesture
  designates only eligible trees and places a translucent annotation on each
  selected tree rather than covering its terrain tile with a generic overlay.
- Till marks only the flat ground surface which will be worked. Its appearance
  does not imply designation of the tile's vertical side faces.
- The existing crop Plant designation uses the same flat-surface visual
  language as Till, but with a light green category colour, and exists only
  while its land remains tilled.
- Planned structure pieces and planned buildings display the art of the target
  that will actually be built, through the target's normal geometry and
  facemap, across THREE states the player can tell apart at a glance:
  - while the mouse moves or drags with a build tool armed, a **very light
    25% preview** answers "where would this go?", and turns red where the
    placement is invalid;
  - once committed, a **60% designated ghost** answers "this is queued to be
    built here" — the same art, visibly more solid than the preview, and
    never showing invalid-placement feedback;
  - when construction actually begins, a structure ghost disappears and
    leaves the site visually empty until the finished piece appears, while a
    work-constructed building switches to its own required
    construction-progress art.
  No state uses a dedicated structure/building marker texture or a generic
  progress fallback.
- Every tool remains visually legible at ordinary gameplay zoom and under the
  existing runtime alpha multipliers.

## Scope

### In scope

- Committed in-world designation interaction and rendering for Chop, Till,
  the existing crop Plant tool, and Construction.
- The pre-commit construction PREVIEW as well as the committed designation:
  the two are one visual language (D-19) and cannot be settled apart.
- The engine-side art resolution a structure ghost needs for an unplaced,
  abstract piece (D-11, DTV-9).
- Preservation and regression coverage of Mine's full three-face behavior.
- Any narrowly shared render or gesture boundary required by more than one
  in-scope tool.
- User-authored final bitmap assets, gameplay-scale visual QA, and explicit
  signoff for every changed marker.
- Focused tests or probes for selection identity, facemap choice, lifecycle,
  and visible output.

### Out of scope

- Mining job execution, terrain carving, or a new Mine interaction model.
- Till and Plant toolbar icons, already owned by #1692.
- The separate player-directed Harvest gameplay arc in
  `docs/harvest_tool_design.md`.
- A new general planting or tree-planting feature. Planting a tree on ordinary
  ground needs its own design for species choice, placement eligibility,
  designation state, worker behavior, persistence, and visual feedback; it is
  not the behavior of the existing crop Plant tool.
- Changing farming, chopping, planting, or construction AI job execution except
  where designation invalidation is inseparable from the settled marker
  lifecycle.
- A general redraw of unrelated tool or cursor art.
- The construction corner-progress SLOPE stamping
  (`applyConstructSlopeToChunk` → `ctSlopes`, read by
  `src/Unit/Pathing/Cost.hs`). Fenced by D-18: it is pathing state rather
  than designation rendering, and player-created construction is not a
  slope-building feature. This arc neither removes nor extends it.
- Adding a variant field to the save-serialized `StructurePiece` (D-24 —
  the build path is variant-free too, so nothing needs one).
- Creating the epic or child issues during design.

## Design

### Semantic render classes

The arc distinguishes four render meanings rather than treating every marker
as a differently coloured copy of one PNG:

1. **Volume:** Mine occupies the full three-face tile silhouette and continues
   through the isometric facemap.
2. **Target annotation:** Chop identifies a tree instance selected for work.
   Its marker belongs at a settled tree-relative screen/world anchor and is not
   a terrain-tile fill.
3. **Surface:** Till and the existing crop Plant marker cover only the settled
   top-surface region and use top-facing lighting. Their source textures,
   facemap selection, and placement must agree so side faces cannot reappear
   through either mask.
4. **Target ghost:** Construction uses the actual selected structure or
   building presentation. Its texture, sprite dimensions/anchor, facemap,
   camera-facing behavior, and footprint geometry remain target-owned; the
   designation layer forces only the settled ghost alpha and, in the preview
   state alone, the invalid-placement tint. This class spans THREE states —
   preview, designated, and building — enumerated in §Construction ghost
   states below. After work begins, structures intentionally emit nothing
   until completion and constructed buildings hand off to their own progress
   art.

### Construction ghost states

One target presentation, three states. The art is identical across preview
and designated; only alpha and the invalid tint differ, which is what makes
the two read as the same planned object at two levels of commitment.

| State | Trigger | Alpha | Invalid feedback |
|---|---|---|---|
| Preview | a build tool is armed and the mouse is hovering or dragging; nothing committed | 25% | yes — red tint (D-20) |
| Designated | the designation is committed and work has not started | 60% | no |
| Building | work has entered its progress phase | structure: nothing rendered · building: its own progress art at full opacity | n/a |

The preview is deliberately LIGHTER than the designated ghost, so commitment
reads as solidity. This inverts today's building behaviour, where the
cursor-following preview (0.6) is more opaque than the committed marker
(≈0.26 effective) — that inversion is the point, not an incidental
side effect, and a reviewer should expect to see it change.

The Building-state transitions are unchanged from D-15/D-16: for a structure
the durable trigger is the designation's payment record — DTV-13 (#1844)
replaced `cdMaterialsPaid` with `cdPayment`, whose RECEIPT records the exact
materials removed and whose presence IS the paid state; for a work-constructed
building it is the material gate opening and the instance entering progress
rendering.

### Interaction and target identity

Chop's current two-click rectangle already resolves eligible wood-tagged flora,
but D-9 replaces its tile-coordinate box with a screen-space tree selector.
Clicks use sprite hits and drags use rendered ground-contact anchors, including
around cliffs. The nearby Harvest design establishes the same selection model,
so Chop should share that projection and gesture lifecycle if delivery order
permits while retaining its own eligibility, designation state, cancellation,
and work consumer.

`FloraInstance` is already the per-plant data record, but its generated base
contents are recreated when a chunk materializes. D-27 therefore adds a stable
`FloraInstanceId` and an explicit per-instance Chop-designated Boolean while
also recording every Boolean transition in durable identity-keyed world state
or the equivalent replayable edit. Loading a chunk hydrates the live Boolean
from that durable state; changing it updates both through one owning operation.
The field may not be a transient-only flag which silently resets on eviction.
Selection, marker rendering, claims, exact harvest, regrowth and invalidation
all carry the same id instead of falling back to tile identity.

Till retains its same-z rectangle semantics and cumulative fluid, flora, and
already-tilled filters. The owner expanded its admission rule during DTV-2:
farming is possible only on level ground, so a nonzero surface slope is also
ineligible. The repository's current Plant tool is specifically a single-tile
crop workflow: it opens a crop-selection panel on tilled soil and accepts only
`row_crop` or `groundcover_crop` species. It cannot plant a tree. DTV-3 changes
that existing crop designation's committed visual meaning and invalidation
lifecycle, not its target-selection flow or supported species.

### Lifecycle

The existing crop Plant designation is refused unless the tile is tilled and
unoccupied at commit time. D-14 extends that from admission to a standing
invariant: if the land ceases to be tilled, the designation and any claimed or
in-flight job are cancelled. Rendering must never merely hide a still-active
job. Tree planting on ordinary ground is a different, currently unspecified
feature and must not be made subject to this crop-only rule by accident.

Structure placeability is likewise a standing invariant, not merely a
pre-commit hint. D-28's shared plan resolver is authoritative for both preview
and commit. A later resolvable world mutation which makes a pending or active
designation unbuildable triggers an atomic cancellation attempt: the durable
designation and progress slope are removed, any live claim is abandoned, and
already-paid materials are refunded exactly once. A competing completion or
cancel may win the atomic pop, in which case the invalidator has nothing left
to clear. Unloaded terrain is unknown rather than invalid; retain its
designation and revalidate it when the chunk materializes. Known catalog or
metadata failure does not need terrain and remains invalid.

### Asset ownership

The user will author the final Chop, Till, and crop Plant textures while solving
their corresponding art slices. Construction reuses each target's existing
finished art and adds no final category-marker assets, but every building with
positive construction work must provide its own progress sequence. The user
will author the missing Kitchen, Machine Shop, and Workbench sequences in
dedicated DTV-5, DTV-6, and DTV-7 art slices, one issue and PR per sequence,
with gameplay-scale signoff on each. DTV-8 applies the no-fallback guard only
after all three land. The design records purpose, dimensions/render class,
category meaning, and required gameplay-scale signoff; it does not fabricate
interim final art. An issue which cannot be implemented until an asset choice
exists must stop at that explicit owner checkpoint rather than guessing pixels.

## Proposals

### P-1. Replace #1780 through the epic rather than continue rereviewing it

Originally adopted by D-8; its child-before-closure timing was superseded by
D-23 after epic #1837 existed.

Create the umbrella epic first, process the construction-ghost child early,
then close #1780 as superseded with links to both the epic and its concrete
replacement child. This preserves the original verified defect and its review
history while ensuring it never becomes an ownerless redirect. Closing #1780
before the replacement child exists is possible but creates a temporary gap in
actionable ownership. D-23 records why the owner later accepted that temporary
gap and closed #1780 against the already-filed epic instead.

### P-2. Reuse the Harvest gesture/projection boundary for Chop

Adopted by D-9.

If HARV-2 lands first, Chop should reuse its press-drag lifecycle and
flora-render projection rather than create a second nearly identical selector.
If Chop lands first, its shared boundary should be deliberately suitable for
HARV-2. The two tools retain separate eligibility and commands.

### P-3. Establish the flat designation path in Till, then reuse it for Plant

Adopted by D-10.

Till is the simpler surface-only case and can establish the explicit top-only
render contract. Plant can then reuse that boundary while adding its light-green
art and settled tilled-soil lifecycle.

## Decisions

### D-1. Mine remains a full three-dimensional tile designation

Mining acts on solid volume, so its committed marker continues to cover and
light the top, left, and right faces of the isometric tile. No Mine redesign is
required unless verification finds that current output violates this contract.

### D-2. Chop moves from two-click rectangle input to a mouse drag box

The player will press and drag a box to designate trees for chopping rather
than click one corner and then another. D-9 and D-12 settle target selection,
the click/drag threshold, and symmetric add/erase behavior.

### D-3. Chop marks trees with a translucent icon

A committed Chop designation annotates each eligible selected tree with an
alpha-bearing icon rather than covering the whole terrain tile with the current
flat green overlay. D-12 settles its ground-contact-relative anchor.

### D-4. Till is a flat surface designation

Till represents work on a two-dimensional ground surface. Its committed marker
must show only the settled top-surface shape and must not imply that either
vertical tile side is designated. Farming is admitted only when the stored
surface slope ID is exactly zero; an absent or out-of-range slope entry is not
silently treated as level.

### D-5. The crop Plant marker is a light-green flat surface

The existing crop Plant tool remains in this arc. Its marker uses the settled
Till surface language but remains distinguishable with a light-green category
colour. It is shown only under the settled tilled-soil rule. This does not
specify future tree planting.

### D-6. Final marker textures are owner-directed and signed off during delivery

The owner directs each final alpha-bearing texture while solving its delivery
slice, and every final texture requires gameplay-scale validation and explicit
signoff before merge. For DTV-2 the approved Till marker is a 96×64 flat
top-surface diamond whose sole visible colour is RGBA `(232, 126, 38, 88)`;
the owner approved a live contiguous 5×5 arena field on 2026-08-30.

### D-7. The earlier exact-mask, hue-only construction proposal is superseded

The previous proposal to make both construction markers exact Mine-mask
recolours no longer defines the design. D-11 replaces it with actual-target
ghost rendering owned by DTV-10 and DTV-11.

### D-8. Create the replacement tracker chain before closing #1780

*Amended 2026-08-28: the named replacement child changed when DTV-4 was
retired. D-23 later superseded this decision's closure timing for #1780.*

Process the umbrella epic first. Only once a CONCRETE replacement child also
exists should #1780 close as superseded, with durable links to the epic and
that child. The remaining children continue one at a time after the redirect
is in place. This preserves #1780's verified defect and review history without
leaving it temporarily ownerless.

Epic #1837 was filed before the concrete children. D-23 settles the later
exception: #1780 closed as superseded against that epic before a replacement
child existed, and no child adopts its number.

### D-9. Chop selects rendered tree identities in screen space

A click selects the topmost eligible tree sprite under the pointer. A press-drag
box selects eligible trees whose rendered ground-contact anchors lie inside the
screen-space rectangle. The selection oracle must use the flora renderer's
projection inputs so elevation, cliffs, camera facing, wrapping, sub-tile
offsets, and sprite geometry cannot drift from what the player sees. Merely
changing the gesture while retaining the old tile-coordinate rectangle is
rejected because its selected set can disagree with the visible box around
cliffs.

### D-10. Till and crop Plant use the default top-facing facemap and authored alpha masks

Till and crop Plant marker quads use the 1×1 default/noface map, which treats
every surviving texture pixel as top-facing. Each user-authored designation
texture therefore owns the complete visible shape through its alpha channel.
This is safe because the shader multiplies final alpha by both the texture alpha
and the facemap alpha; the default map's alpha is fully opaque, so transparent
texture pixels remain transparent and no extra facemap silhouette is imposed.
The `vegface.png` clipping mask is not used at runtime for these markers; it may
serve as the level-top validation envelope. DTV-2 also bakes that exact support
into the actual tilled-soil source while preserving its RGB plane byte-for-byte.

### D-11. Planned construction shows the target's own rendered art

*Amended 2026-08-28 — the original wording assumed a structure's art was
already reachable from the renderer. It is not; see §Current state and
evidence. The intent is unchanged, the required plumbing is now stated.*

A `CtStructure` designation displays the selected structure piece's own
texture through the target's structure slot, facemap, camera-facing behavior,
and geometry. A `CtBuilding` designation displays the named building's own
sprite once at its actual anchor, dimensions, and footprint placement. This
holds in BOTH the preview and designated states (D-19). The renderer forces
only alpha, and in the preview state the invalid tint, instead of
substituting or repeating a category marker. The two
`construct_designate_*.png` placeholders and their category-texture plumbing
become obsolete. Stale #1780-era documentation must redirect to the new epic
or its replacement child after those tracker numbers exist.

Delivering this for structures REQUIRES new engine-side art resolution
(DTV-9). A designation stores only `StructurePiece{spPack, spKind, spEdge}`
and no texture, and the pack/kind → texture+facemap translation lives only in
`scripts/structures.lua`. Lua must register each pack's per-kind art with the
engine up front — the same shape as the existing
`structure.registerWallFamily` verb — so the render pass can resolve an
unplaced piece without calling into Lua. Buildings need none of this: their
art is already reachable through `bdSouthTexture`.

### D-12. Chop uses ground-contact-centered icons and symmetric add/erase gestures

A committed Chop icon is horizontally centered on the same rendered
ground-contact anchor used by drag-box selection and sits immediately above
that point, near the tree sprite's bottom center. Left click/drag adds eligible
tree designations; right click/drag erases designated trees through the same
four-screen-pixel click-versus-drag threshold and screen-space identity oracle.
Tree removal invalidates its marker rather than leaving an orphaned annotation.

### D-13. Existing crop planting and future tree planting are separate concepts

The current `Plant` toolbar tool is a tilled-soil crop workflow limited to
`row_crop` and `groundcover_crop`. Planting trees on ordinary ground is a new,
unspecified feature. This epic retains a visual/lifecycle child for the existing
crop marker; it does not silently design the future feature.

### D-14. Losing tilled soil cancels the crop Plant designation and job

Tilled soil is a continuous validity requirement, not only an admission check.
If a pending crop Plant tile ceases to be tilled, its durable designation is
removed and any claimed or in-flight farm job is cancelled and released. The
renderer must not hide invalid state while leaving an invisible job active.
The same invariant is revalidated across live terrain changes and save/load.

### D-15. The designated ghost is fixed at 60% and ends when active work begins

*Amended 2026-08-28: the 60% figure is now specifically the DESIGNATED
state's alpha, paired with D-19's 25% preview.*

Between commitment and the start of work, the target's own finished-form art
renders at a fixed 60% alpha. The ghost does not become more opaque with
designation progress — the existing `0.45 + 0.55 * cdProgress` ramp is
removed, not retuned. That ramp never worked for buildings anyway
(`cdProgress` stays 0 for every `CtBuilding`), so nothing observable is lost.

Once construction actually enters its progress phase, the ghost disappears.
D-16 settles what, if anything, replaces it; completion then hands off to the
built target. For a structure, the durable transition is material payment — the
designation's `cdPayment` gaining its receipt, which is where DTV-13 (#1844)
put the state the bare `cdMaterialsPaid` boolean used to hold — and it occurs
when the worker arrives and enters the building phase, not on mere claim or
travel. For a work-constructed building, it
is the material gate opening and the instance entering progress rendering.

### D-16. Structures build invisibly; constructed buildings require their own progress art

After a structure piece enters active construction, its designation ghost
disappears and no intermediate sprite renders; the finished structure appears
only when work completes. A building with positive `build_work` must instead
declare and render its own target-specific construction-progress sequence.
There is no shared or finished-texture fallback: a constructed building missing
that art is an invalid asset/data definition and must be caught before play.
Buildings with zero `build_work` follow an appearance or instant-placement
lifecycle instead. The portal's time-driven appearance animation is therefore
not construction progress and must remain semantically distinct. #2080 made
that distinction explicit: the two animation roles are now `construction`
and `appearance`, and the derived activities `Constructing` and `Appearing`.

### D-17. Missing building progress sequences land as dedicated art slices

Kitchen, Machine Shop, and Workbench each receive one target-specific
construction-progress sequence authored by the user and landed through its own
issue, PR, visual validation, and signoff. DTV-10 may establish the lifecycle
using existing Cargo Hold and Furnace progress art, but it does not introduce a
generic fallback or enforce the final completeness guard prematurely. DTV-8
depends on all three art slices and then rejects any positive-`build_work`
building without target-specific progress art.

### D-18. The construction slope stamping stays out of this arc

`applyConstructSlopeToChunk` writes corner progress into the chunk's
`ctSlopes`, and `src/Unit/Pathing/Cost.hs` reads that vector — so the
stamping is pathing state, not designation rendering, and changing it would
change movement cost. The project owner's ruling on 2026-08-28: player-created
constructions do not build on slopes; slopes are the world generator's
concern, for the structures that exist at the start of a game. This arc
therefore neither removes nor extends the stamping. D-16's "structures build
invisibly" is a statement about SPRITES, and no slice may quietly reinterpret
it as licence to touch terrain slope state.

### D-19. Construction has three ghost states at two alphas

A build target is shown at 25% alpha while the tool is armed and the player is
hovering or dragging (nothing committed), and at 60% alpha once the
designation is committed and work has not begun. Both states use the target's
own art. The third state — active construction — is unchanged from D-15/D-16.

The preview is deliberately lighter than the designated ghost so that
commitment reads as solidity. These are explicit GHOST-opacity factors rather
than alpha baked into a replacement marker. They still multiply each target
texture pixel's authored alpha and the renderer's existing `tileAlpha`/depth
fade, so “25%” and “60%” name the lifecycle factors, not a promise that every
framebuffer pixel has that literal final alpha. The existing marker PNGs bake
≈59% alpha into the texture, which is why today's numbers cannot be compared
to these directly.

The 25% PREVIEW applies to structure pieces and buildings alike, which is what
makes the two build tools one visual language rather than two conventions that
happen to coexist. The 60% Designated state exists only on paths which actually
create durable construction intent: a `CtBuilding` designation and its staked
pre-delivery `BuildingInstance`, or a `CtStructure` before material payment.
Starting/appearance buildings such as the portal and inventory-placed power
nodes never create that state; after their 25% preview succeeds they follow
their existing appearance or instant-placement lifecycle. The change also
inverts today's work-building behaviour, where the preview is more opaque than
the committed marker.

### D-20. Invalid-placement feedback belongs to the preview only

The red tint that `Building.Render.ghostTint` already applies to an invalid
placement extends to the structure preview, so both tool families warn the
same way while the player can still act on it. A committed designation never
shows invalid-placement feedback: by then the gesture is over, and a ghost
that changes colour under the player would be reporting a condition they
cannot answer in the moment.

If a committed structure site LATER becomes invalid, D-28 removes the
designation rather than recolouring it. Red remains preview-only feedback.

D-20 settles WHEN invalid tint may appear. D-25 settles how the structure tool
presents a mixed-validity rectangle without changing its partial-accept commit
semantics.

### D-21. Structure art registration lands before either ghost slice

The delivery plan is plumbing-first: DTV-9 adds the engine-side pack/kind →
art/buildability resolution and settles wire's treatment (Q-13); DTV-13 then
establishes the authoritative candidate-plan and invalidation boundary; and
the structure ghost slice builds on both. DTV-10 (buildings) does not
technically need either structure slice — `bdSouthTexture` is already reachable —
but the ledger keeps it after them so the arc lands in one coherent order
rather than shipping half the visual language first.

### D-22. A wire ghost runs the autotile rule speculatively

A `SWire` ghost resolves its connection variant by running
`scripts/wire.lua`'s own autotile rule over its neighbours, counting BOTH
already-placed wire and other wire DESIGNATIONS as connections. A designated
run therefore previews as one connected line rather than a row of unrelated
segments, which is the whole point of the #359 path tool's line snapping.

Consequences the implementing slices must handle: the resolved variant is
derived per frame from live neighbour state, not stored on the designation, so
adding or cancelling a neighbouring wire designation re-resolves its
neighbours' ghosts with no extra bookkeeping. The rule must produce the SAME
variant the placer would choose given the same neighbour set, or the ghost
lies about what will be built — that equivalence is the slice's acceptance
signal, and it is why the rule is shared rather than reimplemented on the
render side.

For a pre-commit line, that neighbour set includes placed wire, committed wire
designations (including an actively worked designation whose ghost is hidden),
and the ELIGIBLE proposed candidates from D-25. An invalid red candidate does
not make another candidate connect through a tile which commit will omit;
already-placed or already-designated wire on that same tile still contributes
through its durable state. Every neighbour lookup uses canonical tile identity
across the cylindrical seam.

DTV-9 owns exposing the rule and the neighbour query the render pass needs;
DTV-11 owns drawing the result.

### D-23. #1780 closes as superseded against the epic; no child adopts it

The owner abandoned #1780 on 2026-08-28. It closes as superseded with a
durable link to epic #1837, and its number is NOT reused: DTV-9, DTV-10 and
DTV-11 are filed as fresh issues. The epic already gives the work an owner, so
D-8's ownerless-gap concern does not apply, and #1780's verified defect and
review history stay readable on the closed issue rather than being overwritten
by a rewrite. Its `reviewed:changes` label was a further reason not to adopt
it — an adopted body would re-enter review carrying a verdict about a proposal
that no longer exists.

*Applied 2026-08-28: #1780 is closed as `not planned` with a superseding
comment linking #1837, and the epic's own Related section records the
closure.*

### D-24. A structure designation carries no pack variant

`StructurePiece` gains no variant field. The designate→build pipeline is
variant-free at BOTH ends, so a ghost drawn with default pack art is not an
approximation — it is exactly what will be built:

- `build_tool.lua`'s `construction.designate(..., target.pack, target.piece,
  target.edge)` passes no variant, and `StructurePiece` has nowhere to put
  one.
- `unit_ai_construct.lua`'s `placeStructurePiece` — the build AI that executes
  a designation — calls `structures.floor/ceiling/wall/post` with no variant
  argument either, so `handles(nil)` returns the pack's default art.

The only variant in the tree is `dungeon_1`'s `damaged`, and its only consumer
is `scripts/locations.lua`'s `builders.room_small_damaged`, which stamps
pieces directly and never creates a designation. Worldgen-placed ruins
therefore never pass through this arc's ghost rendering at all.

Adding the field would mean an append-only change to a save-serialized
positional type and a component migration, bought for a feature that does not
exist. DTV-9's acceptance signal is already the tripwire: it requires an
unplaced piece to resolve to exactly the texture and facemap `structure.place`
would later be called with, so adding variant selection to one end without the
other fails a test rather than silently producing a lying ghost.

### D-25. Structure preview validity is shown per candidate tile

The structure preview and commit share one per-tile eligibility oracle, with
D-26 settling its maximum extent and D-28 settling what “eligible” includes.
Every loaded candidate that the oracle would let commit draws the target art at
the normal 25% preview alpha; every loaded candidate it would filter out draws
the same target art red-tinted at 25%. A position whose world location cannot
be resolved because it is unloaded or off-world remains absent. Missing target
art is also absent rather than replaced by a red generic shape, with D-28's
deduplicated stdout warning. Mixed-validity
rectangles therefore show the exact subset that will land instead of falsely
implying an all-or-nothing rejection. This is feedback only: commit continues
to accept eligible tiles and skip ineligible ones. Preview is a snapshot; the
commit re-evaluates authoritatively at click time, so a world-state race may
legitimately change a tile between the two without creating a second rule.

### D-26. Structure drags use one 64-tile anchor-origin extent rule

Preview and commit use the same structure-specific bounded-drag helper. It
first localizes the endpoint into the first-click anchor's seam frame. For a
wire it then chooses the dominant axis from the RAW localized delta. Finally it
clamps the resulting rectangle or line outward from the anchor to at most 64
tiles inclusive on either side. The anchor always remains in the result.

This deliberately reduces structure commit reach from the shared 128-tile
designation cap to the existing preview guard. Mine, Till and other rectangle
tools retain their own current caps. A structure implementation must not route
one side through the old low-coordinate-end `designateRect` rule because that
can exclude the starting tile and, for wires, choose a different axis.

### D-27. Chop designation and harvest state belong to exact flora instances

Every flora object already has its own `FloraInstance` data record. DTV-12
extends that model with a stable `FloraInstanceId` and an explicit
per-instance Chop-designated Boolean. The marker may read that Boolean
directly, while selection, cancellation, claims, worker targeting,
`world.harvestFlora`, regrowth and invalidation carry the stable id. A chop of
one tree therefore cannot silently select, harvest, hide or regrow another
wood-tagged instance sharing its tile.

The Boolean cannot live only in the currently loaded `FloraChunkData`:
world-generated instances are recreated after chunk eviction. Its transitions
must also be persisted in an identity-keyed world overlay or equivalent
replayable edit, and the loaded record must be hydrated from that durable
authority through one owning operation. Per-instance harvest/regrowth state
uses the same identity. Generated and player-planted flora must both receive
stable identities, with deterministic behavior across the cylindrical seam.
A generated id is derived from canonical placement provenance, a stable species
name and that species' local instance ordinal — never solely from the catalog's
registration-order `FloraId` or `placeTileFlora`'s whole-list index. A planted
instance records its allocated id in the persistent placement edit. Adding or
reordering unrelated species must not rename an unchanged surviving instance.

Legacy tile-keyed Chop designations migrate to the single deterministic
eligible instance the old tagged-harvest ordering would have targeted on that
tile; if its chunk is not yet available, migration remains deferred until the
instance can be resolved. A legacy entry with no matching flora is discarded
with a diagnostic. The existing tile-level maps are not retained as a second
runtime authority after migration.

### D-28. Structure planning filters invalid candidates and clears invalidated jobs

One structure-plan resolver supplies preview, commit and later revalidation.
For every candidate whose target art resolves, it derives the exact slot,
final grid z and facemap and checks loaded/same-z terrain, kind-specific
prerequisites such as a post's placed floor, slot occupancy, existing
designation occupancy, and complete build metadata. The resolver receives its
operation context: preview/commit treat any outstanding designation as a
conflict, while later revalidation excludes the exact designation being
checked so it does not invalidate itself. A failed check draws that
candidate red at the 25% preview opacity and commit omits it while still
inserting every candidate which was not red when authoritatively rechecked.
Material availability is a scheduling concern and never makes a location red.

Missing or undecodable target art is a distinct asset failure: draw no
candidate and no generic/red fallback, reject it at commit, and emit an
observable stdout warning identifying the pack, kind and missing asset. The
warning is deduplicated per failed asset registration/load rather than emitted
once per candidate or render frame. Missing build metadata with otherwise
valid art remains visible red because the piece can be shown but cannot become
a viable job.

Placeability remains a standing invariant. Relevant terrain and structure
mutations, chunk/load publication, and the worker boundaries before claim,
material payment and final placement re-run the same resolver. When a
resolvable condition becomes invalid, cancellation atomically removes the
designation and its progress slope, abandons the matching live claim, and
refunds already-paid materials exactly once. If completion or another cancel
wins the atomic race, the invalidator is a no-op. An unloaded site is unknown,
not invalid: retain it without drawing and revalidate when its chunk becomes
available. This cleanup is why a designated ghost never needs to turn red.
Live mutation hooks revalidate only the canonical designation keys whose
inputs changed; they do not rescan every construction job after every tile
edit. Load/catalog reconciliation may perform the bounded page-level sweep
needed to validate restored state.

The worker's own successful placement is not an external occupancy change. Its
final place-and-complete handoff must either consume the designation in the
same serialized transaction which places the piece, or carry a unique
completion token which the invalidator recognizes and excludes until the
matching completion removes it. The current two queued operations
(`WorldSetStructure` followed by `WorldSetConstructStatus Complete`) may not be
left as an unguarded interval in which the new piece cancels and refunds its
own completed job.

## Open questions

### Q-19. Does structure-plan validity fail closed on jobs which cannot build?

Resolved by D-28.

The current commit admits more than the eventual placer can execute: a post
without an existing floor and a pack/kind without build metadata both become
durable jobs which the AI skips indefinitely. Under DTV-9, a missing or refused
art registration could likewise leave a selectable target whose preview and
designation render nothing.

The accepted choice defines one resolver for preview, commit and later
revalidation. Candidates which can render but cannot build are red and are
filtered individually at commit; valid candidates in the same drag still land.
Missing art draws nothing, emits a deduplicated stdout warning, and cannot
commit. If a committed site's resolvable conditions later become invalid, the
designation is cleared with claim release, slope cleanup and exactly-once
refund when needed. Unavailable materials and unloaded terrain remain waiting
conditions rather than invalid placement.

**Alternative:** preserve today's narrower commit filter and let later AI
viability decide. That is less code movement, but a green preview can still
commit a post which never starts, and a missing art registration can create an
invisible designation. Showing those cases red while still committing them is
a third option, but makes red mean “accepted but dormant” rather than invalid.

### Q-18. Is Chop's durable target one flora instance or one tile-level tree group?

Resolved by D-27.

D-9 currently promises rendered tree identities, but every shipped chopping
and regrowth authority is tile-keyed and `FloraInstance` has no persistent id.
World generation can place several species on one tile; `world.harvestFlora`
then takes the first matching wood instance and the tile-level regrowth timer
changes every harvestable co-tenant's visual state.

The accepted choice preserves exact tree identity. The repository does already
have one `FloraInstance` record per plant, as the owner expected, but those
records currently have neither a stable id nor durable mutable state and are
regenerated with their chunk. D-27 therefore adds the requested per-instance
designation Boolean together with the stable identity and persistent overlay
needed to rehydrate it after eviction/save-load. Chop, exact tagged harvest,
regrowth, marker anchoring and invalidation all use that identity.

**Alternative:** explicitly retain tile-level chopping. Deduplicate selected
wood sprites to canonical tiles, treat every co-tenant wood tree as one chop
site, and anchor one marker to a deterministic primary wood sprite. This keeps
the existing job/harvest/save model but weakens D-9: clicking or boxing one of
several co-tenant trees designates the whole tile-level group. Enforcing one
wood tree per tile in worldgen alone is not a safe substitute because existing
saves may already contain co-tenants.

### Q-17. What exact bounded rectangle or line does a structure drag commit?

Resolved by D-26.

The current structure preview clamps each side to 64 tiles from the first-click
anchor, while the shared commit helper clamps at 128 from the rectangle's
low-coordinate end. Wire preview chooses its dominant axis after clamping both
deltas; commit chooses from the raw localized endpoint first. D-25 cannot
truthfully promise that the previewed set is the committed set while the size,
clamp origin, and operation order differ.

The accepted choice gives structure planning one anchor-origin, 64-tile side
cap shared by preview and commit. It localizes the endpoint to the anchor's seam
frame, chooses a wire's dominant axis from that RAW delta, then clamps the
chosen rectangle/line outward from the anchor. This preserves the existing
per-frame guard and removes both the direction/axis surprises and the invisible
portion of an oversized commit.

**Alternative:** use the same anchor-origin/snap-first rule at 128 tiles. That
preserves current maximum commit reach, but the slice must prove acceptable
frame cost for up to 16,384 target-art candidates, including front-wall strip
geometry, rather than assuming the old 64-tile guard was unnecessary. Keeping
the low-coordinate-end commit clamp is not recommended because a bounded drag
can omit the tile where the player began it.

### Q-16. How does a mixed-validity structure preview render?

Resolved by D-25.

The current structure commit filters the selection tile-by-tile: unloaded or
wrong-z tiles, occupied target slots, and tiles already carrying an outstanding
construction designation do not land, while eligible tiles in the same
rectangle still do. D-20 requires red invalid feedback but does not say how
that partial-acceptance model appears before commit.

The accepted choice uses the shared plan-eligibility oracle per loaded tile;
D-26 and D-28 settle its extent and admission boundary without reopening the
per-tile eligible/red/absent presentation chosen here.
Draw eligible candidates with the target art at the normal 25% preview alpha
and draw ineligible candidates with that same art red-tinted at 25%; tiles whose
position cannot be resolved because they are unloaded/off-world remain absent.
This makes a mixed rectangle truthfully preview the subset that will land and
does not change commit semantics.

**Alternative:** tint the entire preview red when any candidate is ineligible.
That is simpler to read as a warning but falsely implies the whole commit will
be rejected when the backend will still accept its eligible subset.

### Q-15. Is #1780 closed outright, or edited into one of the new children?

Resolved by D-23.

The owner's position on 2026-08-28: #1780 is being abandoned, and there is no
reason to keep it around unless it is edited into one of the new children.
That reframes D-8's question. #1780's own proposal — recolour the two
placeholder markers per category — is dead either way (D-7); what is open is
whether its NUMBER is reused.

Two readings remain:

- **Close it as superseded**, linked to epic #1837. The epic is filed, so the
  work is no longer ownerless and D-8's caution is already satisfied; #1780's
  verified defect and review history stay readable on the closed issue.
- **Adopt it as one of the new children**, rewriting its title and body to a
  ghost slice. `/process-design-doc`'s "existing issue" disposition supports
  this directly. DTV-9 is a poor fit — it is engine art resolution, not marker
  art — so the natural targets are DTV-10 or DTV-11, each of which removes one
  of the two placeholders #1780 names. Note that #1780 currently carries
  `reviewed:changes`, so a rewrite re-enters review.

Either way this does not block DTV-9, which touches neither placeholder.

### Q-13. What does a wire ghost draw?

Resolved by D-22.

A `SWire` piece's rendered sprite is one of the wire pack's sixteen connection
variants, chosen by `scripts/wire.lua` from which cardinal neighbours also
carry wire, at PLACEMENT time. Neither ghost state can resolve one true sprite
without running that autotile rule speculatively — against neighbours that may
themselves still be designations rather than placed pieces.

Options: run the autotile rule speculatively over designated-plus-placed
neighbours so a designated run previews as a connected line; ghost one fixed
representative sprite (for example the straight or isolated variant) and
accept that the drawn connection is wrong until built; or leave wire alone on
its current generic cursor-tile treatment and scope this arc's structure ghost
to non-wire kinds. The choice affects DTV-9's registration shape and DTV-11's
scope, and it interacts with the #359 path tool's line-snapping preview.

Resolving this needs the owner's call on whether a wrong-but-present
connection reads better than no ghost.

### Q-14. Does a designated structure piece need to remember its pack variant?

Resolved by D-24.

Pack YAML supports `variants` that override a subset of piece art, but
`StructurePiece` has only pack, kind and edge. The question was whether a
ghost could therefore draw art the builder would not use. Tracing the pipeline
answered it: the BUILD path drops variants too, so both ends agree on default
art and the ghost cannot be wrong. No field, and no migration.

### Q-1. What geometry does the Chop drag box select?

Resolved by D-9.

Does the press-drag rectangle select tree identities by their rendered anchors
in screen space, including correct behavior across cliffs, or does it retain
the current tile-coordinate rectangle and merely change the input gesture?
This determines whether DTV-1 needs the flora projection boundary designed by
HARV-2.

### Q-2. Where is the Chop icon anchored, and how is it erased?

Resolved by D-12.

The icon could sit at the tree's ground-contact anchor, above its canopy, or at
another stable sprite-relative position. The tool also needs settled click,
right-drag or modifier behavior for cancelling existing designations, plus a
policy for camera rotation, overlapping canopies, and tree removal races.

### Q-3. What exactly is Till and Plant's top-surface mask/facemap contract?

Resolved by D-10.

The repository has two relevant mechanisms: a 1×1 default/noface map which
treats every surviving texture pixel as top-facing, and a 96×64 `vegface` map
whose alpha itself clips output to the top face. The phrase "only the bottom of
the shape" must identify the intended screen region or reference asset before
the issue can specify pixels and rendering independently.

### Q-4. Does the epic retain planned-construction feedback from #1780?

Resolved by D-11.

#1780 concerns planned construction, not deconstruction. The behavior remains
in scope but its proposed marker-texture solution is deliberately abandoned.
DTV-10 and DTV-11 replace it with the actual-target ghost contract. D-23 records
that #1780 closed as superseded against epic #1837 before either child existed;
no child adopts its number.

### Q-5. Does DTV-3 redesign the existing crop Plant marker?

Resolved by D-5, D-13, and D-14.

The current repository already has a Plant tool, but it means crop planting:
clicking tilled soil opens a panel and creates a single-tile job for a
`row_crop` or `groundcover_crop`. It cannot plant trees. DTV-3 remains as a
light-green surface redesign of this existing crop marker, while tree planting
is designed separately. Tilled soil is continuously enforced: losing it
cancels the designation and any job rather than merely hiding the marker.

### Q-6. When should #1780 close?

Originally resolved by D-8; superseded by D-23.

P-1 recommends creating both the epic and a concrete replacement child before
closing #1780 with durable links. The alternative is to close it immediately
after the epic exists and let the epic ledger temporarily carry the unresolved
construction work. The latter is what the owner chose and applied.

### Q-7. Is construction ghost alpha fixed or progress-driven?

Resolved by D-15.

The current generic construction marker begins at 45% alpha and ramps to fully
opaque with build progress. Existing building placement and pre-delivery
ghosts use 60% alpha. The replacement contract uses fixed 60% target art until
construction begins, then removes the ghost and follows D-16's target-class
lifecycle.

### Q-8. What progress art do structure pieces use?

Resolved by D-16.

Buildings with a `construction` animation already have target-specific progress
frames, but structure packs currently define only finished floor, wall,
ceiling, post, and wire art. Structures intentionally show nothing between the
fixed ghost and finished piece. Every positive-`build_work` building must have
its own progress sequence with no fallback. Zero-work appearance animations,
including the portal's, remain a separate role.

### Q-9. Who supplies the missing building progress sequences, and how do they land?

Resolved by D-6 and D-17.

The user supplies the Kitchen, Machine Shop, and Workbench construction
sequences while solving dedicated art children. Each sequence is independently
reviewable and receives gameplay-scale signoff before DTV-8 enables the
repository-wide no-fallback requirement.

### Q-10. What is the final Kitchen construction sequence?

Deliberately open in DTV-5 until the user authors it while solving that art
slice. It must use Kitchen-compatible dimensions and anchoring, communicate
visible construction progress, end in a clean handoff to the finished sprite,
and receive explicit gameplay-scale signoff. DTV-5 must stop rather than invent
or generate missing frames.

### Q-11. What is the final Machine Shop construction sequence?

Deliberately open in DTV-6 until the user authors it while solving that art
slice. It must use Machine-Shop-compatible dimensions and anchoring,
communicate visible construction progress, end in a clean handoff to the
finished sprite, and receive explicit gameplay-scale signoff. DTV-6 must stop
rather than invent or generate missing frames.

### Q-12. What is the final Workbench construction sequence?

Deliberately open in DTV-7 until the user authors it while solving that art
slice. It must use Workbench-compatible dimensions and anchoring, communicate
visible construction progress, end in a clean handoff to the finished sprite,
and receive explicit gameplay-scale signoff. DTV-7 must stop rather than invent
or generate missing frames.

## Verification strategy

- Headless coverage for unchanged designation eligibility and state ownership,
  plus crop Plant cancellation of durable and in-flight work when tilled soil
  is lost, including save/load revalidation.
- Focused input tests for Chop's press-drag threshold, add/erase semantics,
  target identities, and cancellation on focus/tool/session transitions.
- Multiple wood-tagged flora instances on one tile: exact instance
  selection/designation/claim/harvest/regrowth, stable identity across
  eviction and save/load, and proof that the unselected co-tenant is unchanged.
  Verify the loaded `FloraInstance` Boolean and its durable identity-keyed
  authority cannot drift, including legacy tile-designation migration.
- Cliff-bearing identity tests or probes if Chop selects rendered tree anchors
  rather than a tile-coordinate rectangle.
- A render-level assertion that Mine uses the three-face isometric facemap while
  Till and crop Plant use the 1×1 default top-facing map and their authored
  alpha masks determine the visible surface shape.
- An offscreen or graphical acceptance path which captures committed Mine,
  Chop, Till, crop Plant, actual-target Structure and Building ghosts, and the
  structure ghost-to-empty-to-finished transition plus the constructed-building
  ghost-to-progress-to-finished transition at gameplay zoom.
- A render-level assertion that the SAME target art resolves in the preview and
  designated states and differs only in alpha (25% vs 60%) and the preview's
  invalid tint — checked as the two states' resolved texture/geometry being
  equal, so the two paths cannot drift into separate conventions.
- A pure test that an unplaced `StructurePiece` resolves to the same texture and
  facemap the piece is actually PLACED with, driven from a real pack YAML, so
  the registration cannot certify art the builder would not use.
- Structure-plan parity at D-26's 64-tile boundary, including positive and
  negative drags just beyond the limit and long diagonal wires. Cover a post
  without a floor, an occupied slot, missing build metadata, and a
  missing/refused art registration. Preview and commit must agree from one
  snapshot; a commit-time world race is revalidated rather than cached.
- Invalidation coverage proving terrain/structure mutation, chunk publication,
  and pre-claim/payment/placement checks remove a now-invalid designation,
  release its live claim, reset its progress slope and refund a paid job once;
  an unloaded site waits, and a cancellation/completion race has one winner.
- A missing-art preview emits no quad or fallback, cannot commit, and writes one
  stdout warning per failed asset registration/load rather than per tile/frame.
- Geometry parity proving floor/wall/wire ghosts use surface+1, ceilings use
  surface+2, posts use their supporting floor z, and a wall ghost selects the
  same cap facemap as placement for every same-tile post combination.
- A registration test showing unplaced-art discovery does not add unused paths
  to the persisted texture palette, plus the required capability/persistence
  inventory audits if DTV-9 adds or generalizes render-handoff state.
- Coverage that a designated ghost never shows invalid-placement feedback and
  that an invalid preview does, for both target kinds.
- Structure-preview coverage for the no-anchor single-piece state and the
  anchored rectangle/line state, with D-25's mixed-validity behavior
  derived from the same eligibility oracle as commit.
- Asset checks for the user-authored Chop, Till, and crop Plant marker
  paths, including decoded RGBA geometry/alpha contracts. Construction instead
  needs render assertions proving that the designation resolves the same
  target texture/facemap/geometry as the built object, plus a data/asset guard
  rejecting every positive-`build_work` building without target-specific
  progress frames while exempting zero-work appearance lifecycles.
- Explicit user signoff for every new or changed texture.

## Delivery plan

### DTV-9. Resolve structure-pack piece art engine-side for unplaced pieces

> Filed 2026-08-28 as #1842.

- **Outcome:** Given only an abstract `StructurePiece` (pack, kind, optional
  edge), the engine can resolve the exact texture and facemap the piece will be
  BUILT with and whether that kind has build metadata, without calling into
  Lua.
- **Scope:** A Lua-side registration of each loaded pack's per-kind art and
  buildability metadata,
  shaped like the existing `structure.registerWallFamily` verb; the engine
  catalogue it feeds; the lookup the renderer uses; all-or-nothing
  registration, a deduplicated stdout warning naming missing/undecodable art,
  and an explicit answer for a pack that fails to register; and
  D-22's shared wire autotile rule plus the seam-canonical neighbour query the
  render pass needs, counting designated wire as connecting; dynamic wall-cap
  facemap resolution from same-tile posts; render-handoff capability and
  persistence-inventory compliance; and a guarantee that merely registering
  unplaced art does not intern paths into the saved texture palette. No ghost
  rendering changes here — this slice is the resolution capability and its
  tests. DTV-13 owns candidate validity, commit filtering and later
  invalidation.
- **Phase:** 1 — construction presentation
- **Depends on:** `none`
- **Ordering:** `critical path`; `can land first`
- **Relevant decisions:** D-11, D-21, D-22, D-28
- **Acceptance signals:** An unplaced `StructurePiece` resolves to exactly the
  texture and facemap `structure.place` would later be called with, verified
  against a real pack YAML for every kind the build picker offers; a partial
  or malformed registration is refused whole rather than half-applied; a pack
  with no registration resolves nothing rather than guessing and produces one
  stdout warning per failed registration/load rather than per render frame;
  build metadata presence is exposed independently from art resolution; and
  the shared wire rule returns exactly the variant the placer would choose for the same
  neighbour set, proven over the full sixteen-variant space and across the
  cylindrical seam; wall cap combinations resolve exactly as placement does;
  and registration leaves the persistent texture palette unchanged.
- **Out of scope:** Any change to what is DRAWN — both ghost states stay on
  their current textures until DTV-10 and DTV-11 — plus placed-piece
  rendering, candidate validity/invalidation (DTV-13), the #1712 wall-rotation
  catalogue, and Q-14's variant field.
- **Open questions:** `None` — Q-13 is settled by D-22 and Q-19 by D-28.

### DTV-13. Make structure drag planning authoritative and self-clearing

> Filed 2026-08-28 as #1844.

- **Outcome:** Structure preview, partial commit and later invalidation use one
  64-tile candidate-plan contract, so a red candidate never commits and a
  committed job which becomes unbuildable does not remain stranded.
- **Scope:** D-26's seam-local, anchor-origin rectangle/line helper; one pure or
  snapshot-driven plan resolver consuming DTV-9's registered art/buildability
  catalogue; exact slot/final-z/wall-cap/prerequisite checks; independently
  filtered commit; invalidation after relevant terrain/structure mutations and
  chunk/load publication; revalidation before worker claim, payment and final
  placement; an atomic or token-guarded final place-and-complete handoff;
  atomic designation/progress cleanup, claim abandonment and exactly-once
  material refund; and focused state/race coverage. The resolver
  distinguishes valid, visible-invalid, missing-art and unresolved-terrain
  outcomes for DTV-11 to render.
- **Phase:** 1 — construction presentation
- **Depends on:** DTV-9
- **Ordering:** `critical path` after DTV-9 and before DTV-11
- **Relevant decisions:** D-20, D-25, D-26, D-28
- **Acceptance signals:** Preview-plan and commit enumerate the same maximum
  64-tile set for every drag direction and seam crossing, with wire direction
  chosen before clamping; valid candidates land while red candidates are
  omitted; missing-art candidates are omitted and cannot commit; material
  scarcity remains schedulable; invalidation at every owned mutation/load/AI
  boundary clears the durable job, progress slope and claim, refunds a paid
  job once, and treats unloaded terrain as unknown; race tests prove one
  completion/cancel winner, no duplicate refund, and no self-cancellation
  between the worker's own successful placement and completion.
- **Out of scope:** Drawing target ghosts (DTV-11), building placement,
  structure work rates/costs, and the slope-stamping mechanism itself (D-18).
- **Open questions:** `None`

### DTV-10. Ghost planned buildings with their own art in both ghost states

> Filed 2026-08-28 as #1845.

- **Outcome:** A planned building appears as its own sprite at 25% while the
  player is placing it and 60% once designated, with no category marker and no
  per-footprint-tile repetition.
- **Scope:** Route both the pre-commit preview and the committed designation
  through the building's own `bdSouthTexture`, dimensions, `bdSpriteAnchor` and
  footprint anchor; apply D-19's two alphas and D-20's preview-only invalid
  tint; keep the `CtBuilding` designation ghost and the staked
  `BuildingInstance` pre-delivery ghost at the same 60% state; remove the
  `0.45 + 0.55 * cdProgress` ramp; hand off to the existing progress-render
  path when work begins; emit nothing (with observable diagnostics) for a
  missing building definition rather than retaining the anchor-tile fallback;
  preserve zero-work appearance lifecycles; remove
  `construct_designate_building.png` with its handle, setter and HUD wiring;
  and add focused render plus visual verification.
- **Phase:** 1 — construction presentation
- **Depends on:** `none` — a building's art is already reachable through
  `bdSouthTexture`; sequenced after DTV-9 by D-21 rather than by necessity
- **Ordering:** `not on the critical path`
- **Relevant decisions:** D-11, D-15, D-16, D-19, D-20
- **Acceptance signals:** A building designation emits ONE ghost with the same
  sprite dimensions and anchor as that building rather than one generic tile
  per footprint cell; preview and designated resolve the identical art and
  differ only in alpha and the invalid tint; an invalid preview tints red and
  a designated ghost never does; staking causes no ghost geometry or opacity
  jump before material delivery; buildings with existing progress art hand off
  to it when work begins; an unresolved saved definition produces a diagnostic
  and no fabricated fallback ghost; zero-work appearance animations such as
  the portal are unchanged; the placeholder texture and its routing are
  absent; and gameplay-scale captures receive user signoff.
- **Out of scope:** Structure pieces, construction selection, job execution,
  how work progress accumulates, the missing per-building progress assets and
  final no-fallback enforcement owned by DTV-5 through DTV-8, and the slope
  stamping fenced by D-18.
- **Open questions:** `None`

### DTV-11. Ghost planned structure pieces with their own art in both ghost states

> Filed 2026-08-28 as #1846.

- **Outcome:** A planned structure piece appears as its own art — through its
  real slot, facemap, camera-facing rotation and geometry — at 25% while
  dragging and 60% once designated, and renders nothing at all once work
  begins.
- **Scope:** Replace `constructPreviewQuads`' generic cursor tile and the
  committed designation's category marker with the target presentation
  resolved through DTV-9, drawn through `Structure.Render`'s existing geometry
  after factoring a resolved-art entry point if needed (unplaced art must not
  be interned into the saved palette merely to call `structurePieceQuads`); apply
  D-19's two alphas and D-20's preview-only invalid tint; add a single-piece
  hover preview before the first anchor click, then retain the anchored
  rectangle/line preview; consume DTV-13's D-26/D-28 candidate outcomes for
  D-25's per-tile invalid feedback, drawing missing-art candidates not at all;
  derive the same final grid z and dynamic wall cap
  facemap as the eventual placer rather than rendering at raw `cdZ`; suppress
  rendering once the designation is PAID (its `cdPayment` carries a receipt;
  a bare `cdMaterialsPaid` boolean until #1844); keep the #359 line-mode
  preview snapping
  to what commits; remove `construct_designate_structure.png` with its handle,
  setter and HUD wiring; redirect stale #1780-era documentation once the
  tracker numbers exist; and add focused render plus visual verification.
- **Phase:** 1 — construction presentation
- **Depends on:** DTV-9, DTV-13
- **Ordering:** `critical path` after DTV-13
- **Relevant decisions:** D-7, D-11, D-15, D-16, D-19, D-20, D-21, D-22, D-23, D-25, D-26, D-28
- **Acceptance signals:** Every structure kind previews and designates with its
  selected target art, facemap, camera-facing behavior and slot geometry;
  before the first anchor click the hovered target shows one candidate piece,
  while after anchoring the rectangle/line shows the exact candidate set up to
  the same limit commit uses; piece height and wall caps match actual placement;
  preview and designated resolve identical art differing only in alpha and the
  invalid tint; mixed validity follows D-25 without drifting
  from commit; a missing-art target draws no preview or fallback while DTV-9's
  diagnostic remains observable; a piece under active work renders no site
  sprite until the completed piece appears; the line-mode preview still
  matches what commits;
  the placeholder texture and its routing are absent; stale documentation
  redirects to the replacement tracker; and gameplay-scale captures receive
  user signoff.
- **Out of scope:** Buildings, candidate planning/invalidation (DTV-13), job
  execution beyond consuming that plan, the
  slope stamping fenced by D-18, and Q-14's variant field.
- **Open questions:** `None` — Q-13 is settled by D-22, Q-16 by D-25, Q-17 by
  D-26 and Q-19 by D-28.

### DTV-5. Author Kitchen construction-progress art

> Filed 2026-08-28 as #1848.

- **Outcome:** Kitchen has a user-authored construction sequence suitable for
  progress-driven rendering before its finished sprite appears.
- **Scope:** One Kitchen progress sequence, source/output asset paths, dimension
  and anchor compatibility with the finished Kitchen sprite, preview evidence,
  and explicit user signoff. Runtime/YAML enforcement waits for DTV-8.
- **Phase:** 2 — missing construction art
- **Depends on:** `none`
- **Ordering:** `independent`; must land before DTV-8
- **Relevant decisions:** D-6, D-16, D-17
- **Acceptance signals:** The authored sequence visibly progresses toward the
  finished Kitchen, hands off without an unintended position/size jump, passes
  focused asset validation, and receives gameplay-scale user signoff.
- **Out of scope:** Renderer/lifecycle changes and other buildings.
- **Open questions:** Q-10

### DTV-6. Author Machine Shop construction-progress art

> Filed 2026-08-28 as #1849.

- **Outcome:** Machine Shop has a user-authored construction sequence suitable
  for progress-driven rendering before its finished sprite appears.
- **Scope:** One Machine Shop progress sequence, source/output asset paths,
  dimension and anchor compatibility with the finished Machine Shop sprite,
  preview evidence, and explicit user signoff. Runtime/YAML enforcement waits
  for DTV-8.
- **Phase:** 2 — missing construction art
- **Depends on:** `none`
- **Ordering:** `independent`; must land before DTV-8
- **Relevant decisions:** D-6, D-16, D-17
- **Acceptance signals:** The authored sequence visibly progresses toward the
  finished Machine Shop, hands off without an unintended position/size jump,
  passes focused asset validation, and receives gameplay-scale user signoff.
- **Out of scope:** Renderer/lifecycle changes and other buildings.
- **Open questions:** Q-11

### DTV-7. Author Workbench construction-progress art

> Filed 2026-08-28 as #1850.

- **Outcome:** Workbench has a user-authored construction sequence suitable for
  progress-driven rendering before its finished sprite appears.
- **Scope:** One Workbench progress sequence, source/output asset paths,
  dimension and anchor compatibility with the finished Workbench sprite,
  preview evidence, and explicit user signoff. Runtime/YAML enforcement waits
  for DTV-8.
- **Phase:** 2 — missing construction art
- **Depends on:** `none`
- **Ordering:** `independent`; must land before DTV-8
- **Relevant decisions:** D-6, D-16, D-17
- **Acceptance signals:** The authored sequence visibly progresses toward the
  finished Workbench, hands off without an unintended position/size jump,
  passes focused asset validation, and receives gameplay-scale user signoff.
- **Out of scope:** Renderer/lifecycle changes and other buildings.
- **Open questions:** Q-12

### DTV-8. Require progress art for every work-constructed building

> Filed 2026-08-28 as #1853.

- **Outcome:** Construction and appearance are distinct building lifecycles,
  and every work-constructed building is guaranteed to have target-specific
  progress art with no runtime fallback.
- **Scope:** Introduce or expose a construction-specific animation/activity role;
  wire Cargo Hold, Furnace, Kitchen, Machine Shop, and Workbench to it; retain
  the portal's zero-work appearance role; update Lua/API consumers of building
  activity; and add an asset/data guard rejecting positive-`build_work`
  definitions without a valid construction sequence.
- **Phase:** 3 — construction contract enforcement
- **Depends on:** DTV-10, DTV-5, DTV-6, DTV-7
- **Ordering:** `critical path` after the building ghost hands off to progress
  rendering and all missing progress art has landed
- **Relevant decisions:** D-15, D-16, D-17
- **Acceptance signals:** Positive-work definitions expose a construction state
  and render progress-indexed target art; zero-work appearance definitions
  expose a distinct appearance state; the portal retains its time-driven
  appearance; missing/empty/undecodable construction sequences fail the guard;
  no final-texture or generic fallback exists; and current save data loads with
  definitions re-resolved under the new roles.
- **Out of scope:** New building gameplay, changing work rates/material costs,
  or adding construction art beyond the three owned asset slices.
- **Open questions:** `None`

### DTV-12. Give every flora instance stable identity and exact mutable state

> Filed 2026-08-28 as #1854.

- **Outcome:** Every generated or player-planted `FloraInstance` has stable
  identity, and Chop designation plus harvest/regrowth state can address one
  exact instance without changing a co-tenant on the same tile.
- **Scope:** A stable `FloraInstanceId`; the requested per-instance
  Chop-designated Boolean in loaded flora data; one owning mutation/hydration
  boundary backed by durable identity-keyed state or replayable edits; exact-id
  Chop cancellation, claims, `world.harvestFlora` and regrowth; generated and
  player-planted identity allocation independent of catalog/list order, with
  planted ids carried by the persistent edit; chunk eviction/reload and save/load;
  cylindrical-seam identity; legacy tile-keyed Chop/harvest migration; removal
  cleanup; Lua API/job payload changes; and focused persistence/co-tenant tests.
  This is state foundation only and reuses the gesture/projection boundary with
  the Harvest arc where practical.
- **Phase:** 1 — instance identity foundation
- **Depends on:** `none`; coordinate shared identity ownership with HARV-2
- **Ordering:** `independent`, but must land before DTV-1; whichever Harvest or
  Chop slice first needs exact flora identity should establish the shared type
- **Relevant decisions:** D-9, D-12, D-27
- **Acceptance signals:** Two eligible wood instances may share one tile while
  only the selected instance's Boolean, marker-facing state, claim, exact
  harvest and regrowth change; the same ids and state survive chunk eviction,
  save/load and seam aliases; generated and player-planted flora cannot collide;
  adding or reordering an unrelated species does not rename an unchanged
  generated instance;
  a removed instance leaves no orphan designation/harvest state; legacy
  tile-keyed designations resolve to the deterministic old primary target or
  are diagnosed and discarded; and no tile-level runtime map remains a second
  Chop/harvest authority after migration.
- **Out of scope:** The Chop input gesture and icon rendering (DTV-1), the new
  Harvest tool's product behavior, tree planting, and changes to flora growth
  formulas or placement density.
- **Open questions:** `None`

### DTV-1. Make Chop a drag-box tool with tree-anchored designation markers

> Filed 2026-08-28 as #1856.

- **Outcome:** Chop uses the settled press-drag interaction and visibly marks
  exactly the eligible trees selected by that gesture.
- **Scope:** Gesture lifecycle, selected-identity oracle, existing wood-tag
  eligibility, add/erase behavior, tree-relative marker rendering, final icon,
  invalidation, DTV-12's exact identity/Boolean APIs, and focused input/visual
  verification.
- **Phase:** 1 — interaction and target annotation
- **Depends on:** DTV-12; coordinate shared projection ownership with HARV-2
- **Ordering:** `critical path` after DTV-12, while whichever of DTV-1 and
  HARV-2 lands first should establish a reusable gesture/projection boundary
- **Relevant decisions:** D-2, D-3, D-6, D-9, D-12, D-27
- **Acceptance signals:** A press-drag gesture designates the settled eligible
  tree identities, including the settled cliff case, and each committed tree
  receives the signed-off alpha icon without a full-tile ground overlay.
- **Out of scope:** Non-wood Harvest behavior and the exact identity,
  persistence and work-execution foundation already owned by DTV-12.
- **Open questions:** `None` — Q-18 is settled by D-27.

### DTV-2. Render Till as a flat top-surface designation

> Filed 2026-08-28 as #1857.

- **Outcome:** Till preview and committed work read as a two-dimensional ground
  treatment with no lit or visible vertical tile sides, and farming admission
  agrees by accepting only level ground.
- **Scope:** Explicit reusable surface render path, approved authored-alpha Till
  marker, flat-only Till admission, intrinsic tilled-soil alpha with unchanged
  RGB art, and focused render, asset, and behavior verification.
- **Phase:** 1 — surface render foundation
- **Depends on:** `none`
- **Ordering:** `critical path` for DTV-3
- **Relevant decisions:** D-4, D-6, D-10
- **Acceptance signals:** Preview and committed Till tiles show only the settled
  top-surface region at gameplay zoom across camera facings; Mine still shows
  all three faces; sloped candidates are refused; the actual soil and marker
  sources cannot reveal pixels outside the level-top diamond.
- **Out of scope:** Continuous slope revalidation after admission, AI execution,
  Plant eligibility, the wider vegetation-alpha corpus, and #1692 toolbar art.
- **Open questions:** `None`

### DTV-3. Render the existing crop Plant job as a light-green tilled surface

> Filed 2026-08-28 as #1858.

- **Outcome:** An existing crop Plant designation uses the same settled
  flat-surface language as Till, remains clearly distinct in light green, and
  is cancelled if its land ceases to be tilled.
- **Scope:** Reuse of the top-only render path, final user-authored Plant marker,
  one world-owned tilled-soil predicate shared by admission and invalidation,
  live-terrain and load/chunk-publication reconciliation, durable/in-flight job
  cancellation, and focused render/state coverage. An unresolved/unloaded tile
  is retained for revalidation when its terrain becomes available; it is not
  treated as proof that tilled soil was lost.
- **Phase:** 2 — specialized surface marker
- **Depends on:** DTV-2
- **Ordering:** `critical path` after DTV-2
- **Relevant decisions:** D-5, D-6, D-10, D-13, D-14
- **Acceptance signals:** Plant designations render as signed-off light-green
  top surfaces only while the tile is tilled; losing that state cancels the
  durable designation and any claimed/in-flight work without leaving an
  invisible job, including across save/load and chunk reload; an unloaded tile
  is neither drawn nor spuriously discarded and is checked when it resolves.
- **Out of scope:** Crop choice UI, planting AI behavior beyond the
  invalidation/release path above, tree planting, general planting semantics,
  and #1692 toolbar art.
- **Open questions:** `None`

## Source notes

- The user wants Mine to remain a full three-dimensional tile because mining
  removes volume in three-dimensional space.
- The user reversed an earlier preference and now wants Chop to use a mouse
  drag box, then mark selected trees with a translucent icon.
- The user wants Till to show only a flat surface using the default/top-facing
  facemap concept, and Plant to share that surface treatment in light green,
  appearing only on already-tilled land.
- The user plans to author the final textures while solving the corresponding
  work rather than supplying them during design.
- The user proposed replacing #1780 with a new epic tracker and one child per
  redesigned designation family.
- On 2026-08-28 the user chose a two-alpha ghost model after seeing that the
  build tools already behave inconsistently at the preview stage: "we want to
  have the ghosts available for when we move the mouse around, so that we can
  see where we build, then we want other ghosts for when the structure is
  designated for construction. perhaps the best idea would be two levels of
  alpha. super light when its just the mouse dragging around, then more
  opaque when its designated."
- On the construction slope stamping, the user ruled: "player created
  constructions dont build on slopes, that is for the world generator only,
  those are for the structures that appear at the beginning of the game, out
  of scope."
- The user approved creating the epic and a replacement child before closing
  #1780 with links to both, then processing the remaining children one at a
  time. (Said when that child was the since-retired DTV-4; see Q-15.)
- The user approved screen-space Chop selection: clicks choose the topmost
  eligible tree sprite and drags include rendered tree anchors inside the box.
- The user approved the 1×1 default/noface map for Till and Plant; each authored
  designation texture's alpha channel will cut out its own flat surface shape.
- During DTV-2 on 2026-08-30 the user restored the intended rule that farming is
  possible only on level ground, and required source alpha to remain correct
  even when runtime masking would otherwise hide an error.
- The user approved the final Till marker in a live contiguous 5×5 arena field:
  one flat, slightly orange translucent layer with no texture detail.
- The user clarified that planned structures and buildings should ghost their
  own target textures with forced alpha. The category-marker solution proposed
  by #1780 is intentionally abandoned, and stale documentation may redirect to
  the new epic tracker.
- The user settled Chop's marker at the bottom-center ground-contact anchor used
  by box selection, immediately above ground contact, with the recommended
  matching add/erase gestures.
- The repository's existing Plant tool is crop-only and tilled-soil-only.
  Planting trees on ordinary ground is a separate feature which the user has
  not yet specified.
- The user kept the existing crop Plant marker in this epic and required any
  designation/job to cancel when its tile ceases to be tilled.
- The user selected a fixed construction ghost which disappears once active
  work begins.
- The user clarified that structure pieces render nothing during active work
  and simply appear completed. Every work-constructed building instead requires
  its own progress art with no fallback; the portal's appearance animation is a
  distinct zero-work lifecycle, not construction progress.
- After the second code audit, the user selected the shared 64-tile
  anchor-origin structure-drag limit.
- The user required exact flora identities and an explicit designation Boolean
  on each flora instance. The repository does have per-instance records; D-27
  preserves that requested model while recording the durable identity overlay
  needed because generated instance records are recreated on chunk reload.
- The user required individually red structure-preview candidates to be
  filtered at commit while valid candidates in the same drag still land.
  Missing art must draw nothing and warn on stdout. A committed structure which
  later becomes unbuildable should be cleared rather than left as a stranded
  ghost/job.
