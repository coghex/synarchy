# Designation tool visual language design

Synarchy's standing work designations currently share one generic tile-quad
render path even when they describe different kinds of targets. This design
separates those meanings: excavation may occupy a full three-dimensional tile,
surface work should read as a flat ground treatment, and tree work should mark
the tree the player selected rather than painting the whole ground tile.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Give each designation tool target-appropriate interaction and visual feedback — [#1837]
- [ ] DTV-4. Ghost planned construction with the target's own rendered art
- [ ] DTV-5. Author Kitchen construction-progress art
- [ ] DTV-6. Author Machine Shop construction-progress art
- [ ] DTV-7. Author Workbench construction-progress art
- [ ] DTV-8. Require progress art for every work-constructed building
- [ ] DTV-1. Make Chop a drag-box tool with tree-anchored designation markers
- [ ] DTV-2. Render Till as a flat top-surface designation
- [ ] DTV-3. Render the existing crop Plant job as a light-green tilled surface

## Epic contract

- **Goal:** Make every standing work designation communicate what the player
  actually targeted: a three-dimensional mine tile, one or more trees, a flat
  tilling surface, an existing crop-planting job, or the actual structure or
  building planned for construction.
- **Done when:** Mine retains its full three-face tile reading; Chop uses the
  settled drag gesture and marks only eligible trees; Till and the existing
  crop Plant tool use settled flat-surface rendering; crop Plant designations
  cancel if their soil ceases to be tilled; planned construction ghosts the
  target's own texture, geometry, and facemap at fixed alpha until construction
  begins; structures then remain invisible until complete, while every
  work-constructed building uses its own required progress art; appearance-only
  buildings remain a distinct lifecycle; every changed asset has explicit user
  signoff; and focused automated plus UI-capable checks distinguish the settled
  behaviors.
- **Users and operators:** Players directing colony work; maintainers of world
  tool input, flora selection, designation state, world rendering, and HUD art.
- **Arc label:** None proposed

## Current state and evidence

- `World.Render.CursorQuads` renders Mine, Chop, Till, Plant, and both
  construction categories through `worldCursorToQuad`. That helper always
  supplies `wtIsoFaceMap`, whose red/green/blue channels represent the right,
  top, and left faces. All six standing marker families therefore enter the
  same three-face lighting and alpha-mask path even when their source PNG masks
  differ.
- Mine already follows the desired three-dimensional model. Its 96×64 marker
  covers the full tile silhouette, and the shared isometric facemap preserves
  distinct top and side faces. The arc should preserve this behavior rather
  than redesign it without evidence of a defect.
- Chop is currently a two-click tile rectangle. The first click stores an
  anchor and the second commits; this is not a press-drag gesture. The world
  handler already filters the rectangle to currently harvestable, wood-tagged
  flora and stores each tree tile at its own surface height, but the renderer
  paints the same ground-aligned tile quad shape used by other designations.
- Till is also a two-click rectangle. Its handler keeps only tillable tiles at
  the anchor's surface z: no fluid, no flora, and not already tilled. Its
  committed marker nevertheless uses the three-face isometric cursor helper.
- Plant is a single-tile tool reached through the crop-selection panel. Both
  Lua and the world command require tilled soil before a designation is
  accepted; the command additionally excludes existing flora and crop plots.
  Its committed marker stores the accepted surface z and renders through the
  same three-face cursor helper. Rendering does not independently re-check the
  soil state each frame.
- The default face map is a 1×1 pure-green texture meaning 100% top-facing.
  `assets/textures/facemap/vegface.png` is a separate 96×64 top-face-only mask,
  while `isoface.png` includes top, left, and right faces. D-10 selects the 1×1
  default/noface path and makes each authored designation texture's own alpha
  mask the authority for its visible surface shape.
- `construct_designate_structure.png` and `construct_designate_building.png`
  are category-level placeholders from #1780. That proposed solution is now
  intentionally abandoned: planned construction should not use replacement
  category marker art at all. #1780 and any documentation that still describes
  that solution are stale and should point to the new epic/replacement child
  once those tracker artifacts exist.
- A planned `CtBuilding` already names a building definition whose
  `bdTexture`, pixel dimensions, footprint anchor, and `bdSpriteAnchor` drive
  the placement ghost and final renderer. The current committed designation
  instead expands one generic 96×64 marker across every footprint tile. DTV-4
  must render one target-shaped building ghost, not repeat a tile marker.
- A planned `CtStructure` stores pack, kind, and optional edge. The build picker
  already resolves the selected structure piece's own texture, while the final
  structure renderer also owns piece slot, facemap, camera-facing wall
  rotation, and special wall/post geometry. DTV-4 must resolve and reuse that
  target presentation rather than flattening every structure into one generic
  cursor quad.
- Cargo Hold and Furnace currently provide progress-driven construction frames.
  Kitchen, Machine Shop, and Workbench declare positive `build_work` but no
  construction animation, so they need target-specific progress art under
  DTV-4. The Acolyte Portal has an appearance animation with zero `build_work`;
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
  facemap, at fixed 60% ghost alpha. When construction actually begins, a
  structure ghost disappears and leaves the site visually empty until the
  finished piece appears; a work-constructed building instead switches to its
  own required construction-progress art. They do not use dedicated
  structure/building marker textures or generic progress fallbacks.
- Every tool remains visually legible at ordinary gameplay zoom and under the
  existing runtime alpha multipliers.

## Scope

### In scope

- Committed in-world designation interaction and rendering for Chop, Till,
  the existing crop Plant tool, and Construction.
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
   designation layer forces only the settled ghost alpha. After work begins,
   structures intentionally emit nothing until completion and constructed
   buildings hand off to their own progress art.

### Interaction and target identity

Chop's current two-click rectangle already resolves eligible wood-tagged flora,
but D-9 replaces its tile-coordinate box with a screen-space tree selector.
Clicks use sprite hits and drags use rendered ground-contact anchors, including
around cliffs. The nearby Harvest design establishes the same selection model,
so Chop should share that projection and gesture lifecycle if delivery order
permits while retaining its own eligibility, designation state, cancellation,
and work consumer.

Till retains its existing eligibility and same-z rectangle semantics unless the
user expands this design. The repository's current Plant tool is specifically a
single-tile crop workflow: it opens a crop-selection panel on tilled soil and
accepts only `row_crop` or `groundcover_crop` species. It cannot plant a tree.
DTV-3 changes that existing crop designation's committed visual meaning and
invalidation lifecycle, not its target-selection flow or supported species.

### Lifecycle

The existing crop Plant designation is refused unless the tile is tilled and
unoccupied at commit time. D-14 extends that from admission to a standing
invariant: if the land ceases to be tilled, the designation and any claimed or
in-flight job are cancelled. Rendering must never merely hide a still-active
job. Tree planting on ordinary ground is a different, currently unspecified
feature and must not be made subject to this crop-only rule by accident.

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

Adopted by D-8.

Create the umbrella epic first, process the construction-ghost child early,
then close #1780 as superseded with links to both the epic and its concrete
replacement child. This preserves the original verified defect and its review
history while ensuring it never becomes an ownerless redirect. Closing #1780
before the replacement child exists is possible but creates a temporary gap in
actionable ownership.

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
vertical tile side is designated.

### D-5. The crop Plant marker is a light-green flat surface

The existing crop Plant tool remains in this arc. Its marker uses the settled
Till surface language but remains distinguishable with a light-green category
colour. It is shown only under the settled tilled-soil rule. This does not
specify future tree planting.

### D-6. Final marker textures are authored by the user during delivery

The user, not an image generator or an autonomous implementation guess, will
author the final alpha-bearing textures while solving their corresponding
delivery slice. Every final texture still requires gameplay-scale validation
and explicit signoff before merge.

### D-7. The earlier exact-mask, hue-only construction proposal is superseded

The previous proposal to make both construction markers exact Mine-mask
recolours no longer defines the design. D-11 replaces it with actual-target
ghost rendering owned by DTV-4.

### D-8. Create the replacement tracker chain before closing #1780

Process the umbrella epic first and DTV-4 as its first child. Only after both
tracker artifacts exist should #1780 close as superseded, with durable links to
the epic and DTV-4. The remaining children continue one at a time after that
redirect is in place. This preserves #1780's verified defect and review history
without leaving it temporarily ownerless.

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
The `vegface.png` clipping mask is not used for these markers.

### D-11. Planned construction ghosts the target's own rendered art

A `CtStructure` designation displays the selected structure piece's own
texture through the target's structure slot, facemap, camera-facing behavior,
and geometry. A `CtBuilding` designation displays the named building's own
sprite once at its actual anchor, dimensions, and footprint placement. The
designation renderer forces ghost alpha instead of substituting or repeating a
category marker. The two `construct_designate_*.png` placeholders and their
category-texture plumbing become obsolete. Stale #1780-era documentation must
redirect to the new epic or DTV-4 after those tracker numbers exist.

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

### D-15. The fixed construction ghost ends when active work begins

Before construction starts, the target's own finished-form art renders at a
fixed 60% ghost alpha. The ghost does not become more opaque with designation
progress. Once construction actually enters its progress phase, the ghost
disappears. D-16 settles what, if anything, replaces it; completion then hands
off to the built target. For a structure, the durable transition is material
payment (`cdMaterialsPaid`), which occurs when the worker arrives and enters the
building phase—not mere claim or travel. For a work-constructed building, it is
the material gate opening and the instance entering progress rendering.

### D-16. Structures build invisibly; constructed buildings require their own progress art

After a structure piece enters active construction, its designation ghost
disappears and no intermediate sprite renders; the finished structure appears
only when work completes. A building with positive `build_work` must instead
declare and render its own target-specific construction-progress sequence.
There is no shared or finished-texture fallback: a constructed building missing
that art is an invalid asset/data definition and must be caught before play.
Buildings with zero `build_work` follow an appearance or instant-placement
lifecycle instead. The portal's time-driven appearance animation is therefore
not construction progress and must remain semantically distinct even though
the current runtime calls both animation roles `appearing`.

### D-17. Missing building progress sequences land as dedicated art slices

Kitchen, Machine Shop, and Workbench each receive one target-specific
construction-progress sequence authored by the user and landed through its own
issue, PR, visual validation, and signoff. DTV-4 may establish the lifecycle
using existing Cargo Hold and Furnace progress art, but it does not introduce a
generic fallback or enforce the final completeness guard prematurely. DTV-8
depends on all three art slices and then rejects any positive-`build_work`
building without target-specific progress art.

## Open questions

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
DTV-4 replaces it with the actual-target ghost contract and lets #1780 close as
superseded after the new tracker chain exists.

### Q-5. Does DTV-3 redesign the existing crop Plant marker?

Resolved by D-5, D-13, and D-14.

The current repository already has a Plant tool, but it means crop planting:
clicking tilled soil opens a panel and creates a single-tile job for a
`row_crop` or `groundcover_crop`. It cannot plant trees. DTV-3 remains as a
light-green surface redesign of this existing crop marker, while tree planting
is designed separately. Tilled soil is continuously enforced: losing it
cancels the designation and any job rather than merely hiding the marker.

### Q-6. When should #1780 close?

Resolved by D-8.

P-1 recommends creating both the epic and DTV-4 replacement child before
closing #1780 with durable links. The alternative is to close it immediately
after the epic exists and let the epic ledger temporarily carry the unresolved
construction work.

### Q-7. Is construction ghost alpha fixed or progress-driven?

Resolved by D-15.

The current generic construction marker begins at 45% alpha and ramps to fully
opaque with build progress. Existing building placement and pre-delivery
ghosts use 60% alpha. The replacement contract uses fixed 60% target art until
construction begins, then removes the ghost and follows D-16's target-class
lifecycle.

### Q-8. What progress art do structure pieces use?

Resolved by D-16.

Buildings with an `appearing` animation already have target-specific progress
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
- Cliff-bearing identity tests or probes if Chop selects rendered tree anchors
  rather than a tile-coordinate rectangle.
- A render-level assertion that Mine uses the three-face isometric facemap while
  Till and crop Plant use the 1×1 default top-facing map and their authored
  alpha masks determine the visible surface shape.
- An offscreen or graphical acceptance path which captures committed Mine,
  Chop, Till, crop Plant, actual-target Structure and Building ghosts, and the
  structure ghost-to-empty-to-finished transition plus the constructed-building
  ghost-to-progress-to-finished transition at gameplay zoom.
- Asset checks for the user-authored Chop, Till, and crop Plant marker
  paths, including decoded RGBA geometry/alpha contracts. Construction instead
  needs render assertions proving that the designation resolves the same
  target texture/facemap/geometry as the built object, plus a data/asset guard
  rejecting every positive-`build_work` building without target-specific
  progress frames while exempting zero-work appearance lifecycles.
- Explicit user signoff for every new or changed texture.

## Delivery plan

### DTV-4. Ghost planned construction with the target's own rendered art

- **Outcome:** Every planned structure piece or building appears as a
  translucent preview of the object that will actually be built, with no
  category-marker substitution.
- **Scope:** Resolve designation targets into their production texture,
  facemap, dimensions, slot/rotation, sprite anchor, and footprint presentation;
  apply the fixed-60% pre-work ghost; suppress structure rendering during active
  work; hand constructed buildings to the existing progress-render path when
  work begins; preserve zero-work appearance lifecycles; remove obsolete
  category marker assets/handles/setters; redirect stale #1780-era documentation
  after tracker creation; and add focused render plus visual verification.
- **Phase:** 1 — construction presentation
- **Depends on:** `none`
- **Ordering:** `first child`; create immediately after the epic so it can
  supersede #1780 under D-8
- **Relevant decisions:** D-7, D-8, D-11, D-15, D-16, D-17
- **Acceptance signals:** A building designation emits one ghost with the same
  target sprite dimensions and anchor as that building rather than one generic
  tile per footprint cell; every structure kind uses its selected target art,
  facemap, camera-facing behavior, and slot geometry; only the settled ghost
  treatment differs from built output before work starts; active structure work
  renders no site sprite until the completed piece appears; constructed
  buildings with existing progress art hand off to it; zero-work appearance
  animations such as the portal remain unchanged; the placeholder textures and
  routing are absent; stale documentation redirects to the replacement tracker;
  and gameplay-scale captures receive user signoff.
- **Out of scope:** Construction selection, job execution, changes to how work
  progress is accumulated, missing per-building progress assets and the final
  no-fallback enforcement owned by DTV-5 through DTV-8, and unrelated marker
  families.
- **Open questions:** `None`

### DTV-5. Author Kitchen construction-progress art

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

- **Outcome:** Construction and appearance are distinct building lifecycles,
  and every work-constructed building is guaranteed to have target-specific
  progress art with no runtime fallback.
- **Scope:** Introduce or expose a construction-specific animation/activity role;
  wire Cargo Hold, Furnace, Kitchen, Machine Shop, and Workbench to it; retain
  the portal's zero-work appearance role; update Lua/API consumers of building
  activity; and add an asset/data guard rejecting positive-`build_work`
  definitions without a valid construction sequence.
- **Phase:** 3 — construction contract enforcement
- **Depends on:** DTV-4, DTV-5, DTV-6, DTV-7
- **Ordering:** `critical path` after the construction presentation and all
  missing progress art
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

### DTV-1. Make Chop a drag-box tool with tree-anchored designation markers

- **Outcome:** Chop uses the settled press-drag interaction and visibly marks
  exactly the eligible trees selected by that gesture.
- **Scope:** Gesture lifecycle, selected-identity oracle, existing wood-tag
  eligibility, add/erase behavior, tree-relative marker rendering, final icon,
  invalidation, and focused input/visual verification.
- **Phase:** 1 — interaction and target annotation
- **Depends on:** `none`; coordinate shared ownership with HARV-2
- **Ordering:** `independent`, but whichever of DTV-1 and HARV-2 lands first
  should establish a reusable gesture/projection boundary
- **Relevant decisions:** D-2, D-3, D-6, D-9, D-12
- **Acceptance signals:** A press-drag gesture designates the settled eligible
  tree identities, including the settled cliff case, and each committed tree
  receives the signed-off alpha icon without a full-tile ground overlay.
- **Out of scope:** Non-wood Harvest behavior and Chop work execution.
- **Open questions:** `None`

### DTV-2. Render Till as a flat top-surface designation

- **Outcome:** Committed Till work reads as a two-dimensional ground treatment
  with no lit or visible vertical tile sides.
- **Scope:** Explicit surface render path, settled mask/facemap pairing, final
  user-authored Till marker, and focused render/asset verification.
- **Phase:** 1 — surface render foundation
- **Depends on:** `none`
- **Ordering:** `critical path` for DTV-3
- **Relevant decisions:** D-4, D-6, D-10
- **Acceptance signals:** A designated tillable tile shows only the settled
  top-surface region at gameplay zoom across camera facings; Mine still shows
  all three faces.
- **Out of scope:** Till eligibility, AI execution, and #1692 toolbar art.
- **Open questions:** `None`

### DTV-3. Render the existing crop Plant job as a light-green tilled surface

- **Outcome:** An existing crop Plant designation uses the same settled
  flat-surface language as Till, remains clearly distinct in light green, and
  is cancelled if its land ceases to be tilled.
- **Scope:** Reuse of the top-only render path, final user-authored Plant marker,
  continuous tilled-soil invalidation, durable/in-flight job cancellation, and
  focused render/state coverage.
- **Phase:** 2 — specialized surface marker
- **Depends on:** DTV-2
- **Ordering:** `critical path` after DTV-2
- **Relevant decisions:** D-5, D-6, D-10, D-13, D-14
- **Acceptance signals:** Plant designations render as signed-off light-green
  top surfaces only while the tile is tilled; losing that state cancels the
  durable designation and any claimed/in-flight work without leaving an
  invisible job, including across save/load.
- **Out of scope:** Crop choice UI, planting AI, tree planting, general planting
  semantics, and #1692 toolbar art.
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
- The user approved creating the epic and DTV-4 replacement child before
  closing #1780 with links to both, then processing the remaining children one
  at a time.
- The user approved screen-space Chop selection: clicks choose the topmost
  eligible tree sprite and drags include rendered tree anchors inside the box.
- The user approved the 1×1 default/noface map for Till and Plant; each authored
  designation texture's alpha channel will cut out its own flat surface shape.
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
