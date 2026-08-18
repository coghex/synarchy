# Engine contracts: as-built detail

`CLAUDE.md` is auto-loaded into every session, so it carries the rules
that prevent damage: what you must not undo, and which gate proves it.
This file carries the layer below that — the as-built mechanics behind
those rules, extracted from CLAUDE.md on 2026-08-18 to keep the
always-loaded file navigable.

**This is not a design document.** The design docs
(`docs/texture_infrastructure.md`, `docs/unified_item_transfers.md`,
`docs/expedition_gameplay_loop.md`, `docs/persistence_contract.md`, …)
record what was *decided*; this records what was *built*, and it is the
only prose record of most of it. Read the section here before changing
code in the area it covers — CLAUDE.md points you at each one by name.

Every contract below is mechanically enforced by the gate its CLAUDE.md
entry names, so a breach fails loudly rather than silently. That is
exactly why the detail could move out of the always-loaded file.

---

## Contents

**Assets and rendering**

- [Unit animation art: inventory structural invariants](#unit-animation-art-inventory-structural-invariants)
- [Unit animation atlas runtime: index validation and digests](#unit-animation-atlas-runtime-index-validation-and-digests)
- [Preview mode: the two viewers and the dump contract](#preview-mode-the-two-viewers-and-the-dump-contract)

**UI**

- [Container window stack: panes, widget naming, teardown reasons](#container-window-stack-panes-widget-naming-teardown-reasons)

**World and naming**

- [Name etymology: internals (#1104)](#name-etymology-internals-1104)

**Gameplay systems**

- [Player transfers: the three player-facing modes](#player-transfers-the-three-player-facing-modes)
- [The expedition loop: the unprepared control](#the-expedition-loop-the-unprepared-control)

**Persistence**

- [Autosave: staging, rotation order, and the intent mutex](#autosave-staging-rotation-order-and-the-intent-mutex)
- [Save/load transaction: phases and failure semantics](#saveload-transaction-phases-and-failure-semantics)
- [Enum append-only audit: baseline and payload normalization](#enum-append-only-audit-baseline-and-payload-normalization)

**CLI**

- [CLI value validation (#1191)](#cli-value-validation-1191)

**Process gates**

- [Findings-report lane split: why it matters](#findings-report-lane-split-why-it-matters)
- [Docs landing: docs-wip, autostash, and the protected-ref warning](#docs-landing-docs-wip-autostash-and-the-protected-ref-warning)

---

## Unit animation art: inventory structural invariants

#1261 (TEX-6) promoted `tiller`, `unknown_unit` and `white_tailed_deer`
to real `units:` entries. `unknown_unit`'s hard-coded missing-texture
fallback (`unknownUnitTexture` in
`Engine.Scripting.Lua.API.Units.List`) is untouched by any of this.
Outside this inventory's scope:
`assets/textures/units/unknown_unit/rotations/*.png` and the per-unit
`portrait.png` files, referenced from hard-coded Haskell or non-animation
YAML fields. In preview mode a `flora/unknown_flora.png`-style FILE where
a directory was expected is a pre-boot rejection, not a fallback.

Enforced by `python3 tools/pack_atlas.py --validate-only --strict`; gate
for the checker itself is `tools/test_pack_atlas.py`. Each breach names
the real problem rather than failing generically.

- A unit identifier is one lowercase `[a-z0-9_]+` path component. An
  animation identifier is the same, plus ONE narrowly matched approved
  exception, `<lowercase>_RH_<lowercase>`, for the documented
  asymmetric-weapon animations — so `attack_heavy_RH_dagger` passes while
  `AnyThing`, `attack_heavy_RH_Dagger` and `attack_LH_dagger` do not.
- Frames are `frame_NNN.png` with exactly three digits, so `frame_1.png`
  and `frame_0002.png` are rejected rather than read as another spelling
  of an index.
- A declared path is relative, `..`-free, symlink-free, and resolves
  inside its EXACT `<unit>/animations/<animation>/<direction>/`
  directory, so cross-unit, cross-animation and cross-direction
  references are each named as such.
- `flip: true` declares exactly the canonical five authored directions;
  `flip: false` exactly all eight.
- Per direction, indices start at 0, ASCEND in the order they are
  declared, and have no gaps or duplicates. Ascending order matters
  because playback walks the declared list: a contiguous-but-shuffled
  list plays out of sequence while every set-based check still passes.
  Different directions of one animation may hold different counts.
- `fps` is a positive number that survives the engine's 32-bit `Float`,
  and `loop` a boolean — rejected rather than coerced when they are not.
  The `fps` guards stack because a positivity test alone is not enough:
  PyYAML resolves `.nan`/`.inf` to real floats (`nan <= 0` is False like
  every NaN comparison, and infinity really is greater); a Python int has
  unbounded precision, so a thousand-digit `fps:` is valid YAML that
  makes `math.isfinite` RAISE rather than answer; and `1.0e+100` /
  `1.0e-100` fit a 64-bit double but land in `UnitYamlAnim`'s
  single-precision field as infinity and zero.
- No symlink may appear anywhere in the walk — unit directory,
  `animations/` root, animation directory, direction directory, or frame.
  A symlinked entry is an ERROR, never a skipped one, or a linked tree
  would evade the inventory while its frames still ship.
- A `--unit` naming neither a declaration nor an asset tree exits
  non-zero rather than reporting a clean run of an empty inventory.

### Content validation: why three checks (#1311)

Every declared frame is opened and decoded, in three checks because each
covers ground the others cannot:

1. A full `decode_rgba8` covers the compressed pixel stream —
   truncation, corrupt deflate data, a non-image, and (via its own format
   check) a valid image of another format renamed `.png`.
2. Pillow's `verify()` then CRCs the chunks, which is the only thing that
   sees an intact payload under a WRONG checksum: the decoder reads and
   discards IDAT CRCs while streaming.
3. `locate_png_stream_end` covers the terminal **IEND** chunk, which
   `verify()` breaks ON without checksumming and the decoder never reads,
   plus anything appended after the image ends.

That last one walks chunk FRAMING only — length, type, payload, CRC —
decoding nothing, knowing no chunk type but IEND, and running only after
Pillow has CRC-validated that sequence, so it cannot disagree with the
real decoder about where a chunk lies. **Keep it that narrow**: a second
hand-rolled PNG parser is what sank the previous attempt at this issue.

Checking the FILE's last bytes is NOT equivalent, and was the round-2
review finding: appending a second canonical IEND leaves a perfect tail
while the real image ended 12 bytes earlier.

Do not "simplify" the three into one — `tools/test_pack_atlas.py`'s
`every content check earns its keep` case exists because each has a
fixture the other two accept.

Every frame of one animation must then decode to the same pixel size (the
atlas cell is that size and nothing resamples), while frame COUNTS may
still differ per direction. The rule is "decodes as a PNG", never "is
already RGBA8": paletted, greyscale, greyscale+alpha, 16-bit and
interlaced frames all pass.

Pillow is therefore load-bearing for validation, not just compilation —
an absent decoder is one loud error naming the install command, never a
silent skip that would print OK while checking nothing. It is still
imported lazily, which now only spares a run with no declared frames. The
content pass adds roughly half a second over the whole 4,620-frame corpus
(~1 s structural, ~1.5 s total), so it is unconditional rather than
hidden behind a flag.

---

## Unit animation atlas runtime: index validation and digests

Gate entry points worth knowing by name: `registerUnitDefs` is what
`loadUnitYamlFn` delegates to, the on-disk fixture tree is driven through
`loadUnitAtlasIndexIn`, and the pinned-nearest survival of a global filter
toggle is checked through `planFilterRebind`.

Enforced by hspec `--match "Unit.Atlas"` and
`--match "the real unit registration boundary"`.

`Unit.Atlas.Load.loadUnitAtlasIndex` validates in three passes, cheapest
first, stopping at the first failure.

**(1) The index parses and is structurally sound.** Supported
`schema_version` and `digest_algorithm`; the unit's own identity;
duplicate animation names; containment of `atlas_path` inside that unit's
`atlas/` directory AND its equality with that animation's canonical
`<animation>.png`; positive geometry; every reachable cell lying inside
the sheet; unique and in-range direction rows; real frame counts bounded
by row capacity; a positive finite `fps`.

The canonical-path equality is what makes D-2's one-atlas-per-animation
hold *by construction*: no two animations can name one file, so the
upload path's otherwise-correct same-path aliasing can never collapse two
animations onto one image and one bindless slot.

**(2) It still describes what the unit YAML declares.** Animation set,
`fps`/`loop`/`flip`, direction set, per-direction frame counts, columns.
`Unit.Atlas.Index.planUnitAtlasStorage` owns this half, including the
reverse coverage: an animation the YAML DECLARES that the index does not
name rejects, because publishing the unit without it would silently drop
art the file asks for. Its result therefore covers exactly the YAML's
animation set, which is what lets the loader publish straight from
`atlasTextureRequests` — its own upload set, each request carrying the
animation's index record — with no second lookup that could miss.

**(3) Each atlas decodes to the image the index describes** (dimensions
plus `atlas_digest` over decoded RGBA8), AND every declared SOURCE frame
decodes to exactly the pixels its atlas cell holds.

### Both digests earn their keep

`atlas_digest` catches an artifact the index does not describe.
`source_digest` (`Unit.Atlas.Digest.sourceDigest`, recomputed from the
same inputs the compiler digests) catches a forged digest and a frame
whose PATH changed while its pixels did not — nothing else in the index
records paths. The per-frame pixel comparison still runs first, because
it localizes a stale artifact to one direction and one frame where the
digest can only say that something moved.

Reproducing `source_digest` means reproducing Python's `repr()` of the
narrowed fps (`pythonFloatRepr`), whose positional/scientific thresholds
Haskell's own `show` does not share. That is pinned against
CPython-generated reference values across the whole float32 range: a
formatting divergence must fail in the test, not by rejecting every atlas
of a unit whose fps lands in the disagreeing range.

Pass 3's source reading SURVIVED TEX-6, which #1259 expected to retire
it. Its cost was measured rather than assumed: decoding all 4,620 shipped
source frames across all seven units totals ~1.8 s of one-time unit-def
loading (`bear_brown`, the largest, 0.74 s), paid on the Lua thread while
YAMLs load and not on any frame. The source PNGs remain the tracked,
hand-edited artwork (D-1), so they remain something a developer can
repaint without recompiling, and CI's asset gate only runs on a push —
this stays the check that catches a stale artifact locally, in the same
run that would otherwise have drawn the stale art.

### The policy-aware upload cache

Atlas slots are registered PINNED to the nearest sampler with one mip
level (D-6), so a runtime `setTextureFilter` toggle cannot start
bilinearly resampling unit art — which on a sheet would additionally
bleed neighbouring cells across every frame edge.

The upload path's path cache is therefore **policy-aware**: `apAssetPaths`
is keyed by path alone, while a slot's sampler was fixed by whichever
policy first uploaded it. A cache hit is taken only when the canonical
texture's pinned-ness matches the request (`cacheEntryReusable` against
`btsPinned`), and otherwise re-uploads into its own slot. Both directions
matter — an atlas inheriting an ordinary slot would stop being nearest,
and an ordinary texture inheriting a pinned one would be stuck on a
filter it never asked for.

Cell UVs sit on exact cell EDGES with no half-texel inset: unit art is
nearest and pixel-snapped, so a fragment centre lands inside its cell,
and an inset would shift the sampled texels and break pixel-identity with
what the per-frame path drew.

---

## Preview mode: the two viewers and the dump contract

Enforced by `tools/preview_cli_probe.py` (CI-eligible, no boot) and
`tools/preview_probe.py` (manual-only, `needs-gpu`); pure logic by hspec
`--match "Preview.Discovery"` / `"Preview.UnitAnimation"` /
`"Preview.Building"`.

### Simple-category browser behavior

- **Bare category** (`--preview icons`): a scrollable left-hand list of
  every texture found recursively under the category root, labeled by its
  category-relative path with `/` separators and the file extension
  INCLUDED (e.g. `skill/climbing.png`), sorted lexicographically. The
  first entry auto-selects; its texture renders in the main panel,
  nearest-neighbour scaled, fit to the panel with aspect ratio preserved.
  Click a row to select it; wheel-scroll the list. A resize (the preview
  window is resizable) reflows the panel/list bounds while preserving the
  current selection and scroll offset
  (`previewManager.onFramebufferResize`).
- A label displayed here is ALWAYS a valid item target for the focused
  form — discovery and item resolution apply the identical extension
  rule, so they can never disagree.
- **Focused item** (`--preview icons/skill/climbing.png`): shows only
  that one texture, no list.
- `previewManager.init` forces `engine.setTextureFilter("nearest")`
  live-session-only — never assumed from the default video config, which
  a user's persisted `config/video.local.yaml` can override to
  `"linear"`.

### Units viewer (#887/#1261)

- **Ordering + default selection:** animations sort case-sensitively by
  exact directory name (the same `Ord`-on-the-label rule
  `Engine.Preview.Discovery.sortEntries` uses); `idle` is selected when
  present, else the first entry in that order, direction south.
- **Directions:** the game's own `S, SW, W, NW, N, NE, E, SE` order. A
  directly authored direction ALWAYS wins; W/SW/NW mirror SE/E/NE only
  when flipping is permitted, which is the index's `flip` (proved equal
  to the YAML's before anything is published). A direction the animation
  does not author and may not mirror stays unavailable rather than being
  invented or filled from another unit's textures.
  #1261 retired the no-YAML-entry INFERENCE with the rest of the
  YAML-less path: since #1257 every shipped animation declares its own
  `flip`, and the three trees #1261 promoted declare `flip: true` over the
  canonical five, which is exactly what the inference used to produce.
- A mirrored cell renders genuinely mirrored via `UI.setSpriteFlipX`
  (#887's `ussFlipX`), applied to the CLIPPED UV slice — flipping before
  clipping would sample the wrong slice; #1259 generalized that
  reflection to the sprite's own source sub-rect.
- **Playback:** ONE clock per selected animation. Every direction
  computes its own index from the SAME elapsed value against its OWN
  frame count, so unequal per-direction frame counts (four checked-in
  acolyte animations have them) stay phase-aligned. Selecting a different
  ANIMATION resets the clock; enlarging a different DIRECTION does not.
  Non-loop end-of-clip HOLDS the last frame — the same clamp
  `Unit.Render.pickFrame` applies in game. The frame index comes from a
  wall clock, so the script tick rate only affects smoothness.
- **Reflow:** a resize preserves the selected animation, selected
  direction, list scroll offset, AND playback phase.
- **Pre-boot rejection:** an unknown unit, a name with path structure or
  `.`/`..`/absolute traversal, a symlinked unit directory OR symlinked
  `animations/` root, and a unit with no animations all exit 1 before a
  window exists. Both symlink levels matter — `doesDirectoryExist`
  follows links, so a real unit directory with a symlinked `animations/`
  would otherwise browse and load another tree's assets, breaking trimmed
  loading. Since #1261 a missing, animation-less, or uncompiled YAML IS a
  rejection: with no declaration there is nothing to browse
  (`UnitNoAnimations`), and a declaration whose compiled artifacts are
  missing or stale rejects as `UnitAtlasRejected` — the same refusal the
  game makes.

### Buildings viewer (#888)

- **The filesystem is authoritative**, the same split the units viewer
  uses. The building's own folder decides which entries exist and, in an
  animation directory, the numeric `frame_NNN.png` order;
  `data/buildings/<name>.yaml` only AUGMENTS a matched animation with
  `fps`/`loop` and supplies the default-selection hints. A missing,
  malformed, or unmatched YAML never rejects a valid asset folder
  (`dungeon_1` has no YAML at all; `cargo_hold_S`/`furnace` ship a
  `demolish/` folder no YAML mentions).
- **One list, both kinds.** A recognized animation directory is ONE entry
  labeled by its directory name; every other directory is descended into
  so its textures surface as ordinary item-relative statics
  (`dungeon_1/damaged/floor.png`) rather than being played as one clip or
  silently lost. Ordering is the single label-lexicographic rule the rest
  of the browser uses, across both kinds together.
- **A directory is an animation** iff a YAML animation's declared frame
  paths live in it, OR every `.png` in it follows the numbered-frame
  convention (`frame_000.png`, `frame_10.png`, `frame-3.png`).
- **YAML association is by CONTENT, never by equal names.**
  `acolyte_portal.yaml` names its animations
  `portal-appear`/`portal-idle` while the directories are
  `appear/`/`idle/`, so a directory is matched through the frame paths its
  animation declares.
- **Default selection ladder:** `state_animations.built`'s animation
  (resolved that same way — selected label `idle`, not `portal-idle`),
  else the def's own `sprite` when it names a discovered static, else
  `default.png`, else the first entry. `dungeon_1` (no YAML, no
  `default.png`) lands on the last rung.
- **Playback defaults are `fps=8`, `loop=false`** — `BuildingYamlAnim`'s
  own, NOT the units viewer's `loop=true`. One wall clock per selected
  animation, reset on a real selection change but preserved across a
  resize; non-loop end-of-clip HOLDS the last frame. A STATIC selection
  has no playback at all.

### The dump contract

`require("scripts.preview_manager").dump()` (self-registered into
`package.loaded` the same way `unit_ai.lua`/`debug.lua` are, despite
being `engine.loadScript`-loaded, not `require`d) reports `mode`
(`"list"`/`"item"`/`"unit"`/`"building"` — #632's `"placeholder"` is GONE
as of #888, every canonical category now dispatching to real behavior),
`state` (`"loading"`/`"ready"`/`"empty"`), the current `selected` entry,
and in list mode the FULL ordered `entries` list (not just its
`entryCount` — a probe needs the complete list to catch an
omission/substitution anywhere past the visible/selected rows),
`scrollOffset`, and per-visible-row interactive bounds/handles (`rows`,
`scripts/ui/list.lua`'s existing F3 dump contract) — enough to drive real
`input.click`/`input.scroll` against a located row without ever
hardcoding a screen coordinate.

**Unit mode** adds `unit`, the animation `entries` list (each with
`fps`/`loop`/`flip`/`thumb`/`directionCount`, plus #1260's `storage` and
`atlas` path — the WHOLE list, so a probe can prove every animation
selected the atlas, not just the one playing; since #1261 `storage` can
only read `"atlas"`, but it is still DERIVED Lua-side from the atlas path
the engine actually pushed rather than asserted, so a missing one reports
`"legacy"` and fails a probe instead of passing silently), `defaultAnim`,
and `playback` — current `animation`, `direction`, `mirrored`,
`sourceDirection`, `frameIndex`, effective `fps`/`loop`, the same
`storage`/`atlas` pair with the playing frame's `texturePath` and
index-derived `cell`, plus a per-direction `directions` array carrying
each cell's own mirrored flag, source, frame index, sampled
`texturePath`/`uv`, and interactive bounds/handle.

**Building mode** adds `building`, the ordered `entries` list (each with
`kind` `"animation"`/`"static"`, `animated`, `fps`, `loop`,
`frameCount`), `defaultEntry`, `selected`, `scrollOffset`,
per-visible-row `rows` bounds/handles, and — for an animation selection
ONLY — `playback` (`entry`, `frameIndex`, `frameCount`, effective
`fps`/`loop`, `ready`).

### Trimmed loading

Preview mode loads only its font, the list widget's own chrome textures
(`assets/textures/ui/{highlight,scroll*}.png`, loaded once, list-mode
only), and textures within the requested category/item — never
`data/*.yaml` gameplay catalogs. There are exactly TWO exceptions, both a
single file for the requested item: the units viewer's
`data/units/<name>.yaml` and the buildings viewer's
`data/buildings/<name>.yaml`.

`tools/preview_probe.py` verifies this against
`engine.getLoadedTexturePaths()` — `Engine.Asset`'s `apAssetPaths`,
populated by `engine.loadTexture`'s own Haskell handler regardless of Lua
caller, so it is the engine's own authoritative loaded-texture record,
not previewManager's self-reported bookkeeping.

---

## Container window stack: panes, widget naming, teardown reasons

A world-page panel is reopened after a resize through its own real entry
point — `reopenWithTab` / `reopenWithState` / `restoreStack` — and widgets
that hold raw text (textbox, randbox, dropdown filters) round-trip via
`snapshotPage`/`restoreAll`. Stacking-only modal pages opt out of the
boundary with `UI.setPageInputExclusive(page, false)` (e.g. `popup.lua`
cards); the F8 overlay hit-tests itself through a parallel
`tryClaimClick`.

Enforced by hspec `--match "container window stack"` /
`"Container knowledge"` / `"Nested item contents"` / `"Item list widget"`,
plus `tools/item_list_widget_probe.py` (manual-only, `needs-gpu`).

**The four level kinds.** `endpoint` (a storage building or a unit);
`unitItem` (LIVE, `unit.getItemContents`, which searches loose inventory,
equipment AND accessories — the three the unit-info list merges);
`buildingItem` (the player's REMEMBERED contents,
`building.getRememberedItemContents`, carrying the PARENT record's own
`revealedAt` — never a live storage read, never a knowledge write); and
`escort` (#1250's Mode A pair).

The two item kinds descend by EXACT INSTANCE IDENTITY along a path of
instance ids, and a path that stops resolving closes that level AND every
level below it rather than retargeting a same-def sibling. An
item-container level is RENDER-ONLY (D-5): no transfer endpoint, no
transfer operation — only inspection (scroll, close, open a child), so a
building row keeps its Withdraw entry and merely GAINS "Contents".

`scripts/item_contents_panel.lua` no longer owns a window lifecycle
(D-13): it supplies the two item-level kinds and nothing else — no page,
no panel, no singleton, no `setup()`, no `update()`.
`scripts/transfer_session_panels.lua` supplies the `escort` kind the same
way and owns no lifecycle either.

**Panes (#1250).** A level owns one or more PANES — a pane being one panel
box, its header and one item list, with its own tab and scroll — and for
every kind but `escort` the level table IS its own single pane
(`panes[1] == level`), so `level.listId`/`activeTab`/`scroll` still mean
exactly what they meant before. A level stays the unit of NESTING,
modality, teardown and restore, which is what makes two flanking panels
ONE level.

**Widget naming is load-bearing.** The stack is transient session UI:
`hud.createUI()` snapshots and restores the WHOLE thing across a resize
(path + per-PANE tab and scroll), and every pane names its widgets from
`paneWidgetName` — the single pane keeps the historic bare `cargo_inv`, a
further pane appends its key — because keyboard control focus is restored
BY NAME to the first visible match, so two panes sharing one name would
return focus to the wrong one. Also, `uiManager.onSaveLoaded` drops it.

**Teardown reasons.** A level teardown carries a REASON, and `"layout"` —
passed only by that resize snapshot/restore pass and by
`view_teardown`'s `resize` hook — is the one that does NOT fire a kind's
`onClose`; every other teardown does. That distinction is what lets an
escort session (and the unit it holds) survive a resize while a zoom-band
change, a HUD hide, Escape, or another container replacing it all end it.

---

## Name etymology: internals (#1104)

The chosen expression is deterministic from the instance's own stable
`liId` (plus the language seed/version and the def id). Growing the
catalogue never re-renders a stored name even though `assignLanguageRoots`
re-resolves collisions over the whole concept set. River event/feature
pairing walks `gtFeatures` order.

Enforced by hspec `--match "Language etymology"` / `"Etymology panel"`
and `tools/etymology_probe.py` (manual-only, `needs-gpu`).

What makes decomposition possible is a small optional `EtymologySource`
(the originating `NameExpr` plus the `LanguageProvenance` that rendered
it) persisted beside the name on all three carriers: `wiEtymology`,
`liEtymology`, `rvnEtymology`. A precomputed morpheme list is
deliberately NOT stored — the presentation is reconstructed on query.

`Language.Generated.Render` produces an ordered token TRACE and
`renderNative` IS its concatenation, so "concatenating the trace
reproduces the stored name" holds by construction;
`Language.Generated.Boundary.joinMorphemesTrace` is the one
implementation both views of a boundary share.

`Language.Etymology` re-renders from the source and CHECKS the result
against the authoritative stored text before showing any of it — a
mismatch (a tampered name, a source from another language, a historical
version this build renders differently) reports unavailable rather than
explaining the wrong word.

Morpheme identity is `(LanguageProvenance, ConceptId)` — never spelling —
so #1096's bound form and its free root are ONE morpheme while two
languages' homographs, and the SAME seed under two generator versions,
are not. Capitalization is a surface-POSITION effect: the leading token
carries it, every canonical free spelling stays the unmarked lowercase
root.

A source is additionally required to belong to the PAGE's own recorded
language (`decomposeEntityName`): the surface check proves an expression
renders to the stored text under ITS OWN language, so a stale or foreign
source that happens to reproduce those letters would otherwise pass while
attributing every morpheme — and every recurrence link — to a language
the world does not have. A page with no provenance admits no source at
all.

`world.getEtymology(kind[, id][, pageId])` feeds world/location/river
adapters into that one path; an unavailable reply still carries the
stored name so the UI can keep showing it.

### Recurrence, and why self-exclusion is page-qualified

Recurrence is computed on demand from the ACTIVE page — current world +
`LifecycleDiscovered`-or-later locations + ONLY the river being inspected
(a world or location target admits no river at all), the inspected entity
excluded from its own links, entries exposing nothing but an entity kind
and an already-visible name. There is no session history.

The optional `pageId` names the TARGET only (#1265) and never widens that
set: omitted, target and recurrence are both `resolveActiveWorld`'s page;
a live INACTIVE page resolves the target there — its stored name, gloss,
source and page-language validation all that page's — while candidates
still come only from the active page, so no inactive name is ever a
recurrence entry; a page that does not exist is the unchanged
`available=false`/`no_entity`.

With no visible page, recurrence follows `resolveActiveWorld` exactly,
head-of-`wmWorlds` fallback included, and substitutes nothing when that
resolves to `Nothing` — a missing ingredient on the RECURRENCE page (no
active page, no gen params) leaves an explicitly selected target's result
intact with recurrence empty, never downgrading it.

That crossing is what makes self-exclusion PAGE-QUALIFIED: every page's
world entry is `("world", Nothing)` and location ids are page-local, so
comparing kind and id alone would silently drop the active page's own
world name, or an equal-numbered active location, from an inactive
target's links. A river target on another page admits no river at all —
the inspected river is not on the active page, and its `GeoFeatureId`
re-resolved there is a different river.

### The suggestion chain

The expression travels the whole Create World chain —
`world.suggestName`'s `expr` → `name_suggest` → `generation` →
`world_view` → `world_manager` → `world.init`'s 9th argument — and is
cleared with the gloss and provenance the moment the player edits the
name.

### Persistence

`world-pages` v7 (v6 frozen by #1230 as
`PageCoreDTOv6`/`WorldGenParamsDTOv5`/`LocationInstancesDTOv3`), with
`PageCoreDTOv5`/`WorldGenParamsDTOv4`/`WorldIdentityDTOv2`/
`LocationInstanceDTOv2`/`RiverNameDTOv1` frozen — every historical shape
decodes with the source ABSENT, never inferred.

---

## Player transfers: the three player-facing modes

Design authority for the *decisions* is
`docs/unified_item_transfers.md`; this is the as-built behavior. The pure
policy itself (`src/Unit/Transfer.hs`) and the lax-AI-verb rule stay in
CLAUDE.md, because routing AI work through the strict path is the mistake
that has to be prevented on sight.

Enforced by hspec `--match "Unit transfer"` / `"Transfer context menu"` /
`"durable transfer orders survive"`, plus
`tools/transfer_order_probe.py` and `tools/item_list_widget_probe.py`
(both manual-only; the latter owns the real-AI behavioural proof that a
MOVING target is preempted and then stays put for the whole approach,
which no fixture that ticks no simulation can state).

The durable ORDER store is #1246's per-page `wsTransferOrdersRef`.
Outcome vocabulary is deliberately small: a stall is `out_of_range`, an
arrival refusal is `became_stale` carrying the real cause, a worn item is
refused as `item_not_transferable`, and an escort source that never
registered the action is refused as `source_not_escortable`. Only
`ready_to_commit` entries are ever submitted. `unit.cancelTransferOrder`
takes pending entries only (via `cancelBatch`); escort/hold eligibility
stays `isPlayerCommandable` of the live faction, never a def allowlist.

- **Durable orders (#1246/#1247/#1253).** `createTransferOrder`
  validates with adjacency DEFERRED (`ReachPolicy`; same page still
  required); `checkTransfer`/`commitTransfer` still require it.
  `unit_ai_transfer.lua` walks the ACTING unit under a 7.5 lock, and
  ARRIVAL IS THE COMMIT (`unit.commitTransferOrder` re-validates
  atomically) — a refusal there is `became_stale` carrying
  the real cause, and a create-time refusal is never retried. The 60 s
  timer is a STALL timer over ELIGIBLE time, reset on every new closest
  approach — never a trip budget. Every way an order ENDS is one rule
  (`unit_ai_transfer_outcome.lua`): surface once via `unit_warning`,
  then PRUNE unconditionally, so nothing terminal rides a save and
  handling stays edge-triggered and idempotent. `cancelTransferOrder`
  (pending only) + `pruneTransferOrder` (terminal only,
  ownership-scoped, idempotent); the player's way in is **"Cancel
  transfer"** on the unit's context menu, omitted (never disabled) when
  it carries no live order. A CARRIER ceasing to act is the one exit
  the executor can't reach, so `retireTransferOrdersEverywhere` drops
  orders engine-side from BOTH destroy and kill — death is easier to
  miss than destruction, since the instance remains and every reference
  still resolves. Collapsed/crawling are excluded (merely suspended). A
  commit result reports EVERY requested item, so the arrival report
  excludes what the command-time gate already surfaced (`settledIds`).
- **Mode B — queued gestures (#1249, `transfer_gestures.lua`, ONE
  builder both hosts call).** **Store 1 / Store all** from a unit-info
  row into the open container window's ACTIVE level; **Retrieve 1 /
  Retrieve all** from a container row into the unit
  `transfer_session.resolveSource` picks. NEITHER requires adjacency —
  that is the whole promotion. Granularity is 1-and-all only, and "all"
  is every instance id the merged row stands for
  (`itemList.rowInstanceIds`, signed into the rebuild identity), never
  a count. A gesture is OMITTED, never disabled, whenever it could not
  run: no window, no eligible source, an equipped/accessory item, a
  self-transfer, or an ACTIVE level that is an item container
  (render-only — never fall back to a transfer-capable ancestor) or an
  escort pair.
- **Mode A — escort (#1250/#1251, `transfer_session.lua`).** Walk
  FIRST, then choose items. `unit_ai_escort.lua`'s `escort_transfer` (a
  7.5 lock, peer of the queued order) walks the source to the
  destination's FOOTPRINT and stops. An eligible SOURCE is one whose
  species actually registered that action (`unit_ai_actions.lua` records
  every species' action names); an EMPTY action inventory means no AI is
  loaded and answers yes to everything, never a refusal invented from
  absence. The two 440-wide panes are fitted as a PAIR
  (`responsive.fitScale`, a level kind's `paneScale`, against
  `reserved_regions.maxAvailableWidth`)
  then placed as ONE rect that is split — both halves matter at the
  800x600 minimum. The one-way transition to open/held fires EXACTLY
  ONCE and does everything else: `building.refreshContainerKnowledge`
  (its only caller in the game), opening the panes, and the camera snap
  — each reading LIVE endpoint positions, never the creation-time
  snapshot. Rows commit IMMEDIATELY through `checkTransfer` then
  `commitTransfer`, the COMMIT authoritative: drift out of reach is
  refused with the contract's own proximity reason and the session
  stays open. The hold is released BY the session ending, and that
  release STOPS the unit rather than merely letting go. A UNIT
  destination is held too (#1251) — unit-to-unit is the one pairing
  where BOTH ends can walk away. The session's `roleOf` is the one
  answer both actions consult: `"source"` walks then stands, `"target"`
  (`escort_hold`) stands from CREATION, both scoring 7.5 so neither end
  outscores the other. Being a source is a per-species capability;
  being a target is player-commandability and nothing else, so
  `escort_hold` is auto-prepended to EVERY species by
  `registerActions`. Every teardown path is the same coupled,
  idempotent one, extended to the pair; only a resize is exempt.

---

## The expedition loop: the unprepared control

Enforced by `tools/expedition_loop_probe.py` (manual-only, fixed-seed,
~15 min, two engine boots). `docs/expedition_gameplay_loop.md` is the
design authority for the arc; CLAUDE.md states that the control exists and
must end measurably worse off. This enumerates the six conditions that
keep the comparison honest — weakening any one turns the control into
theatre — and the traps found while building it.

1. **`find_water` retired and `forage_max_fraction` disabled** for the
   session. #94's emergency foraging ladder has its own gate,
   `foraging_probe.py`.
2. **BOTH travellers shed to inside carrying capacity first.** An
   over-encumbered acolyte crawls, its order stall-times-out, and it never
   arrives (`docs/expedition_survival_calibration.md` E1).
3. **The control gets NO retrieval target of its own** — a ruin can roll
   food, and a control that eats what it finds destroys the measurement.
4. **The travel VERB matches.** `commandMove` walks at
   `movement_speed.ordered` = comfort × 1.15, while `pickup_ground` walks
   at comfort, so the retrieval order is issued only after the
   measurement.
5. **The ORIGINS are equalised as a PLACE, not merely a distance.** Hunger
   drains with time on the road and route shape is time; a radial band is
   satisfied anywhere on a circle, so the check asserts separation as well
   as distance spread, verified with the simulation STOPPED.
6. **The observation point is both travellers at the ruin in ONE COHERENT
   SNAPSHOT** — a single paired read revalidated with the simulation
   stopped. Two separate `unit.getInfo` round trips let the sim run in
   between, and a pair that was never inside together can satisfy them; a
   unit that finishes its move reverts to wander and can drift back out
   while the other is still walking.

**Canteens stay full on both.** A dry one puts `refill_canteen` at its 7.5
peak, above `follow_command`, and the control then abandons the leg to
walk to the water the scout radioed about — a behavioural difference, not
the supply being measured. The gated metric is FOOD (stomach fraction),
matching what the calibration measured actually goes live on a trip this
length; water is reported as evidence, not gated. The eating itself is
watched live as a real `eat_from_inventory` action, so the delta is
attributed to a mechanism rather than inferred from a number two
differently-massed acolytes could reach by other routes.

**Don't "fix" that by seeding a thirst deficit.** `scripts/salts.lua`
derives blood salt concentration as saltFrac/hydrationFrac and
`scripts/brain.lua` folds it straight into consciousness, so a unit
dehydrated far enough to prefer drinking over its orders is knocked
unconscious by the electrolyte imbalance — and scaling the `salt` pool
down to compensate just moves the blackout to the first meal's salt bolus
(`salts.mealSalt` restores 0.30 of max_salt per feed). Both were observed
live while building the gate.

**Two instrument gotchas.** A completed move order does NOT hold position
(E3). And **`unit.setFrozen` is not a hold at all**: `uiFrozen` only makes
`publishToRender` skip the sim-derived update, so a "frozen" unit keeps
walking while `unit.getInfo` reports where it was when the flag went up.
Use `engine.setPaused` when you need a unit to actually stay put, and
re-read positions after pausing.

---

## Autosave: staging, rotation order, and the intent mutex

Enforced by `tools/autosave_probe.py` (manual-only). Slots are the
reserved `autosave-<n>` family, `autosave-1` newest; ownership is the
durable `smAutosave` metadata flag (`"metadata"` v2; v1 migrates to manual),
NEVER the name — a manual save squatting on one of those names fails
the attempt with nothing rotated. PUBLISH FIRST, ROTATE SECOND: every
autosave writes to the reserved `autosave-incoming` staging slot and
the family ages down only once that transaction succeeds; a staged
generation left by a crash is rotated in next cycle. The rotation is
ordered the same way — the oldest is RETIRED by rename and deleted only
once every other move succeeded — so an interrupted rotation leaves a
partially shifted family, never a shorter one, and the shift plan is
DERIVED from what's on disk. A SUCCESSFUL autosave restores the
pre-request pause + visible time scale only if `playerIntentGenRef`
still matches — an `MVar` doubling as the mutex, so the comparison and
the writes are one critical section: any `engine.setPaused` /
`world.setTimeScale` during the window means the player wins. A FAILED
one stays paused and zero-scaled. Gate: `autosave_probe.py`
(manual-only).

---

## Save/load transaction: phases and failure semantics

CLAUDE.md carries the four architectural bullets (the Lua save-module
registry, `publishGeneration`'s write-fsync-revalidate-rotate transaction,
the whole-session load transaction, and the typed-reference integrity
graph). This is the phase and failure detail it defers.

**`engine.getLoadStatus()` exposes a 12-phase lifecycle plus a 13th
terminal phase, `LoadReconciliationFailed` (#1204):** publication
SUCCEEDED but a Lua `onSaveLoaded` callback raised, so the live session is
incompletely reconciled.

It is a THIRD terminal disposition, not a flavour of either existing one.
Every poller must treat it as terminal (its outcome is non-nil, so
`loadInProgress` is already false) AND as UNSUCCESSFUL. It deliberately
leaves `failedAtPhase` unset, because that field's presence promises the
old session survived unchanged — which a post-publish failure cannot. The
outcome aggregates every failing module, and `reconciliationFailures`
carries the per-module `{module, error}` breakdown. Callback isolation is
unchanged: the broadcast still attempts every module.

**Storage failures name their `StoragePhase`** through
`engine.getSaveStatus()`. A corrupt authoritative file falls back to
`.prev` and says so loudly (`recovered` in `engine.listSaves()`); an
INCOMPATIBLE one reports directly with no fallback. Symlinked slot
dirs/files are refused.

---

## Enum append-only audit: baseline and payload normalization

Enforced by `tools/enum_append_only_audit.py` (CI + `make ci`, with its
own `--self-test`). CLAUDE.md states the rule and the two hard facts about
the baseline (it is GENERATED; a pure append ratchets it with
`--update-baseline`). This is the rest.

**Coverage.** Of the 43 guarded types, 38 are on the save wire and 28 are
named by a live component today; the rest are guarded pre-emptively, which
is the point of keying on the `Serialize`-via-`Generic` instance rather
than on save reachability. Don't hand-count these: the audit prints the
guarded total on every run, and `docs/save_compat/enum_baseline.json`'s
per-type `onSaveWire` / `components` fields are the other two.

**What the baseline records.** Module-qualified constructor lists, each
constructor recording its name and its ordered PAYLOAD signature, plus the
save-wire attribution captured alongside.

**How a payload slot is normalized.** A slot is the field's declared type
with strictness markers, `{-# UNPACK #-}`, layout, `::`/`∷` and the
parentheses a `!` forces all erased. Field order and type structure are
NOT erased. For a record alternative the selector is kept — which is what
makes swapping two same-typed record fields visible, and means a selector
rename reports too.

**Diagnostics.** An incompatible change's output names every component and
historical shape that carries the type, with the reachability path. That
holds even for a type that was renamed or deleted, read back from the
recorded attribution because there is nothing left in the tree to walk.

**Boundary against `tools/save_compat_audit.py`.** Since #1270 this audit
is the one exhaustive gate owning payload drift INSIDE a
multi-constructor sum. Single-constructor record field order stays the
frozen-DTO boundary's and `save_compat_audit.py`'s.

---

## CLI value validation (#1191)

Enforced by hspec `--match "App.Cli"` and `tools/preview_cli_probe.py`
(no boot). CLAUDE.md states the rule, the flags it covers, and
`--region`'s exclusion. This is the rest.

**Empty selections and empty segments** are errors too, not just unknown
layers: `--dump=` and `--dump=terrain,` each exit 1 naming the flag and
the offending token.

**Ordering.** Validation runs AFTER the mode-compatibility rejection,
which keeps its priority — a malformed `--seed` given to `--headless`
still reports as unsupported in headless mode, not as a bad number. It
runs BEFORE every mode-specific early exit, regardless of whether the
selected mode would ever consume the value.

**`--region`'s exclusion** is deliberate and tracked: its identical silent
default is `docs/code_health_findings.md` CH-67, sequenced after #1081.

---

## Findings-report lane split: why it matters

Enforced by `tools/findings_report_audit.py` (CI + `make ci`). The
ownership rule — the processing lane owns all three status markers, an
implementation PR owns only the narrative body — stays in CLAUDE.md.

That split is not stylistic. The two lanes had already drifted an entry
in each direction, and each drift re-files merged work: the processor
selects a bare-headed finding as unprocessed, and the "headings win,
correct the checklist" tie-break then unchecks a finding an issue already
resolved. The cost lands on other people's PRs too —
`.github/workflows/review-gate.yml` strips `reviewed:approve` when a push
touches a file an open PR also owns, so every master-side report edit
costs an open PR its approval.

---

## Docs landing: docs-wip, autostash, and the protected-ref warning

The rule — the primary checkout stays CLEAN, uncommitted work lives in
the docs worktree, land with `tools/docs_land.sh` — stays in CLAUDE.md.

`--autostash` is required there, not decorative. Should ITS restore
conflict, the damage is confined to this worktree and surfaces
immediately in front of you — it cannot wedge the drainer, which is the
whole point of doing the work here.

**`docs-wip` is not a feature branch.** It tracks `origin/master` and
lands by direct push, so it is a second working copy of master rather
than something that accumulates and merges later. Uncommitted work can
sit in it indefinitely without the drainer ever seeing it; that is its
job. A bare `git push` from it fails safe (`push.default=simple` refuses
the differing name) — use the explicit refspec above. That push prints
`Cannot update this protected ref` and `N of N required status checks are
expected` and then **succeeds anyway** under admin bypass — judge it by
`git rev-list --left-right --count HEAD...origin/master`, not the warning.
