# Texture sampler routing design

The engine ships a player-facing nearest/linear texture-filter setting. This arc
routes every texture surface to the sampler it should have: gameplay unit
atlases follow the player through isolated cells, UI chrome and icons stay
pinned nearest, and derived world surfaces keep their explicit samplers.

Design state: `ready for issue processing`

Status legend: `[ ]` unprocessed · `[#N]` linked to issue N · `[no-issue]`
reviewed and deliberately not tracked separately · `[deferred]` blocked on a
concrete precondition

## Processing status

- [x] EPIC. Route every texture surface to its correct sampler — [#2072]
- [x] TSR-1. Pin UI surfaces to nearest independently of the player filter — [#2075]
- [x] TSR-2. Add cell extrusion padding to the unit atlas compiler and runtime — [#2076]
- [x] TSR-3. Route gameplay unit atlases through the player-selected sampler — [#2085]

## Epic contract

- **Goal:** The player's nearest/linear setting governs exactly the scene art it
  is meant to govern, and every surface that must not follow it is pinned
  deliberately and recorded as such.
- **Done when:** Selecting linear changes tiles, units, buildings and flora and
  nothing else; unit animation frames show no cross-cell bleed under either
  filter; UI chrome, icons and text stay pixel-crisp under both; and the
  steering documents describe the shipped behavior.
- **Users and operators:** Players using the video settings; maintainers of the
  atlas compiler, the bindless upload path, and the asset budgets.
- **Arc label:** `bug` plus `hotfix` on the two player-visible slices; no new
  arc label proposed.

## Current state and evidence

The completed routing separates scene art from pinned UI and derived surfaces:

| Family | Sampler today | Follows the player? |
|---|---|---|
| Unit-animation atlases | shared global | Yes |
| World preview thumbnail | pinned nearest | No |
| Zoom (world map) atlas | pinned linear | No |
| Font atlases | own fixed `SamplerFont` | No |
| UI chrome, icons, tiles, buildings, flora, blood | shared global sampler | Yes |

- `src/Engine/Scripting/Lua/Message/Texture.hs` — `handleLoadAtlasTextureBatch`
  selects `UploadGlobalSampler` for every compiled gameplay unit atlas.
- `src/Engine/Scripting/Lua/Message/WorldTexture.hs:133,280` — the world preview
  pins nearest and the zoom atlas pins linear.
- `src/Engine/Graphics/Vulkan/Texture/Rebind.hs:74` — `planFilterRebind` keeps
  every `btsPinned` handle on its own sampler across a toggle, so the pins are
  permanent rather than until-next-regen.
- `src/Engine/Graphics/Font/Upload.hs:130` — font atlases acquire `SamplerFont`,
  outside the global/pinned split entirely.
- `scripts/settings/data.lua:477` — the setting is player-reachable and applied
  live through `engine.setTextureFilter`.

The bleed risk was real rather than theoretical, and TSR-2 (#2076) has now
removed it. `src/Engine/Loop/Camera.hs:252` integrates zoom as a float with
velocity and friction, so scene art is sampled at arbitrary non-integer scale;
pixel snap only removes a fractional *position* offset
(`src/Engine/Graphics/Vulkan/ShaderCode.hs:105`) and does not make one texel
equal one screen pixel. `pack_atlas.py` used to pack cells edge to edge at exact
integer boundaries, so a linear sample near a cell edge would read the
neighbouring animation frame.

**As of TSR-2 it no longer can.** Every cell now occupies a
`(cell_width + 2) x (cell_height + 2)` slot with the logical frame at offset
`(1, 1)` and a one-texel gutter copying that cell's own edge texels outward,
corners included (`pack_atlas.py`'s `extruded_slot`; index `schema_version` 2's
required `cell_padding`). `atlasCellUV` strides by the slot and still addresses
the inner cell exactly, so nearest-mode output is unchanged texel for texel and
D-3's precondition is satisfied. TSR-3 routes those isolated atlases through
`UploadGlobalSampler`, so the table above now describes the shipped sampler
routing.

The divergence has a traceable cause. Epic #1256 states that gameplay atlases
"continue to honor the user-selectable global nearest/linear texture-filter
setting" and that this "corrects the nearest-only wording in
`docs/texture_infrastructure.md` D-6". Child #1259, which owned runtime
sampling, restated the *pre-epic* D-6 in its own body and requirement 7, so the
correction never reached the child that would have implemented it. The epic
closed with its acceptance criteria 5 and 12 unmet.

Two comments described behavior that no longer existed:
`src/Engine/Scripting/Lua/Message/WorldTexture.hs:125` and `:274` both claim a
live filter toggle repaints those slots "to the global sampler until the next
regen", which predates `btsPinned` being honored in the rebind plan. TSR-3
corrects both comments to state that each derived slot retains its pin.

## Desired experience

A player who selects linear sees smoothing on the world — terrain, units,
buildings, flora — and a HUD that stays exactly as crisp as it was. A player who
selects nearest sees today's presentation, pixel-identical to what ships now.
Neither choice can make one animation frame bleed into its neighbour, at any
zoom.

## Scope

### In scope

- The sampler policy for UI surfaces, scene art, and the derived surfaces.
- Cell isolation in the compiled unit atlases sufficient for linear sampling.
- Reconciling D-6, `docs/engine_contracts.md`, and CLAUDE.md with the result.

### Out of scope

- Mipmaps for unit art; the arc keeps one mip level.
- KTX2 / TEX-5, which stays deferred on its own recorded trigger.
- Atlasing non-unit texture families.
- Changing which logical frame `pickFrame` selects (D-3 arithmetic is frozen).
- Re-authoring artwork.

## Design

**Three categories, and every surface belongs to exactly one.**

1. **Scene art** — tiles, units, buildings, flora, blood. Follows the player's
   setting. This is the only category the toggle is for, and the reason it
   exists is that continuous zoom makes fractional sampling unavoidable.
2. **UI** — chrome, icons, and text. Pinned nearest, never player-controlled.
   Pixel-perfect UI is a correctness property, not a taste setting.
3. **Derived surfaces** — the zoom atlas and the world preview thumbnail. Each
   keeps its own fixed sampler because it is a resampled representation rather
   than authored art: linear for the downscaled map, nearest for the thumbnail.

**Isolation before unpinning.** Unpinning unit atlases without isolation is the
one thing this arc must not do. Extrusion padding — pad each cell and duplicate
its edge texels into the padding — is the standard fix and is the only candidate
that preserves the frozen contracts: UVs keep addressing the inner true cell, so
under nearest a fragment centre maps to the same source texel it does today and
pixel-identity with the retired per-frame path holds. A half-texel UV inset is
rejected for exactly the reason `docs/engine_contracts.md:592` already records:
it shifts the sampled texels and breaks that identity. Per-cell array textures
are rejected as a change to the resident representation that D-2/D-10 fix at one
image per animation.

Extrusion is a compiler-side change that propagates: cell stride in
`pack_atlas.py`, the generated index, both per-animation `sha256` digests, all
131 tracked atlases across 8 units, `atlasCellUV`'s arithmetic, and the resident
memory measured against the owner-confirmed 384 MiB threshold.

**Preview's forced-nearest requirement is satisfied by the unpin, not despite
it.** `scripts/preview_manager.lua:738` already sets the global filter to
nearest for the session, live-only and without touching the player's saved
setting. Today that call is redundant for unit art, because the atlases ignore
the global sampler anyway. Once TSR-3 routes them through it, that same existing
call is what keeps the preview browser nearest — so epic #1256's "preview
retains its explicit forced-nearest behavior" needs no preview-specific code,
and TSR-3 must not add any.

**Classification is the UI half's real problem.** UI textures load through the
same ordinary Lua `engine.loadTexture` path as world textures, so nothing today
distinguishes them, and a directory rule does not work: `icons/location/*.png`
are drawn on the zoom map in the world (`src/Location/Types.hs:195`,
`data/locations/ruin_small.yaml:63`) while the rest of `icons/` is toolbar
chrome, and `utility/white.png` is used by both. The policy-aware path cache
already re-uploads a texture into its own slot when the same path is requested
under a different policy (`cacheEntryReusable`), so a dual-use texture costs a
duplicate slot rather than breaking. D-4 settles this by making the caller
declare intent rather than inferring it from the path.

## Decisions

### D-1. Keep the player-facing texture-filter setting

The setting stays in video settings and stays live-applied. A setting that
applies to only some surfaces is worse than either extreme, so the arc fixes the
routing rather than removing the option.

### D-2. Route surfaces by the three-category model

Scene art follows the player; UI is pinned nearest; derived surfaces keep their
own fixed sampler. This inverts today's behavior for both units and UI and
leaves the zoom atlas and preview thumbnail as they are.

### D-3. Unit atlases gain real cell isolation before they are unpinned

Extrusion padding, or a proven equivalent, lands first and the unpin depends on
it. The half-texel UV inset is rejected because it breaks nearest-mode
pixel-identity. Unpinning sheets whose cells are not isolated is not an
acceptable intermediate state, so TSR-3 may not land before TSR-2. TSR-2 (#2076)
has landed the one-texel gutter, so this precondition is met and TSR-3 is free
to proceed.

### D-4. The caller declares a load's upload policy

`engine.loadTexture` gains an explicit policy argument; UI and HUD load sites
pass it, and everything that omits it stays scene art following the player.
Classification is never inferred from a path, because no directory rule survives
`icons/location/*` being world-drawn while the rest of `icons/` is chrome, or
`utility/white.png` being used by both. The accepted cost is that a genuinely
dual-use path is uploaded under both policies and occupies two bindless slots;
`cacheEntryReusable` already implements exactly that, so it is existing
behavior rather than new machinery. Intent stays visible at the call site, and a
new asset directory cannot silently inherit a wrong default.

### D-5. Extrude one texel per side and re-baseline the budget in TSR-2

One texel is exactly sufficient for bilinear filtering at the single mip level
this arc keeps; two texels would only buy headroom for mipmaps, which are
explicitly out of scope. Growth is roughly +13% pixels on a 32px cell and +6% on
a 64px cell. TSR-2 re-measures the resident total against the owner-confirmed
384 MiB threshold and records the new baseline as part of the slice, rather than
treating the movement as a breach. Measured on the shipped corpus (#2076): the
whole roster moved from 115,723,968 to 121,620,320 bytes, +5.10%, projecting to
231.97 MiB against the unchanged 384 MiB threshold.

### D-6. Headless proofs block; GPU confirmation is recorded, not blocking

The blocking gate is headless and runs in CI: per-category sampler selection,
a texel-level proof that a cell's linear footprint never reaches a neighbouring
cell's source texels, an unchanged `pickFrame` matrix, and nearest-mode output
pixel-identical to today. GPU confirmation under both filters is maintainer-run
through `preview_probe.py` or `--offscreen` and recorded in the PR. This keeps
the merge path automated given that no GPU CI runner exists and the only
graphics-capable machine is the owner's laptop, while still putting real sampled
pixels in front of a human before the change reaches players.

### D-7. Font atlases stay outside this arc

UI text is already immune to the filter setting by construction, not by policy:
`uploadFontAtlasToGPU` allocates its own descriptor set
(`src/Engine/Graphics/Font/Upload.hs:141`) and never calls `registerTexture` or
`registerPinnedTexture`, so `setTextureFilter` cannot reach a font atlas at all.
Folding fonts into the UI upload policy would require first moving them into the
bindless system — an architectural change with no user-visible effect, since the
outcome is already correct. No slice touches font rendering.

## Open questions

### Q-1. How is a texture classified as UI rather than scene art?

Resolved by D-4.

### Q-2. What extrusion width, and what atlas growth is acceptable?

Resolved by D-5.

### Q-3. What is the blocking verification gate, given there is no GPU CI runner?

Resolved by D-6.

### Q-4. Do font atlases join the UI policy or keep `SamplerFont`?

Resolved by D-7. Font atlases do not merely use a
different sampler — they never enter the bindless system at all: `uploadFontAtlasToGPU`
allocates its own descriptor set (`src/Engine/Graphics/Font/Upload.hs:141`) and
calls neither `registerTexture` nor `registerPinnedTexture`. `setTextureFilter`
therefore cannot reach them by construction, so UI text is already immune
architecturally rather than by policy. Folding fonts into the UI policy would
mean first moving font atlases into the bindless system, which is a separate
architectural change with no bearing on this arc's goal.

## Verification strategy

- Pure headless tests are the arc's blocking gate: sampler-policy selection per
  category, and texel-level proof that a cell's sampled footprint under linear
  never reaches a neighbouring cell's source texels.
- The former hspec group asserting an atlas slot stayed nearest across a filter
  toggle is inverted by TSR-3, not deleted: it now proves both global sampler
  values reach the atlas while a derived preview slot remains pinned.
- `tools/pack_atlas.py --validate-only --strict` must stay green with the new
  cell geometry, including the artifact-count and digest checks.
- `pickFrame`'s logical-frame matrix must be unchanged, proving TSR-2 moved
  geometry without moving frame selection.
- Nearest-mode output must be pixel-identical to today; that is the regression
  most likely to be missed.
- GPU confirmation of both filters is maintainer-run and recorded in the PR
  rather than blocking, per D-6.

## Delivery plan

### TSR-1. Pin UI surfaces to nearest independently of the player filter

- **Outcome:** Selecting linear no longer changes UI chrome or icons.
- **Scope:** An explicit upload-policy argument on `engine.loadTexture`, and the
  UI/HUD load sites passing it.
- **Phase:** 1
- **Depends on:** `none`
- **Ordering:** independent, can land first
- **Relevant decisions:** D-2, D-4
- **Acceptance signals:** A filter toggle leaves UI slots on nearest; scene
  textures still repaint; a path loaded under both policies resolves to two
  slots with each keeping its own sampler; omitting the argument still yields
  scene-art behavior.
- **Out of scope:** Unit atlases, extrusion, font atlases.
- **Open questions:** `None`

### TSR-2. Add cell extrusion padding to the unit atlas compiler and runtime

- **Status:** DELIVERED as [#2076]. One texel per side, index
  `schema_version` 2 with a required `cell_padding`, `source_digest` domain tag
  at `v2`, all 131 tracked atlases regenerated, nearest-mode output verified
  pixel-identical, and the resident baseline re-recorded at 121,620,320 bytes
  against the unchanged threshold. TSR-3's precondition is met.
- **Outcome:** Compiled atlases carry isolated cells; nothing about sampling
  policy changes yet.
- **Scope:** `pack_atlas.py` layout, index format version, both per-animation
  digests, regeneration of all tracked atlases, `atlasCellUV` stride, budget
  re-measurement.
- **Phase:** 2
- **Depends on:** `none`
- **Ordering:** critical path
- **Relevant decisions:** D-3, D-5
- **Acceptance signals:** Strict validation green; digests regenerate
  deterministically; `pickFrame` selection unchanged; nearest-mode rendering
  pixel-identical to before; resident memory re-measured and the new baseline
  recorded.
- **Out of scope:** Any change to which sampler an atlas uses.
- **Open questions:** `None`

### TSR-3. Route gameplay unit atlases through the player-selected sampler

- **Outcome:** Unit animations follow the player's filter with no cross-cell
  bleed; preview keeps its explicit forced-nearest path.
- **Scope:** Drop the unconditional `UploadPinnedNearest`; invert the filter-toggle
  regression test; add both-mode coverage; reconcile D-6,
  `docs/engine_contracts.md` and CLAUDE.md; correct the two stale
  `WorldTexture.hs` comments.
- **Phase:** 3
- **Depends on:** TSR-2
- **Ordering:** critical path
- **Relevant decisions:** D-1, D-2, D-3, D-6
- **Acceptance signals:** Both global modes reach unit atlases; preview still
  forces nearest; the headless texel-level isolation proof passes under both
  filters; steering documents match the code; maintainer-run GPU confirmation
  recorded in the PR.
- **Out of scope:** Extrusion itself; UI classification.
- **Open questions:** `None`
