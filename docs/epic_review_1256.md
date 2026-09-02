# Epic Review Findings: Epic #1256 — Compile unit animations into validated runtime atlases

This report records the completed-arc review of epic #1256 at
`master@b4631ca6eca1`, against its reconciled six-child scope: body-declared
required children #1257, #1258, #1259, #1260, #1261, and #1262. GitHub's
native sub-issue set is empty; TEX-5 is explicitly deferred and non-blocking,
so it is not a missing child. The inventory, compiler, runtime representation,
full-roster migration, preview integration, tracked artifacts, and freshness
gates compose successfully at current HEAD, including the later expansion to
eight compiled unit trees and 131 animations. Two new current contract/gate
mistakes remain. The review's focused checks ran at `006721bf0590`; the five
later commits through the stated HEAD change only probe-failure retention and
do not touch the atlas implementation, tests, assets, or steering contracts.
The separate Cabal source-distribution omission already
recorded as HPA-8 in `docs/holistic_project_audit_findings.md` is not
duplicated here.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Status

- [x] ER-1. Gameplay unit atlases ignore the global texture-filter setting — [#2085]
- [x] ER-2. The image/slot budget counts atlas files, not runtime registrations — [#2217]

## 1. Sampling policy

### [#2085] ER-1. Gameplay unit atlases ignore the global texture-filter setting

> **Captured note:** Restore #1256's explicit sampling contract: gameplay
> unit atlases must follow the player-selected global nearest/linear filter,
> while preview may continue forcing nearest. Linear sampling needs real cell
> isolation rather than being avoided by permanently pinning gameplay atlases
> to nearest.

**Verification:** Confirmed. The current upload path always routes compiled
unit-animation atlases through `UploadPinnedNearest`, and the focused test
requires a global filter toggle to leave the atlas slot nearest. The compiler
packs cells edge-to-edge with exact cell-boundary UVs, so simply unpinning the
slot would introduce the cross-cell bleed the epic required this arc to solve.
Current steering docs repeat the pre-epic nearest-only D-6 wording even though
#1256 says its sampling section corrects that wording and takes precedence.

**Evidence:**

- `src/Engine/Graphics/Config.hs:79` — the supported player setting has both
  `FilterNearest` and `FilterLinear`, mapped to the corresponding Vulkan
  filters.
- `src/Engine/Scripting/Lua/API/YamlTextures.hs:199` — every compiled unit
  atlas request is documented and queued as permanently pinned nearest across
  a runtime `setTextureFilter` toggle.
- `src/Engine/Scripting/Lua/Message/Texture.hs:297` —
  `handleLoadAtlasTextureBatch` unconditionally selects
  `UploadPinnedNearest`, unlike ordinary gameplay textures' global sampler.
- `test-headless/Test/Headless/Unit/Atlas.hs:1199` — the current regression
  test enforces the opposite of the epic contract by requiring an atlas slot
  to remain nearest when the global filter changes.
- `tools/pack_atlas.py:124` — atlas cells are packed directly adjacent at
  exact integer boundaries, with no isolation gutter for bilinear sampling.
- `docs/texture_infrastructure.md:276` and
  `docs/engine_contracts.md:576` — current steering documentation canonizes
  pinned-nearest gameplay atlases rather than #1256's corrected global-filter
  behavior.

**Handoff context:**

- **Current behavior:** Selecting linear filtering changes ordinary gameplay
  textures but not unit-animation atlases; unit animations remain nearest in
  both gameplay and preview.
- **Expected behavior:** Gameplay unit atlases honor the global nearest/linear
  setting, preview retains its explicit forced-nearest behavior, and interior,
  edge, padding-adjacent, mirrored, and partially clipped cells remain
  isolated under both filters.
- **Scope and constraints:** Preserve one image/handle/slot per animation,
  no mipmaps, logical-frame choice, real per-direction counts, mirroring and
  flip-after-clip semantics, and preview/gameplay use of the same index path.
  Do not merely unpin the existing edge-adjacent sheets; add an isolation
  representation such as gutters or another proven equivalent first.
- **Verification target:** Extend the pure sampler-policy tests so gameplay
  atlases follow both global modes while preview stays nearest, and add
  GPU-capable/offscreen sampling evidence for every isolation case enumerated
  by #1256 under both filters.
- **Deduplication:** All-state tracker searches across titles, bodies, and
  comments for global nearest/linear atlas filtering, pinned-nearest unit
  atlases, sampling isolation, and global-filter toggles found only closed
  epic #1256 itself. The docs-worktree report corpus contains the implemented
  pinned-nearest contract but no pending finding that owns this mismatch.
- **Remaining uncertainty:** The exact isolation technique is an
  implementation decision; the current divergence from the epic's explicit
  player-setting contract is not uncertain.

## 2. Operational gate accuracy

### [#2217] ER-2. The image/slot budget counts atlas files, not runtime registrations

> **Captured note:** Make TEX-7's one-image/slot gate prove what it claims, or
> rename and re-scope it honestly. `pack_atlas.py` currently labels generated
> atlas-file counts as resident images and bindless registrations, so a loader
> regression that queues per-frame or duplicate uploads can leave the asset
> tree unchanged and pass this budget.

**Verification:** Confirmed. `validate_budget` reads each generated index and
counts non-index entries in the unit's `atlas/` directory; its resident-byte
number is likewise projected from indexed dimensions. It never observes the
loader, queued texture requests, bindless slots, or a running engine. The
current Haskell loader really does queue one atlas request per animation, and
the focused Hspec group independently proves that, so this is not a current
over-registration defect. It is a false gate boundary and false diagnostic:
the Python check cannot detect the runtime regression its policy document,
docstring, output, and self-test say it catches.

**Evidence:**

- `tools/unit_texture_budget.json:12` — the policy calls its measure
  "resident images (and bindless registrations)" and says its file-count rule
  automatically catches reintroduced per-frame registration.
- `tools/pack_atlas.py:2424` — the budget docstring derives a runtime
  image/slot conclusion from generated artifacts and states that a per-frame
  registration regression must put per-frame files into `atlas/`.
- `tools/pack_atlas.py:2468` — the implementation reads indexes, sums declared
  dimensions, and counts directory entries through line 2536; it has no
  runtime-registration input.
- `tools/test_pack_atlas.py:2083` — the self-test models a "per-frame
  registration" by writing ten extra PNG files into the generated atlas
  directory, so it proves artifact hygiene rather than loader behavior.
- `src/Engine/Scripting/Lua/API/Units/Yaml.hs:160` — the real runtime invariant
  is owned separately by the `atlasTextureRequests` loop, which currently
  queues one atlas upload per animation.
- `test-headless/Test/Headless/Unit/Atlas/Loader.hs:173` — the focused runtime
  gate genuinely asserts one queued upload, distinct handles, and no
  per-frame requests.

**Handoff context:**

- **Current behavior:** Strict validation accurately enforces one generated
  atlas file per indexed animation and projects decoded bytes from metadata,
  but reports those values as actual resident images and registrations and
  claims runtime-regression coverage it does not possess.
- **Expected behavior:** Either give runtime image/slot enforcement a named
  mechanical gate that observes the production registration boundary, while
  describing the Python check as generated-artifact and projected-memory
  validation, or make the budget itself consume trustworthy runtime/static
  registration evidence. Gate names, diagnostics, policy prose, and
  self-tests must describe the same measurement.
- **Scope and constraints:** Preserve the useful artifact-count equality,
  orphan/shared-atlas checks, owner-confirmed 384 MiB TEX-5 trigger, and the
  current Hspec loader coverage. Do not require a GPU merely to validate
  compiler output; the registration boundary is already headless-testable.
- **Verification target:** Add a non-vacuous mutation in which the production
  loader queues a duplicate or per-frame request while generated assets stay
  unchanged, and prove the specifically documented runtime gate fails. If the
  Python check is deliberately retained as artifact-only, update every
  resident/registration claim and keep the loader Hspec group as the named
  runtime authority.
- **Deduplication:** All-state tracker searches for atlas runtime-registration
  budgets, resident-image counts, bindless-registration counts, and
  reintroduced per-frame registration returned no owning issue. The
  docs-worktree report corpus has no pending finding for this measurement
  mismatch.
- **Remaining uncertainty:** None about what the current check reads; only the
  preferred ownership split between the existing Python and Hspec gates is a
  maintainer choice.
