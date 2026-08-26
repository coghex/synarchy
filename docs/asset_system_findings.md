# Asset-system findings

This report audits Synarchy’s tracked raster and font assets together with the Haskell/Lua paths that discover, load, cache, render, validate, and release them. It records verified current behavior without choosing tracker dispositions or implementation designs.

Status legend: `[ ]` unprocessed · `[#N]` filed as issue N · `[no-issue]`
reviewed and deliberately never to be filed · `[deferred]` blocked on a
concrete precondition

## Methodology

The audit covered all 5,616 tracked PNGs, the three bundled fonts, 95 data YAML files, tracked configuration YAML, direct texture references across `data/`, `scripts/`, `src/`, `app/`, and `config/`, and the principal texture upload, bindless registration, preview, flora, vegetation, and shutdown paths.

Every tracked PNG decoded successfully. A YAML walk found no malformed documents or missing direct asset references. `texture_subset_audit.py`, `location_map_icon_asset_check.py`, and all 131 `test_pack_atlas.py` cases passed. The focused `Asset.TextureFallback` suite passed 20 examples and `World.FloraGrowth` passed 31 examples.

`check_texture_paths.py` currently fails on one comment-only false positive. The strict atlas validator also fails in this checkout because an ignored, untracked `.DS_Store` exists under the acolyte animation tree; this local file is not treated as a durable repository finding.

No graphical engine or forced bindless-exhaustion run was performed. GPU-state findings below are established from the complete state-transition paths and their focused tests.

## Status

- [ ] ASSET-1. Failed bindless registrations are published as loaded textures
- [ ] ASSET-2. Ordinary texture GPU resources have no reachable release boundary
- [ ] ASSET-3. Stale preview completions create repeated handle aliases
- [ ] ASSET-4. The all-reference texture validator is red and absent from the gates
- [ ] ASSET-5. Three semantically distinct designation markers use identical art
- [ ] ASSET-6. Five four-variant vegetation families contain only one visual variant
- [ ] ASSET-7. Raspberry fruiting is harvestable but visually indistinguishable
- [ ] ASSET-8. Production crops and their yields still use unrelated placeholder art
- [ ] ASSET-9. The bundled shell font has no auditable redistribution provenance

---

## Runtime loading and lifetime

### ASSET-1. Failed bindless registrations are published as loaded textures

A texture that exhausts the bindless slot allocator is still entered into the asset pool as ready, cached by path, assigned dimensions, and announced to Lua as loaded. Its uploaded image has no bindless mapping, so consumers receive a success callback for a handle that cannot resolve to that image.

The cached-alias path has the same false-success behavior when its canonical atlas lacks a bindless slot.

**Evidence:**

- `src/Engine/Graphics/Vulkan/Texture/Bindless.hs:394-421` — slot exhaustion returns `Left TextureSlotsExhausted` with no slot, descriptor, handle mapping, or table write.
- `src/Engine/Scripting/Lua/Message/Texture.hs:362-424` — the result is converted only into an optional `taBindlessSlot`; both `Left` and `Right` then create an atlas, set `AssetReady`, cache the path and dimensions, and enqueue `LuaAssetLoaded`.
- `src/Engine/Scripting/Lua/Message/Texture.hs:124-158` — cached reuse logs a missing canonical slot but unconditionally performs the ready/refcount/size/callback writes afterward.
- `test-headless/Test/Headless/Graphics/BindlessRelease.hs:281-308` — the release tests explicitly model an atlas whose registration never obtained a slot and whose aliases nevertheless carry `AssetReady`.
- The focused `Asset.TextureFallback` suite proves registration reports exhaustion correctly, but does not exercise the enclosing publication transaction.

**Handoff context:**

- **Current behavior:** Lua can build UI or render state after receiving a texture-loaded callback even though the handle has no mapping to the uploaded image.
- **Expected direction:** Registration success must gate ready-state publication, path caching, aliasing, and the success callback. Failure must leave an observable failure state and dispose of resources that were prepared for the refused registration.
- **Scope and constraints:** Cover ordinary and pinned fresh uploads plus cached aliases. Preserve the reserved-handle guard and the existing successful-cache path. Add a test at the full publication boundary, not only the slot allocator.
- **Remaining uncertainty:** Real-device exhaustion was not forced. The incorrect transition after any returned `Left` is explicit in the current control flow.

### ASSET-2. Ordinary texture GPU resources have no reachable release boundary

Every file texture stores a cleanup closure for its image view, image, and device memory, but the only functions that execute those closures have no production callers. Normal shutdown cleans transient preview/zoom textures and standard Vulkan cleanup registrations, but does not drain the asset pool.

Process exit will eventually reclaim driver resources, but session-long texture tables, bindless slots, atlas entries, aliases, and manually managed Vulkan objects are never deliberately released.

**Evidence:**

- `src/Engine/Asset/Manager.hs:11-25` — the module documents that `unloadAsset` and `cleanupAssetManager` are the only closure runners and that nothing currently calls them.
- `src/Engine/Scripting/Lua/Message/Texture.hs:384-400` — every uploaded file texture stores `taCleanup = Just (cleanView >> tupCleanImage prep)`.
- `src/Engine/Asset/Manager.hs:118-176` — `unloadAsset` contains the complete bindless invalidation, bookkeeping purge, and final cleanup path.
- `src/Engine/Asset/Manager.hs:182-210` — `cleanupAssetManager` is the complete pool drain.
- `src/Engine/Loop/Shutdown.hs:70-93` — shutdown destroys transient textures, registered Vulkan resources, and the sampler cache without calling the asset-manager drain.
- A repository-wide production call search finds no references beyond the two definitions. The same ownership gap is recorded in `docs/engineenv_capability_inventory.md` and `docs/project_review_direct_84f6fa2-1122797.md`.

**Handoff context:**

- **Current behavior:** Loaded file textures and their handle bookkeeping live until process termination, including resources belonging to UI or preview surfaces that have already been destroyed.
- **Expected direction:** Establish and wire an explicit lifetime boundary: at minimum an orderly shutdown drain, plus owner-level release wherever a surface is expected to unload assets during a session.
- **Scope and constraints:** Destruction must occur on the render owner while the device and bindless system still exist, after GPU use has idled. Font and transient-texture ownership use different mechanisms and should not be conflated.
- **Remaining uncertainty:** Peak VRAM and table growth were not measured during a long session.

### ASSET-3. Stale preview completions create repeated handle aliases

The simple asset preview caches a path only when its callback still belongs to the current selection. Selecting A and then B before A finishes causes A’s successful completion to be ignored rather than cached. Revisiting A calls `engine.loadTexture` again.

The engine avoids a second GPU upload, but it allocates another handle alias, increments the atlas refcount, and adds more handle-indexed bookkeeping. Repeating the race grows those structures for the preview session.

**Evidence:**

- `scripts/preview_manager.lua:96-110` — the preview has a completed-path cache but only one current pending handle/path pair.
- `scripts/preview_manager.lua:135-147` — every cache miss allocates a new texture handle; an in-flight request is not recorded by path.
- `scripts/preview_manager.lua:473-502` — only the callback matching `pendingHandle` is applied and cached; stale successful completions are discarded.
- `src/Engine/Scripting/Lua/API/Graphics.hs:41-58` — every `engine.loadTexture` call allocates a fresh handle.
- `src/Engine/Scripting/Lua/Message/Texture.hs:113-158` — a same-path cache hit maps the fresh alias, increments `taRefCount`, records dimensions, and emits another loaded callback.
- `scripts/preview_manager.lua:529-563` — shutdown forgets the Lua cache but cannot unload any accumulated aliases.
- `docs/project_review_909-874.md:69-95` independently records the same current race.

**Handoff context:**

- **Current behavior:** Stale images are correctly prevented from replacing the current sprite, but their successful loads are forgotten. Revisits accumulate aliases around one shared atlas.
- **Expected direction:** Track in-flight and completed identity per path. Any successful completion should make its path reusable, while only the current selection may change the displayed sprite.
- **Scope and constraints:** Limit the behavioral repair to simple list/item preview. Unit and building preview modes cache handles immediately and use a different readiness model.
- **Remaining uncertainty:** The timing window was not reproduced through the graphical preview, but the callback sequence is deterministic when two requests remain pending together.

## Validation coverage

### ASSET-4. The all-reference texture validator is red and absent from the gates

The tool intended to validate every texture reference currently reports a missing path that exists only inside a Haddock counterexample. It scans raw source lines with a regular expression and cannot distinguish comments from executable strings.

More importantly, neither CI nor `make ci` invokes it. The passing subset audit explicitly does not replace this cross-family check, so a real typo in an ordinary Lua or Haskell texture path can still merge.

**Evidence:**

- `tools/check_texture_paths.py:1-14` — the tool describes itself as the guard over every texture reference.
- `tools/check_texture_paths.py:18-49` — it applies one regular expression to raw lines across the source trees.
- `src/Engine/Preview/Discovery.hs:305-308` — `assets/textures/iconsEvil/x.png` is documentation demonstrating a path-prefix boundary, not a runtime reference.
- Running the tool scanned 5,225 unique references and failed only on that comment.
- `.github/workflows/ci.yml`, `tools/ci-local.sh`, the Makefile, and Cabal contain no invocation of `check_texture_paths.py`.
- `tools/texture_subset_audit.py:27-35` explicitly limits its scope rather than replacing the all-reference validator.
- `src/Engine/Scripting/Lua/Message/Texture.hs:163-169` — a missing direct path reaches image decoding and throws `TextureLoadFailed`.
- `docs/project_review_432-412.md:25-34` independently records the same gate gap.

**Handoff context:**

- **Current behavior:** The broad validator is unusable as a clean gate and is not run automatically; narrower checks pass while ordinary source literals remain unguarded.
- **Expected direction:** Make the reference extraction comment-aware or provide narrow, tested suppression syntax, make the tool’s own fixtures clean, and then add it symmetrically to local and CI gate definitions.
- **Scope and constraints:** Preserve directory/base-path references and concatenation-prefix coverage. Gate wiring must maintain `ci_parity_audit.py` parity.
- **Remaining uncertainty:** No real missing tracked path was found in the current tree; today’s sole reported failure is false.

## Semantic asset correctness

### ASSET-5. Three semantically distinct designation markers use identical art

Mining, planned structures, and planned buildings are assigned separate paths and handles, and the HUD comments promise that each category reads differently. All three files are byte-identical 96×64 red diamonds.

This removes the intended at-a-glance distinction and also uploads three separately named copies of the same pixels.

**Evidence:**

- `scripts/hud.lua:139-145` — the comments require mining to differ from cursor selection and structures to differ from buildings.
- `scripts/hud.lua:400-409` — the three handles are wired into three separate world/construction texture roles.
- `assets/textures/ui/hud/utility/mine_designate.png`, `construct_designate_structure.png`, and `construct_designate_building.png` all have SHA-256 `ad3ca506046f8337ab3c4bac2e0e839812a057603bdf73a664fe728644488362`.
- Commit `6316eafe` introduced the construction images with an explicit note that they were mine-marker placeholders pending dedicated art; they remain unchanged apart from their later directory move.

**Handoff context:**

- **Current behavior:** All three designation types paint the same marker despite distinct runtime roles.
- **Expected direction:** Supply visually distinct category art and retain the separate handles already present in the interaction model.
- **Scope and constraints:** This can remain an asset-only change. A focused asset assertion should prevent the three semantic markers from becoming byte-identical again.
- **Remaining uncertainty:** None at draft time.

### ASSET-6. Five four-variant vegetation families contain only one visual variant

The engine selects one of four vegetation IDs per tile and loads every declared path separately. For desert scrub, fallen leaves, heavy ivy, lichen, and snow, all four files within each family are byte-identical.

Those five families therefore consume 20 IDs, paths, handles, uploads, and bindless slots while producing only five visible designs. Their deterministic variant selection has no visual effect.

**Evidence:**

- `src/World/Vegetation.hs:113-115` — every vegetation family is modeled as four variants.
- `src/World/Vegetation.hs:173-175` — each tile deterministically selects variant 0–3.
- `src/World/Vegetation.hs:227-241` — snow, lichen, and desert scrub add that variant to their base IDs; the remaining families use the same selection path.
- `src/Engine/Scripting/Lua/API/YamlTextures.hs:155-170` — every declared variant path receives its own texture registration.
- `data/vegetation/ground_cover.yaml:2-18`, `data/vegetation/mosses.yaml:29-45`, and `data/vegetation/snow.yaml:2-9` each declare four distinct files for the affected families.
- SHA-256 comparison found one hash per family across all four `frame_000.png` through `frame_003.png` files.

**Handoff context:**

- **Current behavior:** The world stores and selects four IDs, but every selected variant renders identically for these five vegetation types.
- **Expected direction:** Provide genuinely distinct variant art, or deliberately revise the fixed-four-variant data/runtime contract instead of representing duplicates as variation.
- **Scope and constraints:** An art-only repair preserves IDs and save/worldgen structure. Because vegetation colors feed zoom presentation, validation should include close-up and zoom rendering.
- **Remaining uncertainty:** Whether the duplicates were deliberate temporary copies is not documented; their lack of variation is certain.

### ASSET-7. Raspberry fruiting is harvestable but visually indistinguishable

Red raspberry harvesting is deliberately restricted to the annual fruiting window, and the renderer deliberately switches to `matured_fruiting.png` for that stage. That PNG is byte-identical to ordinary `matured.png`, so a harvestable bush has no visible fruit cue.

The focused tests prove the gameplay window opens and closes, but do not validate the visual state corresponding to it.

**Evidence:**

- `data/flora/temperate_shrubs.yaml:72-104` — red raspberry declares distinct mature and fruiting textures, with fruiting beginning on day 180.
- `data/flora/temperate_shrubs.yaml:130-140` — it is the reference fruit forage and yields wild berries.
- `src/World/Flora/Render.hs:59-73` — annual-stage texture selection overrides the mature phase during fruiting.
- `src/World/Flora/Growth.hs:171-191` — species with a fruiting stage yield only while that stage is active.
- `assets/textures/flora/red_raspberry/matured.png` and `matured_fruiting.png` are both 32×32 and share SHA-256 `e3caa33355fdb4cb9c0881952870c8486898a46829a3552a6bfa58f78e8f165d`.
- The focused `World.FloraGrowth` suite passed its mature-in-season, out-of-season, and senescing-window assertions.

**Handoff context:**

- **Current behavior:** The plant’s interaction state changes, but the pixels do not tell the player when berries are available.
- **Expected direction:** Give the fruiting stage visibly fruit-bearing art and preserve the current renderer and harvest-window semantics.
- **Scope and constraints:** The same texture set is reused by the placeholder tomato plant, so replacement sequencing should account for ASSET-8.
- **Remaining uncertainty:** None at draft time.

### ASSET-8. Production crops and their yields still use unrelated placeholder art

The implemented farming content loads tomato plants as red raspberry bushes and wheat as white clover. Harvested tomatoes render as wild berries, while wheat grain renders as a quinoa sack.

The YAML explicitly identifies all four substitutions as placeholders, but these definitions are included in normal startup and participate in the real flora, item, rendering, and harvest pipelines.

**Evidence:**

- `data/flora/crops.yaml:1-18` — both crops are documented as placeholder art; tomato uses the red raspberry texture directory.
- `data/flora/crops.yaml:100-120` — wheat uses the white clover directory while being rendered through the real crop-plot pipeline.
- `data/items/tomato.yaml:4-6` — the Tomato item uses `wild_berries.png`.
- `data/items/wheat_grain.yaml:6-8` — Wheat Grain uses `quinoa_sack.png`.
- `scripts/startup_loader.lua:188-202` — normal startup loads all flora and recursively loads all item definitions.
- `scripts/flora_catalog.lua:8-10` — the flora catalog registers every file under `data/flora`.

**Handoff context:**

- **Current behavior:** Mechanically distinct crops and inventory items are visually represented as unrelated wild plants or foods.
- **Expected direction:** Add dedicated crop lifecycle art and dedicated yield sprites, then point the existing definitions at those assets.
- **Scope and constraints:** Preserve crop type, growth, harvest, and item data; this is primarily an asset and data-reference change. New paths should join the existing reference and decode checks.
- **Remaining uncertainty:** Final art direction and required crop-state count need an art decision; the placeholder status does not.

## Distribution provenance

### ASSET-9. The bundled shell font has no auditable redistribution provenance

`shell.ttf` identifies itself as “Madou Futo Maru Gothic” and embeds third-party copyright names, including Lawrence! Entertainment and SEGA/D4Enterprise, but it contains no discoverable license text or source URL. Git history records only “switching to better fonts,” and the repository contains no third-party asset notice beyond its own MIT license.

The other two fonts provide stronger provenance signals: `arcade.ttf` embeds the full SIL Open Font License, Google publishes Press Start 2P in its official OFL tree, and the original FontStruct page lists Old English Gothic Pixel under the Open Font License. The official OFL site notes that bundling is permitted subject to its conditions. [Google Fonts: Press Start 2P](https://github.com/google/fonts/tree/main/ofl/pressstart2p), [FontStruct: Old English Gothic Pixel](https://fontstruct.com/fontstructions/show/1535174/old-english-gothic-pixel), [official SIL OFL site](https://openfontlicense.org/).

**Evidence:**

- `assets/fonts/shell.ttf` — embedded strings identify version 1.10, Lawrence! Entertainment, and SEGA/D4Enterprise, but expose no license or authoritative source.
- `scripts/shell.lua:222`, `scripts/debug.lua:81`, and `scripts/debug_anim_panel.lua:347` — the font is bundled for active runtime surfaces, not an unused artifact.
- `LICENSE:1-12` — the sole repository license names only `coghex` and the MIT terms; there is no third-party notice or asset manifest.
- Commit `b6be9b2b` added the current font set without provenance or license details.
- Searches for authoritative provenance for the exact `shell.ttf` binary found only third-party download/catalog sites, which are not adequate evidence of redistribution authority.

**Handoff context:**

- **Current behavior:** A distributed production font cannot be traced from the repository to an authoritative source or redistribution license.
- **Expected direction:** Record each bundled third-party asset’s upstream source, exact version or checksum, copyright holder, and license text. Replace `shell.ttf` if authoritative redistribution terms cannot be established.
- **Scope and constraints:** This is a provenance and packaging concern, not a rendering change. It should cover all three fonts even though the immediate uncertainty is `shell.ttf`.
- **Remaining uncertainty:** The exact license under which this `shell.ttf` binary was obtained remains unknown; this finding does not conclude that distribution is unlawful.
