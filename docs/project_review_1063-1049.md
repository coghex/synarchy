# Project Review Findings: PRs #1063–#1049

These entries record focused evidence from the senior review of merged PRs #1063 through #1049 for later one-at-a-time disposition.

Status legend: `[ ]` unprocessed · `[#N]` filed · `[no-issue]` closed without an issue · `[deferred]` blocked on a concrete precondition

## Status

- [ ] PRR-1. Canonical texture unload leaves aliases bound to a reusable slot
- [ ] PRR-2. Bindless descriptors use an update-after-bind feature that the device never enables

## 1. Bindless texture lifecycle

### PRR-1. Canonical texture unload leaves aliases bound to a reusable slot

> **Captured note:** Invalidate every stable handle that shares an atlas before freeing its bindless slot. Cached aliases exist only in the handle map and shader handle table, while `unloadAsset` unregisters only the canonical handle; the aliases can therefore survive after their shared slot is returned to the allocator and later resolve to an unrelated texture.

**Verification:** Partially verified — the stale state transition and immediate slot reuse are confirmed statically, but `unloadAsset` is currently an unwired teardown API, so there is no present production caller with which to reproduce the wrong-texture render end to end.

**Evidence:**

- `src/Engine/Scripting/Lua/Message/Texture.hs:86` — cached-atlas reuse creates a fresh stable texture handle for the same `AssetId` and canonical `TextureAtlas`.
- `src/Engine/Scripting/Lua/Message/Texture.hs:97` — the alias is inserted only into `btsHandleMap`; it intentionally receives no canonical `btsImageViews` or `btsPinned` entry.
- `src/Engine/Scripting/Lua/Message/Texture.hs:103` — the alias's GPU handle-table entry is written directly to the canonical slot, and `:112-119` leaves an `AssetReady` state plus an incremented atlas refcount behind for that alias.
- `src/Engine/Asset/Manager.hs:86` — once the shared atlas refcount reaches zero, `unloadAsset` has only the atlas's one `taTextureHandle` and calls `unregisterTexture` with that canonical handle at `:103`; it has no inventory of all aliases that share the asset.
- `src/Engine/Asset/Manager.hs:106` — final removal deletes the atlas and its paths only; it does not remove canonical or alias entries from `apTextureHandles` or the texture-size map.
- `src/Engine/Graphics/Vulkan/Texture/Bindless.hs:370` — `unregisterTexture` looks up and clears only the requested handle, writes only that handle's shader-table entry back to slot 0, and deletes only that handle from the three bookkeeping maps.
- `src/Engine/Graphics/Vulkan/Texture/Bindless.hs:385` — the shared slot is returned to the allocator even if other `btsHandleMap` entries still name it.
- `src/Engine/Graphics/Vulkan/Texture/Slot.hs:51` — allocation prefers the lowest freed slot, so the next registered texture can reuse it immediately with a new generation; neither the stale alias map entry nor the GPU handle table carries or validates that generation.
- `src/Engine/Graphics/Vulkan/Texture/Rebind.hs:55` — filter rebinding already models a handle whose slot has no canonical image owner as unrecoverable, with the classification produced at `:77-81`; that diagnostic neither removes the stale alias nor prevents the slot from being reused.
- A tracker search over open and closed issues for alias unload, unregister, and bindless slot reuse found only the already-closed #983 discussion, which explicitly left alias lifecycle redesign out of scope; no live issue owns the teardown defect.

**Handoff context:**

- **Current behavior:** If the retained teardown API is wired or called, releasing the last reference destroys the canonical image and frees its slot while alias handles, ready-state entries, texture sizes, and shader handle→slot entries can remain. A subsequent texture registration may place a different image in that slot, making an old alias resolve to unrelated content rather than undefined.
- **Expected behavior:** Final atlas release invalidates every stable handle associated with that asset before the slot or image is released, or the bindless system tracks shared ownership strongly enough that the slot cannot be freed while any alias remains. No shader-visible handle may survive a slot generation change unnoticed.
- **Scope and constraints:** Surfaced across PR #1054 / issue #983's alias-aware rebind work and PR #1051 / issue #1007's deliberate retention of the unwired teardown path. Preserve cached-atlas deduplication, stable handles for render caches, slot-0 undefined fallback, and the canonical-image model used by filter rebinding.
- **Remaining uncertainty:** The teardown entry points have no current caller, and the intended future release unit may be an individual alias handle rather than an `AssetId`. The processor should first settle that ownership contract, then test canonical-plus-alias release followed by immediate slot reuse at the pure bookkeeping seam and, if practical, on a real device.

## 2. Vulkan bindless capability

### PRR-2. Bindless descriptors use an update-after-bind feature that the device never enables

> **Captured note:** Query and enable the exact Vulkan descriptor-indexing features the bindless layout uses. The logical device enables the umbrella `descriptorIndexing` bit and several siblings but omits `descriptorBindingSampledImageUpdateAfterBind`, while the bindless combined-image-sampler binding is created with `VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT`.

**Verification:** Verified statically against the Vulkan specification — the missing fine-grained feature remains false in the zero-initialized `VkPhysicalDeviceVulkan12Features`, and Vulkan explicitly prohibits the binding flag for combined image samplers unless that feature is enabled. A validation-layer reproduction was not run.

**Evidence:**

- `src/Engine/Graphics/Vulkan/Device.hs:86` — the device feature record starts from `zero` and enables six named descriptor-indexing fields, but not `descriptorBindingSampledImageUpdateAfterBind`.
- `src/Engine/Graphics/Vulkan/Device.hs:98` — that incomplete feature record is the one chained into `DeviceCreateInfo`; no later device-feature enablement exists.
- `src/Engine/Graphics/Vulkan/Texture/Bindless.hs:192` — the texture binding uses `DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT`.
- `src/Engine/Graphics/Vulkan/Texture/Bindless.hs:204` — the binding carrying that flag is `DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER`, the descriptor type governed by the omitted feature.
- The official [`VkPhysicalDeviceVulkan12Features` reference](https://docs.vulkan.org/refpages/latest/refpages/source/VkPhysicalDeviceVulkan12Features.html) states both that enabling `descriptorIndexing` does not enable the other minimum descriptor-indexing features and that `descriptorBindingSampledImageUpdateAfterBind` must be enabled before using update-after-bind with a combined image sampler.
- `src/Engine/Graphics/Vulkan/Capability.hs:37` — bindless support queries only the Vulkan version and an update-after-bind numeric property; it never queries the corresponding feature booleans.
- `src/Engine/Graphics/Vulkan/Device.hs:155` — physical-device suitability considers queue families and extensions only, so a higher-scored GPU that lacks one of the requested bindless features can be selected ahead of a usable device.
- `src/Engine/Graphics/Vulkan/Init.hs:80` — logical-device creation happens before `createTextureSystem` at `:206`, so an unsupported requested feature can fail before #1055's descriptive bindless-capability branch runs. The official [`vkCreateDevice` reference](https://docs.vulkan.org/refpages/latest/refpages/source/vkCreateDevice.html) requires `VK_ERROR_FEATURE_NOT_PRESENT` for an unsupported requested feature.
- Tracker searches for the exact feature name, descriptor-indexing device selection, and bindless capability found no open or closed issue that owns this mismatch.

**Handoff context:**

- **Current behavior:** On every device, the bindless descriptor-set layout asks for sampled-image update-after-bind without enabling its prerequisite feature. A permissive current driver can let normal probes appear green, but the API use is outside the Vulkan validity contract; a stricter driver or validation layer may reject/report layout creation. On a device lacking any blindly requested descriptor-indexing feature, logical-device creation can fail before the renderer reports the real bindless requirement, and multi-GPU selection can prefer that unusable device.
- **Expected behavior:** Device selection queries the required Vulkan 1.2 feature chain, considers a GPU suitable only when the bindless features actually used by shaders/layouts are supported, and device creation enables those exact features — including `descriptorBindingSampledImageUpdateAfterBind`. An unsupported machine should fail through one descriptive, intentional capability path rather than an incidental Vulkan object-creation error.
- **Scope and constraints:** Surfaced while reviewing PR #1055 / issue #977's unsupported-bindless failure contract and PR #1061 / issue #975's device-derived bindless limits. Preserve MoltenVK compatibility, the fixed shader limits, update-after-bind semantics, offscreen/windowed device selection, and portability-subset handling.
- **Remaining uncertainty:** Current real-GPU probes pass without the dev validation profile, so the processor should capture validation output on the supported test GPU and add a device-free feature-requirements seam. It should also decide whether an unsupported discrete GPU should be excluded, scored below a supported integrated GPU, or selected only to produce a richer terminal diagnostic.
