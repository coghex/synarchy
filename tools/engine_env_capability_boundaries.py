#!/usr/bin/env python3
"""The SS3 main-render and SS7.3 LuaThread structural boundaries (issues
#891 and #892, EngineEnv capability splits E3 and E4; extracted from
tools/engine_env_capability_audit.py by issue #2064).

Two boundaries of the same shape, each turning a thread-private field
into something the module graph enforces rather than something a
Haddock comment asks for. E1's capability convention exports each
record as `Capability(..)` -- constructor AND accessors -- so a
worker-visible record carrying a private handle would hand worker-thread
code a way to reach it no matter what its documentation claimed. Each
boundary therefore splits its capability into a FULL record and a
worker-safe VIEW, and three checks per boundary make the split real:

  1. Only a module that runs on the owning thread may import the full
     capability.
  2. Only the field's genuine owners may name it (or its prefixed
     accessor) at all.
  3. The worker-visible view must not so much as MENTION the field --
     no field, no accessor, no re-export, hence no path to reach it.

Like the SS6 ratchet, sets 1 and 2 are checked in BOTH directions: a
stale entry (a module listed here that no longer does the thing) fails
too, so neither set can silently decay into a mere upper bound.

Both checks read the SAME `{relative_path: source_text}` map the
aggregate scans once per run (#2064 requirement 13); neither walks the
tree itself, and neither reads the inventory document.

Not independently a gate: `python3 tools/engine_env_capability_audit.py`
remains the one command CI and tools/ci-local.sh run.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore
    _strip_haskell_comments, imports_module, module_identifier,
)


# ===========================================================================
# SS3 main-render ownership boundary (issue #891, capability split E3)
# ===========================================================================
#
# docs/engineenv_capability_inventory.md SS3 makes `EngineState`
# main-render-thread-private, and SS5 lists `MainRender` as
# `engineStateRef`'s ONLY reader and writer. E1's capability convention
# exports each record as `Capability(..)` -- constructor AND accessors
# -- so a worker-visible record carrying `engineStateRef` would hand
# worker-thread code a way to inspect that pointer no matter what its
# Haddock claimed. #891 therefore splits `render-gpu-asset` into two
# interfaces, and these three checks are what make the split a boundary
# rather than a convention:
#
#   1. Only a module classified `MainRender` may import the full
#      `Engine.Core.Capability.Render`.
#   2. Only the pointer's genuine owners may name `engineStateRef` (or
#      its `rcEngineStateRef` accessor) at all.
#   3. The worker-visible view must not so much as MENTION the field --
#      no field, no accessor, no re-export, hence no path to dereference
#      it.
#
# Like the SS6 ratchet, sets 1 and 2 are checked in BOTH directions: a
# stale entry (a module listed here that no longer does the thing) fails
# too, so neither set can silently decay into a mere upper bound.
RENDER_CAPABILITY_MODULE = "Engine.Core.Capability.Render"
RENDER_VIEW_MODULE = "Engine.Core.Capability.RenderView"

# Production modules that legitimately run on `MainRender` and may hold
# the full 21-field record. Every one of these is a SS6.2
# `render-gpu-asset` module #891 migrated whose execution domain SS5
# records as `MainRender` (the Vulkan device/pipeline/swapchain/texture
# family, font rasterization and upload, the GLFW window, UI/text
# rendering, and the `processLuaMessages`-dispatched Message handlers).
#
# A module reached from a worker thread does NOT belong here even if it
# also has a `MainRender` caller: a dual-domain module must satisfy the
# boundary with the worker-safe view alone (e.g. `World.Render.BloodQuads`,
# whose `renderBloodDecalQuads` runs on `WorldThread` while
# `uploadBloodTextures` runs on `MainRender` -- neither path needs
# `engineStateRef`, so the view serves both).
RENDER_MAIN_ONLY_MODULES = frozenset({
    "Engine.Graphics.Font.Load", "Engine.Graphics.Font.Upload",
    "Engine.Graphics.Vulkan.Command.Sprite", "Engine.Graphics.Vulkan.Command.Text",
    "Engine.Graphics.Vulkan.Init", "Engine.Graphics.Vulkan.Recreate",
    "Engine.Graphics.Vulkan.Texture.Bindless",
    "Engine.Graphics.Vulkan.Texture.DefaultFaceMap",
    "Engine.Graphics.Window.GLFW", "Engine.Scene.Batch.Text",
    "Engine.Scripting.Lua.Message.Texture", "Engine.Scripting.Lua.Message.Video",
    "Engine.Scripting.Lua.Message.WorldTexture", "UI.Render",
})

# The only production modules that may name the main-render-private
# pointer: `Engine.Core.State` declares it, `Engine.Core.Init` seeds it,
# `Engine.Core.Monad` carries it through the CPS Reader environment (the
# "carrying mechanism, not an ownership signal" SS3 describes), and
# `Engine.Core.Capability.Render` projects it into the MainRender-only
# record.
ENGINE_STATE_REF_OWNERS = frozenset({
    "Engine.Core.State", "Engine.Core.Init", "Engine.Core.Monad",
    RENDER_CAPABILITY_MODULE,
})

_ENGINE_STATE_REF_RE = re.compile(r"(?<![A-Za-z0-9_'])(?:rcE|e)ngineStateRef(?![A-Za-z0-9_'])")


def audit_render_boundary(
    sources: dict[str, str], *,
    main_only: frozenset[str] = RENDER_MAIN_ONLY_MODULES,
    state_ref_owners: frozenset[str] = ENGINE_STATE_REF_OWNERS,
) -> list[str]:
    """Pure core of the SS3 boundary check. `sources` is
    `{relative_path: source_text}` for every production Haskell file
    (the same input `classify_production_sources` takes)."""
    violations: list[str] = []
    live_render_importers: set[str] = set()
    live_state_ref_users: set[str] = set()
    view_source: str | None = None

    for relpath, text in sorted(sources.items()):
        module = module_identifier(relpath)
        code = _strip_haskell_comments(text)
        if module == RENDER_VIEW_MODULE:
            view_source = code
        if imports_module(text, RENDER_CAPABILITY_MODULE):
            live_render_importers.add(module)
        if _ENGINE_STATE_REF_RE.search(code):
            live_state_ref_users.add(module)

    for module in sorted(live_render_importers - main_only - {RENDER_CAPABILITY_MODULE}):
        violations.append(
            f"`{module}` imports `{RENDER_CAPABILITY_MODULE}` but is not a "
            f"`MainRender` module (RENDER_MAIN_ONLY_MODULES in "
            f"tools/{Path(__file__).name}) -- the full render "
            f"capability carries `engineStateRef`, which "
            f"docs/engineenv_capability_inventory.md SS3 makes main-render "
            f"private. Use `{RENDER_VIEW_MODULE}`'s worker-safe view "
            f"instead; a dual-domain module must satisfy the boundary with "
            f"the view alone")

    for module in sorted(main_only - live_render_importers):
        violations.append(
            f"`{module}` is listed in RENDER_MAIN_ONLY_MODULES but no longer "
            f"imports `{RENDER_CAPABILITY_MODULE}` -- remove the stale entry "
            f"so the checked-in MainRender set stays an exact mirror of the "
            f"live one, not merely an upper bound")

    for module in sorted(live_state_ref_users - state_ref_owners):
        violations.append(
            f"`{module}` names `engineStateRef`/`rcEngineStateRef` but is not "
            f"one of its owners (ENGINE_STATE_REF_OWNERS in "
            f"tools/{Path(__file__).name}) -- "
            f"docs/engineenv_capability_inventory.md SS3 confines the "
            f"main-render-private `EngineState` pointer to `MainRender`")

    for module in sorted(state_ref_owners - live_state_ref_users):
        violations.append(
            f"`{module}` is listed in ENGINE_STATE_REF_OWNERS but no longer "
            f"names `engineStateRef` -- remove the stale entry")

    if view_source is None:
        violations.append(
            f"`{RENDER_VIEW_MODULE}` is missing from the production sources "
            f"-- the worker-safe render view is what keeps non-`MainRender` "
            f"consumers off `engineStateRef`; SS3's boundary has no "
            f"enforcement without it")
    elif _ENGINE_STATE_REF_RE.search(view_source):
        violations.append(
            f"`{RENDER_VIEW_MODULE}` mentions `engineStateRef` -- the "
            f"worker-visible render view must provide NO path to the "
            f"main-render-private pointer (no field, no accessor, no "
            f"re-export); see docs/engineenv_capability_inventory.md SS3")

    return violations


# ===========================================================================
# SS7.3 LuaThread ownership boundary (issue #892, capability split E4)
# ===========================================================================
#
# The exact same shape as the SS3 render boundary above, for the exact
# same reason, applied to `input-lua-transport`'s two LuaThread-PRIVATE
# fields: `inputBarrierNextRef` (SS5: "`LuaThread` (only)" -- the
# synthetic-injection barrier-token allocator) and `currentKeyDownRef`
# (SS5: "`LuaThread` (only)" -- the transient `onKeyDown` current-key
# handoff). E1's convention exports each record as `Capability(..)`, so
# a single eight-field record visible to the input/world threads would
# hand them a way to allocate barrier tokens and to inspect or clobber
# the Lua thread's in-flight key. #892 therefore splits the capability
# into two interfaces, enforced by the same three checks:
#
#   1. Only a module that runs on `LuaThread` may import the full
#      `Engine.Core.Capability.Input`.
#   2. Only the two fields' genuine owners may name either one (or its
#      `ic`-prefixed accessor) at all.
#   3. The worker-visible view must not so much as MENTION either field
#      -- no field, no accessor, no re-export, hence no path to reach it.
#
# Sets 1 and 2 are checked in BOTH directions, like SS3's and SS6's.
INPUT_CAPABILITY_MODULE = "Engine.Core.Capability.Input"
INPUT_VIEW_MODULE = "Engine.Core.Capability.InputView"

# Production modules that legitimately run on `LuaThread` and may hold
# the full eight-field record. Both are SS6.2 `input-lua-transport`
# modules #892 migrated whose execution domain SS5 records as
# `LuaThread`: `API.InputInject` is the barrier allocator's only
# non-boot owner, `API.Keybinds` the current-key handoff's.
#
# A module reached from the input or world thread does NOT belong here.
# In particular `Engine.Input.Thread.Dispatch` publishes the barrier
# WATERMARK (`inputBarrierRef`) and must satisfy the boundary with the
# worker-safe view alone -- the view carries the watermark precisely so
# it never needs the allocator.
INPUT_LUA_ONLY_MODULES = frozenset({
    "Engine.Scripting.Lua.API.InputInject",
    "Engine.Scripting.Lua.API.Keybinds",
})

# The only production modules that may name either LuaThread-private
# field: `Engine.Core.State` declares them, `Engine.Core.Init` seeds
# them, `Engine.Core.Capability.Input` projects them into the
# LuaThread-only record, and the three LuaThread consumers actually use
# them -- `Engine.Scripting.Lua.Thread.Dispatch` (a permanent SS6.1
# full-access orchestration module: it is what WRITES
# `currentKeyDownRef` around each `onKeyDown` broadcast) plus the two
# SS6.2 modules above.
#
# `Engine.Core.Monad` is deliberately absent: unlike `engineStateRef`
# it never names either field -- the CPS Reader environment carries
# them structurally, as part of `EngineEnv`, without mentioning them.
INPUT_LUA_ONLY_FIELD_OWNERS = frozenset({
    "Engine.Core.State", "Engine.Core.Init", INPUT_CAPABILITY_MODULE,
    "Engine.Scripting.Lua.Thread.Dispatch",
    "Engine.Scripting.Lua.API.InputInject",
    "Engine.Scripting.Lua.API.Keybinds",
})

_INPUT_LUA_ONLY_FIELD_RE = re.compile(
    r"(?<![A-Za-z0-9_'])"
    r"(?:(?:icI|i)nputBarrierNextRef|(?:icC|c)urrentKeyDownRef)"
    r"(?![A-Za-z0-9_'])")


def audit_input_boundary(
    sources: dict[str, str], *,
    lua_only: frozenset[str] = INPUT_LUA_ONLY_MODULES,
    field_owners: frozenset[str] = INPUT_LUA_ONLY_FIELD_OWNERS,
) -> list[str]:
    """Pure core of the SS7.3 LuaThread boundary check. `sources` is
    `{relative_path: source_text}` for every production Haskell file
    (the same input `classify_production_sources` takes)."""
    violations: list[str] = []
    live_input_importers: set[str] = set()
    live_field_users: set[str] = set()
    view_source: str | None = None

    for relpath, text in sorted(sources.items()):
        module = module_identifier(relpath)
        code = _strip_haskell_comments(text)
        if module == INPUT_VIEW_MODULE:
            view_source = code
        if imports_module(text, INPUT_CAPABILITY_MODULE):
            live_input_importers.add(module)
        if _INPUT_LUA_ONLY_FIELD_RE.search(code):
            live_field_users.add(module)

    for module in sorted(live_input_importers - lua_only - {INPUT_CAPABILITY_MODULE}):
        violations.append(
            f"`{module}` imports `{INPUT_CAPABILITY_MODULE}` but is not a "
            f"`LuaThread` module (INPUT_LUA_ONLY_MODULES in "
            f"tools/{Path(__file__).name}) -- the full input "
            f"capability carries `inputBarrierNextRef` and "
            f"`currentKeyDownRef`, which "
            f"docs/engineenv_capability_inventory.md SS5 makes `LuaThread` "
            f"private. Use `{INPUT_VIEW_MODULE}`'s worker-safe view "
            f"instead; a dual-domain module must satisfy the boundary with "
            f"the view alone")

    for module in sorted(lua_only - live_input_importers):
        violations.append(
            f"`{module}` is listed in INPUT_LUA_ONLY_MODULES but no longer "
            f"imports `{INPUT_CAPABILITY_MODULE}` -- remove the stale entry "
            f"so the checked-in LuaThread set stays an exact mirror of the "
            f"live one, not merely an upper bound")

    for module in sorted(live_field_users - field_owners):
        violations.append(
            f"`{module}` names `inputBarrierNextRef`/`currentKeyDownRef` "
            f"(or an `ic`-prefixed accessor) but is not one of their owners "
            f"(INPUT_LUA_ONLY_FIELD_OWNERS in "
            f"tools/{Path(__file__).name}) -- "
            f"docs/engineenv_capability_inventory.md SS5 confines the "
            f"barrier-token allocator and the `onKeyDown` current-key "
            f"handoff to `LuaThread`")

    for module in sorted(field_owners - live_field_users):
        violations.append(
            f"`{module}` is listed in INPUT_LUA_ONLY_FIELD_OWNERS but no "
            f"longer names `inputBarrierNextRef`/`currentKeyDownRef` -- "
            f"remove the stale entry")

    if view_source is None:
        violations.append(
            f"`{INPUT_VIEW_MODULE}` is missing from the production sources "
            f"-- the worker-safe input view is what keeps non-`LuaThread` "
            f"consumers off the barrier allocator and the current-key "
            f"handoff; SS7.3's boundary has no enforcement without it")
    elif _INPUT_LUA_ONLY_FIELD_RE.search(view_source):
        violations.append(
            f"`{INPUT_VIEW_MODULE}` mentions `inputBarrierNextRef`/"
            f"`currentKeyDownRef` -- the worker-visible input view must "
            f"provide NO path to either `LuaThread`-private field (no "
            f"field, no accessor, no re-export); see "
            f"docs/engineenv_capability_inventory.md SS7.3")

    return violations
