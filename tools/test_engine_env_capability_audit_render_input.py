#!/usr/bin/env python3
"""The SS3 main-render and SS7.3 LuaThread input structural boundaries
of engine_env_capability_audit.py (issues #891 and #892, capability
split E3/E4; extracted from tools/test_engine_env_capability_audit.py
by issue #2062).

Both boundaries have the same shape: a capability whose full record
carries thread-private fields, a worker-safe VIEW that carries the
rest, a checked-in set of modules allowed the full record, and a
checked-in set of modules allowed to name the private fields at all.
`audit_render_boundary` and `audit_input_boundary` each get a synthetic
production tree (`_boundary_sources`, `_input_sources`) and are proven
in both directions over it: the clean tree passes; a worker importing
the full capability, a non-owner naming a private field, the view
itself carrying one, a deleted view, and a stale entry in either
checked-in set are each rejected; and a comment mentioning the field
is not a use. The input block adds the watermark-versus-allocator
distinction the split exists for. Each block ends with its
real-repository case.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_engine_env_capability_audit.py
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_audit import (  # type: ignore  # noqa: E402
    INPUT_CAPABILITY_MODULE, INPUT_VIEW_MODULE, RENDER_CAPABILITY_MODULE,
    RENDER_VIEW_MODULE, REPO_ROOT, audit_input_boundary,
    audit_render_boundary, scan_production_sources,
)
from test_engine_env_capability_audit_support import expect  # noqa: E402


# ----- SS3 main-render boundary (issue #891, capability split E3) -------
#
# The SS3 boundary is what makes `render-gpu-asset`'s two-interface
# split a real access boundary rather than a documented convention:
# worker-thread code must have NO interface through which it can
# construct or inspect a record containing `engineStateRef`. These
# fixtures exercise `audit_render_boundary`'s pure core with synthetic
# sources, never by editing real production modules.

_MAIN = "Main.Render.Mod"
_WORKER = "Worker.Mod"


def _boundary_sources(*, worker_imports_full=False, worker_names_ref=False,
                      view_names_ref=False, include_view=True,
                      main_imports_full=True):
    """Minimal synthetic production tree: one MainRender module, one
    worker module, and the worker-safe view module itself."""
    view_body = "module Engine.Core.Capability.RenderView where\n"
    if view_names_ref:
        view_body += "  rvEngineStateRef = engineStateRef env\n"
    else:
        view_body += "  rvCameraRef = cameraRef env\n"

    main_body = f"module {_MAIN} where\n"
    if main_imports_full:
        main_body = f"import {RENDER_CAPABILITY_MODULE}\n" + main_body

    worker_body = f"module {_WORKER} where\n"
    if worker_imports_full:
        worker_body = f"import {RENDER_CAPABILITY_MODULE}\n" + worker_body
    else:
        worker_body = f"import {RENDER_VIEW_MODULE}\n" + worker_body
    if worker_names_ref:
        worker_body += "  x = readIORef (rcEngineStateRef cap)\n"

    sources = {
        "src/Main/Render/Mod.hs": main_body,
        "src/Worker/Mod.hs": worker_body,
    }
    if include_view:
        sources["src/Engine/Core/Capability/RenderView.hs"] = view_body
    return sources


def test_boundary_clean_tree_has_no_violations():
    violations = audit_render_boundary(
        _boundary_sources(),
        main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(violations == [],
           f"a tree where only the MainRender module imports the full "
           f"render capability, the worker imports only the view, and the "
           f"view never names engineStateRef must pass, got: {violations}")


def test_boundary_worker_importing_full_capability_rejected():
    violations = audit_render_boundary(
        _boundary_sources(worker_imports_full=True),
        main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(any(_WORKER in v and RENDER_CAPABILITY_MODULE in v
               for v in violations),
           "a non-MainRender production module importing the full "
           "RenderCapability must be rejected -- that record carries "
           "engineStateRef, which SS3 makes main-render private")


def test_boundary_non_owner_naming_engine_state_ref_rejected():
    violations = audit_render_boundary(
        _boundary_sources(worker_names_ref=True),
        main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(any(_WORKER in v and "engineStateRef" in v for v in violations),
           "a production module outside ENGINE_STATE_REF_OWNERS naming "
           "engineStateRef/rcEngineStateRef must be rejected")


def test_boundary_engine_state_ref_in_a_comment_is_not_a_violation():
    # Haddock on the view legitimately EXPLAINS why the field is absent.
    # Only live code counts, or the enforcement would forbid documenting
    # its own rule.
    sources = _boundary_sources()
    sources["src/Engine/Core/Capability/RenderView.hs"] = (
        "-- | Deliberately contains no engineStateRef field.\n"
        "module Engine.Core.Capability.RenderView where\n"
        "  rvCameraRef = cameraRef env  -- not engineStateRef\n")
    violations = audit_render_boundary(
        sources, main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(violations == [],
           f"a Haddock/line comment mentioning engineStateRef must not "
           f"count as naming it, got: {violations}")


def test_boundary_view_carrying_engine_state_ref_rejected():
    violations = audit_render_boundary(
        _boundary_sources(view_names_ref=True),
        main_only=frozenset({_MAIN}),
        state_ref_owners=frozenset({RENDER_VIEW_MODULE}))
    expect(any(RENDER_VIEW_MODULE in v for v in violations),
           "the worker-visible view must be rejected if it so much as "
           "names engineStateRef -- even being listed as an owner must "
           "not buy it an exemption from the structural check")


def test_boundary_missing_view_module_rejected():
    violations = audit_render_boundary(
        _boundary_sources(include_view=False),
        main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(any(RENDER_VIEW_MODULE in v and "missing" in v
               for v in violations),
           "deleting the worker-safe view must fail loudly -- SS3's "
           "boundary has no enforcement without it")


def test_boundary_stale_main_only_entry_rejected():
    # Same both-directions discipline as the SS6 ratchet: a module listed
    # as MainRender that no longer imports the full record is drift.
    violations = audit_render_boundary(
        _boundary_sources(main_imports_full=False),
        main_only=frozenset({_MAIN}), state_ref_owners=frozenset())
    expect(any(_MAIN in v and "stale" in v for v in violations),
           "a stale RENDER_MAIN_ONLY_MODULES entry must be flagged, so the "
           "checked-in set stays an exact mirror of the live one")


def test_boundary_stale_state_ref_owner_rejected():
    violations = audit_render_boundary(
        _boundary_sources(),
        main_only=frozenset({_MAIN}),
        state_ref_owners=frozenset({"Ghost.Owner"}))
    expect(any("Ghost.Owner" in v and "stale" in v for v in violations),
           "a stale ENGINE_STATE_REF_OWNERS entry must be flagged too")


def test_real_repo_render_boundary_holds():
    violations = audit_render_boundary(scan_production_sources(REPO_ROOT))
    expect(violations == [],
           f"the real repo must satisfy SS3's main-render boundary after "
           f"issue #891's render-gpu-asset migration, got: {violations}")


# ----- SS7.3 LuaThread input boundary (issue #892, capability split E4) -
#
# Same shape as the SS3 fixtures above, for `input-lua-transport`'s two
# LuaThread-private fields. A worker-thread module must have NO
# interface through which it can allocate a barrier token
# (`inputBarrierNextRef`) or reach the `onKeyDown` current-key handoff
# (`currentKeyDownRef`) -- it gets the barrier WATERMARK and nothing
# else. These fixtures exercise `audit_input_boundary`'s pure core with
# synthetic sources, never by editing real production modules.

_LUA = "Lua.Api.Mod"
_INPUT_WORKER = "Input.Worker.Mod"


def _input_sources(*, worker_imports_full=False, worker_names_alloc=False,
                   worker_names_keydown=False, view_names_field=False,
                   include_view=True, lua_imports_full=True):
    """Minimal synthetic production tree: one LuaThread module, one
    input-thread worker module, and the worker-safe view module."""
    view_body = "module Engine.Core.Capability.InputView where\n"
    if view_names_field:
        view_body += "  ivInputBarrierNextRef = inputBarrierNextRef env\n"
    else:
        view_body += "  ivInputBarrierRef = inputBarrierRef env\n"

    lua_body = f"module {_LUA} where\n"
    if lua_imports_full:
        lua_body = f"import {INPUT_CAPABILITY_MODULE}\n" + lua_body

    worker_body = f"module {_INPUT_WORKER} where\n"
    if worker_imports_full:
        worker_body = f"import {INPUT_CAPABILITY_MODULE}\n" + worker_body
    else:
        worker_body = f"import {INPUT_VIEW_MODULE}\n" + worker_body
    if worker_names_alloc:
        worker_body += "  t = newBarrierToken (icInputBarrierNextRef cap)\n"
    if worker_names_keydown:
        worker_body += "  k = readIORef (currentKeyDownRef env)\n"

    sources = {
        "src/Lua/Api/Mod.hs": lua_body,
        "src/Input/Worker/Mod.hs": worker_body,
    }
    if include_view:
        sources["src/Engine/Core/Capability/InputView.hs"] = view_body
    return sources


def test_input_boundary_clean_tree_has_no_violations():
    violations = audit_input_boundary(
        _input_sources(),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(violations == [],
           f"a tree where only the LuaThread module imports the full input "
           f"capability, the worker imports only the view, and the view "
           f"names neither private field must pass, got: {violations}")


def test_input_boundary_worker_importing_full_capability_rejected():
    violations = audit_input_boundary(
        _input_sources(worker_imports_full=True),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(any(_INPUT_WORKER in v and INPUT_CAPABILITY_MODULE in v
               for v in violations),
           "a non-LuaThread production module importing the full "
           "InputCapability must be rejected -- that record carries the "
           "barrier-token allocator and the onKeyDown current-key handoff, "
           "which SS5 makes LuaThread-private")


def test_input_boundary_non_owner_naming_allocator_rejected():
    violations = audit_input_boundary(
        _input_sources(worker_names_alloc=True),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(any(_INPUT_WORKER in v and "inputBarrierNextRef" in v
               for v in violations),
           "a production module outside INPUT_LUA_ONLY_FIELD_OWNERS naming "
           "inputBarrierNextRef/icInputBarrierNextRef must be rejected -- "
           "the input thread publishes the watermark, it never allocates")


def test_input_boundary_non_owner_naming_current_key_rejected():
    violations = audit_input_boundary(
        _input_sources(worker_names_keydown=True),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(any(_INPUT_WORKER in v and "currentKeyDownRef" in v
               for v in violations),
           "a production module outside INPUT_LUA_ONLY_FIELD_OWNERS naming "
           "currentKeyDownRef/icCurrentKeyDownRef must be rejected too -- "
           "both private fields are covered, not just the barrier one")


def test_input_boundary_watermark_is_not_confused_with_allocator():
    # The whole point of the split: `inputBarrierRef` (the watermark the
    # input thread publishes) must stay freely nameable, or the check
    # would forbid the very access the view exists to grant. A substring
    # -blind rule would flag it, since `inputBarrierNextRef` contains no
    # `inputBarrierRef` but a sloppy `inputBarrier` prefix match would
    # catch both.
    sources = _input_sources()
    sources["src/Input/Worker/Mod.hs"] += (
        "  w = modifyTVar' (ivInputBarrierRef view) (max tok)\n")
    violations = audit_input_boundary(
        sources, lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(violations == [],
           f"naming the barrier WATERMARK (inputBarrierRef/"
           f"ivInputBarrierRef) must never be a violation -- it is exactly "
           f"what the worker-safe view grants, got: {violations}")


def test_input_boundary_private_field_in_a_comment_is_not_a_violation():
    # Haddock on the view legitimately EXPLAINS why the fields are
    # absent; only live code counts, or the enforcement would forbid
    # documenting its own rule.
    sources = _input_sources()
    sources["src/Engine/Core/Capability/InputView.hs"] = (
        "-- | Deliberately carries no inputBarrierNextRef and no\n"
        "--   currentKeyDownRef field.\n"
        "module Engine.Core.Capability.InputView where\n"
        "  ivInputBarrierRef = inputBarrierRef env  -- not the allocator\n")
    violations = audit_input_boundary(
        sources, lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(violations == [],
           f"a Haddock/line comment mentioning either private field must "
           f"not count as naming it, got: {violations}")


def test_input_boundary_view_carrying_private_field_rejected():
    violations = audit_input_boundary(
        _input_sources(view_names_field=True),
        lua_only=frozenset({_LUA}),
        field_owners=frozenset({INPUT_VIEW_MODULE}))
    expect(any(INPUT_VIEW_MODULE in v for v in violations),
           "the worker-visible view must be rejected if it so much as "
           "names a LuaThread-private field -- even being listed as an "
           "owner must not buy it an exemption from the structural check")


def test_input_boundary_missing_view_module_rejected():
    violations = audit_input_boundary(
        _input_sources(include_view=False),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(any(INPUT_VIEW_MODULE in v and "missing" in v
               for v in violations),
           "deleting the worker-safe input view must fail loudly -- "
           "SS7.3's boundary has no enforcement without it")


def test_input_boundary_stale_lua_only_entry_rejected():
    violations = audit_input_boundary(
        _input_sources(lua_imports_full=False),
        lua_only=frozenset({_LUA}), field_owners=frozenset())
    expect(any(_LUA in v and "stale" in v for v in violations),
           "a stale INPUT_LUA_ONLY_MODULES entry must be flagged, so the "
           "checked-in LuaThread set stays an exact mirror of the live one")


def test_input_boundary_stale_field_owner_rejected():
    violations = audit_input_boundary(
        _input_sources(),
        lua_only=frozenset({_LUA}),
        field_owners=frozenset({"Ghost.Owner"}))
    expect(any("Ghost.Owner" in v and "stale" in v for v in violations),
           "a stale INPUT_LUA_ONLY_FIELD_OWNERS entry must be flagged too")


def test_real_repo_input_boundary_holds():
    violations = audit_input_boundary(scan_production_sources(REPO_ROOT))
    expect(violations == [],
           f"the real repo must satisfy SS7.3's LuaThread input boundary "
           f"after issue #892's input-lua-transport migration, got: "
           f"{violations}")


#: This owner's inventory, in the relative order these groups hold
#: within the aggregate's run sequence. `tools/test_engine_env_capability_audit.py`
#: composes that sequence from every owner's inventory; nothing here
#: decides when, or whether, it runs.
TESTS = (
    test_boundary_clean_tree_has_no_violations,
    test_boundary_worker_importing_full_capability_rejected,
    test_boundary_non_owner_naming_engine_state_ref_rejected,
    test_boundary_engine_state_ref_in_a_comment_is_not_a_violation,
    test_boundary_view_carrying_engine_state_ref_rejected,
    test_boundary_missing_view_module_rejected,
    test_boundary_stale_main_only_entry_rejected,
    test_boundary_stale_state_ref_owner_rejected,
    test_real_repo_render_boundary_holds,
    test_input_boundary_clean_tree_has_no_violations,
    test_input_boundary_worker_importing_full_capability_rejected,
    test_input_boundary_non_owner_naming_allocator_rejected,
    test_input_boundary_non_owner_naming_current_key_rejected,
    test_input_boundary_watermark_is_not_confused_with_allocator,
    test_input_boundary_private_field_in_a_comment_is_not_a_violation,
    test_input_boundary_view_carrying_private_field_rejected,
    test_input_boundary_missing_view_module_rejected,
    test_input_boundary_stale_lua_only_entry_rejected,
    test_input_boundary_stale_field_owner_rejected,
    test_real_repo_input_boundary_holds,
)
