#!/usr/bin/env python3
"""The SaveLoadCapability projection contract of
engine_env_capability_audit.py (issue #899, E8; extracted from
tools/test_engine_env_capability_audit.py by issue #2062).

`audit_save_load_projection` is the static half of E8's correspondence
proof: the record's projection must bind every one of SS5's
`save-load-coordination` handles from its matching `EngineEnv`
accessor, no more and no fewer, and the module must be listed in
`synarchy.cabal` or a warning-clean build says nothing about it. Every
case drives that function over the synthetic `_SAVE_LOAD_GOOD` module
text: the clean case, a transposed binding, a missing field, an extra
field, an unlisted module, a missing module, and a Haddock comment
whose `field = accessor env` pair must not read as a binding. The
real-repository assertion for this contract is part of
`test_real_repo_end_state`, owned by
`test_engine_env_capability_audit_boundary`.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_engine_env_capability_audit.py
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_audit import (  # type: ignore  # noqa: E402
    SAVE_LOAD_CAPABILITY_FILE, SAVE_LOAD_CAPABILITY_MODULE,
    SAVE_LOAD_PROJECTION, audit_save_load_projection,
    parse_projection_bindings,
)
from test_engine_env_capability_audit_support import expect  # noqa: E402


# ----- SaveLoadCapability projection correspondence (#899, E8) ----------

_SAVE_LOAD_GOOD = """\
module Engine.Core.Capability.SaveLoad where

-- | Haddock mentioning slSaveBarrierRef = loadStatusRef env must not count.
data SaveLoadCapability = SaveLoadCapability
  { slLoadStatusRef ∷ LoadStatusRef
  }

toSaveLoadCapability ∷ EngineEnv → SaveLoadCapability
toSaveLoadCapability env = SaveLoadCapability
  { slLoadStatusRef         = loadStatusRef env
  , slPendingLoadRef        = pendingLoadRef env
  , slSaveBarrierRef        = saveBarrierRef env
  , slLastSaveTimeRef       = lastSaveTimeRef env
  , slNextItemInstanceIdRef = nextItemInstanceIdRef env
  }
"""

_SAVE_LOAD_CABAL = "                     Engine.Core.Capability.SaveLoad\n"


def test_save_load_projection_clean_case_passes():
    violations = audit_save_load_projection(
        {SAVE_LOAD_CAPABILITY_FILE: _SAVE_LOAD_GOOD}, _SAVE_LOAD_CABAL)
    expect(violations == [],
           f"a projection binding all five handles from their matching "
           f"EngineEnv accessors, with the module listed in the cabal "
           f"file, must pass, got: {violations}")


def test_save_load_projection_transposed_binding_rejected():
    bad = _SAVE_LOAD_GOOD.replace(
        "slPendingLoadRef        = pendingLoadRef env",
        "slPendingLoadRef        = loadStatusRef env")
    violations = audit_save_load_projection(
        {SAVE_LOAD_CAPABILITY_FILE: bad}, _SAVE_LOAD_CABAL)
    expect(any("slPendingLoadRef" in v and "loadStatusRef" in v
               for v in violations),
           "a field bound from the WRONG EngineEnv accessor must be "
           "caught: the static check is what a Python audit can do about "
           "an aliasing mistake that typechecks silently")


def test_save_load_projection_missing_field_rejected():
    bad = _SAVE_LOAD_GOOD.replace(
        "  , slSaveBarrierRef        = saveBarrierRef env\n", "")
    violations = audit_save_load_projection(
        {SAVE_LOAD_CAPABILITY_FILE: bad}, _SAVE_LOAD_CABAL)
    expect(any("slSaveBarrierRef" in v for v in violations),
           "a projection that is not TOTAL over the five documented "
           "handles must be rejected")


def test_save_load_projection_extra_field_rejected():
    bad = _SAVE_LOAD_GOOD.replace(
        "  }\n", "  , slSomethingElse         = engineConfig env\n  }\n")
    violations = audit_save_load_projection(
        {SAVE_LOAD_CAPABILITY_FILE: bad}, _SAVE_LOAD_CABAL)
    expect(any("slSomethingElse" in v for v in violations),
           "silently widening the record past SS5's five documented "
           "handles must be rejected")


def test_save_load_projection_unlisted_in_cabal_rejected():
    violations = audit_save_load_projection(
        {SAVE_LOAD_CAPABILITY_FILE: _SAVE_LOAD_GOOD}, "no module list here\n")
    expect(any("synarchy.cabal" in v for v in violations),
           "an unlisted source file is never compiled, so a warning-clean "
           "build says nothing about it -- the cabal listing must be "
           "checked explicitly")


def test_save_load_projection_missing_module_rejected():
    violations = audit_save_load_projection({}, _SAVE_LOAD_CABAL)
    expect(len(violations) == 1
           and SAVE_LOAD_CAPABILITY_MODULE in violations[0],
           f"a missing capability module must fail on its own, not as five "
           f"per-field errors, got: {violations}")


def test_save_load_projection_ignores_haddock_bindings():
    bindings = parse_projection_bindings(_SAVE_LOAD_GOOD, SAVE_LOAD_PROJECTION)
    expect(bindings.get("slSaveBarrierRef") == "saveBarrierRef",
           f"a `field = accessor env` pair inside a Haddock comment must "
           f"never be read as a real binding (the fixture's comment says "
           f"`slSaveBarrierRef = loadStatusRef env`), got: "
           f"{bindings.get('slSaveBarrierRef')}")


#: This owner's inventory, in the relative order these groups hold
#: within the aggregate's run sequence. `tools/test_engine_env_capability_audit.py`
#: composes that sequence from every owner's inventory; nothing here
#: decides when, or whether, it runs.
TESTS = (
    test_save_load_projection_clean_case_passes,
    test_save_load_projection_transposed_binding_rejected,
    test_save_load_projection_missing_field_rejected,
    test_save_load_projection_extra_field_rejected,
    test_save_load_projection_unlisted_in_cabal_rejected,
    test_save_load_projection_missing_module_rejected,
    test_save_load_projection_ignores_haddock_bindings,
)
