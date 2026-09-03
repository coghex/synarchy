#!/usr/bin/env python3
"""SaveLoadCapability projection correspondence (issue #899, EngineEnv
capability split E8; extracted from
tools/engine_env_capability_audit.py by issue #2064).

The static half of the aliasing contract. A Python audit cannot
observe runtime container identity, so it checks the SOURCE-LEVEL
correspondence -- every capability field bound from the matching
`EngineEnv` accessor -- in the same shape the SS3/SS7.3 boundary checks
(tools/engine_env_capability_boundaries.py) already use. Genuine
aliasing (the same live IORef/TVar) is proven separately by the hspec
module `Test.Headless.Capability.SaveLoad`, using the established
`sameContainer` pattern. Both are required: the static check catches a
transposed or renamed binding in review, the runtime one catches a
projection that copies or reconstructs a container.

`parse_projection_bindings` -- and the alias-preserving wrapper rules
it applies -- is ONE implementation, in
tools/engine_env_capability_common.py, read both here and by the writer
scanner's capability-accessor map (issue #2059). Neither owner holds a
second copy, and neither imports the other.

Not independently a gate: `python3 tools/engine_env_capability_audit.py`
remains the one command CI and tools/ci-local.sh run.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore
    module_identifier, parse_projection_bindings,
)


SAVE_LOAD_CAPABILITY_MODULE = "Engine.Core.Capability.SaveLoad"
SAVE_LOAD_CAPABILITY_FILE = "src/Engine/Core/Capability/SaveLoad.hs"
SAVE_LOAD_PROJECTION = "toSaveLoadCapability"

# `{capability field: EngineEnv accessor}` -- the exact five handles
# docs/engineenv_capability_inventory.md SS5's `save-load-coordination`
# table lists, and nothing else.
SAVE_LOAD_FIELD_MAP = {
    "slLoadStatusRef": "loadStatusRef",
    "slPendingLoadRef": "pendingLoadRef",
    "slSaveBarrierRef": "saveBarrierRef",
    "slLastSaveTimeRef": "lastSaveTimeRef",
    "slNextItemInstanceIdRef": "nextItemInstanceIdRef",
}


def audit_save_load_projection(
    sources: dict[str, str], cabal_text: str, *,
    field_map: dict[str, str] | None = None,
) -> list[str]:
    """Pure core of the E8 record check: the module exists in the
    production sources, is listed in the library's explicit
    `synarchy.cabal` module list (an unlisted source file compiles
    nowhere and so could satisfy a warning-clean build while being
    dead), and its projection binds exactly the five documented handles
    from their matching `EngineEnv` accessors."""
    expected = dict(SAVE_LOAD_FIELD_MAP if field_map is None else field_map)
    violations: list[str] = []

    source = None
    for relpath, text in sources.items():
        if module_identifier(relpath) == SAVE_LOAD_CAPABILITY_MODULE:
            source = text
            break
    if source is None:
        return [f"`{SAVE_LOAD_CAPABILITY_MODULE}` is missing from the "
                f"production sources ({SAVE_LOAD_CAPABILITY_FILE}) -- the "
                f"`save-load-coordination` capability record is what "
                f"non-permanent barrier/load-status consumers narrow to "
                f"(docs/engineenv_capability_inventory.md SS7.8)"]

    if not re.search(rf"^\s*{re.escape(SAVE_LOAD_CAPABILITY_MODULE)}\s*$",
                     cabal_text, re.MULTILINE):
        violations.append(
            f"`{SAVE_LOAD_CAPABILITY_MODULE}` is not listed in "
            f"synarchy.cabal's explicit library module list -- an "
            f"unlisted source file is never compiled, so a warning-clean "
            f"build would say nothing about it")

    bindings = parse_projection_bindings(source, SAVE_LOAD_PROJECTION)
    if not bindings:
        return violations + [
            f"`{SAVE_LOAD_CAPABILITY_MODULE}` defines no "
            f"`{SAVE_LOAD_PROJECTION} env = ...` record construction -- "
            f"E1's convention requires one total, one-way "
            f"`EngineEnv -> XCapability` projection"]

    for field, accessor in sorted(expected.items()):
        actual = bindings.get(field)
        if actual is None:
            violations.append(
                f"`{SAVE_LOAD_PROJECTION}` does not bind `{field}` -- the "
                f"projection must be TOTAL over the five "
                f"`save-load-coordination` handles")
        elif actual != accessor:
            violations.append(
                f"`{SAVE_LOAD_PROJECTION}` binds `{field}` from "
                f"`{actual} env`, not `{accessor} env` -- a projection "
                f"wired to the wrong same-typed `EngineEnv` handle "
                f"typechecks silently and detaches the capability's view "
                f"from the live state")
    for field in sorted(set(bindings) - set(expected)):
        violations.append(
            f"`{SAVE_LOAD_PROJECTION}` binds `{field}`, which is not one "
            f"of the five documented `save-load-coordination` handles -- "
            f"widening the record needs a SS5/SS6.4 inventory change "
            f"first, not a silent addition")
    return violations
