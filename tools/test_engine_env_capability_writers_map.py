#!/usr/bin/env python3
"""The §5 writer-MAP contract of engine_env_capability_writers.py --
`audit_writer_modules` and the pass-on residue (issue #1892, CMA-1;
extracted from tools/test_engine_env_capability_writers.py by issue
#2228).

Seven groups, the whole of what the checked-in `CAPABILITY_WRITER_MODULES`
map promises: both consumer shapes (a capability selector and a raw
accessor from one module) canonicalize onto the same field, a declared
write passes, an undeclared write fails, a mapped module that no longer
writes fails just as loudly, the map's KEYS track the live field set in
both directions, a §6.1 permanent module is exempt, and a handle merely
PASSED ON is residue rather than a write. The scan's own mechanics --
scope, tokenization, primitive position -- belong to
`test_engine_env_capability_writers_scanner`; capability-record and
projection discovery to `..._projections`; the real repository to
`..._conformance`.

Not a gate of its own. Run through the focused façade or the aggregate:

  python3 tools/test_engine_env_capability_writers.py --only map
  python3 tools/test_engine_env_capability_audit.py
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_writers import (  # type: ignore  # noqa: E402
    audit_writer_modules, capability_accessor_map,
)
from test_engine_env_capability_writers_support import (  # noqa: E402
    DECLARED_WRITER as _DECLARED_WRITER,
    TRAP_MODULE as _TRAP_MODULE,
    WRITER_FIELDS as _WRITER_FIELDS,
    expect,
    scan as _scan,
    writer_sources,
)


# ----- This owner's fixtures -------------------------------------------

# Same write, from a module the map does not list.
_UNDECLARED_WRITER = """\
module Interloper.Mod where

import Data.IORef

import Engine.Core.State (EngineEnv, fieldTwo)
import Engine.Core.Capability.Fake (FakeCapability(..), toFakeCapability)

sneakCapability ∷ EngineEnv → IO ()
sneakCapability env = modifyIORef' (fkFieldOne (toFakeCapability env)) (+ 1)

sneakRaw ∷ EngineEnv → IO ()
sneakRaw env = writeIORef (fieldTwo env) 9
"""

# SS6.1's cohort (D-4): writes the same field, must not be reported and
# must not be admitted into the map.
_PERMANENT_WRITER = """\
module Permanent.Mod where

import Data.IORef

import Engine.Core.State

seedEverything ∷ EngineEnv → IO ()
seedEverything env = writeIORef (fieldOne env) 0
"""

#: The paths this owner's own fixtures occupy in the synthetic tree.
#: The declared writer's and the trap module's live in the support
#: module's `SHARED_PATHS`, beside the fixtures themselves, because the
#: conformance and scanner owners drive them too.
_PATHS = {
    "undeclared": "src/Interloper/Mod.hs",
    "permanent": "src/Permanent/Mod.hs",
}


def _writer_sources(**modules: str) -> dict[str, str]:
    """This owner's synthetic tree, over its own paths."""
    return writer_sources(_PATHS, modules)


# ----- This owner's cases ----------------------------------------------

def test_writer_map_canonicalizes_both_consumer_shapes():
    """A capability accessor and the raw `EngineEnv` accessor it
    projects canonicalize onto the SAME field, so one map covers both
    consumer shapes."""
    accessors = capability_accessor_map(
        _writer_sources(), _WRITER_FIELDS)
    expect(accessors == {
        "fkFieldOne": (("fieldOne", "Engine.Core.Capability.Fake",
                        "FakeCapability"),),
        "fkFieldTwo": (("fieldTwo", "Engine.Core.Capability.Fake",
                        "FakeCapability"),),
    }, f"capability_accessor_map must derive each accessor's field, owner "
       f"and record type from the LIVE projection, got: {accessors}")

    writes, _ = _scan(_writer_sources(declared=_DECLARED_WRITER))
    expect(writes["fieldOne"] == {"Consumer.Mod"},
           f"a `writeIORef (fkFieldOne ...)` must be attributed to "
           f"`fieldOne`, got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Consumer.Mod"},
           f"a `writeIORef (fieldTwo env)` must be attributed to the same "
           f"field through the raw accessor, got: "
           f"{sorted(writes['fieldTwo'])}")
    expect(writes["fieldThree"] == set(),
           f"an unwritten field must map to the empty set, got: "
           f"{sorted(writes['fieldThree'])}")


def test_writer_map_accepts_a_declared_write():
    """The permitted case: both writes are declared, so nothing fails."""
    writes, _ = _scan(_writer_sources(declared=_DECLARED_WRITER))
    declared = {"fieldOne": frozenset({"Consumer.Mod"}),
                "fieldTwo": frozenset({"Consumer.Mod"}),
                "fieldThree": frozenset()}
    violations = audit_writer_modules(
        writes, _WRITER_FIELDS, declared=declared)
    expect(violations == [],
           f"a fully declared write set must produce no violation, got: "
           f"{violations}")


def test_writer_map_rejects_an_undeclared_write():
    """Requirement 1, through BOTH consumer shapes: a write from a
    module the field's map does not list is a violation."""
    writes, _ = _scan(_writer_sources(declared=_DECLARED_WRITER,
                                      undeclared=_UNDECLARED_WRITER))
    expect(writes["fieldOne"] == {"Consumer.Mod", "Interloper.Mod"},
           f"the undeclared capability-accessor write must be detected, "
           f"got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"Consumer.Mod", "Interloper.Mod"},
           f"the undeclared raw-accessor write must be detected, got: "
           f"{sorted(writes['fieldTwo'])}")

    declared = {"fieldOne": frozenset({"Consumer.Mod"}),
                "fieldTwo": frozenset({"Consumer.Mod"}),
                "fieldThree": frozenset()}
    violations = audit_writer_modules(
        writes, _WRITER_FIELDS, declared=declared)
    expect(len(violations) == 2 and all("Interloper.Mod" in v
                                        for v in violations),
           f"both undeclared writes must be reported, one per field, got: "
           f"{violations}")


def test_writer_map_rejects_a_stale_entry():
    """Requirement 2 -- the both-directions half
    `RENDER_MAIN_ONLY_MODULES` already has: a mapped module that no
    longer writes the field fails as loudly as an undeclared write."""
    writes, _ = _scan(_writer_sources(declared=_DECLARED_WRITER))
    declared = {"fieldOne": frozenset({"Consumer.Mod", "Departed.Mod"}),
                "fieldTwo": frozenset({"Consumer.Mod"}),
                "fieldThree": frozenset()}
    violations = audit_writer_modules(
        writes, _WRITER_FIELDS, declared=declared)
    expect(len(violations) == 1 and "Departed.Mod" in violations[0]
           and "no longer writes" in violations[0],
           f"a mapped module with no backing write must be reported, got: "
           f"{violations}")


def test_writer_map_keys_track_the_live_field_set():
    """The reviewer's key-set requirement, both ways: a live field with
    no map entry fails, and a map key that is no longer a field fails."""
    writes, _ = _scan(_writer_sources())
    missing = audit_writer_modules(
        writes, _WRITER_FIELDS,
        declared={"fieldOne": frozenset(), "fieldTwo": frozenset()})
    expect(len(missing) == 1 and "fieldThree" in missing[0]
           and "no entry in CAPABILITY_WRITER_MODULES" in missing[0],
           f"a live field with no map entry must be reported, got: {missing}")

    stale = audit_writer_modules(
        writes, _WRITER_FIELDS,
        declared={"fieldOne": frozenset(), "fieldTwo": frozenset(),
                  "fieldThree": frozenset(), "fieldGone": frozenset()})
    expect(len(stale) == 1 and "fieldGone" in stale[0]
           and "remove the stale key" in stale[0],
           f"a map key that is not a live field must be reported, got: "
           f"{stale}")


def test_permanent_module_writes_are_exempt():
    """Requirement 4 (design decision D-4): SS6.1's 24 permanent
    full-access modules hold whole-session orchestration authority by
    job description, so their writes are neither violations nor map
    entries."""
    writes, residue = _scan(_writer_sources(permanent=_PERMANENT_WRITER))
    expect(writes["fieldOne"] == set(),
           f"a write from an SS6.1 permanent module must not enter the "
           f"write map, got: {sorted(writes['fieldOne'])}")
    violations = audit_writer_modules(
        writes, _WRITER_FIELDS,
        declared={f: frozenset() for f in _WRITER_FIELDS})
    expect(violations == [],
           f"an SS6.1 permanent module's write must not be a violation, "
           f"got: {violations}")
    expect(residue == [],
           f"the permanent fixture uses raw accessors only, so it "
           f"contributes no capability-accessor residue, got: {residue}")


def test_passed_on_handle_is_residue_not_a_write():
    """Requirement 5 (D-5): a handle handed to a helper is counted and
    listed, never attributed and never a violation."""
    writes, residue = _scan(_writer_sources(trap=_TRAP_MODULE))
    expect(writes["fieldTwo"] == set(),
           f"`Trap.Mod` performs no attributable write -- its only "
           f"accessor use passes the handle onward, got: "
           f"{sorted(writes['fieldTwo'])}")
    passed = [item for item in residue if item.module == "Trap.Mod"]
    expect(len(passed) == 1 and passed[0].accessor == "fkFieldTwo"
           and passed[0].field == "fieldTwo"
           and passed[0].relpath == "src/Trap/Mod.hs",
           f"the passed-on handle must appear once in the residue with its "
           f"path, accessor and canonical field, got: {passed}")
    expect(audit_writer_modules(
        writes, _WRITER_FIELDS,
        declared={f: frozenset() for f in _WRITER_FIELDS}) == [],
           "residue must never be reported as a violation")


#: This owner's inventory of the §5 writer-map contract, in the relative order
#: these groups hold within the façade's run sequence.
#: `tools/test_engine_env_capability_writers.py` composes that
#: sequence from every owner's inventory; nothing here decides when,
#: or whether, it runs.
TESTS = (
    test_writer_map_canonicalizes_both_consumer_shapes,
    test_writer_map_accepts_a_declared_write,
    test_writer_map_rejects_an_undeclared_write,
    test_writer_map_rejects_a_stale_entry,
    test_writer_map_keys_track_the_live_field_set,
    test_permanent_module_writes_are_exempt,
    test_passed_on_handle_is_residue_not_a_write,
)
