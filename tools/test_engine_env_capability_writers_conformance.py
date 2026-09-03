#!/usr/bin/env python3
"""Mutation-primitive provenance, shadow exemptions, and the
real-repository writer map of engine_env_capability_writers.py (issue
#1892, CMA-1; extracted from
tools/test_engine_env_capability_writers.py by issue #2228).

Four groups. Three are about `resolve_primitive`: a name spelled
`writeIORef` counts only when it IS `Data.IORef`'s -- a module-local
definition and a qualifier naming somebody else are not it -- and
`SHADOW_EXEMPTIONS` suppresses exactly its own (module, name) pair and
is itself validated against the live sources. The fourth is the
writer-map conformance assertion against the REAL repository: every
live `EngineEnv` field's declared writing modules, checked in both
directions over the actual production tree, which is what makes the
checked-in map a statement about this repository rather than about the
fixtures.

The map policy over synthetic trees belongs to
`test_engine_env_capability_writers_map`, the scanner's lexical
mechanics to `..._scanner`, and the other two real-repository
assertions -- both projection questions -- to `..._projections`.

Read-only against the working tree: the real-repository case reads
`src/Engine/Core/State.hs` and scans the production sources, and writes
nothing.

Not a gate of its own. Run through the focused façade or the aggregate:

  python3 tools/test_engine_env_capability_writers.py --only conformance
  python3 tools/test_engine_env_capability_audit.py
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore  # noqa: E402
    ENGINE_ENV_FILE, ENGINE_ENV_PATTERN, PERMANENT_DEFINER,
    PERMANENT_IMPORTERS, REPO_ROOT, extract_record_fields,
    scan_production_sources,
)
from engine_env_capability_writers import (  # type: ignore  # noqa: E402
    CAPABILITY_WRITER_MODULES, SHADOW_EXEMPTIONS, audit_mutation_sites,
    audit_shadow_exemptions, audit_writer_modules, parse_imports,
    resolve_primitive, scan_capability_writes,
)
from test_engine_env_capability_writers_support import (  # noqa: E402
    DECLARED_WRITER as _DECLARED_WRITER,
    WRITER_FIELDS as _WRITER_FIELDS,
    expect,
    full_scan as _full_scan,
    scan as _scan,
    writer_sources,
)


# ----- This owner's fixtures -------------------------------------------

# A module-local `writeIORef` is a different function, and calling it
# mutates no `IORef`. Attributing its argument would invent a write out
# of code that performs none.
_LOCAL_PRIMITIVE = """\
module LocalPrim.Mod where

import Engine.Core.State (EngineEnv, fieldOne)

writeIORef ∷ (EngineEnv → IORef Int) → Int → IO ()
writeIORef _ _ = pure ()

use ∷ EngineEnv → IO ()
use env = writeIORef (fieldOne env) 1
"""

# The same, qualified: `Other.writeIORef` is whatever `Other` exports,
# not `Data.IORef`'s. The control beside it proves the resolution is
# not simply refusing every qualified spelling.
_QUALIFIED_HOMONYM = """\
module QualHomonym.Mod where

import qualified Vendor.Refs as Other
import qualified Data.IORef as Ref
import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

foreign ∷ EngineEnv → IO ()
foreign env = Other.writeIORef (fieldOne env) 1

genuine ∷ EngineEnv → IO ()
genuine env = Ref.writeIORef (fieldTwo env) 2
"""

# A module may legally define its own `writeIORef` beside a `hiding`
# import -- the only TOP-LEVEL form of that which compiles, since an
# unqualified import plus a local definition is an ambiguous occurrence
# at every use site. The local one mutates nothing.
_SHADOWED_PRIMITIVE = """\
module ShadowPrim.Mod where

import Data.IORef hiding (writeIORef)

import Engine.Core.State (EngineEnv, fieldOne, fieldTwo)

writeIORef ∷ (EngineEnv → IORef Int) → Int → IO ()
writeIORef _ _ = pure ()

localHelper ∷ EngineEnv → IO ()
localHelper env = writeIORef (fieldOne env) 1

genuine ∷ EngineEnv → IO ()
genuine env = modifyIORef' (fieldTwo env) (+ 1)
"""

#: The paths this owner's own fixtures occupy in the synthetic tree.
#: The declared writer's lives in the support module's `SHARED_PATHS`,
#: beside the fixture itself, because the map owner drives it too.
_PATHS = {
    "localPrim": "src/LocalPrim/Mod.hs",
    "qualHomonym": "src/QualHomonym/Mod.hs",
    "shadowPrim": "src/ShadowPrim/Mod.hs",
}


def _writer_sources(**modules: str) -> dict[str, str]:
    """This owner's synthetic tree, over its own paths."""
    return writer_sources(_PATHS, modules)


# ----- This owner's cases ----------------------------------------------

def test_a_primitive_must_be_the_one_from_data_ioref():
    """The primitive is held to the same scope rule as the accessor. A
    module-local `writeIORef`, or an unrelated module's qualified
    homonym, is a different function whose argument mutates no `IORef`
    -- attributing it would fabricate a write, and then an undeclared
    writer or a stale map entry, out of code that performs none."""
    writes, _ = _scan(_writer_sources(localPrim=_LOCAL_PRIMITIVE))
    expect(writes["fieldOne"] == set(),
           f"a module-local `writeIORef` is not the primitive, got: "
           f"{sorted(writes['fieldOne'])}")

    writes, _ = _scan(_writer_sources(qualHomonym=_QUALIFIED_HOMONYM))
    expect(writes["fieldOne"] == set(),
           f"`Other.writeIORef` is whatever `Other` exports, not "
           f"`Data.IORef`'s, got: {sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"QualHomonym.Mod"},
           f"but the genuine qualified primitive in the same module must "
           f"still be read, got: {sorted(writes['fieldTwo'])}")

    declarations = parse_imports(
        "import qualified Data.IORef as Ref\n")
    expect(resolve_primitive(declarations, "Ref.writeIORef") == "writeIORef",
           "a qualified primitive resolves through its alias")
    expect(resolve_primitive(declarations, "writeIORef") is None,
           "and a qualified-only import does not put the bare spelling "
           "in scope")
    expect(resolve_primitive(parse_imports("import Data.IORef\n"),
                             "writeIORef") == "writeIORef",
           "a bare import does")

    # The only TOP-LEVEL homonym that compiles: `hiding` the primitive
    # and defining one. An unqualified import beside a local definition
    # is an ambiguous occurrence at every use site, so it cannot reach
    # this scan at all. A LOCAL shadow -- a `let`, a `where`, a lambda
    # parameter -- is the mirror of an accessor shadowed the same way,
    # and requirement 7 sends both to SHADOW_EXEMPTIONS rather than to
    # a scope analysis.
    writes, _ = _scan(_writer_sources(shadowPrim=_SHADOWED_PRIMITIVE))
    expect(writes["fieldOne"] == set(),
           f"a hidden primitive leaves the module's own helper standing "
           f"alone, and it mutates nothing, got: "
           f"{sorted(writes['fieldOne'])}")
    expect(writes["fieldTwo"] == {"ShadowPrim.Mod"},
           f"while the primitives it did NOT hide still read, got: "
           f"{sorted(writes['fieldTwo'])}")


def test_a_shadow_exemption_suppresses_only_its_own_pair():
    """Requirement 7's mechanism. An exemption suppresses exactly the
    module/field pair it names -- the same module's other writes are
    untouched -- and is itself checked, so it cannot quietly outlive
    what it was suppressing."""
    sources = _writer_sources(declared=_DECLARED_WRITER)
    admitted = {("Consumer.Mod", "fieldOne"):
                "`fieldOne` here is the equation's own parameter"}
    scan = _full_scan(sources, exemptions=admitted)
    expect(scan.writes["fieldOne"] == set(),
           f"the exempted pair must not be attributed, got: "
           f"{sorted(scan.writes['fieldOne'])}")
    expect(scan.writes["fieldTwo"] == {"Consumer.Mod"},
           f"but the same module's OTHER write must survive, got: "
           f"{sorted(scan.writes['fieldTwo'])}")
    expect(scan.suppressed == frozenset({("Consumer.Mod", "fieldOne")}),
           f"and the suppression must be recorded, got: {scan.suppressed}")
    expect(audit_shadow_exemptions(scan.suppressed, _WRITER_FIELDS,
                                   exemptions=admitted) == [],
           "a live, reasoned exemption is valid")


def test_shadow_exemptions_are_validated():
    """Its three failure modes: a field that is not live, a reason that
    says nothing, and an entry that no longer suppresses anything."""
    sources = _writer_sources(declared=_DECLARED_WRITER)
    suppressed = _full_scan(sources).suppressed

    unknown = {("Consumer.Mod", "fieldGone"): "a real reason"}
    violations = audit_shadow_exemptions(
        _full_scan(sources, exemptions=unknown).suppressed,
        _WRITER_FIELDS, exemptions=unknown)
    expect(len(violations) == 1 and "fieldGone" in violations[0]
           and "not a live `EngineEnv` field" in violations[0],
           f"an exemption naming a dead field must fail as such, not "
           f"merely as a stale one, got: {violations}")

    for reason in ("", "   ", "TBD"):
        blank = {("Consumer.Mod", "fieldOne"): reason}
        violations = audit_shadow_exemptions(
            _full_scan(sources, exemptions=blank).suppressed,
            _WRITER_FIELDS, exemptions=blank)
        expect(len(violations) == 1 and "no real reason" in violations[0],
               f"reason {reason!r} must fail, got: {violations}")

    stale = {("Nobody.Mod", "fieldOne"): "a real reason"}
    violations = audit_shadow_exemptions(
        suppressed, _WRITER_FIELDS, exemptions=stale)
    expect(len(violations) == 1 and "no such write is detected"
           in violations[0],
           f"an exemption that suppresses nothing must fail, got: "
           f"{violations}")


def test_writer_map_against_the_real_repo():
    """The live gate, asserted against the REAL tree and the REAL
    checked-in map: every field is mapped, no undeclared write, no
    stale entry, and the residue is a deterministic, non-empty
    measurement."""
    engine_env_source = (REPO_ROOT / ENGINE_ENV_FILE).read_text(
        encoding="utf-8")
    live_fields = extract_record_fields(engine_env_source,
                                        ENGINE_ENV_PATTERN)
    sources = scan_production_sources(REPO_ROOT)
    scan = scan_capability_writes(sources, live_fields)
    writes, residue = scan.writes, scan.residue

    expect(set(CAPABILITY_WRITER_MODULES) == set(live_fields),
           f"CAPABILITY_WRITER_MODULES' keys must equal the live EngineEnv "
           f"field set; extra: "
           f"{sorted(set(CAPABILITY_WRITER_MODULES) - set(live_fields))}, "
           f"missing: "
           f"{sorted(set(live_fields) - set(CAPABILITY_WRITER_MODULES))}")

    violations = audit_writer_modules(writes, live_fields)
    expect(violations == [],
           f"the real tree must have no undeclared or stale writing-module "
           f"entry, got: {violations}")

    exempt = set(PERMANENT_IMPORTERS) | {PERMANENT_DEFINER}
    leaked = sorted({module
                     for modules in CAPABILITY_WRITER_MODULES.values()
                     for module in modules} & exempt)
    expect(leaked == [],
           f"no SS6.1 permanent module may appear in the map -- D-4 puts "
           f"them outside this boundary entirely, got: {leaked}")

    expect(residue and residue == sorted(residue),
           "the residue must be non-empty and deterministically ordered")
    expect(all(item.field in set(live_fields) for item in residue),
           "every residue entry must canonicalize to a live EngineEnv field")

    expect(scan_capability_writes(sources, live_fields).writes == writes,
           "the scan must be deterministic across runs")

    unclassified = audit_mutation_sites(scan.sites)
    expect(unclassified == [],
           f"every mutation-primitive occurrence in the real tree must "
           f"classify -- requirement 6's whole point is that an "
           f"unreadable site fails instead of vanishing; got: "
           f"{unclassified[:3]}")
    expect(all(site.kind in ("write", "other") for site in scan.sites)
           and len(scan.sites) > len(
               [s for s in scan.sites if s.kind == "write"]),
           "the site census covers both attributed and ignored sites")
    expect(SHADOW_EXEMPTIONS == {},
           f"SHADOW_EXEMPTIONS is empty in this tree -- the two shape "
           f"rules separate every real case; got: {SHADOW_EXEMPTIONS}")
    expect(audit_shadow_exemptions(scan.suppressed, live_fields) == [],
           "and an empty exemption list has nothing to be stale about")


#: This owner's inventory of primitive provenance and the real repository, in the relative order
#: these groups hold within the façade's run sequence.
#: `tools/test_engine_env_capability_writers.py` composes that
#: sequence from every owner's inventory; nothing here decides when,
#: or whether, it runs.
TESTS = (
    test_a_primitive_must_be_the_one_from_data_ioref,
    test_a_shadow_exemption_suppresses_only_its_own_pair,
    test_shadow_exemptions_are_validated,
    test_writer_map_against_the_real_repo,
)
