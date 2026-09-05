#!/usr/bin/env python3
"""Mutation-primitive provenance, shadow exemptions, and the
real-repository writer map of engine_env_capability_writers.py (issue
#1892, CMA-1; extracted from
tools/test_engine_env_capability_writers.py by issue #2228).

Seven groups. Three are about `resolve_primitive`: a name spelled
`writeIORef` counts only when it IS `Data.IORef`'s -- a module-local
definition and a qualifier naming somebody else are not it -- and
`SHADOW_EXEMPTIONS` suppresses exactly its own (module, name) pair and
is itself validated against the live sources. The fourth is the
writer-map conformance assertion against the REAL repository: every
live `EngineEnv` field's declared writing modules, checked in both
directions over the actual production tree, which is what makes the
checked-in map a statement about this repository rather than about the
fixtures.

The remaining three are the #2230 split's structural conformance,
which is a real-repository question of the same kind: the four
implementation owners' import edges run one way and none of them
reaches back through the facade, the facade re-exports every symbol
those owners declare bound to the owner's own object, and a writer-map
violation still cites the stable facade path rather than whichever
child now holds the check.

The map policy over synthetic trees belongs to
`test_engine_env_capability_writers_map`, the scanner's lexical
mechanics to `..._scanner`, and the other two real-repository
assertions -- both projection questions -- to `..._projections`.

Read-only against the working tree: the real-repository case reads
`src/Engine/Core/State.hs` and scans the production sources, the three
structural cases parse the writer modules' own source, and none of them
writes anything.

Not a gate of its own. Run through the focused façade or the aggregate:

  python3 tools/test_engine_env_capability_writers.py --only conformance
  python3 tools/test_engine_env_capability_audit.py
"""
from __future__ import annotations

import ast
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore  # noqa: E402
    ENGINE_ENV_FILE, ENGINE_ENV_PATTERN, PERMANENT_DEFINER,
    PERMANENT_IMPORTERS, REPO_ROOT, extract_record_fields,
    scan_production_sources,
)
import engine_env_capability_writers as writers  # type: ignore  # noqa: E402
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


# ----- The #2230 split's structural conformance ------------------------

#: The four implementation owners, in the order their dependencies are
#: allowed to run, and the facade that re-exports all four. The tuple
#: order IS the contract: an owner may import a module listed before it
#: and nothing listed at or after it, which is what makes "syntax and
#: projection discovery never see each other" checkable rather than a
#: claim in a docstring.
WRITER_OWNER_ORDER = (
    "engine_env_capability_writer_authority",
    "engine_env_capability_writer_syntax",
    "engine_env_capability_writer_projections",
    "engine_env_capability_writer_scan",
)
WRITER_FACADE = "engine_env_capability_writers"

#: The one legal edge between the two middle owners: none. `syntax` and
#: `projections` are siblings that must not reach each other, and the
#: scan owner composes them instead -- so `projections` may not import
#: `syntax` even though `syntax` is listed before it.
WRITER_SIBLING_OWNERS = frozenset({
    "engine_env_capability_writer_syntax",
    "engine_env_capability_writer_projections",
})


def _writer_module_source(name: str) -> str:
    """One writer module's own source text, read from `tools/`."""
    return (Path(__file__).resolve().parent / f"{name}.py").read_text(
        encoding="utf-8")


def _imported_modules(source: str) -> set[str]:
    """Every module name `source` imports, from the WHOLE tree.

    `ast.walk` rather than `tree.body`: a deferred import inside a
    function, or one under `if TYPE_CHECKING:`, is the same dependency
    edge and would otherwise pass a structural check by hiding one
    level down."""
    imported: set[str] = set()
    for node in ast.walk(ast.parse(source)):
        if isinstance(node, ast.Import):
            imported.update(alias.name.split(".")[0] for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.level == 0 and node.module:
            imported.add(node.module.split(".")[0])
    return imported


def _declared_names(source: str) -> set[str]:
    """Every top-level name a module DECLARES -- functions, classes and
    module-level assignments alike. A re-exported name is not declared,
    so this separates an owner's implementation from the facade's
    imports without either side listing the other."""
    declared: set[str] = set()
    for node in ast.parse(source).body:
        if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef,
                             ast.ClassDef)):
            declared.add(node.name)
        elif isinstance(node, ast.Assign):
            declared.update(target.id for target in node.targets
                            if isinstance(target, ast.Name))
        elif isinstance(node, ast.AnnAssign) and isinstance(node.target,
                                                            ast.Name):
            declared.add(node.target.id)
    return declared


def test_writer_owner_imports_run_one_way() -> None:
    """The four implementation owners form a one-way chain and no owner
    imports the facade (#2230 requirements 16 and 17).

    Import direction is the whole reason the split is safe to make: the
    aggregate audit imports one module, that module imports four, and
    the four never form a cycle or route back through the thing that
    imported them. Nothing else in the repository checks it -- both
    self-tests and the production gate would keep passing with an owner
    importing the facade right up until an unrelated import order made
    it a circular-import crash."""
    print("test_writer_owner_imports_run_one_way")

    violations: list[str] = []
    for position, owner in enumerate(WRITER_OWNER_ORDER):
        imported = _imported_modules(_writer_module_source(owner))
        if WRITER_FACADE in imported:
            violations.append(
                f"{owner} imports {WRITER_FACADE} -- the facade imports "
                f"every owner, so this is a cycle")
        forward = sorted(imported & set(WRITER_OWNER_ORDER[position:]))
        if forward:
            violations.append(
                f"{owner} imports {forward}, which it is not allowed to "
                f"depend on: the owner order is {list(WRITER_OWNER_ORDER)}")
        if owner in WRITER_SIBLING_OWNERS:
            siblings = sorted(imported & (WRITER_SIBLING_OWNERS - {owner}))
            if siblings:
                violations.append(
                    f"{owner} imports its sibling {siblings} -- syntax and "
                    f"projection discovery are composed by the scan owner, "
                    f"never wired to each other")
    expect(not violations,
           "the writer owners' import edges must run one way: "
           + "; ".join(violations))

    # Guard the guard: a check that found no import edge at all would
    # report the same clean result as a correct one.
    edges = sum(len(_imported_modules(_writer_module_source(owner))
                    & set(WRITER_OWNER_ORDER))
                for owner in WRITER_OWNER_ORDER)
    expect(edges >= 4,
           f"the scan found {edges} owner-to-owner import edge(s); the "
           f"split wires syntax, projections and scan onto authority and "
           f"scan onto all three, so a near-zero count means the parse "
           f"stopped seeing imports rather than that the tree is clean")


def test_the_writer_facade_re_exports_its_owners() -> None:
    """`engine_env_capability_writers` exposes every name its four
    owners declare, bound to the owner's own object, and declares no
    implementation of its own (#2230 requirements 3 and 4).

    Every consumer -- the aggregate audit and all five self-test
    modules -- imports the facade and nothing below it, so a symbol
    that stopped being reachable through it is a broken import for
    somebody even while each owner is perfectly healthy. Binding is
    checked by identity, not by name: a facade that redefined a
    constant instead of re-exporting it would satisfy a `hasattr`
    check while the audit and the map disagreed about what the
    authority is."""
    print("test_the_writer_facade_re_exports_its_owners")

    missing: list[str] = []
    aliased: list[str] = []
    for owner in WRITER_OWNER_ORDER:
        module = sys.modules[owner]
        for name in sorted(_declared_names(_writer_module_source(owner))):
            if not hasattr(writers, name):
                missing.append(f"{owner}.{name}")
            elif getattr(writers, name) is not getattr(module, name):
                aliased.append(f"{owner}.{name}")
    expect(not missing,
           f"names their owners declare but the facade does not "
           f"re-export: {missing}")
    expect(not aliased,
           f"names the facade exposes as a DIFFERENT object than the "
           f"owner that declares them: {aliased}")

    # The facade is documentation and re-exports: it declares nothing.
    own = sorted(_declared_names(_writer_module_source(WRITER_FACADE)))
    expect(not own,
           f"{WRITER_FACADE} declares its own top-level names ({own}) -- "
           f"the split leaves it a documentation-and-re-export module, so "
           f"implementation that reappears here has no owner")

    # Guard the guard: an owner whose source stopped parsing into names
    # would make both loops vacuous.
    surface = sum(len(_declared_names(_writer_module_source(owner)))
                  for owner in WRITER_OWNER_ORDER)
    expect(surface >= 57,
           f"the owners declare {surface} name(s) between them; the "
           f"pre-split module declared 57, and the facade promised to "
           f"keep every one of them importable")


def test_writer_map_violations_name_the_facade() -> None:
    """A writer-map violation cites `tools/engine_env_capability_writers.py`
    (#2230 requirement 19).

    The check moved into the authority owner, but the map is reached
    through the facade and that is the path the message has always
    named. `Path(__file__).name` would now resolve to the child and
    silently send a reader looking for `CAPABILITY_WRITER_MODULES` to a
    filename nothing else in the repository mentions -- a diagnostic
    regression no count, gate or import check can see."""
    print("test_writer_map_violations_name_the_facade")

    # An unmapped live field, and a declared writer that no longer
    # writes: the two messages that interpolate the audit's filename.
    unmapped = audit_writer_modules({}, ["fieldOne"], declared={})
    expect(len(unmapped) == 1,
           f"one unmapped live field is one violation; got {unmapped}")
    expect("tools/engine_env_capability_writers.py" in unmapped[0],
           f"the unmapped-field diagnostic must name the stable facade "
           f"path, not the owner it now lives in; got: {unmapped[0]}")

    undeclared = audit_writer_modules(
        {"fieldOne": {"Some.Module"}}, ["fieldOne"],
        declared={"fieldOne": frozenset()})
    expect(len(undeclared) == 1,
           f"one undeclared write is one violation; got {undeclared}")
    expect("tools/engine_env_capability_writers.py" in undeclared[0],
           f"the undeclared-write diagnostic must name the stable facade "
           f"path, not the owner it now lives in; got: {undeclared[0]}")

    expect(writers.WRITER_FACADE_FILENAME
           == Path(writers.__file__).name,
           f"WRITER_FACADE_FILENAME pins the name of the module that "
           f"re-exports the map; it reads "
           f"{writers.WRITER_FACADE_FILENAME!r} while the facade is "
           f"{Path(writers.__file__).name!r}")


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
    test_writer_owner_imports_run_one_way,
    test_the_writer_facade_re_exports_its_owners,
    test_writer_map_violations_name_the_facade,
)
