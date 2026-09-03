#!/usr/bin/env python3
"""Component registration, coverage map, registry derivation, split (#2138).

Sixteen groups over the audit's structural contracts rather than its
parsing:

  #760  a registered persistent owner is accepted, an unregistered one
        flagged, a reset owner needs no registration, and a registered
        component missing an inventory row is flagged;
  #767  the test-coverage map must carry a row for every Haskell
        component and Lua module, and none for a reset owner;
  #760  registry membership is DERIVED by tracing the real list rather
        than read from literals, an unresolvable codec is refused, and a
        defined-but-unregistered id is excluded and then flagged;
  #2124 the ownership split's own structure -- scanners are
        filesystem-free leaves, the policy owner imports only the common
        leaf, nothing imports the façade, the canonical record parser is
        one shared object, the façade reads each repository input once,
        and the aggregate check order is the contracted one.

The #2124 groups read owner modules from `support.TOOLS_DIR`; that one
resolution is the shared support module's, not repeated per child.
"""
from __future__ import annotations

from pathlib import Path

from . import support
from .support import expect
from .fixtures_haskell import (
    FAKE_ROOT_RECORDS,
    SYNTHETIC_ENGINE_ENV,
)
from persistence_inventory_audit import audit  # type: ignore
from persistence_inventory_audit_haskell import extract_record_fields  # type: ignore


# ----- #760 component-registration checks --------------------------------

# The derived registered set, as `derive_registered_component_ids` would
# produce it for these fixtures: only "registered-comp" is truly wired
# into the registry (see the derivation fixtures further below).
SYNTHETIC_REGISTERED_IDS = {"registered-comp"}

# The inventory classifies a component persistent AND documents its
# (registered) ComponentId -- the well-formed case.
SYNTHETIC_INVENTORY_COMPONENT_OK = """\
# Fake inventory

### Save components

| Component DTO | ComponentId | Classification |
|---|---|---|
| `RegisteredDTO` | `registered-comp` | Persist exactly |
"""

# The inventory classifies a component persistent but its ComponentId is
# NOT registered in the Haskell source -- must fail.
SYNTHETIC_INVENTORY_COMPONENT_UNREGISTERED = """\
# Fake inventory

### Save components

| Component DTO | ComponentId | Classification |
|---|---|---|
| `RegisteredDTO` | `registered-comp` | Persist exactly |
| `GhostDTO` | `ghost-comp` | Persist exactly |
"""

# The would-be unregistered owner is instead classified Reset/Exclude --
# a rebuilt/reset/excluded owner requires no registration, so no failure.
SYNTHETIC_INVENTORY_COMPONENT_RESET_OK = """\
# Fake inventory

### Save components

| Component DTO | ComponentId | Classification |
|---|---|---|
| `RegisteredDTO` | `registered-comp` | Persist exactly |
| `GhostDTO` | `ghost-comp` | Reset to default |
"""


def test_component_check_accepts_registered_persistent_owner():
    from persistence_inventory_audit_policy import (  # type: ignore
        find_component_registration_violations)
    v = find_component_registration_violations(
        SYNTHETIC_INVENTORY_COMPONENT_OK, SYNTHETIC_REGISTERED_IDS)
    expect(v == [],
           f"a persistent component whose ComponentId IS registered passes, "
           f"got {v}")


def test_component_check_flags_unregistered_persistent_owner():
    from persistence_inventory_audit_policy import (  # type: ignore
        find_component_registration_violations)
    v = find_component_registration_violations(
        SYNTHETIC_INVENTORY_COMPONENT_UNREGISTERED, SYNTHETIC_REGISTERED_IDS)
    expect(any("ghost-comp" in x for x in v),
           f"a persistent Haskell save-component owner WITHOUT a registered "
           f"ComponentId fails the audit, got {v}")


def test_component_check_reset_owner_needs_no_registration():
    from persistence_inventory_audit_policy import (  # type: ignore
        find_component_registration_violations)
    v = find_component_registration_violations(
        SYNTHETIC_INVENTORY_COMPONENT_RESET_OK, SYNTHETIC_REGISTERED_IDS)
    expect(v == [],
           f"an owner classified reset/rebuilt/excluded requires no component "
           f"registration and does NOT fail, got {v}")


def test_component_check_flags_registered_component_missing_a_row():
    from persistence_inventory_audit_policy import (  # type: ignore
        find_component_registration_violations)
    # registry has "registered-comp" but the inventory documents no row
    # for it at all -- a new component owner landed without a decision.
    v = find_component_registration_violations(
        "# empty inventory\n", SYNTHETIC_REGISTERED_IDS)
    expect(any("registered-comp" in x for x in v),
           f"a registered component with no persistent inventory row fails "
           f"the audit, got {v}")


# ----- #767 test-coverage-map checks --------------------------------------
#
# Requirement 3 (issue #767, save-overhaul D1): a persistent §10 save
# component, or a `Persist exactly` §7 Lua persistence module, with no
# row under the `### Test coverage map` heading fails the audit --
# mirroring the existing "detects an intentionally introduced
# unclassified root state owner" pattern for a MISSING CONTRACT-COVERAGE
# entry rather than a missing classification decision.

SYNTHETIC_INVENTORY_COVERAGE_MAP_OK = """\
# Fake inventory

### Save components

| Component DTO | ComponentId | Classification |
|---|---|---|
| `RegisteredDTO` | `registered-comp` | Persist exactly |

### Lua persistence registry

| Module | Owner | Scope | Classification |
|---|---|---|---|
| `unit_ai` | fake.lua | global | Persist exactly |

### Test coverage map

| Component | Canonical inspection path | Round-trip assertion | Reset/rebuild assertion | Focused test |
|---|---|---|---|---|
| `registered-comp` | fake path | fake probe | — | fake test |
| `lua.unit_ai` | fake path | fake probe | — | fake test |
"""

SYNTHETIC_INVENTORY_COVERAGE_MAP_MISSING_COMPONENT = """\
# Fake inventory

### Save components

| Component DTO | ComponentId | Classification |
|---|---|---|
| `RegisteredDTO` | `registered-comp` | Persist exactly |

### Test coverage map

| Component | Canonical inspection path | Round-trip assertion | Reset/rebuild assertion | Focused test |
|---|---|---|---|---|
"""

SYNTHETIC_INVENTORY_COVERAGE_MAP_MISSING_LUA = """\
# Fake inventory

### Lua persistence registry

| Module | Owner | Scope | Classification |
|---|---|---|---|
| `unit_ai` | fake.lua | global | Persist exactly |

### Test coverage map

| Component | Canonical inspection path | Round-trip assertion | Reset/rebuild assertion | Focused test |
|---|---|---|---|---|
"""

# A rebuilt/reset/excluded owner needs no coverage-map row either.
SYNTHETIC_INVENTORY_COVERAGE_MAP_RESET_OK = """\
# Fake inventory

### Save components

| Component DTO | ComponentId | Classification |
|---|---|---|
| `GhostDTO` | `ghost-comp` | Reset to default |

### Lua persistence registry

| Module | Owner | Scope | Classification |
|---|---|---|---|
| `unit_resources` | fake.lua | global | Reset to default |

### Test coverage map

| Component | Canonical inspection path | Round-trip assertion | Reset/rebuild assertion | Focused test |
|---|---|---|---|---|
"""


def test_coverage_map_check_accepts_fully_covered_components():
    from persistence_inventory_audit_policy import (  # type: ignore
        find_coverage_map_violations)
    v = find_coverage_map_violations(SYNTHETIC_INVENTORY_COVERAGE_MAP_OK)
    expect(v == [],
           f"a persistent Haskell component and a persistent Lua module "
           f"both documented in the Test coverage map pass, got {v}")


def test_coverage_map_check_flags_missing_haskell_component_row():
    from persistence_inventory_audit_policy import (  # type: ignore
        find_coverage_map_violations)
    v = find_coverage_map_violations(
        SYNTHETIC_INVENTORY_COVERAGE_MAP_MISSING_COMPONENT)
    expect(any("registered-comp" in x for x in v),
           f"a persistent save component with no Test coverage map row "
           f"fails the audit, got {v}")


def test_coverage_map_check_flags_missing_lua_module_row():
    from persistence_inventory_audit_policy import (  # type: ignore
        find_coverage_map_violations)
    v = find_coverage_map_violations(
        SYNTHETIC_INVENTORY_COVERAGE_MAP_MISSING_LUA)
    expect(any("lua.unit_ai" in x or "unit_ai" in x for x in v),
           f"a persistent Lua save module with no Test coverage map row "
           f"fails the audit, got {v}")


def test_coverage_map_check_reset_owners_need_no_row():
    from persistence_inventory_audit_policy import (  # type: ignore
        find_coverage_map_violations)
    v = find_coverage_map_violations(SYNTHETIC_INVENTORY_COVERAGE_MAP_RESET_OK)
    expect(v == [],
           f"a reset/rebuilt/excluded Haskell component or Lua module "
           f"requires no coverage-map row and does NOT fail, got {v}")


# ----- #760 registry-DERIVATION checks (round-4 review) ------------------
#
# The audit must derive its registered set from real registry membership,
# not from every `ComponentId "..."` literal that exists. These fixtures
# model that: `ghost-comp` HAS an id literal (and is documented) but is
# never added to `saveComponentRegistry`, so it must NOT be treated as
# registered.

# A registry list that registers ONE codec (regCodec) and the metadata
# component is wired by the envelope, not this list.
SYNTHETIC_REGISTRY_LIST = """\
saveComponentRegistry ∷ [RegisteredComponent]
saveComponentRegistry =
    [ registerComponent regCodec
    ]
"""

# The codec definitions: regCodec resolves to registeredComponentId;
# ghostCodec exists in source but is NOT in the registry list above.
SYNTHETIC_CODEC_SOURCE = """\
regCodec ∷ ComponentCodec RegDTO
regCodec = componentCodec ComponentSpec
    { csComponent     = registeredComponentId
    , csVersion       = 1
    , csRequired      = True
    , csDeps          = []
    , csEncode        = encodeReg
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = const []
    }

ghostCodec ∷ ComponentCodec GhostDTO
ghostCodec = componentCodec ComponentSpec
    { csComponent     = ghostComponentId
    , csVersion       = 1
    , csRequired      = True
    , csDeps          = []
    , csEncode        = encodeGhost
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = const []
    }
"""

# Both id identifiers are DEFINED here -- a naive literal grep would treat
# BOTH as registered. Only registeredComponentId is actually wired.
SYNTHETIC_ID_TYPES = """\
registeredComponentId ∷ ComponentId
registeredComponentId = ComponentId "registered-comp"
ghostComponentId ∷ ComponentId
ghostComponentId = ComponentId "ghost-comp"
metadataComponentId ∷ ComponentId
metadataComponentId = ComponentId "metadata"
"""

# The envelope wires in the metadata component as a direct spec tuple,
# exactly like the real World.Save.Envelope.
SYNTHETIC_ENVELOPE = """\
encodeSessionSnapshot meta snap =
    let metaSpec = (metadataComponentId, metadataComponentVersion, True
                   , S.encode meta)
        specs    = metaSpec : encodeComponentSpecs snap
    in encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion specs
"""


def test_derive_registered_ids_traces_real_membership_not_literals():
    from persistence_inventory_audit_policy import (  # type: ignore
        derive_registered_component_ids)
    ids = derive_registered_component_ids(
        SYNTHETIC_REGISTRY_LIST, SYNTHETIC_CODEC_SOURCE, SYNTHETIC_ID_TYPES,
        SYNTHETIC_ENVELOPE)
    # regCodec -> registeredComponentId -> "registered-comp"; envelope
    # -> "metadata". ghost-comp is DEFINED but never registered.
    expect(ids == {"registered-comp", "metadata"},
           f"the derived set traces registry membership (registered-comp + "
           f"the envelope's metadata) and EXCLUDES the defined-but-"
           f"unregistered ghost-comp, got {ids}")


def test_derive_registered_ids_refuses_an_unresolvable_registered_codec():
    """#1087 (PR review round 1): COMPONENT_CODEC_FILES used to be a
    hand-maintained 3-file tuple, so a component declared in a NEW file
    under the same directory resolved to nothing and was SILENTLY dropped
    from the registered set -- taking its required `### Save components`
    row with it, with no violation reported anywhere. The file list is
    now globbed, and an unresolvable codec fails loudly instead of being
    skipped, so even a component defined somewhere else entirely cannot
    vanish."""
    from persistence_inventory_audit_policy import (  # type: ignore
        derive_registered_component_ids)
    registry_with_unknown_codec = (
        SYNTHETIC_REGISTRY_LIST.replace(
            "]", "    , registerComponent codecInAnotherFile\n    ]"))
    try:
        derive_registered_component_ids(
            registry_with_unknown_codec, SYNTHETIC_CODEC_SOURCE,
            SYNTHETIC_ID_TYPES, SYNTHETIC_ENVELOPE)
    except ValueError as e:
        expect("codecInAnotherFile" in str(e),
               f"the raised error names the unresolvable codec, got {e}")
    else:
        expect(False,
               "a registered codec with no discoverable definition must "
               "raise, not be silently skipped")


def test_derive_registered_ids_excludes_defined_but_unregistered_and_audit_flags_it():
    """The exact round-4 gap: an id literal defined + documented persistent
    but NOT wired into saveComponentRegistry must be flagged."""
    from persistence_inventory_audit_policy import (  # type: ignore
        derive_registered_component_ids, find_component_registration_violations)
    registered = derive_registered_component_ids(
        SYNTHETIC_REGISTRY_LIST, SYNTHETIC_CODEC_SOURCE, SYNTHETIC_ID_TYPES,
        SYNTHETIC_ENVELOPE)
    expect("ghost-comp" not in registered,
           f"a defined-but-unregistered ComponentId is NOT in the derived "
           f"registered set, got {registered}")
    # The inventory documents ghost-comp as a persistent component; because
    # it is not truly registered, the linkage check must flag it -- which a
    # literal-grep audit (round-4 bug) would have missed.
    inventory = (
        "# Fake inventory\n\n### Save components\n\n"
        "| Component DTO | ComponentId | Classification |\n"
        "|---|---|---|\n"
        "| `RegDTO` | `registered-comp` | Persist exactly |\n"
        "| `MetaDTO` | `metadata` | Persist exactly |\n"
        "| `GhostDTO` | `ghost-comp` | Persist exactly |\n")
    v = find_component_registration_violations(inventory, registered)
    expect(any("ghost-comp" in x for x in v),
           f"a documented-persistent-but-unregistered component is flagged, "
           f"got {v}")

# ----- #2124 ownership split ---------------------------------------------
#
# The audit is one public façade over three implementation owners plus a
# data-only leaf. Every group above pins BEHAVIOUR through whichever
# module owns it; these pin the STRUCTURE the split promised: scanners
# are filesystem-free leaves, the policy owner imports only the common
# leaf, nothing imports the façade, the canonical record parser is one
# object shared with the EngineEnv audit, the façade reads each
# repository input exactly once and hands out read-only views, and the
# aggregate check order is the contracted one.

# `tools/`, resolved once by the shared support module -- from inside this
# package `Path(__file__).parent` is the package directory, not the
# directory the owner modules being read actually live in.
_TOOLS_DIR = support.TOOLS_DIR
_FACADE_MODULE = "persistence_inventory_audit"
# owner module -> the complete set of modules it may import.
_OWNER_IMPORT_ALLOWLIST = {
    "persistence_inventory_audit_common": {"__future__", "pathlib"},
    "persistence_inventory_audit_haskell": {"__future__", "re", "collections.abc"},
    "persistence_inventory_audit_lua": {"__future__", "re", "collections.abc"},
    "persistence_inventory_audit_policy": {
        "__future__", "re", "collections.abc", "persistence_inventory_audit_common"},
}
# Modules that must never read the repository themselves (requirement 12
# for the two scanners; the policy owner consumes extracted facts).
_FILESYSTEM_FREE_OWNERS = (
    "persistence_inventory_audit_haskell",
    "persistence_inventory_audit_lua",
    "persistence_inventory_audit_policy",
)
_FILESYSTEM_CALL_NAMES = {
    "open", "read_text", "read_bytes", "glob", "rglob", "iterdir", "listdir",
    "walk", "scandir",
}


def _module_ast(module_name: str):
    import ast
    return ast.parse((_TOOLS_DIR / f"{module_name}.py").read_text(encoding="utf-8"))


def _imported_modules(module_name: str) -> set[str]:
    import ast
    names: set[str] = set()
    for node in ast.walk(_module_ast(module_name)):
        if isinstance(node, ast.Import):
            names.update(alias.name for alias in node.names)
        elif isinstance(node, ast.ImportFrom):
            names.add(node.module or "")
    return names


def _filesystem_calls(module_name: str) -> list[str]:
    import ast
    calls: list[str] = []
    for node in ast.walk(_module_ast(module_name)):
        if not isinstance(node, ast.Call):
            continue
        func = node.func
        name = (func.id if isinstance(func, ast.Name)
                else func.attr if isinstance(func, ast.Attribute) else None)
        if name in _FILESYSTEM_CALL_NAMES:
            calls.append(f"{name}() at line {node.lineno}")
    return calls


def test_split_owners_import_only_their_declared_dependencies():
    """Requirement 18: scanners are leaves, policy consumes only the
    common leaf, and no extracted owner imports the façade -- so the
    dependency graph cannot grow a cycle without this failing."""
    for module, allowed in _OWNER_IMPORT_ALLOWLIST.items():
        imported = _imported_modules(module)
        expect(imported <= allowed,
               f"{module} imports only {sorted(allowed)}, got {sorted(imported)}")
        expect(_FACADE_MODULE not in imported,
               f"{module} does not import the façade {_FACADE_MODULE}")
    facade_imports = _imported_modules(_FACADE_MODULE)
    expect(set(_OWNER_IMPORT_ALLOWLIST) <= facade_imports,
           f"the façade composes every owner, got {sorted(facade_imports)}")


def test_scanner_and_policy_owners_never_read_the_filesystem():
    """Requirement 12: source scanners receive text and return facts.
    Repository reads are the façade's alone (`_load_repo_state`) and the
    common leaf's one import-time glob, so a `read_text`/`glob`/`open`
    call appearing in any owner is a scanner growing a second loader."""
    for module in _FILESYSTEM_FREE_OWNERS:
        calls = _filesystem_calls(module)
        expect(not calls, f"{module} performs no filesystem call, got {calls}")
    facade_calls = _filesystem_calls(_FACADE_MODULE)
    expect(facade_calls,
           "the façade is where repository reads live, so it does call the "
           "filesystem (the mutation that moves loading into an owner would "
           "empty this)")


def test_facade_reexports_the_single_canonical_record_parser():
    """Requirements 5 and 23: `persistence_inventory_audit.extract_record_fields`
    stays import-compatible, and it, the Haskell owner, and the EngineEnv
    capability audit's substrate all bind the ONE parser object -- a copy
    in any of them would be a second notion of "the live field set"."""
    import persistence_inventory_audit as facade  # type: ignore
    import persistence_inventory_audit_haskell as haskell  # type: ignore
    import engine_env_capability_common as engine_env_common  # type: ignore
    expect(facade.extract_record_fields is haskell.extract_record_fields,
           "the façade re-exports the Haskell owner's extract_record_fields "
           "rather than defining its own")
    expect(engine_env_common.extract_record_fields is haskell.extract_record_fields,
           "the EngineEnv capability audit binds the same canonical parser "
           "object")
    expect(extract_record_fields is haskell.extract_record_fields,
           "this self-test binds the canonical parser too")


def test_load_repo_state_reads_each_repository_input_once():
    """Requirement 14: one read per distinct repository input. The id-types
    file is BOTH `COMPONENT_ID_TYPES_FILE` and a member of
    `COMPONENT_CODEC_FILES`' glob of the same directory, so before #2124 it
    was read twice; a memoised reader collapses that without changing the
    separate arguments `derive_registered_component_ids` receives.
    Requirement 15: the mappings handed on are read-only views."""
    import persistence_inventory_audit_common as common  # type: ignore
    from persistence_inventory_audit import _load_repo_state  # type: ignore
    reads: dict[str, int] = {}
    original_read_text = Path.read_text

    def counting_read_text(self, *args, **kwargs):
        reads[str(self)] = reads.get(str(self), 0) + 1
        return original_read_text(self, *args, **kwargs)

    Path.read_text = counting_read_text  # type: ignore[method-assign]
    try:
        record_sources, scripts_text_by_file, inventory_text, registered_ids, \
            component_sources = _load_repo_state()
    finally:
        Path.read_text = original_read_text  # type: ignore[method-assign]

    repeated = sorted(path for path, count in reads.items() if count > 1)
    expect(not repeated,
           f"every repository input is read exactly once, got repeats: {repeated}")
    id_types_path = str(common.REPO_ROOT / common.COMPONENT_ID_TYPES_FILE)
    expect(reads.get(id_types_path) == 1,
           f"the id-types file (also matched by the codec-file glob) is read "
           f"once, got {reads.get(id_types_path)}")
    expect(common.COMPONENT_ID_TYPES_FILE in component_sources,
           "the id-types file is still a member of component_sources, so the "
           "dedup reused one read rather than dropping an input")
    expect(str(common.INVENTORY_PATH) in reads and inventory_text,
           "the inventory document was read through the same accounting")

    for name, mapping in (("record_sources", record_sources),
                          ("scripts_text_by_file", scripts_text_by_file),
                          ("component_sources", component_sources)):
        mutable = True
        try:
            mapping["__probe__"] = ""  # type: ignore[index]
        except TypeError:
            mutable = False
        expect(not mutable, f"{name} is handed on as a read-only view")
    expect(isinstance(registered_ids, frozenset),
           f"registered_ids is a frozenset, got {type(registered_ids).__name__}")


# One fixture per violation category, so a single audit() run yields at
# least one violation of every family and their relative order is
# observable. The inventory classifies two of the three EngineEnv fields
# (so `fieldThree` is the root-owner offender), documents a persistent
# component with no coverage-map row, and has no Lua-registry, typed-
# reference or reference-kind headings at all.
_CHECK_ORDER_INVENTORY = """\
# Fake inventory

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldTwo` | Exclude |

### Save components

| Component | ComponentId | Classification |
|---|---|---|
| `RegDTO` | `registered-comp` | Persist exactly |

### Test coverage map

| Component | Canonical inspection path |
|---|---|
"""
_CHECK_ORDER_LUA = {
    "scripts/order_a_register.lua":
        'local saveMods = require("scripts.lib.save_modules")\n'
        'saveMods.register("unlisted_mod", nil, nil)\n',
    "scripts/order_b_alias.lua":
        'local saveMods = require("scripts.lib.save_modules")\n'
        'local reg = saveMods.register\n',
    "scripts/order_c_dynamic.lua":
        'local saveMods = require("scripts.lib.save_modules")\n'
        'saveMods.register("dyn_" .. "mod", nil, nil)\n',
    "scripts/order_d_untracked.lua":
        'local registry = require("scripts.lib.save_modules")\n',
    "scripts/order_e_kinds.lua":
        'local M = {}\n'
        'M.references = function(data)\n'
        '  return { { kind = "widget", id = data.id } }\n'
        'end\n'
        'return M\n',
}
_CHECK_ORDER_COMPONENT_SOURCES = {
    "src/World/Save/Component/Fake.hs":
        "module Fake where\n\n"
        "data FakeDTO = FakeDTO\n"
        "  { fakeRef ∷ !(SamePageRef UnitId)\n"
        "  }\n",
}
# Requirement 16's nine check families, in the contracted order, each
# identified by a phrase only ITS diagnostics carry.
_CHECK_ORDER_MARKERS = (
    ("root-owner fields", "has no classification under the '### EngineEnv'"),
    ("Lua save modules", 'Lua save module "'),
    ("aliased register functions", "without calling it directly"),
    ("dynamic registration names", "module-name argument that isn't a complete"),
    ("untracked registry aliases", "aliases the save-modules registry table"),
    ("component registration", "registered save component"),
    ("coverage-map correspondence", "'### Test coverage map'"),
    ("typed Haskell references", 'Typed persistent reference field "'),
    ("Lua reference kinds", 'Lua reference kind "'),
)


def test_audit_reports_every_check_family_in_the_contracted_order():
    """Requirement 16: root-owner fields, Lua save modules, aliased
    register functions, dynamic registration names, untracked registry
    aliases, component registration, coverage-map correspondence, typed
    Haskell references, Lua reference kinds. Swapping any two families in
    `audit()` reorders the violation list and fails here."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV}, _CHECK_ORDER_LUA,
        _CHECK_ORDER_INVENTORY, root_records=FAKE_ROOT_RECORDS,
        registered_ids={"registered-comp", "undocumented-comp"},
        component_sources=_CHECK_ORDER_COMPONENT_SOURCES)
    families: list[int] = []
    for v in violations:
        matched = [i for i, (_, marker) in enumerate(_CHECK_ORDER_MARKERS)
                   if marker in v]
        expect(len(matched) == 1,
               f"each violation belongs to exactly one check family, "
               f"{v!r} matched {matched}")
        families.extend(matched)
    seen = {_CHECK_ORDER_MARKERS[i][0] for i in families}
    missing = [name for name, _ in _CHECK_ORDER_MARKERS if name not in seen]
    expect(not missing,
           f"the fixture exercises every check family, missing {missing} "
           f"in {violations}")
    expect(families == sorted(families),
           f"violations are reported in the contracted family order, got "
           f"{[_CHECK_ORDER_MARKERS[i][0] for i in families]}")


# ----- Registry ----------------------------------------------------------
#: The 16 groups this family owns, in the aggregate's order. One block,
#: and the last of the run.
TESTS = (
    test_component_check_accepts_registered_persistent_owner,
    test_component_check_flags_unregistered_persistent_owner,
    test_component_check_reset_owner_needs_no_registration,
    test_component_check_flags_registered_component_missing_a_row,
    test_coverage_map_check_accepts_fully_covered_components,
    test_coverage_map_check_flags_missing_haskell_component_row,
    test_coverage_map_check_flags_missing_lua_module_row,
    test_coverage_map_check_reset_owners_need_no_row,
    test_derive_registered_ids_traces_real_membership_not_literals,
    test_derive_registered_ids_refuses_an_unresolvable_registered_codec,
    test_derive_registered_ids_excludes_defined_but_unregistered_and_audit_flags_it,
    test_split_owners_import_only_their_declared_dependencies,
    test_scanner_and_policy_owners_never_read_the_filesystem,
    test_facade_reexports_the_single_canonical_record_parser,
    test_load_repo_state_reads_each_repository_input_once,
    test_audit_reports_every_check_family_in_the_contracted_order,
)
