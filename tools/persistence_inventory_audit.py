#!/usr/bin/env python3
"""Persistence-inventory audit (issue #756, persistence contract req 10).

Guards docs/persistence_state_inventory.md against silent drift. Per
docs/persistence_contract.md SS2, a "root state owner" is a field on one
of the aggregator records everything else hangs off (EngineEnv,
EngineState, WorldManager, WorldState, and the World.Save.Types
envelope), a field on one of the three gameplay managers EngineEnv
reaches through a bare IORef pointer (UnitManager, BuildingManager,
UnitThreadState -- see ROOT_RECORDS, added by #1703), or a Lua module
registered with scripts/lib/save_modules.lua.
Every such field/module must have a classification entry in the
inventory doc: a backtick-quoted name in the first column of one of its
markdown tables, under the SAME `### OwnerName` heading that owns that
field/module (see ROOT_RECORDS/LUA_OWNER_HEADING below) -- classification
is scoped PER OWNER, not per name or per numbered section, so a field on
one record can't be "classified" by sheer coincidence of sharing a name
with an unrelated Lua module or a field on a DIFFERENT record that
happens to share the same `## N.` section (e.g. WorldManager and
WorldState both live under "## 3.", but each gets its own "### " heading
so a name collision between them still can't mask a missing decision).
The row's own Classification cell must also contain one of the five
taxonomy labels (VALID_CLASSIFICATIONS) -- a bare placeholder like "—"
is name-presence without an actual decision, and is rejected too.

This is a static presence/well-formedness check, not a serialization-
correctness proof (it cannot verify a field classified "Persist exactly"
is actually wired into the save/load path) -- see
docs/persistence_contract.md SS7 for what it does and does not
guarantee. Its job is narrower and mechanical: nothing gets ADDED to a
root owner or the Lua save registry without an explicit classification
decision landing alongside it.

Internal owners (issue #2124). This file is the public façade and the
composition boundary, not the implementation: it declares the audited
root owners (ROOT_RECORDS), loads every repository input exactly once
(`_load_repo_state`), composes the owners below in the fixed check
order `audit()` documents, and prints the report. The three source-
language and policy contracts each have one owner, and the dependency
direction is one way only:

  * tools/persistence_inventory_audit_common.py -- data-only leaf:
    repository paths and source scope. Imported by the façade and the
    policy owner; imports nothing local.
  * tools/persistence_inventory_audit_haskell.py -- comment/literal-
    aware Haskell record-field extraction and typed persistent-
    reference discovery. A pure leaf (source text in, facts out). Its
    `extract_record_fields` is the single canonical record parser; the
    EngineEnv capability audit (tools/engine_env_capability_common.py
    and its self-test) imports it from that owner, and it is also
    RE-EXPORTED here under the same name as a compatibility contract
    for any consumer that still names this module.
  * tools/persistence_inventory_audit_lua.py -- Lua comment/string
    handling, save-module registration discovery, registry-alias
    detection and Lua reference-kind discovery. A pure leaf.
  * tools/persistence_inventory_audit_policy.py -- the inventory
    document's classification parser and taxonomy, the save-component
    owner rows, registered-component derivation, and the coverage-map
    check. Consumes extracted facts; imports only the common leaf.

No extracted owner imports this façade, and no scanner reads the
repository: every file is read once here and handed on as an immutable
mapping.

Usage:
  python3 tools/persistence_inventory_audit.py
Exit codes: 0 = every root-owner field and Lua module has a valid
classification, 1 = one or more are missing or invalid.
"""
from __future__ import annotations

from collections.abc import Mapping, Set as AbstractSet
from types import MappingProxyType

import persistence_inventory_audit_common as common
# The canonical Haskell record parser, re-exported under this module's
# name as a compatibility contract (#2124 requirement 5): a consumer
# importing `extract_record_fields` from here binds the SAME object the
# EngineEnv capability audit binds from the owner, so both audits
# derive "the live field set" through ONE parser.
from persistence_inventory_audit_haskell import (  # noqa: F401
    extract_record_fields, find_typed_reference_fields)
from persistence_inventory_audit_lua import (
    extract_lua_registered_modules, find_lua_reference_kinds,
    find_lua_register_aliases, find_lua_register_dynamic_names,
    find_untracked_registry_aliases)
from persistence_inventory_audit_policy import (
    LUA_OWNER_HEADING, LUA_REFERENCE_KIND_OWNER, REFERENCE_FIELD_OWNER,
    VALID_CLASSIFICATIONS, derive_registered_component_ids,
    find_component_registration_violations, find_coverage_map_violations,
    is_valid_classification, parse_classified_names)

# (label, file relative to repo root, regex matching the record's
# `data X = X` line). `label` doubles as the exact `### label` heading
# text the inventory doc must use to classify this record's fields --
# see OWNER_HEADING_RE / parse_classified_names.
#
# These are DIRECTLY scanned owners, not a transitive closure: the audit
# reads exactly these records' own field lists. Records reached THROUGH
# one of them (UnitInstance via umInstances, BuildingInstance via
# bmInstances, UnitSimState via utsSimStates) are covered separately by
# the inventory's own sections and stay out of scope here, as does the
# worker-thread state nothing on EngineEnv/WorldState reaches at all
# (inventory SS6).
ROOT_RECORDS: list[tuple[str, str, str]] = [
    ("EngineEnv", "src/Engine/Core/State.hs", r"^data EngineEnv = EngineEnv\b"),
    ("EngineState", "src/Engine/Core/State.hs", r"^data EngineState = EngineState\b"),
    ("WorldManager", "src/World/State/Types.hs", r"^data WorldManager = WorldManager\b"),
    ("WorldState", "src/World/State/Types.hs", r"^data WorldState = WorldState\b"),
    # The three gameplay managers EngineEnv reaches through a bare
    # IORef pointer (#1703). Each pointer field is itself classified on
    # EngineEnv as `Rebuild`, delegating the real decisions onto these
    # records' own fields -- so without them here, a field added inside
    # an already-reachable manager would land with every gate green and
    # no persistence decision recorded, which is exactly the drift this
    # audit exists to stop.
    ("UnitManager", "src/Unit/Types/Manager.hs", r"^data UnitManager = UnitManager\b"),
    ("BuildingManager", "src/Building/Types.hs", r"^data BuildingManager = BuildingManager\b"),
    ("UnitThreadState", "src/Unit/Sim/Types.hs", r"^data UnitThreadState = UnitThreadState\b"),
    ("SaveHeader", "src/World/Save/Types.hs", r"^data SaveHeader = SaveHeader\b"),
    ("SaveMetadata", "src/World/Save/Types.hs", r"^data SaveMetadata = SaveMetadata\b"),
    ("WorldPageSave", "src/World/Save/Types.hs", r"^data WorldPageSave = WorldPageSave\b"),
    ("SaveData", "src/World/Save/Types.hs", r"^data SaveData = SaveData\b"),
]


def audit(record_sources: Mapping[str, str],
          scripts_text_by_file: Mapping[str, str],
          inventory_text: str,
          root_records: list[tuple[str, str, str]] | None = None,
          registered_ids: AbstractSet[str] | None = None,
          component_sources: Mapping[str, str] | None = None) -> list[str]:
    """Pure audit core. Returns a list of human-readable violations.

    The check ORDER is a contract (#2124 requirement 16): root-owner
    fields, Lua save modules, aliased register functions, dynamic
    registration names, untracked registry aliases, component
    registration, coverage-map correspondence, typed Haskell
    references, Lua reference kinds. `registered_ids` and
    `component_sources` are optional: when either is None its check
    family is skipped entirely, which is what lets the fixture-driven
    self-test groups exercise the others in isolation.
    """
    if root_records is None:
        root_records = ROOT_RECORDS
    classified = parse_classified_names(inventory_text)
    violations: list[str] = []

    for label, relpath, pattern in root_records:
        source = record_sources.get(relpath)
        if source is None:
            violations.append(f"{label}: source not provided for {relpath}")
            continue
        try:
            fields = extract_record_fields(source, pattern)
        except ValueError as exc:
            violations.append(f"{label}: {exc}")
            continue
        if not fields:
            violations.append(
                f"{label}: no fields extracted from {relpath} -- the parser "
                f"may be out of sync with this record's layout")
            continue
        classified_here = classified.get(label, {})
        for field in fields:
            if field not in classified_here:
                violations.append(
                    f"{label}.{field} ({relpath}) has no classification under "
                    f"the '### {label}' heading in {common.INVENTORY_PATH.name}")
            elif not is_valid_classification(classified_here[field]):
                violations.append(
                    f"{label}.{field} ({relpath})'s classification "
                    f"{classified_here[field]!r} under the '### {label}' "
                    f"heading in {common.INVENTORY_PATH.name} is not one of "
                    f"{VALID_CLASSIFICATIONS}")

    classified_lua = classified.get(LUA_OWNER_HEADING, {})
    for name, relpath in extract_lua_registered_modules(scripts_text_by_file):
        if name not in classified_lua:
            violations.append(
                f'Lua save module "{name}" (registered in {relpath}) has no '
                f"classification under the '### {LUA_OWNER_HEADING}' heading "
                f"in {common.INVENTORY_PATH.name}")
        elif not is_valid_classification(classified_lua[name]):
            violations.append(
                f'Lua save module "{name}" (registered in {relpath})\'s '
                f"classification {classified_lua[name]!r} under the "
                f"'### {LUA_OWNER_HEADING}' heading in {common.INVENTORY_PATH.name} "
                f"is not one of {VALID_CLASSIFICATIONS}")

    for relpath in find_lua_register_aliases(scripts_text_by_file):
        violations.append(
            f"{relpath} references saveMods.register/saveModules.register "
            f"without calling it directly (e.g. assigning it to a local "
            f"or table field) -- the audit can only trace direct calls; "
            f"call saveMods.register(...) directly instead of aliasing it")

    for relpath in find_lua_register_dynamic_names(scripts_text_by_file):
        violations.append(
            f"{relpath} calls saveMods.register/saveModules.register with "
            f"a module-name argument that isn't a complete, standalone "
            f"string/long-bracket literal (e.g. a concatenation or other "
            f"expression) -- the audit can only trace a plain literal "
            f"name; pass the module name as one literal string instead "
            f"of a computed expression")

    for relpath in find_untracked_registry_aliases(scripts_text_by_file):
        violations.append(
            f"{relpath} aliases the save-modules registry table "
            f'(via require("scripts.lib.save_modules") or the '
            f"saveMods/saveModules name) into something other than a "
            f"direct .register()/[\"register\"] access or a local named "
            f"exactly saveMods/saveModules -- the audit cannot trace a "
            f".register(...) call made through an arbitrarily-named "
            f"alias; use one of the two sanctioned patterns instead")

    if registered_ids is not None:
        violations.extend(
            find_component_registration_violations(inventory_text, registered_ids))

    violations.extend(find_coverage_map_violations(inventory_text))

    # Issue #764 requirement 15: a new typed-reference DTO field or Lua
    # reference kind with no documented classification fails the audit,
    # the same way a new root-owner field or Lua save module already does.
    if component_sources is not None:
        classified_refs = classified.get(REFERENCE_FIELD_OWNER, {})
        for field, relpath in find_typed_reference_fields(component_sources):
            if field not in classified_refs:
                violations.append(
                    f'Typed persistent reference field "{field}" ({relpath}) '
                    f"has no classification under the "
                    f"'### {REFERENCE_FIELD_OWNER}' heading in "
                    f"{common.INVENTORY_PATH.name}")
            elif not is_valid_classification(classified_refs[field]):
                violations.append(
                    f'Typed persistent reference field "{field}" ({relpath})\'s '
                    f"classification {classified_refs[field]!r} under the "
                    f"'### {REFERENCE_FIELD_OWNER}' heading in "
                    f"{common.INVENTORY_PATH.name} is not one of {VALID_CLASSIFICATIONS}")

    classified_kinds = classified.get(LUA_REFERENCE_KIND_OWNER, {})
    for kind, relpath in find_lua_reference_kinds(scripts_text_by_file):
        if kind not in classified_kinds:
            violations.append(
                f'Lua reference kind "{kind}" (used in {relpath}) has no '
                f"classification under the '### {LUA_REFERENCE_KIND_OWNER}' "
                f"heading in {common.INVENTORY_PATH.name}")
        elif not is_valid_classification(classified_kinds[kind]):
            violations.append(
                f'Lua reference kind "{kind}" (used in {relpath})\'s '
                f"classification {classified_kinds[kind]!r} under the "
                f"'### {LUA_REFERENCE_KIND_OWNER}' heading in "
                f"{common.INVENTORY_PATH.name} is not one of {VALID_CLASSIFICATIONS}")

    return violations


def _load_repo_state() -> tuple[Mapping[str, str], Mapping[str, str], str,
                                 AbstractSet[str], Mapping[str, str]]:
    """Read every repository input the aggregate run needs, each exactly
    once, and return them as immutable views.

    One memoised reader serves every path, so a file that two inputs
    name -- `COMPONENT_ID_TYPES_FILE` is also matched by
    `COMPONENT_CODEC_FILES`'s glob of the same directory -- is read a
    single time while `derive_registered_component_ids` still receives
    the joined codec source and the id-types source as the separate
    arguments it always has (#2124 requirement 14). The returned
    mappings are read-only views (a caller that needs a mutated copy
    takes `dict(...)` of one, as the self-test does) so no owner can
    rescan or reshape the repository state behind another's back.
    """
    cache: dict[str, str] = {}

    def read(relpath: str) -> str:
        if relpath not in cache:
            cache[relpath] = (common.REPO_ROOT / relpath).read_text(encoding="utf-8")
        return cache[relpath]

    record_sources: dict[str, str] = {}
    for _, relpath, _ in ROOT_RECORDS:
        if relpath not in record_sources:
            record_sources[relpath] = read(relpath)
    scripts_text_by_file: dict[str, str] = {}
    for path in (common.REPO_ROOT / common.SCRIPTS_DIR).rglob("*.lua"):
        rel = str(path.relative_to(common.REPO_ROOT))
        scripts_text_by_file[rel] = path.read_text(encoding="utf-8")
    inventory_text = common.INVENTORY_PATH.read_text(encoding="utf-8")
    registry_list_source = read(common.COMPONENT_REGISTRY_LIST_FILE)
    component_sources: dict[str, str] = {
        f: read(f) for f in common.COMPONENT_CODEC_FILES
    }
    codec_source = "\n".join(
        component_sources[f] for f in common.COMPONENT_CODEC_FILES)
    id_types_source = read(common.COMPONENT_ID_TYPES_FILE)
    envelope_source = read(common.COMPONENT_ENVELOPE_FILE)
    registered_ids = derive_registered_component_ids(
        registry_list_source, codec_source, id_types_source, envelope_source)
    return (MappingProxyType(record_sources), MappingProxyType(scripts_text_by_file),
            inventory_text, frozenset(registered_ids),
            MappingProxyType(component_sources))


def main() -> int:
    record_sources, scripts_text_by_file, inventory_text, registered_ids, \
        component_sources = _load_repo_state()
    violations = audit(record_sources, scripts_text_by_file, inventory_text,
                       registered_ids=registered_ids,
                       component_sources=component_sources)
    if violations:
        print(f"{len(violations)} persistence-inventory violation(s):")
        for v in violations:
            print(f"  - {v}")
        print(f"\nAdd a classification row for each item above to "
              f"{common.INVENTORY_PATH.relative_to(common.REPO_ROOT)} (see "
              f"docs/persistence_contract.md for the taxonomy).")
        return 1

    total_fields = sum(
        len(extract_record_fields(record_sources[relpath], pattern))
        for _, relpath, pattern in ROOT_RECORDS)
    total_lua = len(extract_lua_registered_modules(scripts_text_by_file))
    total_refs = len(find_typed_reference_fields(component_sources))
    total_kinds = len(find_lua_reference_kinds(scripts_text_by_file))
    print(f"persistence-inventory audit: {total_fields} root-owner fields + "
          f"{total_lua} Lua save module(s) + {total_refs} typed reference "
          f"field(s) + {total_kinds} Lua reference kind(s) all classified")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
