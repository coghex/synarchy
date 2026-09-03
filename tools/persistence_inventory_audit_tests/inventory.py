#!/usr/bin/env python3
"""Inventory-document policy: scope, taxonomy, and the real repo (#2138).

Thirteen groups in four fragments, matching the aggregate's order:

  `TESTS_CLASSIFICATION_PARSING`     4 direct `parse_classified_names`
                                     cases (owner-heading scope, column
                                     position, no cross-owner merge) plus
                                     the clean-state control;
  `TESTS_OWNER_SCOPING_AND_TAXONOMY` 5 groups proving a same-named entry
                                     under another owner does not count,
                                     and that the classification taxonomy
                                     accepts a decorated value while
                                     refusing a blank or compound one;
  `TESTS_INTENTIONALLY_UNCLASSIFIED` #756's own acceptance -- an
                                     intentionally unclassified Haskell
                                     field and Lua module are both caught;
  `TESTS_REAL_REPO`                  the end-to-end smoke test over the
                                     checked-out tree that CI runs.

Direct parsing goes through `persistence_inventory_audit_policy`, which
owns the document's rules since #2124. The real-repository group reads
the tree read-only.
"""
from __future__ import annotations

from .support import expect
from .fixtures_haskell import (
    FAKE_ROOT_RECORDS,
    SYNTHETIC_ENGINE_ENV,
)
from .fixtures_inventory import (
    SYNTHETIC_INVENTORY_COMPLETE,
    SYNTHETIC_INVENTORY_MISSING_ONE,
)
from .fixtures_lua import SYNTHETIC_LUA_REGISTER
from persistence_inventory_audit import audit  # type: ignore
from persistence_inventory_audit_policy import parse_classified_names  # type: ignore


# fieldTwo HAS a row and is present by name, but its classification cell
# is a bare em-dash placeholder -- not one of the five taxonomy values.
# This is the "accepts no classification as a classification" gap:
# name-presence alone must not be enough.
SYNTHETIC_INVENTORY_INVALID_CLASSIFICATION = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldTwo` | — |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |
"""

# fieldTwo's classification uses a valid taxonomy label wrapped in bold
# markup with a parenthetical suffix -- must still count as valid.
SYNTHETIC_INVENTORY_DECORATED_VALID_CLASSIFICATION = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldTwo` | **Exclude (new-format target differs)** |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |
"""

# fieldTwo is only classified under the Lua registry heading (as if it
# were a Lua module name), NOT under `### EngineEnv` where the real
# EngineEnv.fieldTwo lives -- this must NOT satisfy EngineEnv.fieldTwo's
# requirement, even though both owners share `## 1.`/`## 7.`'s sibling
# status under the same document.
SYNTHETIC_INVENTORY_OWNER_COLLISION = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `fieldTwo` | Persist exactly (opaque blob) |
| `unit_ai` | Persist exactly (opaque blob) |
"""

# Two DIFFERENT Haskell records sharing a field name, mirroring the
# real inventory's WorldManager/WorldState pair (both live under the
# same "## 3." numbered section but must still be scoped as separate
# owners). Only OwnerB's `shared` is classified.
SYNTHETIC_TWO_OWNERS_SHARED_FIELD_NAME = """\
module Fake where

data OwnerA = OwnerA
  { shared ∷ Int
  }

data OwnerB = OwnerB
  { shared ∷ Int
  }
"""

TWO_OWNER_ROOT_RECORDS = [
    ("OwnerA", "Fake.hs", r"^data OwnerA = OwnerA\b"),
    ("OwnerB", "Fake.hs", r"^data OwnerB = OwnerB\b"),
]

SYNTHETIC_INVENTORY_ONLY_OWNER_B_CLASSIFIED = """\
# Fake inventory

## 3. OwnerA / OwnerB (share one numbered section, like WorldManager/WorldState)

### OwnerB

| Field | Classification |
|---|---|
| `shared` | Persist exactly |
"""

# Two names sharing one classification, joined with "+" -- looks
# plausible but is not a single taxonomy label.
SYNTHETIC_INVENTORY_COMPOUND_CLASSIFICATION = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldTwo` | Rebuild + Persist (mixed) |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |
"""


def test_parse_classified_names_scoped_by_owner_heading():
    by_owner = parse_classified_names(SYNTHETIC_INVENTORY_COMPLETE)
    expect(set(by_owner.get("EngineEnv", {})) == {"fieldOne", "fieldTwo", "fieldThree"},
           f"the '### EngineEnv' heading gets exactly its own backtick-quoted "
           f"first-column names, got {by_owner.get('EngineEnv')}")
    expect(by_owner.get("EngineEnv", {}).get("fieldOne") == "Persist exactly",
           f"captures each name's own classification cell text, got "
           f"{by_owner.get('EngineEnv')}")
    expect(set(by_owner.get("Lua persistence registry", {})) == {"unit_ai"},
           f"the Lua registry heading gets exactly its own names, got "
           f"{by_owner.get('Lua persistence registry')}")


def test_parse_classified_names_ignores_other_columns():
    # A name that only appears in a NON-first column (e.g. a cross-
    # reference in "Restoration dependency") must not count as classified,
    # and it's the CLASSIFICATION column's own text that's captured, not
    # some other column's.
    text = ("### Fake\n\n"
            "| Field | Classification | Restoration dependency |\n"
            "|---|---|---|\n"
            "| `realField` | Exclude | depends on `otherField` |\n")
    by_owner = parse_classified_names(text)
    expect(set(by_owner.get("Fake", {})) == {"realField"},
           f"only the first column's names count as classified, got {by_owner.get('Fake')}")
    expect(by_owner.get("Fake", {}).get("realField") == "Exclude",
           f"the Classification column's text is captured, not a later "
           f"column's, got {by_owner.get('Fake')}")


def test_parse_classified_names_does_not_merge_across_owners():
    text = ("### OwnerX\n\n"
            "| Field | Classification |\n|---|---|\n"
            "| `shared` | Persist exactly |\n\n"
            "### OwnerY\n\n"
            "| Field | Classification |\n|---|---|\n"
            "| `shared` | Exclude |\n"
            "| `only_in_y` | Rebuild |\n")
    by_owner = parse_classified_names(text)
    expect(by_owner.get("OwnerX") == {"shared": "Persist exactly"},
           f"OwnerX keeps only its own copy of a shared name (with its own "
           f"classification text), got {by_owner.get('OwnerX')}")
    expect(by_owner.get("OwnerY") == {"shared": "Exclude", "only_in_y": "Rebuild"},
           f"OwnerY keeps its own names and classifications independently, "
           f"got {by_owner.get('OwnerY')}")


def test_parse_classified_names_finds_classification_column_at_any_index():
    # The Lua registry table's real header puts Classification 4th
    # (Module | Owner | Scope | Classification | ...), not 2nd -- the
    # parser must locate it by name, not assume a fixed position.
    text = ("### Fake\n\n"
            "| Module | Owner | Scope | Classification | Test oracle |\n"
            "|---|---|---|---|---|\n"
            "| `mod_a` | some/file.lua | global | Persist exactly | none yet |\n")
    by_owner = parse_classified_names(text)
    expect(by_owner.get("Fake") == {"mod_a": "Persist exactly"},
           f"finds Classification wherever it sits in the header, got {by_owner.get('Fake')}")


def test_audit_clean_repo_state_has_no_violations():
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": SYNTHETIC_LUA_REGISTER},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not violations,
           f"a fully classified fixture reports no violations at all, got {violations}")


def test_audit_does_not_let_a_same_named_entry_under_another_owner_count():
    """Regression for the owner-scoping gap: a Lua-module row happening
    to be named `fieldTwo` must not satisfy the classification
    requirement for the UNRELATED EngineEnv.fieldTwo -- they are
    different owners and need independent decisions, even though both
    live in the same document."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_OWNER_COLLISION,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("EngineEnv.fieldTwo" in v for v in violations),
           f"EngineEnv.fieldTwo is still reported unclassified even though "
           f"a same-named row exists under the Lua registry heading, got {violations}")


def test_audit_does_not_let_a_sibling_record_in_the_same_numbered_section_count():
    """Regression for the reviewer's exact WorldManager/WorldState
    scenario: two DIFFERENT records sharing a `## N.` numbered section
    (but each with its own `### OwnerName` heading) must not let one's
    classified field satisfy the other's requirement for a same-named
    field."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_TWO_OWNERS_SHARED_FIELD_NAME},
        {},
        SYNTHETIC_INVENTORY_ONLY_OWNER_B_CLASSIFIED,
        root_records=TWO_OWNER_ROOT_RECORDS,
    )
    expect(any("OwnerA.shared" in v for v in violations),
           f"OwnerA.shared is reported unclassified even though OwnerB.shared "
           f"(a sibling record under the same numbered section, same field "
           f"name) IS classified, got {violations}")
    expect(not any("OwnerB.shared" in v for v in violations),
           f"OwnerB.shared, which IS classified under its own heading, is "
           f"not falsely reported, got {violations}")


def test_audit_rejects_a_blank_placeholder_as_a_classification():
    """Regression for the "no classification counts as a classification"
    gap: a row whose NAME is present but whose classification cell is a
    bare '—' placeholder (none of the five taxonomy values) must still
    be reported -- name-presence alone is not enough."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_INVALID_CLASSIFICATION,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("EngineEnv.fieldTwo" in v and "not one of" in v for v in violations),
           f"fieldTwo's blank '—' classification is reported as invalid, "
           f"got {violations}")
    expect(not any("EngineEnv.fieldOne" in v or "EngineEnv.fieldThree" in v
                    for v in violations),
           f"fields with a real taxonomy classification are not falsely "
           f"reported, got {violations}")


def test_audit_accepts_a_decorated_valid_classification():
    """A valid taxonomy label wrapped in bold markup with a trailing
    parenthetical aside (e.g. the real inventory's '**Exclude
    (new-format target differs)**' rows) must still count as valid --
    the check strips that decoration down to a CORE value and requires
    the core to equal one of the five canonical labels exactly."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_DECORATED_VALID_CLASSIFICATION,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("EngineEnv.fieldTwo" in v for v in violations),
           f"a bold-wrapped, parenthetical-suffixed but still-valid "
           f"classification is accepted, got {violations}")


def test_audit_rejects_a_compound_classification():
    """Regression for the "more than one label at once" gap: a value
    like 'Rebuild + Persist (mixed)' looks plausible but its CORE
    (after stripping the trailing parenthetical) is 'Rebuild + Persist'
    -- not a single canonical label -- so it must be rejected. A plain
    substring test would have missed this, since "Persist exactly"
    isn't literally present in the text."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_COMPOUND_CLASSIFICATION,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("EngineEnv.fieldTwo" in v and "not one of" in v for v in violations),
           f"a compound 'Rebuild + Persist (mixed)' classification is "
           f"rejected as not a single taxonomy label, got {violations}")


def test_audit_detects_intentionally_unclassified_field():
    """The req-10 acceptance test: introduce an unclassified root-owner
    field (by using the inventory with one entry deleted) and confirm the
    audit reports it by name."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_MISSING_ONE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("fieldTwo" in v for v in violations),
           f"an unclassified field (fieldTwo, deliberately dropped from the "
           f"fixture inventory) is reported, got {violations}")
    expect(not any("fieldOne" in v or "fieldThree" in v for v in violations),
           f"fields that ARE classified are not falsely reported, got {violations}")


def test_audit_detects_intentionally_unclassified_lua_module():
    """The req-10 acceptance test, Lua half: a registered save module
    absent from the inventory is reported by name."""
    lua_source = SYNTHETIC_LUA_REGISTER + '\nsaveMods.register("new_untracked_module", nil, nil)\n'
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake.lua": lua_source},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("new_untracked_module" in v for v in violations),
           f"a newly registered, unclassified Lua module is reported, got {violations}")
    expect(not any('"unit_ai"' in v for v in violations),
           f"the already-classified unit_ai module is not falsely reported, got {violations}")


def test_audit_against_the_real_repo():
    """End-to-end smoke test against the actual checked-out inventory and
    source files -- this is what CI/make ci actually runs via main()."""
    from persistence_inventory_audit import _load_repo_state  # type: ignore
    record_sources, scripts_text_by_file, inventory_text, registered_ids, \
        component_sources = _load_repo_state()
    violations = audit(record_sources, scripts_text_by_file, inventory_text,
                       registered_ids=registered_ids,
                       component_sources=component_sources)
    expect(not violations,
           f"the real repo's inventory has no unclassified root-owner fields, "
           f"Lua save modules, unregistered persistent save components, typed "
           f"reference fields, or Lua reference kinds, got {violations}")


# ----- Registry ----------------------------------------------------------
#: The 13 groups this family owns, in the aggregate's order. Four
#: fragments: this family is the most interleaved of the six, its blocks
#: separated by the Haskell field mutations, the two Lua-audit blocks and
#: the reference groups.

#: 4 direct `parse_classified_names` cases plus the clean-state control
#: that proves a fully classified inventory raises nothing.
TESTS_CLASSIFICATION_PARSING = (
    test_parse_classified_names_scoped_by_owner_heading,
    test_parse_classified_names_ignores_other_columns,
    test_parse_classified_names_does_not_merge_across_owners,
    test_parse_classified_names_finds_classification_column_at_any_index,
    test_audit_clean_repo_state_has_no_violations,
)

#: 5 groups: a same-named entry under another owner and a sibling record
#: in the same numbered section must not satisfy a classification, and the
#: taxonomy accepts a decorated value while refusing a blank or compound one.
TESTS_OWNER_SCOPING_AND_TAXONOMY = (
    test_audit_does_not_let_a_same_named_entry_under_another_owner_count,
    test_audit_does_not_let_a_sibling_record_in_the_same_numbered_section_count,
    test_audit_rejects_a_blank_placeholder_as_a_classification,
    test_audit_accepts_a_decorated_valid_classification,
    test_audit_rejects_a_compound_classification,
)

#: #756's own acceptance criterion, one group per language.
TESTS_INTENTIONALLY_UNCLASSIFIED = (
    test_audit_detects_intentionally_unclassified_field,
    test_audit_detects_intentionally_unclassified_lua_module,
)

#: The end-to-end smoke test over the checked-out tree, read-only.
TESTS_REAL_REPO = (
    test_audit_against_the_real_repo,
)

#: The family's complete inventory: its fragments, in their own order.
TESTS = TESTS_CLASSIFICATION_PARSING + TESTS_OWNER_SCOPING_AND_TAXONOMY + TESTS_INTENTIONALLY_UNCLASSIFIED + TESTS_REAL_REPO
