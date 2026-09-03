#!/usr/bin/env python3
"""Typed Haskell and Lua persistent references, #764 (#2138).

Six groups: `find_typed_reference_fields` finding `SamePageRef` and its
`Maybe`-wrapped form directly, the audit enforcing a classification for
one, and the audit omitting the check entirely when no component source
is supplied; then `find_lua_reference_kinds` over a literal and a
helper-call shape, the same following a delegated helper module, and the
audit enforcing a reference kind.

Discovery is `persistence_inventory_audit_haskell`'s and
`persistence_inventory_audit_lua`'s; enforcement is the composed
`audit()`'s, so both are exercised where they are owned.
"""
from __future__ import annotations

from .support import expect
from .fixtures_haskell import (
    FAKE_ROOT_RECORDS,
    SYNTHETIC_ENGINE_ENV,
)
from .fixtures_inventory import SYNTHETIC_INVENTORY_COMPLETE
from persistence_inventory_audit import audit  # type: ignore
from persistence_inventory_audit_haskell import find_typed_reference_fields  # type: ignore
from persistence_inventory_audit_lua import find_lua_reference_kinds  # type: ignore


# ----- #764 requirement 15: typed persistent references ------------------

SYNTHETIC_COMPONENT_DTO = """\
module Fake.Component where

data FakeBillDTO = FakeBillDTO
    { fbdId      ∷ !Int
    , fbdStation ∷ !(SamePageRef BuildingId)
    , fbdOwner   ∷ !(Maybe (SamePageRef UnitId))
    } deriving (Show, Eq)
"""

SYNTHETIC_REFERENCE_INVENTORY = """\
# Fake inventory

## 1. EngineEnv fields

### EngineEnv

| Field | Classification |
|---|---|
| `fieldOne` | Persist exactly |
| `fieldTwo` | Exclude |
| `fieldThree` | Exclude |

## 7. Lua persistence registry

### Lua persistence registry

| Field | Classification |
|---|---|
| `unit_ai` | Persist exactly (opaque blob) |

## 11. Typed persistent references

### Typed persistent references

| Field | Classification |
|---|---|
| `fbdStation` | Persist as identity/reference |
"""


def test_find_typed_reference_fields_detects_samepageref_and_maybe_wrapped():
    """The req-2/12 detector: a bare SamePageRef field and a
    Maybe-wrapped one are both found; a plain (unwrapped) field is not."""
    found = find_typed_reference_fields({"Fake/Component.hs": SYNTHETIC_COMPONENT_DTO})
    names = [f for f, _ in found]
    expect("fbdStation" in names,
           f"a bare SamePageRef field is detected, got {found}")
    expect("fbdOwner" in names,
           f"a Maybe-wrapped SamePageRef field is detected, got {found}")
    expect("fbdId" not in names,
           f"a plain (unwrapped) field is NOT flagged as a reference, got {found}")


def test_audit_detects_unclassified_typed_reference_field():
    """The req-15 acceptance test, Haskell half: a DTO field typed
    SamePageRef with no classification row is reported by name; one
    that DOES have a row is not."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_REFERENCE_INVENTORY,
        root_records=FAKE_ROOT_RECORDS,
        component_sources={"Fake/Component.hs": SYNTHETIC_COMPONENT_DTO},
    )
    expect(any("fbdOwner" in v for v in violations),
           f"an unclassified typed-reference field (fbdOwner) is reported, "
           f"got {violations}")
    expect(not any("fbdStation" in v for v in violations),
           f"a field that IS classified (fbdStation) is not falsely "
           f"reported, got {violations}")


def test_audit_omits_typed_reference_check_when_component_sources_not_given():
    """component_sources is optional (mirrors registered_ids) -- callers
    that don't pass it (e.g. every pre-#764 test in this file) must not
    suddenly start failing merely because SOME OTHER fixture DTO happens
    to use a reference wrapper type."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {},
        SYNTHETIC_INVENTORY_COMPLETE,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(not any("fbd" in v for v in violations),
           f"no typed-reference violation appears when component_sources "
           f"is omitted, got {violations}")


# ----- #764 requirement 15: Lua reference kinds ---------------------------

SYNTHETIC_LUA_REFERENCES_LITERAL = """\
local function fakeReferences(data)
    local refs = {}
    refs[#refs + 1] = { kind = "unit", id = data.uid }
    refs[#refs + 1] = { kind = "totally_new_kind", id = data.other }
    return refs
end
saveMods.register("fake_component", {
    version = 1, inputVersions = {1}, required = true, scope = "global", deps = {},
    references = fakeReferences,
})
"""

SYNTHETIC_LUA_REFERENCES_HELPER = """\
local function addRef(kind, id)
    if id ~= nil then return { kind = kind, id = id } end
end
local function fakeReferences2(data)
    return { addRef("building", data.bid), addRef("yet_another_kind", data.x) }
end
saveMods.register("fake_component_2", {
    version = 1, inputVersions = {1}, required = true, scope = "global", deps = {},
    references = fakeReferences2,
})
"""


def test_find_lua_reference_kinds_detects_literal_and_helper_call_shapes():
    """Both established call shapes are found: a direct table-constructor
    literal (kind = "...") and an addRef("...", ...)-style helper call
    (unit_ai_save.lua's actual pattern) -- see LUA_REFERENCE_KIND_RES."""
    found = find_lua_reference_kinds({
        "scripts/fake_a.lua": SYNTHETIC_LUA_REFERENCES_LITERAL,
        "scripts/fake_b.lua": SYNTHETIC_LUA_REFERENCES_HELPER,
    })
    kinds = [k for k, _ in found]
    expect("unit" in kinds and "totally_new_kind" in kinds,
           f"both literal-form kinds are detected, got {found}")
    expect("building" in kinds and "yet_another_kind" in kinds,
           f"both addRef-helper-form kinds are detected, got {found}")


# Round-5 review (issue #764): a registration site can delegate its
# `references = ` spec field to an imported helper module rather than
# defining/naming the hook inline -- mirrors the REAL
# unit_ai_save.lua (`references = refsMod.references`, `local refsMod =
# require("scripts.unit_ai_save_refs")`) / unit_ai_save_refs.lua split.
# Deliberately gives the helper module NO local `references =` text of
# its own at all (unlike the real unit_ai_save_refs.lua, which happens
# to also carry `M.references = unitAiReferences`) -- proving detection
# follows the real require()/delegation relationship rather than an
# accidental same-file text match.
SYNTHETIC_LUA_REFERENCES_REGISTRATION_SITE = """\
local refsMod = require("scripts.fake_helper_module")
saveMods.register("fake_split_component", {
    version = 1, inputVersions = {1}, required = true, scope = "global", deps = {},
    references = refsMod.references,
})
"""

SYNTHETIC_LUA_REFERENCES_HELPER_MODULE = """\
local M = {}
function M.references(data)
    local refs = {}
    refs[#refs + 1] = { kind = "craft_bill", id = data.billId }
    refs[#refs + 1] = { kind = "split_module_kind", id = data.other }
    return refs
end
return M
"""


def test_find_lua_reference_kinds_follows_a_delegated_helper_module():
    """A registration site that delegates `references = ` to an
    imported helper module (unit_ai_save.lua's real shape) is followed
    to that module's OWN file for kind literals, even when the helper
    module carries no `references = ` text of its own -- round-5 review:
    the original per-file gate only caught this by the ACCIDENT of
    unit_ai_save_refs.lua also carrying an unrelated
    `M.references = ...` re-export line; this fixture has no such
    accident to lean on."""
    found = find_lua_reference_kinds({
        "scripts/fake_registration.lua": SYNTHETIC_LUA_REFERENCES_REGISTRATION_SITE,
        "scripts/fake_helper_module.lua": SYNTHETIC_LUA_REFERENCES_HELPER_MODULE,
    })
    kinds_by_file = {}
    for kind, relpath in found:
        kinds_by_file.setdefault(relpath, set()).add(kind)
    expect(
        kinds_by_file.get("scripts/fake_helper_module.lua") ==
            {"craft_bill", "split_module_kind"},
        f"the delegated helper module's own kind literals are found, "
        f"attributed to ITS file, got {found}")

    # The registration site itself has no kind literals of its own here
    # -- nothing should be spuriously attributed to it.
    expect("scripts/fake_registration.lua" not in kinds_by_file,
           f"the registration site itself contributes no kind literals "
           f"when it has none, got {found}")

    # A helper module that is NEVER require()d/delegated to by anything
    # must not be scanned -- the gate still excludes ordinary unrelated
    # Lua tables that merely happen to use a `kind = "..."` field for
    # something else entirely (e.g. UI element kinds).
    unrelated = find_lua_reference_kinds({
        "scripts/fake_unrelated.lua":
            'table.insert(chrome, { kind = "label", id = 1 })\n',
    })
    expect(unrelated == [],
           f"a file with a 'kind=' literal but no references()/delegation "
           f"connection at all is never scanned, got {unrelated}")


def test_audit_detects_unclassified_lua_reference_kind():
    """The req-15 acceptance test, Lua half: a NEW reference kind string
    with no classification row is reported by name; an already-
    documented kind is not falsely reported."""
    violations = audit(
        {"Fake.hs": SYNTHETIC_ENGINE_ENV},
        {"scripts/fake_a.lua": SYNTHETIC_LUA_REFERENCES_LITERAL},
        SYNTHETIC_REFERENCE_INVENTORY,
        root_records=FAKE_ROOT_RECORDS,
    )
    expect(any("totally_new_kind" in v for v in violations),
           f"an unclassified Lua reference kind is reported, got {violations}")


# ----- Registry ----------------------------------------------------------
#: The 6 groups this family owns, in the aggregate's order. One block,
#: run between the inventory's intentionally-unclassified groups and its
#: real-repository smoke test.
TESTS = (
    test_find_typed_reference_fields_detects_samepageref_and_maybe_wrapped,
    test_audit_detects_unclassified_typed_reference_field,
    test_audit_omits_typed_reference_check_when_component_sources_not_given,
    test_find_lua_reference_kinds_detects_literal_and_helper_call_shapes,
    test_find_lua_reference_kinds_follows_a_delegated_helper_module,
    test_audit_detects_unclassified_lua_reference_kind,
)
