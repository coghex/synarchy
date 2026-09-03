#!/usr/bin/env python3
"""The unrestricted-access and permanent-boundary contracts of
engine_env_capability_access.py (issues #889 and #899; extracted from
tools/test_engine_env_capability_audit.py by issue #2062).

Three blocks, in run order:

* the SS6 full-access ratchet (#889) -- `classify_state_import`'s
  import classification over every spelling of a bare, explicit and
  narrow `Engine.Core.State` import; `parse_temporary_boundary`'s SS6.2
  parsing; and `audit_ratchet`'s both-direction agreement between the
  live unrestricted set, the checked-in constants and the documented
  table, ending with the real-repository consistency case;
* the SS6.1 permanent-boundary parse/compare (#899, E8) --
  `parse_permanent_boundary` and `audit_permanent_boundary` over the
  synthetic `_SECTION_6_1` table: first-column-only parsing, importer
  and definer correspondence in both directions, placeholder
  justifications and a missing table;
* `test_real_repo_end_state` -- the capability-split epic's END STATE
  asserted against the real repository (#899 requirement 7): the live
  unrestricted set IS `PERMANENT_IMPORTERS`, every `TEMPORARY_CEILING`
  value is empty, SS6.2 and SS6.1 document exactly that, and the E8
  SaveLoad record exists, is registered, is total, matches SS5, and is
  adopted by `World.Thread`. It exercises no render or input rule, which
  is why it lives here and not with the structural-boundary owner.

The end-state case is exposed as its own trailing fragment: the
aggregate has always run it after the SaveLoad projection, render/input
and field-total families' cases and before the writer scanner's (it
asks whether the whole epic landed, once the per-family checks have
each passed), and the seam lets `tools/test_engine_env_capability_audit.py`
keep that position without knowing the group by name.

Not a gate of its own. Run through the aggregate:

  python3 tools/test_engine_env_capability_audit.py
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from engine_env_capability_common import (  # type: ignore  # noqa: E402
    PERMANENT_IMPORTERS, REPO_ROOT, _import_chunks, _strip_haskell_comments,
    scan_production_sources,
)
from engine_env_capability_access import (  # type: ignore  # noqa: E402
    TEMPORARY_CEILING, audit_permanent_boundary, audit_ratchet,
    classify_state_import, parse_permanent_boundary, parse_temporary_boundary,
    scan_production_unrestricted_importers,
)
from engine_env_capability_inventory import (  # type: ignore  # noqa: E402
    CAPABILITIES, parse_inventory,
)
from engine_env_capability_saveload import (  # type: ignore  # noqa: E402
    SAVE_LOAD_CAPABILITY_MODULE, SAVE_LOAD_FIELD_MAP,
    audit_save_load_projection,
)
from test_engine_env_capability_audit_support import (  # noqa: E402
    expect, real_inventory_text,
)


# ----- SS6 full-access ratchet (issue #889) -----------------------------


def test_bare_import_detected():
    src = "module Foo where\nimport Engine.Core.State\nfoo ∷ Int\nfoo = 1\n"
    expect(classify_state_import(src) == "bare",
           "a plain `import Engine.Core.State` with no import list at all "
           "must classify as bare (grants full access to every export)")


def test_bare_import_qualified_and_aliased_detected():
    cases = (
        "import qualified Engine.Core.State\n",
        "import Engine.Core.State as ST\n",
        "import qualified Engine.Core.State as ST\n",
    )
    for src in cases:
        full = "module Foo where\n" + src + "foo ∷ Int\nfoo = 1\n"
        expect(classify_state_import(full) == "bare",
               f"a qualified and/or aliased bare import must still "
               f"classify as bare: {src!r}")


def test_bare_import_multiline_detected():
    src = "module Foo where\nimport\n  Engine.Core.State\nfoo ∷ Int\nfoo = 1\n"
    expect(classify_state_import(src) == "bare",
           "a bare import whose module name wraps onto a continuation "
           "line must still classify as bare")


def test_bare_import_as_last_import_not_swallowed_by_later_code():
    # A bare import that happens to be the FILE'S LAST import has no
    # following `^import` line to bound it against -- naively scanning
    # "from this import to the next import-or-EOF" would swallow the
    # entire rest of the module (every subsequent function body's
    # parentheses included) and misreport it as non-bare. The real
    # Engine.Core.Monad.hs is exactly this shape.
    src = (
        "module Foo where\n"
        "import Data.IORef (readIORef)\n"
        "import Engine.Core.State\n"
        "\n"
        "-- | some docs with (parens) and (more parens)\n"
        "data Something = Something { field ∷ Int → (Int, Int) }\n"
    )
    expect(classify_state_import(src) == "bare",
           "a bare import that is the file's last import must not be "
           "misread as narrow merely because later top-level code "
           "contains parentheses")


def test_explicit_engineenv_ordinary_qualified_and_multiline_detected():
    cases = (
        "import Engine.Core.State (EngineEnv(..))\n",
        "import qualified Engine.Core.State as ST (EngineEnv(..))\n",
        "import Engine.Core.State (SomeOtherType, EngineEnv (..))\n",
        "import Engine.Core.State\n  ( EngineEnv(..)\n  , SomeOtherType\n  )\n",
    )
    for src in cases:
        full = "module Foo where\n" + src + "foo ∷ Int\nfoo = 1\n"
        expect(classify_state_import(full) == "explicit",
               f"an explicit EngineEnv(..) import (ordinary, qualified, "
               f"aliased, or multiline) must classify as explicit: {src!r}")


def test_narrow_import_not_classified_unrestricted():
    cases = (
        "import Engine.Core.State (EngineEnv)\n",
        "import Engine.Core.State (loggerRef)\n",
        "import Engine.Core.State (EngineEnv, loggerRef)\n",
    )
    for src in cases:
        full = "module Foo where\n" + src + "foo ∷ Int\nfoo = 1\n"
        expect(classify_state_import(full) == "narrow",
               f"a strictly narrower import (bare EngineEnv type or "
               f"individual field accessors, no (..)) must not classify "
               f"as unrestricted: {src!r}")


def test_no_state_import_returns_none():
    src = "module Foo where\nimport Data.Text (Text)\nfoo ∷ Int\nfoo = 1\n"
    expect(classify_state_import(src) is None,
           "a file that never imports Engine.Core.State must classify as "
           "None, not bare/explicit/narrow")


FAKE_INVENTORY_6_2 = """\
### 6.2 Temporary compatibility boundary (production)

| Target capability | Modules | Roadmap entry |
|---|---|---|
| `core-init` | `Foo.Bar`, `Foo.Baz` | §7.1 |
| `save-load-coordination` | *(none — every module whose dominant field \
usage is save/load coordination is already a permanent orchestration \
exception; `Some.Explanatory.Module` was previously assigned here for its \
`someField` read, but its dominant usage is elsewhere)* | §7.8 |

### 6.3 Test-only exceptions

not part of SS6.2 at all
"""


def test_section_6_2_parser_extracts_real_assignments():
    parsed = parse_temporary_boundary(FAKE_INVENTORY_6_2)
    expect(parsed.get("core-init") == {"Foo.Bar", "Foo.Baz"},
           "a normal SS6.2 row must parse its backtick-quoted module names")


def test_section_6_2_parser_ignores_explanatory_backtick_references():
    parsed = parse_temporary_boundary(FAKE_INVENTORY_6_2)
    expect(parsed.get("save-load-coordination") == set(),
           "a Modules cell that is pure explanatory prose (wrapped "
           "entirely in *(...)*) must parse to zero assigned modules, "
           "even though it cites backtick-quoted names for context -- "
           "the real save-load-coordination row is exactly this shape")


def test_new_full_access_module_rejected():
    permanent = frozenset({"Perm.Mod"})
    ceiling = {"core-init": frozenset({"Temp.Mod"})}
    doc = {"core-init": {"Temp.Mod"}}
    unrestricted = {"Perm.Mod", "Temp.Mod", "New.Unclassified.Mod"}
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(any("New.Unclassified.Mod" in v for v in violations),
           "a module with unrestricted access that is neither permanent "
           "nor in the checked-in temporary ceiling must be rejected")


def test_shrinking_migration_accepted():
    # A migration narrows Temp.B away (it's no longer unrestricted at
    # all) and the checked-in ceiling + SS6.2 doc are updated together
    # to drop it -- this must be a clean, zero-violation migration.
    permanent = frozenset({"Perm.Mod"})
    ceiling = {"core-init": frozenset({"Temp.A"})}
    doc = {"core-init": {"Temp.A"}}
    unrestricted = {"Perm.Mod", "Temp.A"}
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(violations == [],
           "a migration that shrinks the live temporary set, with the "
           "checked-in ceiling and SS6.2 doc updated in tandem, must be "
           "accepted with zero violations")


def test_documented_but_ungoverned_addition_still_rejected():
    # Someone adds real unrestricted access to a new module AND
    # documents it in SS6.2, but never grows the checked-in
    # TEMPORARY_CEILING itself -- the strict ceiling must still reject
    # it; documentation alone never admits a new full-access module.
    permanent = frozenset({"Perm.Mod"})
    ceiling = {"core-init": frozenset({"Temp.A"})}
    doc = {"core-init": {"Temp.A", "Sneaky.New.Mod"}}
    unrestricted = {"Perm.Mod", "Temp.A", "Sneaky.New.Mod"}
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(any("Sneaky.New.Mod" in v for v in violations),
           "a new full-access module must still be rejected even when "
           "SS6.2 is ALSO edited to document it -- only growing the "
           "checked-in ceiling itself admits a new temporary module")


def test_stale_ceiling_entry_rejected():
    # A migration narrows a module (it no longer has live unrestricted
    # access) but never shrinks the checked-in ceiling or its SS6.2
    # row to match -- the ratchet must reject this drift in EITHER
    # direction, not just growth: SS6.2's accounting must stay an
    # exact mirror of the live temporary set, not merely an upper bound.
    permanent = frozenset({"Perm.Mod"})
    ceiling = {"core-init": frozenset({"Temp.A", "Temp.Stale"})}
    doc = {"core-init": {"Temp.A", "Temp.Stale"}}
    unrestricted = {"Perm.Mod", "Temp.A"}  # Temp.Stale no longer unrestricted
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(any("Temp.Stale" in v for v in violations),
           "a checked-in ceiling/SS6.2 entry that no longer has live "
           "unrestricted access must be flagged as stale, not silently "
           "tolerated as a mere upper bound")


def test_stale_permanent_importer_rejected():
    # A permanent (SS6.1) module is narrowed by a later change (no
    # longer live-unrestricted) but PERMANENT_IMPORTERS is never
    # updated to drop it -- this must fail just like a stale temporary
    # ceiling entry does; the permanent allowlist is not exempt from
    # the live-scan agreement requirement.
    permanent = frozenset({"Perm.Stale"})
    ceiling = {"core-init": frozenset({"Temp.Live"})}
    doc = {"core-init": {"Temp.Live"}}
    unrestricted = {"Temp.Live"}  # Perm.Stale no longer unrestricted
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(any("Perm.Stale" in v for v in violations),
           "a checked-in PERMANENT_IMPORTERS entry that no longer has live "
           "unrestricted access must be flagged as stale, matching the "
           "temporary-ceiling side's same requirement")


def test_ceiling_and_doc_mismatch_detected():
    permanent: frozenset[str] = frozenset()
    ceiling = {"core-init": frozenset({"Temp.A", "Temp.B"})}
    doc = {"core-init": {"Temp.A"}}  # Temp.B undocumented
    unrestricted = {"Temp.A", "Temp.B"}
    violations = audit_ratchet(unrestricted, doc, permanent=permanent, ceiling=ceiling)
    expect(any("Temp.B" in v and "core-init" in v for v in violations),
           "a checked-in ceiling entry missing from SS6.2's documented "
           "accounting must be flagged as a doc/ceiling mismatch")


def test_real_repo_ratchet_consistency():
    real_inventory = real_inventory_text()
    unrestricted = scan_production_unrestricted_importers(REPO_ROOT)
    doc_temporary = parse_temporary_boundary(real_inventory)
    violations = audit_ratchet(unrestricted, doc_temporary)
    expect(violations == [],
           f"the real repo's live-scanned production importer set, the "
           f"checked-in PERMANENT_IMPORTERS/TEMPORARY_CEILING constants, "
           f"and SS6.2 as documented should all agree after issue #889's "
           f"core-init migration, got: {violations}")


# ----- SS6.1 permanent-boundary parse/compare (issue #899, E8) ----------
#
# `audit_ratchet` pins the checked-in constants to the LIVE source in
# both directions, but before #899 nothing pinned them to SS6.1's
# DOCUMENTED table -- so growing a live importer AND the Python
# constant together passed with the inventory never recording why the
# module is a genuine whole-session orchestration boundary. These
# fixtures exercise the pure parse/compare core synthetically; the
# real-repository assertion lives in the end-state test below.

_SECTION_6_1 = """\
### 6.1 Permanent (production)

| Module(s) | Category | Reason |
|---|---|---|
| `Perm.One` | Permanent initialization infrastructure | Defines the record; see `Some.Other.Module` for context. |
| `Perm.Two`, `Perm.Three` | Permanent orchestration infrastructure | Boot wire-up: constructs everything, by job description. |

### 6.2 Temporary compatibility boundary (production)

| Target capability | Modules | Roadmap entry |
|---|---|---|
| `core-init` | *(none)* | SS7.1 |
"""


def test_section_6_1_parser_reads_first_column_only():
    rows = parse_permanent_boundary(_SECTION_6_1)
    modules = set()
    for names, _cat, _reason in rows:
        modules |= names
    expect(modules == {"Perm.One", "Perm.Two", "Perm.Three"},
           f"SS6.1's parser must read the Module(s) column ONLY -- the "
           f"Reason cells deliberately cite other backtick-quoted module "
           f"names as supporting context, and those are NOT permanent "
           f"allowlist entries, got: {sorted(modules)}")
    expect(not any("Some.Other.Module" in names for names, _c, _r in rows),
           "a module named only inside a Reason cell must never be "
           "admitted to the documented permanent set")


def test_section_6_1_matching_constants_accepted():
    violations = audit_permanent_boundary(
        _SECTION_6_1,
        permanent=frozenset({"Perm.Two", "Perm.Three"}), definer="Perm.One")
    expect(violations == [],
           f"a SS6.1 table whose documented set equals PERMANENT_DEFINER + "
           f"PERMANENT_IMPORTERS, with real Category/Reason cells, must "
           f"pass, got: {violations}")


def test_section_6_1_undocumented_permanent_importer_rejected():
    violations = audit_permanent_boundary(
        _SECTION_6_1,
        permanent=frozenset({"Perm.Two", "Perm.Three", "Perm.Sneaky"}),
        definer="Perm.One")
    expect(any("Perm.Sneaky" in v for v in violations),
           "growing PERMANENT_IMPORTERS without a matching SS6.1 row (the "
           "'Python constant change without the matching inventory "
           "justification' case) must be rejected")


def test_section_6_1_documented_but_ungoverned_module_rejected():
    violations = audit_permanent_boundary(
        _SECTION_6_1,
        permanent=frozenset({"Perm.Two"}), definer="Perm.One")
    expect(any("Perm.Three" in v for v in violations),
           "documenting a module in SS6.1 without adding it to the "
           "checked-in constants must be rejected -- documentation alone "
           "does not admit a permanent importer")


def test_section_6_1_placeholder_justification_rejected():
    doc = _SECTION_6_1.replace(
        "| `Perm.One` | Permanent initialization infrastructure | Defines "
        "the record; see `Some.Other.Module` for context. |",
        "| `Perm.One` | — | — |")
    violations = audit_permanent_boundary(
        doc, permanent=frozenset({"Perm.Two", "Perm.Three"}),
        definer="Perm.One")
    expect(sum("Perm.One" in v for v in violations) == 2,
           f"a name-only SS6.1 row with placeholder Category AND Reason "
           f"cells must be rejected on BOTH -- otherwise it satisfies the "
           f"module-set equality while providing none of the explicit "
           f"justification SS6.1/SS6.4 demand, got: {violations}")


def test_section_6_1_missing_table_rejected():
    violations = audit_permanent_boundary(
        "## 6. Full-EngineEnv compatibility boundary\n\nno table here\n",
        permanent=frozenset({"Perm.Two"}), definer="Perm.One")
    expect(len(violations) == 1 and "could not be parsed" in violations[0],
           f"an unparseable/absent SS6.1 table must fail loudly rather "
           f"than silently comparing against an empty documented set, "
           f"got: {violations}")


def test_real_repo_end_state():
    """Issue #899 (E8) requirement 7: the epic's END STATE, asserted
    against the REAL repository rather than a fixture.

    Every other real-repo test above asks "do the checked-in constants
    and the live source still agree?" -- which stays true no matter how
    large the temporary ceiling is. This one asserts the ceiling is
    GONE: the boundary is permanent-only, and the record set is
    complete. It is deliberately a set of narrow, independently
    diagnosable assertions rather than one `audit() == []`, so a
    regression names which part of the end state slipped."""
    real_inventory = real_inventory_text()
    sources = scan_production_sources(REPO_ROOT)
    unrestricted = scan_production_unrestricted_importers(REPO_ROOT)

    # 1. The live unrestricted importer set IS the permanent allowlist --
    #    checked as set equality in both directions, so neither a new
    #    full-access module nor a stale allowlist entry passes.
    expect(unrestricted == set(PERMANENT_IMPORTERS),
           f"the live unrestricted production importer set must equal "
           f"PERMANENT_IMPORTERS exactly (SS6.1's permanent-only "
           f"boundary); extra: {sorted(unrestricted - set(PERMANENT_IMPORTERS))}, "
           f"missing: {sorted(set(PERMANENT_IMPORTERS) - unrestricted)}")

    # 2. Every TEMPORARY_CEILING value is empty -- the flip itself.
    nonempty = {cap: sorted(mods) for cap, mods in TEMPORARY_CEILING.items()
                if mods}
    expect(nonempty == {},
           f"every TEMPORARY_CEILING value must be empty after #899's "
           f"flip -- there is no legal path left for a production module "
           f"to take unrestricted access, got: {nonempty}")

    # 3. SS6.2's documented table still carries all eight capability keys,
    #    each with an EMPTY module set. The keys matter as much as the
    #    emptiness: `audit_ratchet`'s doc/ceiling cross-check iterates the
    #    UNION of both key sets, so a dropped row would silently stop
    #    cross-checking that capability rather than fail.
    doc_temporary = parse_temporary_boundary(real_inventory)
    expect(set(doc_temporary) == set(CAPABILITIES),
           f"SS6.2's table must have exactly the eight CAPABILITIES keys, "
           f"got: {sorted(doc_temporary)}")
    expect(set(TEMPORARY_CEILING) == set(CAPABILITIES),
           f"TEMPORARY_CEILING must retain all eight CAPABILITIES keys "
           f"mapped to empty frozensets, not be reduced to {{}} -- "
           f"otherwise this test's own emptiness assertions go vacuous; "
           f"got: {sorted(TEMPORARY_CEILING)}")
    documented_nonempty = {cap: sorted(mods)
                           for cap, mods in doc_temporary.items() if mods}
    expect(documented_nonempty == {},
           f"every SS6.2 row must document an empty module set, got: "
           f"{documented_nonempty}")

    # 4. SS6.1's DOCUMENTED set matches the permanent constants, with a
    #    real justification on every row.
    permanent_violations = audit_permanent_boundary(real_inventory)
    expect(permanent_violations == [],
           f"SS6.1's documented permanent set must equal PERMANENT_DEFINER "
           f"+ PERMANENT_IMPORTERS, with a non-placeholder Category and "
           f"Reason on every row, got: {permanent_violations}")

    # 5. The E8 record exists, is listed in synarchy.cabal, and its
    #    projection binds exactly the five documented handles from their
    #    matching EngineEnv accessors. (Runtime container identity is
    #    proven separately by Test.Headless.Capability.SaveLoad's
    #    sameContainer assertions -- a Python audit cannot observe it.)
    cabal_text = (REPO_ROOT / "synarchy.cabal").read_text(encoding="utf-8")
    save_load_violations = audit_save_load_projection(sources, cabal_text)
    expect(save_load_violations == [],
           f"`{SAVE_LOAD_CAPABILITY_MODULE}` must exist, be listed in "
           f"synarchy.cabal, and alias all five "
           f"`save-load-coordination` handles, got: {save_load_violations}")

    # 6. ...and the five handles it aliases are exactly the five fields
    #    SS5's `save-load-coordination` table classifies -- so the record
    #    and the inventory cannot drift apart.
    inventory_rows, _ = parse_inventory(real_inventory)
    inventory_fields = {row.field for row in inventory_rows
                        if row.capability == "save-load-coordination"}
    expect(inventory_fields == set(SAVE_LOAD_FIELD_MAP.values()),
           f"SAVE_LOAD_FIELD_MAP must name exactly SS5's "
           f"`save-load-coordination` fields, got inventory: "
           f"{sorted(inventory_fields)} vs map: "
           f"{sorted(SAVE_LOAD_FIELD_MAP.values())}")

    # 7. World.Thread -- requirement 2's named real consumer -- must
    #    actually adopt the record: its `Engine.Core.State` import may
    #    name NO save-load-coordination accessor, and its barrier access
    #    must go through the capability. Without this, #899 could ship an
    #    unadopted twelfth-plus record, which E1's convention forbids.
    world_thread = sources.get("src/World/Thread.hs", "")
    expect(bool(world_thread),
           "src/World/Thread.hs must exist in the production sources")
    state_import = "".join(
        chunk for chunk in _import_chunks(_strip_haskell_comments(world_thread))
        if chunk.startswith("import Engine.Core.State"))
    leaked = sorted(f for f in SAVE_LOAD_FIELD_MAP.values()
                    if f in state_import)
    expect(leaked == [],
           f"`World.Thread`'s Engine.Core.State import must name no "
           f"save-load-coordination field accessor after #899 -- it goes "
           f"through SaveLoadCapability now; got: {leaked}")
    expect("slSaveBarrierRef" in world_thread
           and "toSaveLoadCapability" in world_thread,
           "`World.Thread` must reach the save barrier through "
           "`slSaveBarrierRef (toSaveLoadCapability env)` -- E1 forbids "
           "introducing a capability record with no real consumer")


#: This owner's inventory, in two fragments. The aggregate has always
#: run the end-state case after the SaveLoad projection, render/input
#: and field-total families' groups and before the writer scanner's,
#: and exposing the seam lets `tools/test_engine_env_capability_audit.py`
#: keep that position without knowing the group by name. `TESTS` is the
#: complete inventory the façade accounts against.
TESTS_LEADING = (
    test_bare_import_detected,
    test_bare_import_qualified_and_aliased_detected,
    test_bare_import_multiline_detected,
    test_bare_import_as_last_import_not_swallowed_by_later_code,
    test_explicit_engineenv_ordinary_qualified_and_multiline_detected,
    test_narrow_import_not_classified_unrestricted,
    test_no_state_import_returns_none,
    test_section_6_2_parser_extracts_real_assignments,
    test_section_6_2_parser_ignores_explanatory_backtick_references,
    test_new_full_access_module_rejected,
    test_shrinking_migration_accepted,
    test_documented_but_ungoverned_addition_still_rejected,
    test_stale_ceiling_entry_rejected,
    test_stale_permanent_importer_rejected,
    test_ceiling_and_doc_mismatch_detected,
    test_real_repo_ratchet_consistency,
    test_section_6_1_parser_reads_first_column_only,
    test_section_6_1_matching_constants_accepted,
    test_section_6_1_undocumented_permanent_importer_rejected,
    test_section_6_1_documented_but_ungoverned_module_rejected,
    test_section_6_1_placeholder_justification_rejected,
    test_section_6_1_missing_table_rejected,
)

TESTS_TRAILING = (
    test_real_repo_end_state,
)

TESTS = TESTS_LEADING + TESTS_TRAILING
