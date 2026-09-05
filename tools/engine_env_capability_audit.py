#!/usr/bin/env python3
"""EngineEnv capability-inventory audit (issue #876, EngineEnv capability
epic #537 requirement 8) -- the aggregate gate.

This file is the ONE command CI and tools/ci-local.sh run for the whole
EngineEnv capability contract. Since issue #2064 it holds no contract
implementation at all: it reads the repository inputs ONCE, hands the
same immutable inputs to each contract's own owner in a fixed order,
prints that owner's complete violation list and exits 1 on the first
failing group. Each owner carries the recorded rationale for the checks
it owns -- the issue it came from, the bypasses each rule closed, why
the rule is shaped the way it is -- and this file keeps only the
cross-cutting overview below.

What the gate guards
--------------------
docs/engineenv_capability_inventory.md is the authoritative ownership
inventory for every `EngineEnv` field, and this gate is what stops it
drifting away from src/Engine/Core/State.hs. Every check below is a
static presence/well-formedness or source-correspondence check, not a
semantic proof: it cannot verify that a documented decision is TRUE of
the code, only that a decision -- using recognized vocabulary -- has
been recorded and stays in sync with the live record and the live
module graph.

The owners, in the order `main` composes them
---------------------------------------------
  `engine_env_capability_writers.py`     the SS5 writing-module scanner
      (issue #1892, CMA-1; extracted by issue #2036). It owns
      `CAPABILITY_WRITER_MODULES`, `SHADOW_EXEMPTIONS`, the Haskell
      tokenizer and import resolver, the capability-accessor map and
      its #2059 fail-closed completeness audit, mutation-site
      classification, the scan and its three blocking checks. Since
      issue #2230 it is documentation and re-exports over four one-way
      implementation owners -- `..._writer_authority.py` (the
      checked-in map, the exemptions, the recognized primitives and
      the two checks that read them), `..._writer_syntax.py` (the
      tokenizer, the import resolver and mutation-expression
      classification), `..._writer_projections.py` (capability-record
      and projection discovery) and `..._writer_scan.py` (the single
      pass over the tree, the mutation-site check and the residue
      report) -- but it is still ONE owner from here: this module
      imports the facade and nothing below it. It is also the only
      owner with a self-test command of its own, runnable alone for
      iteration.
  `engine_env_capability_inventory.py`   the SS5 inventory-row contract
      (issue #876): one row per live field, under a known capability
      heading, with a valid lifecycle, a strict Readers/Writers role
      grammar, real Sync/Init/Shutdown/Notes cells and grounding
      evidence.
  `engine_env_capability_field_total.py` the SS1 audited field total
      and field span (issue #1669): the marked scope block states the
      live count and the record's first and last field, once, and
      SS6.2's procedure sentence states no total at all.
  `engine_env_capability_access.py`      the SS6 full-access ratchet
      and the SS6.1 permanent-boundary comparison (issues #889, #899):
      the live unrestricted `Engine.Core.State` importer set equals
      SS6.1's permanent allowlist exactly, the SS6.2 temporary ceiling
      is empty and shrink-only, and neither documentation nor a
      constant alone can admit a new full-access module.
  `engine_env_capability_saveload.py`    the E8 save-load projection
      correspondence (issue #899): the capability module exists, is
      registered in synarchy.cabal, and binds exactly the five
      documented `save-load-coordination` handles from their matching
      `EngineEnv` accessors.
  `engine_env_capability_boundaries.py`  the SS3 main-render and SS7.3
      LuaThread structural boundaries (issues #891, #892): only the
      owning thread's modules hold the full capability record, only
      the private fields' owners name them, and the worker-safe views
      mention neither.

`engine_env_capability_common.py` is the substrate every owner reads --
the repository anchors, the live-field derivation, the one production
tree walk, SS6.1's permanent set, the Haskell source and import
helpers, the policy-free inventory-document primitives and the
projection canonicalizer. The import graph is acyclic and one-way:
common imports no owner, no contract owner imports another, and this
file imports common and every owner. The one owner with an interior is
the writer scanner: since issue #2230
`engine_env_capability_writers.py` is a facade over four
implementation owners that DO import each other, in the fixed order
authority -> {syntax, projections} -> scan, and none of which imports
the facade. That interior is invisible from here by construction --
this file imports the facade and nothing below it, so the writer
scanner is still exactly one owner at this level (issue #2230
requirements 16 and 23), and adding an edge from here to a writer
child is what would break the layering. `python3 -m py_compile` does
not execute imports and so proves nothing about any of it; the two run
commands below do, because both load the full graph at run time, where
a `from X import Y` cycle raises `ImportError`, and
`test_engine_env_capability_writers.py`'s conformance owner checks the
writer family's interior edges directly.

Single collection
-----------------
`main` reads `src/`+`app/` exactly once, derives the live `EngineEnv`
field list exactly once, and runs the capability write scan exactly
once. Every owner receives those same immutable inputs; no owner
re-walks the tree, re-extracts the record, or re-scans writes. (The
access owner's `scan_production_unrestricted_importers` does perform
its own walk, but it is a test-only convenience this file never
calls.)

Ordering and residue
--------------------
The pass-on residue is non-blocking evidence (design decision D-5), so
it is printed FIRST -- before any check that can `return 1` -- and the
measurement survives a failure anywhere below it. Each blocking group
then prints its COMPLETE violation list before the exit; the run
short-circuits at the first failing group, so a later group's silence
means "not reached", not "clean".

Usage:
  python3 tools/engine_env_capability_audit.py
Exit codes: 0 = every live EngineEnv field is validly classified and
the SS6 ratchet holds, 1 = one or more violations found.
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
# The shared substrate (#2036): the repository inputs this file loads
# once and the projection parser its summary line counts through.
from engine_env_capability_common import (  # type: ignore
    ENGINE_ENV_FILE, ENGINE_ENV_PATTERN, INVENTORY_PATH, PERMANENT_DEFINER,
    REPO_ROOT, extract_record_fields, parse_projection_bindings,
    scan_production_sources,
)
# The SS5 writing-module scanner (#1892, extracted by #2036).
from engine_env_capability_writers import (  # type: ignore
    CAPABILITY_WRITER_MODULES, SHADOW_EXEMPTIONS,
    audit_capability_projection_completeness, audit_mutation_sites,
    audit_shadow_exemptions, audit_writer_modules,
    discover_capability_records, format_residue, scan_capability_writes,
)
# The five contract owners #2064 extracted from this file.
from engine_env_capability_access import (  # type: ignore
    TEMPORARY_CEILING, audit_permanent_boundary, audit_ratchet,
    classify_production_sources, parse_temporary_boundary,
)
from engine_env_capability_boundaries import (  # type: ignore
    INPUT_LUA_ONLY_MODULES, RENDER_MAIN_ONLY_MODULES, audit_input_boundary,
    audit_render_boundary,
)
from engine_env_capability_field_total import audit_field_total  # type: ignore
from engine_env_capability_inventory import audit  # type: ignore
from engine_env_capability_saveload import (  # type: ignore
    audit_save_load_projection,
)


def main() -> int:
    # The repository inputs, read once and shared by every owner
    # (#2064 requirements 13 and 14).
    engine_env_source = (REPO_ROOT / ENGINE_ENV_FILE).read_text(encoding="utf-8")
    inventory_text = INVENTORY_PATH.read_text(encoding="utf-8")
    live_fields = extract_record_fields(engine_env_source, ENGINE_ENV_PATTERN)
    production_sources = scan_production_sources(REPO_ROOT)

    # The pass-on residue is non-blocking evidence (D-5), so it is
    # printed FIRST -- before any check that can `return 1` -- and the
    # measurement survives a failure anywhere below it. The scan itself
    # runs ONCE: four of the groups below read this one `WriteScan`.
    scan = scan_capability_writes(production_sources, live_fields)
    field_writes, residue = scan.writes, scan.residue
    for line in format_residue(residue):
        print(line)
    print()

    violations = audit(live_fields, inventory_text)
    if violations:
        print(f"{len(violations)} EngineEnv capability-inventory violation(s):")
        for v in violations:
            print(f"  - {v}")
        print(f"\nAdd or fix a classification row for each item above in "
              f"{INVENTORY_PATH.relative_to(REPO_ROOT)} SS5 (see SS2 for the "
              f"capability/thread-role/lifecycle vocabulary).")
        return 1

    total_violations = audit_field_total(live_fields, inventory_text)
    if total_violations:
        print(f"{len(total_violations)} SS1 field-total/field-span "
              f"violation(s):")
        for v in total_violations:
            print(f"  - {v}")
        return 1

    unrestricted = classify_production_sources(production_sources)
    doc_temporary = parse_temporary_boundary(inventory_text)
    ratchet_violations = audit_ratchet(unrestricted, doc_temporary)
    if ratchet_violations:
        print(f"{len(ratchet_violations)} SS6 full-access ratchet violation(s):")
        for v in ratchet_violations:
            print(f"  - {v}")
        return 1

    permanent_violations = audit_permanent_boundary(inventory_text)
    if permanent_violations:
        print(f"{len(permanent_violations)} SS6.1 permanent-allowlist "
              f"violation(s):")
        for v in permanent_violations:
            print(f"  - {v}")
        return 1

    projection_violations = audit_capability_projection_completeness(
        production_sources, live_fields)
    if projection_violations:
        print(f"{len(projection_violations)} capability projection "
              f"completeness violation(s):")
        for v in projection_violations:
            print(f"  - {v}")
        return 1

    save_load_violations = audit_save_load_projection(
        production_sources, (REPO_ROOT / "synarchy.cabal").read_text(encoding="utf-8"))
    if save_load_violations:
        print(f"{len(save_load_violations)} save-load capability record "
              f"violation(s):")
        for v in save_load_violations:
            print(f"  - {v}")
        return 1

    boundary_violations = audit_render_boundary(production_sources)
    if boundary_violations:
        print(f"{len(boundary_violations)} SS3 main-render boundary "
              f"violation(s):")
        for v in boundary_violations:
            print(f"  - {v}")
        return 1

    input_violations = audit_input_boundary(production_sources)
    if input_violations:
        print(f"{len(input_violations)} SS7.3 LuaThread input boundary "
              f"violation(s):")
        for v in input_violations:
            print(f"  - {v}")
        return 1

    site_violations = audit_mutation_sites(scan.sites)
    if site_violations:
        print(f"{len(site_violations)} unclassifiable mutation site(s):")
        for v in site_violations:
            print(f"  - {v}")
        return 1

    exemption_violations = audit_shadow_exemptions(
        scan.suppressed, live_fields)
    if exemption_violations:
        print(f"{len(exemption_violations)} SHADOW_EXEMPTIONS violation(s):")
        for v in exemption_violations:
            print(f"  - {v}")
        return 1

    writer_violations = audit_writer_modules(field_writes, live_fields)
    if writer_violations:
        print(f"{len(writer_violations)} SS5 writing-module map "
              f"violation(s):")
        for v in writer_violations:
            print(f"  - {v}")
        return 1

    total_fields = len(live_fields)
    capability_records = discover_capability_records(production_sources)
    projected_fields = sum(
        len(parse_projection_bindings(production_sources[entry.relpath],
                                      entry.projection))
        for entry in capability_records if entry.projection is not None)
    mapped_fields = sum(1 for m in CAPABILITY_WRITER_MODULES.values() if m)
    mapped_pairs = sum(len(m) for m in CAPABILITY_WRITER_MODULES.values())
    temporary_total = sum(len(m) for m in TEMPORARY_CEILING.values())
    print(f"engine-env capability-inventory audit: {total_fields} EngineEnv "
          f"field(s) all classified and agreeing with SS1's marked field "
          f"total and `{live_fields[0]}`-through-`{live_fields[-1]}` span, {len(unrestricted) + 1} full-access "
          f"modules (incl. the {PERMANENT_DEFINER} definer) within the SS6 "
          f"ratchet, all permanent (SS6.1 documented set == the checked-in "
          f"constants; {temporary_total} temporary), "
          f"{len(RENDER_MAIN_ONLY_MODULES)} MainRender module(s) "
          f"holding the full render capability and no non-owner naming "
          f"`engineStateRef` (SS3), {len(INPUT_LUA_ONLY_MODULES)} LuaThread "
          f"module(s) holding the full input capability and no non-owner "
          f"naming `inputBarrierNextRef`/`currentKeyDownRef` (SS7.3), "
          f"{len(capability_records)} capability record(s) whose "
          f"{projected_fields} projected field(s) all canonicalize onto a "
          f"live EngineEnv accessor (SS2.1), "
          f"{mapped_fields}/{total_fields} field(s) carrying a non-empty "
          f"writing-module map covering {mapped_pairs} field-module pair(s) "
          f"with no undeclared or stale entry (SS5) over "
          f"{len(scan.sites)} classified mutation site(s) and "
          f"{len(SHADOW_EXEMPTIONS)} shadow exemption(s), and "
          f"{len(residue)} reported pass-on residue use(s)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
