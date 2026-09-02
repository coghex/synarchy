#!/usr/bin/env python3
"""Stored-index freshness validation for the unit-atlas tool (issue
#2054, requirement 12).

The ONE owner of how a generated `atlas/index.json` is judged: the
whole-document comparison against a FRESH regeneration (never against
the digests the stored file claims about itself), the canonical
serialization check, the drill-down that names WHERE a mismatch lies
(`report_index_mismatch` — schema, tool, direction order, animation
order, rows, frame counts, geometry, paths and digests), missing and
unexpected atlas detection, and the per-atlas content comparison.

`validate_unit_index` looks `report_index_mismatch` up on this module
at call time, so a test that blinds the drill-down to pin the
whole-document backstop patches THIS module.

Consumes the compiler's canonical planning/digest interfaces, the image
owner and the shared definitions (requirement 16). The public façade is
tools/pack_atlas.py.
"""
from __future__ import annotations

import json
from pathlib import Path, PurePosixPath
from typing import Any, Dict, List, Optional, Sequence

from pack_atlas_compiler import (
    build_index_document, canonical_index_bytes, content_digest,
    plan_animation, resolve_atlas_dir, sweep_atlas_dir,
)
from pack_atlas_image import ImageBackendMissing, decode_rgba8
from pack_atlas_shared import (
    ASSET_PREFIX, ATLAS_DIR_NAME, INDEX_FILENAME, STORAGE_FORMAT, AnimPlan,
    Report, UnitDecl, atlas_dir_rel, read_bytes_or_none, render_scalar,
)


INDEX_METADATA_KEYS = (
    "schema_version", "generator", "tool_version", "digest_algorithm",
    "unit", "direction_order",
)


def report_index_mismatch(
    report: Report, where: str, expected: Dict[str, Any], actual: Any,
) -> None:
    """Name the specific way a stored index differs from a fresh one.

    DIAGNOSTIC ONLY. The authority is the whole-document comparison in
    the caller, which has already found a difference; everything here
    exists to say WHERE. The caller therefore backstops this function:
    a difference it cannot name is still reported, so no shape can slip
    through by being one this code forgot to look for.
    """
    if not isinstance(actual, dict):
        report.err(where, "index is not a JSON object")
        return

    for key in INDEX_METADATA_KEYS:
        if actual.get(key) != expected[key]:
            report.err(
                where,
                f"generated-index metadata mismatch: `{key}` is "
                f"{render_scalar(actual.get(key), 80)}, expected "
                f"{render_scalar(expected[key], 80)}. This file is "
                f"generated — regenerate it with --compile rather than "
                f"editing it.")
    unknown = [k for k in actual if k not in expected]
    if unknown:
        report.err(
            where,
            f"generated-index metadata mismatch: unknown top-level key(s) "
            f"{', '.join(repr(k) for k in unknown)}")

    raw_anims = actual.get("animations")
    if not isinstance(raw_anims, list):
        report.err(where, "index has no `animations` list")
        return
    stored: Dict[str, Any] = {}
    for entry in raw_anims:
        if not isinstance(entry, dict) or not isinstance(entry.get("name"), str):
            report.err(where, "malformed entry in the index `animations` list")
            return
        # Keying by name is what makes the per-animation diagnostics
        # below possible, and it is exactly what would swallow a
        # duplicated entry: two copies of one VALID entry collapse to a
        # dict identical to a fresh compile's, leaving nothing to
        # report even though the file plainly differs.
        if entry["name"] in stored:
            report.err(
                where,
                f"duplicate entry for animation '{entry['name']}' in the "
                f"index `animations` list. This file is generated — "
                f"regenerate it with --compile rather than editing it.")
            return
        stored[entry["name"]] = entry
    fresh = {entry["name"]: entry for entry in expected["animations"]}

    if set(stored) == set(fresh):
        # Same animations, so the per-entry walk below would find
        # nothing: only the ORDER differs. Entries are emitted sorted by
        # name, and that order is part of the generated contract.
        order = [entry["name"] for entry in raw_anims]
        if order != [entry["name"] for entry in expected["animations"]]:
            report.err(
                where,
                f"index `animations` are not in canonical name-sorted "
                f"order: {', '.join(order)}")

    for name in sorted(set(fresh) - set(stored)):
        report.err(where, f"index is stale: no entry for animation '{name}'")
    for name in sorted(set(stored) - set(fresh)):
        report.err(
            where,
            f"index carries an obsolete entry for animation '{name}', which "
            f"is no longer declared")
    for name in sorted(set(fresh) & set(stored)):
        want, have = fresh[name], stored[name]
        if have == want:
            continue
        if have.get("source_digest") != want["source_digest"]:
            report.err(
                f"{where}/{name}",
                f"stale atlas: the recorded source digest no longer matches "
                f"this animation's declarations and frame pixels. "
                f"Regenerate with --compile.")
            continue
        differing = sorted(
            set(want) | set(have),
            key=lambda k: (k not in want, k))
        for key in differing:
            if have.get(key) != want.get(key):
                report.err(
                    f"{where}/{name}",
                    f"index entry disagrees with a fresh compile: `{key}` is "
                    f"{render_scalar(have.get(key), 80)}, expected "
                    f"{render_scalar(want.get(key), 80)}")
                break


def validate_unit_index(
    report: Report, root: Path, decl: UnitDecl,
) -> None:
    """Check one unit's generated artifacts against its live sources.

    The comparison is against a FRESH regeneration, never against the
    numbers the stored file carries about itself. That is what stops a
    tampered index from certifying a tampered atlas: both would have to
    match what the sources actually produce.
    """
    atlas_dir = root / atlas_dir_rel(decl.name)
    index_path = atlas_dir / INDEX_FILENAME
    # `exists()` FOLLOWS links, so a dangling symlink where the output
    # directory belongs would read as "no artifacts" and skip the whole
    # check. `is_symlink()` is what sees it.
    if not (atlas_dir.exists() or atlas_dir.is_symlink()):
        # No generated artifacts. Legitimate for THIS tool: an
        # uncompiled tree is a working copy waiting for `--compile`.
        # The engine rejects it (#1261) — every shipped unit is
        # compiled and tracked.
        return

    where = f"{decl.name}/{ATLAS_DIR_NAME}"
    if resolve_atlas_dir(report, root, decl.name) is None:
        return
    if not index_path.is_file():
        report.err(
            where,
            f"generated atlas directory has no {INDEX_FILENAME}. Compiled "
            f"artifacts are only usable through their index; regenerate "
            f"with --compile or remove the directory.")
        return

    plans: List[AnimPlan] = []
    for anim in sorted(decl.anims, key=lambda a: a.name):
        plan = plan_animation(report, root, anim)
        if plan is None:
            return
        plans.append(plan)
    expected = build_index_document(decl.name, plans)

    stored_bytes = read_bytes_or_none(index_path)
    if stored_bytes is None:
        report.err(where, f"cannot read {INDEX_FILENAME}")
        return
    try:
        stored = json.loads(stored_bytes.decode("utf-8"))
    except (json.JSONDecodeError, UnicodeDecodeError) as error:
        report.err(where, f"{INDEX_FILENAME} is not valid JSON: {error}")
        return

    if stored != expected:
        before = len(report.errors)
        report_index_mismatch(report, where, expected, stored)
        if len(report.errors) == before:
            # The comparison above is the authority and it already
            # found a difference; the drill-down is only there to name
            # it. A shape it cannot name must still FAIL, or a stored
            # index would be accepted purely because the diagnostics
            # did not anticipate its edit.
            report.err(
                where,
                f"{INDEX_FILENAME} does not match a fresh compile of this "
                f"unit. It is a generated file — regenerate it with "
                f"--compile.")
    elif stored_bytes != canonical_index_bytes(expected):
        # Same values, different bytes: the file was reformatted or
        # re-keyed by hand. The index is generated, and TEX-3 will
        # compare it by content digest, so its serialization is part of
        # the contract rather than a style preference.
        report.err(
            where,
            f"{INDEX_FILENAME} is not canonically serialized. It is a "
            f"generated file — regenerate it with --compile.")

    keep = {f"{plan.name}.{STORAGE_FORMAT}" for plan in plans}
    for orphan in sweep_atlas_dir(report, root, decl.name, atlas_dir,
                                  keep) or []:
        report.err(
            where,
            f"obsolete compiler-owned output: {orphan} belongs to no "
            f"declared animation. Regenerate with --compile.")

    fresh = {entry["name"]: entry for entry in expected["animations"]}
    for plan in plans:
        entry = fresh[plan.name]
        atlas_path = root / PurePosixPath(plan.atlas_rel)
        anim_where = f"{decl.name}/{plan.name}"
        if atlas_path.is_symlink():
            report.err(anim_where, f"symlinked atlas: {plan.atlas_rel}")
            continue
        if not atlas_path.is_file():
            report.err(
                anim_where,
                f"indexed atlas is missing from disk: {plan.atlas_rel}")
            continue
        try:
            actual = decode_rgba8(atlas_path)
        except ImageBackendMissing as error:
            report.err(anim_where, str(error))
            return
        except ValueError as error:
            report.err(anim_where, f"{plan.atlas_rel}: {error}")
            continue
        if content_digest(actual.width, actual.height,
                          actual.pixels) != entry["atlas_digest"]:
            report.err(
                anim_where,
                f"atlas content does not match its sources: "
                f"{plan.atlas_rel} does not hold the pixels a fresh compile "
                f"produces. Regenerate with --compile.")


def validate_indices(
    report: Report, root: Path, decls: Sequence[UnitDecl],
    only_unit: Optional[str],
) -> None:
    """Verify every generated artifact set against its live sources."""
    declared = {decl.name for decl in decls}
    for decl in decls:
        validate_unit_index(report, root, decl)

    # A generated directory under a unit nothing declares is orphaned
    # output. The inventory walk cannot see it — it descends
    # `animations/` only — so it is checked here.
    units_root = root.joinpath(*ASSET_PREFIX)
    if not units_root.is_dir():
        return
    for unit_dir in sorted(units_root.iterdir()):
        unit = unit_dir.name
        if only_unit is not None and unit != only_unit:
            continue
        if unit in declared or unit_dir.is_symlink() or not unit_dir.is_dir():
            continue
        if (unit_dir / ATLAS_DIR_NAME).exists():
            report.err(
                f"{unit}/{ATLAS_DIR_NAME}",
                f"generated atlas directory for a unit with no declaration "
                f"in data/units/")
