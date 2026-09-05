#!/usr/bin/env python3
"""The unit-animation texture budget of the unit-atlas tool (issue
#2054, requirement 13).

The ONE owner of budget-policy parsing and its type checks
(`load_budget`, over tools/unit_texture_budget.json — the checked-in
document stays the authority, requirement 14), the stored-index input
it accounts from (`read_stored_index`), the per-unit and aggregate
image/frame/byte accounting, the projected-growth calculation, and the
warning/error thresholds and their diagnostics (`validate_budget`).

It reads the STORED index rather than anything the compiler produced
in this run, so its dependencies are the shared definitions alone —
including the path helpers and `read_bytes_or_none` that live there for
exactly this reason (requirement 16). The public façade is
tools/pack_atlas.py.

Its whole input is the compiled asset tree: stored indices and the
entries beside them. It has no view of the loader, the texture request
queue, the bindless table or a running engine, so it validates
generated artifacts and a projection of their decoded size, and never
runtime registrations (#2217). The Hspec group `Unit.Atlas.Load — the
real unit registration boundary` owns that boundary.
"""
from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path, PurePosixPath
from typing import Any, Dict, List, Optional, Sequence

from pack_atlas_shared import (
    ATLAS_DIR_NAME, INDEX_FILENAME, BudgetTally, Report, UnitDecl,
    atlas_dir_rel, is_representable_number, read_bytes_or_none,
    render_scalar,
)


BUDGET_REL = PurePosixPath("tools/unit_texture_budget.json")
BUDGET_SCHEMA_VERSION = 1

# Every field the check reads or reports, WITH its required type.
# Required rather than defaulted: a bare threshold with no stated unit,
# scope or rule is exactly the artifact this budget exists to avoid, and
# a default would let one go missing silently.
#
# Presence alone is not enough, and the prose fields are the reason. They
# carry the owner's confirmation and the stated comparison rule — the
# provenance of a number nobody may raise unilaterally — so a numeric
# `confirmed_on` or a boolean `comparison_rule` has to fail as loudly as
# a missing one. Types are checked here, in the same sweep, so no field
# can be "present" while saying nothing.
BUDGET_IMAGE_KEYS = (
    ("max_per_animation", int),
    ("measure", str),
    ("aggregation_scope", str),
    ("comparison_rule", str),
    ("excluded", list),
    ("rationale", str),
)
BUDGET_BYTE_KEYS = (
    ("threshold", int),
    ("unit", str),
    ("measure", str),
    ("aggregation_scope", str),
    ("projection", dict),
    ("comparison_rule", str),
    ("distinct_from", str),
    ("derivation", str),
    ("confirmed_by", str),
    ("confirmed_on", str),
)


@dataclass
class Budget:
    """The policy document, parsed. See tools/unit_texture_budget.json."""
    max_per_animation: int
    threshold_bytes: int
    growth_factor: float

    def projected(self, measured: int) -> float:
        return measured * self.growth_factor


def _budget_field(
    report: Report, where: str, block: dict, key: str, kind: type,
) -> Optional[Any]:
    """Check one required field's presence AND type, reporting exactly
    one diagnostic either way.

    A `str` or `list` must additionally be non-empty: these fields carry
    the comparison rule and the owner's confirmation, and an empty one
    documents nothing while satisfying a presence test.
    """
    if key not in block:
        report.err(where, f"missing field: {key}")
        return None
    value = block[key]
    # `bool` is a subclass of `int`, so an unguarded isinstance lets
    # `true` read as 1 — as a threshold of one byte, or as schema
    # version 1.
    if isinstance(value, bool) or not isinstance(value, kind):
        report.err(
            where,
            f"{key} must be {kind.__name__}, got "
            f"{render_scalar(value)}")
        return None
    if kind is str and not value.strip():
        report.err(where, f"{key} must not be empty")
        return None
    if kind is list:
        if not value:
            report.err(where, f"{key} must not be empty")
            return None
        if not all(isinstance(item, str) and item.strip() for item in value):
            report.err(where, f"{key} must hold non-empty strings")
            return None
    return value


def load_budget(report: Report, root: Path) -> Optional[Budget]:
    """Read and validate the budget policy document.

    An unreadable or malformed budget is a hard error, never a skipped
    check: silently continuing would print a clean run while enforcing
    no budget at all, which is the failure mode that makes a guardrail
    worse than none.
    """
    path = root / BUDGET_REL
    where = BUDGET_REL.as_posix()
    try:
        raw = path.read_bytes()
    except OSError as error:
        report.err(where, f"cannot read the unit texture budget: {error}")
        return None
    try:
        doc = json.loads(raw.decode("utf-8"))
    except (json.JSONDecodeError, UnicodeDecodeError) as error:
        report.err(where, f"not valid JSON: {error}")
        return None
    if not isinstance(doc, dict):
        report.err(where, "the budget document is not a JSON object")
        return None

    version = doc.get("schema_version")
    # `True == 1` in Python, so an equality test alone would accept
    # `"schema_version": true` as version 1 and validate the whole
    # document against a version it never declared.
    if isinstance(version, bool) or not isinstance(version, int) \
            or version != BUDGET_SCHEMA_VERSION:
        report.err(
            where,
            f"unsupported schema_version {render_scalar(version)}: this "
            f"tool reads {BUDGET_SCHEMA_VERSION}")
        return None

    images = doc.get("animation_images")
    byte_budget = doc.get("resident_bytes")
    if not isinstance(images, dict) or not isinstance(byte_budget, dict):
        report.err(
            where,
            "the budget document needs both an 'animation_images' and a "
            "'resident_bytes' object")
        return None

    before = len(report.errors)
    # ONE sweep, checking presence and type together, so no required
    # field can be present-but-meaningless. The values the check
    # actually computes with are picked back out of `checked`.
    checked: Dict[str, Any] = {}
    for block, block_name, keys in (
            (images, "animation_images", BUDGET_IMAGE_KEYS),
            (byte_budget, "resident_bytes", BUDGET_BYTE_KEYS)):
        for key, kind in keys:
            checked[f"{block_name}.{key}"] = _budget_field(
                report, f"{where} {block_name}", block, key, kind)

    per_anim = checked["animation_images.max_per_animation"]
    threshold = checked["resident_bytes.threshold"]
    unit_name = checked["resident_bytes.unit"]
    projection = checked["resident_bytes.projection"]
    factor: Optional[float] = None
    if projection is not None:
        raw_factor = projection.get("roster_growth_factor")
        if isinstance(raw_factor, bool) or not isinstance(
                raw_factor, (int, float)):
            report.err(
                f"{where} resident_bytes",
                f"roster_growth_factor must be a number, got "
                f"{render_scalar(raw_factor)}")
        elif not is_representable_number(raw_factor):
            report.err(
                f"{where} resident_bytes",
                f"roster_growth_factor is not finite: "
                f"{render_scalar(raw_factor)}")
        elif raw_factor < 1:
            report.err(
                f"{where} resident_bytes",
                f"roster_growth_factor must be at least 1 (a projection "
                f"that shrinks the roster cannot trigger anything): "
                f"{render_scalar(raw_factor)}")
        else:
            factor = float(raw_factor)

    if per_anim is not None and per_anim < 1:
        report.err(
            f"{where} animation_images",
            f"max_per_animation must be at least 1, got {per_anim}")
        per_anim = None
    if threshold is not None and threshold < 1:
        report.err(
            f"{where} resident_bytes",
            f"threshold must be a positive byte count, got {threshold}")
        threshold = None
    # The tool measures in bytes and reports in bytes. A document
    # declaring some other unit would be compared against the wrong
    # scale while looking perfectly well-formed.
    if unit_name is not None and unit_name != "bytes":
        report.err(
            f"{where} resident_bytes",
            f"unit must be 'bytes' — this tool measures decoded RGBA8 byte "
            f"counts — got {render_scalar(unit_name)}")

    if len(report.errors) != before or per_anim is None \
            or threshold is None or factor is None:
        return None
    return Budget(per_anim, threshold, factor)


def read_stored_index(path: Path) -> Optional[Dict[str, Any]]:
    """The stored index as a plain document, or ``None`` if unusable."""
    raw = read_bytes_or_none(path)
    if raw is None:
        return None
    try:
        doc = json.loads(raw.decode("utf-8"))
    except (json.JSONDecodeError, UnicodeDecodeError):
        return None
    return doc if isinstance(doc, dict) else None


def validate_budget(
    report: Report, root: Path, decls: Sequence[UnitDecl],
    only_unit: Optional[str],
) -> Optional[BudgetTally]:
    """The per-unit atlas-artifact budget, and the TEX-5 memory trigger.

    Two independent budgets, one policy document
    (`tools/unit_texture_budget.json`). **Both read the COMPILED TREE
    and nothing else** — the stored `atlas/index.json` documents and the
    entries beside them. Neither one observes the loader, the texture
    request queue, the bindless table, or a running engine, so neither
    can see a runtime regression that leaves `atlas/` alone.

    **Generated atlas entries per indexed animation.** D-2 fixes atlas
    granularity at one image per ANIMATION, so a unit's compiler-owned
    `atlas/` directory must hold exactly `max_per_animation *
    (animations the index declares)` entries besides `index.json` — a
    bound DERIVED from the authoritative index, never a frozen roster
    total and never a frame count, so it keeps holding as animations
    are added. A per-frame regression IN THE ASSET TREE — one generated
    file per FRAME where one per animation belongs — fails this
    immediately and by path.

    That is an artifact check, not a registration one. The RUNTIME bound
    (one queued atlas upload request and one distinct logical texture
    handle per animation, each published into the definition's animation
    storage, and no per-frame ordinary requests) belongs to the Hspec
    group `Unit.Atlas.Load — the real unit registration boundary` in the
    always-blocking headless suite. A loader change that queued two
    requests per animation while leaving `atlas/` untouched fails there
    and is invisible here.

    Non-animation textures are excluded by construction rather than by
    an exemption list: portraits, the direct `sprite`, its
    `directional_sprites` T-pose overrides and `unknown_unit/rotations`
    all live OUTSIDE `atlas/` and are named by no index, and D-8 leaves
    them on ordinary single-texture loading.

    **Projected decoded bytes.** The decoded RGBA8 footprint the roster
    would cost, PROJECTED from each index's declared `atlas_width x
    atlas_height x 4` rather than measured anywhere — the scope is the
    whole tracked roster because `scripts/startup_loader.lua` feeds
    every `data/units/*.yaml` to the loader at boot. Two quantities,
    kept distinct: that index-projected total, and the same total scaled
    by the recorded `roster_growth_factor`, which is the only one
    compared against the owner-confirmed threshold. Exceeding it is
    D-10's precondition for resuming deferred TEX-5. That one is a
    WARNING — a breach is a project decision to make, not a broken tree
    — so a plain run reports it and `--strict` (CI, `make ci`) fails on
    it rather than letting the trigger pass unnoticed.

    Note this is NOT D-12's guardrail, which caps tracked derived
    artifact bytes ON DISK at two times their source frames. That is
    repository size; this is a projection of decoded memory.
    """
    budget = load_budget(report, root)
    if budget is None:
        return None

    tally = BudgetTally()
    for decl in decls:
        atlas_dir = root / atlas_dir_rel(decl.name)
        index_path = atlas_dir / INDEX_FILENAME
        if not atlas_dir.is_dir() or not index_path.is_file():
            # An uncompiled working copy. `validate_indices` owns that
            # judgement (and the engine's stricter one); the budget has
            # nothing to weigh.
            continue
        doc = read_stored_index(index_path)
        anims = doc.get("animations") if doc else None
        if not isinstance(anims, list):
            # Unusable index. Already reported by `validate_indices`
            # against a fresh regeneration, which gives a far better
            # diagnostic than "the budget could not read this".
            continue

        where = f"{decl.name}/{ATLAS_DIR_NAME}"
        claimed: Dict[str, List[str]] = {}
        for entry in anims:
            if not isinstance(entry, dict):
                continue
            name = entry.get("name")
            atlas_path = entry.get("atlas_path")
            if not isinstance(name, str) or not isinstance(atlas_path, str):
                continue
            claimed.setdefault(PurePosixPath(atlas_path).name, []).append(name)
            width = entry.get("atlas_width")
            height = entry.get("atlas_height")
            if isinstance(width, int) and isinstance(height, int) \
                    and not isinstance(width, bool) \
                    and not isinstance(height, bool):
                tally.resident_bytes += width * height * 4
            for direction in entry.get("directions") or []:
                if isinstance(direction, dict):
                    count = direction.get("frame_count")
                    if isinstance(count, int) and not isinstance(count, bool):
                        tally.frames += count

        # Two animations naming ONE image would keep the entry count
        # right while leaving the second animation with no atlas of its
        # own, so it is reported on its own terms rather than through
        # the count below.
        for basename, owners in sorted(claimed.items()):
            if len(owners) > 1:
                report.err(
                    where,
                    f"budget: {basename} is claimed by "
                    f"{len(owners)} animations ({', '.join(sorted(owners))}); "
                    f"D-2 gives each animation its own atlas")

        expected = budget.max_per_animation * len(anims)
        present = sorted(
            p.name for p in atlas_dir.iterdir() if p.name != INDEX_FILENAME)
        unclaimed = [name for name in present if name not in claimed]
        if len(present) != expected or unclaimed:
            shown = ", ".join(unclaimed[:6]) or "none"
            if len(unclaimed) > 6:
                shown += f", … (+{len(unclaimed) - 6} more)"
            report.err(
                where,
                f"budget: expected {expected} generated atlas entr(ies) "
                f"besides {INDEX_FILENAME} ({len(anims)} indexed "
                f"animation(s) x {budget.max_per_animation}), "
                f"found {len(present)}. Unclaimed by any animation: {shown}. "
                f"One atlas per indexed animation is D-2's contract; this "
                f"counts the compiled tree, never what the loader "
                f"registers at runtime.")

        tally.units += 1
        tally.animations += len(anims)
        tally.images += len(present)

    # The memory trigger aggregates the WHOLE roster (see the docstring),
    # so a single-unit run cannot evaluate it and must not pretend to.
    if only_unit is None and tally.units:
        projected = budget.projected(tally.resident_bytes)
        if projected > budget.threshold_bytes:
            report.warn(
                BUDGET_REL.as_posix(),
                f"unit-texture memory budget exceeded: "
                f"{mib(tally.resident_bytes)} projected from the stored "
                f"indices x {budget.growth_factor:g} roster growth = "
                f"{mib(projected)} > "
                f"{mib(budget.threshold_bytes)} threshold. This is D-10's "
                f"precondition for resuming deferred TEX-5 (KTX2 atlas "
                f"loading) — resume it, or have the owner re-confirm a new "
                f"threshold in {BUDGET_REL.as_posix()}.")
    return tally


def mib(value: float) -> str:
    return f"{value / (1024 * 1024):.2f} MiB"
