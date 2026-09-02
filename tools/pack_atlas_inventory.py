#!/usr/bin/env python3
"""Path ownership and the filesystem-first source inventory of the
unit-atlas tool (issue #2054, requirement 8).

The ONE owner of the inventory gate: exact unit/animation/direction
containment and the absolute/traversal rejections of a declared frame
path (`resolve_frame_path`), the direction-set and numbering rules,
the once-reported decoder gate (`ContentGate`) and the per-animation
dimension-consistency check, the filesystem-first walk that finds every
PNG the declarations must account for (`walk_physical`, with its
symlink, loose-file and unknown-direction diagnostics), and the gate
itself (`validate_sources`), which owns the duplicate-claim ledger, the
auxiliary-path existence checks and the orphaned-frame diagnostic.

The fail-closed rules are preserved exactly (requirement 9): there is
no filename, directory, glob, ignored-file or platform-metadata
exemption here, and none may be added.

Consumes the declarations owner, the image owner and the shared
definitions. The public façade is tools/pack_atlas.py.
"""
from __future__ import annotations

from pathlib import Path, PurePosixPath
from typing import Dict, List, Optional, Sequence, Set, Tuple

from pack_atlas_declarations import load_declarations
from pack_atlas_image import (
    ImageBackendMissing, image_module, validate_frame_image,
)
from pack_atlas_shared import (
    ALL_DIRS, ANIM_IDENT_RE, ASSET_PREFIX, CANONICAL_DIRS, FRAME_RE,
    UNIT_IDENT_RE, AnimDecl, Report, Totals, UnitDecl, check_no_symlink,
    normalise_dir,
)


# --------------------------------------------------------------------
# Path ownership
# --------------------------------------------------------------------


def resolve_frame_path(
    report: Report,
    root: Path,
    anim: AnimDecl,
    direction: str,
    index: int,
    declared: str,
) -> Optional[Path]:
    """Validate one declared frame path and return its real location.

    Containment is checked against the EXACT expected
    ``assets/textures/units/<unit>/animations/<animation>/<direction>/``
    directory — not merely against the repository or the units root — so
    a cross-unit, cross-animation or cross-direction reference is named
    as such instead of passing as "somewhere under assets/".
    """
    where = f"{anim.where}/{direction}/#{index}"
    pure = PurePosixPath(declared)

    if pure.is_absolute() or declared.startswith("/") or ":" in declared[:3]:
        report.err(where, f"absolute path is not allowed: {declared}")
        return None
    if ".." in pure.parts:
        report.err(where, f"'..' traversal is not allowed: {declared}")
        return None
    if "." in pure.parts:
        report.err(where, f"'.' component is not allowed: {declared}")
        return None

    parts = pure.parts
    if len(parts) != 8 or parts[:3] != ASSET_PREFIX:
        report.err(
            where,
            f"path escapes the unit asset root: {declared} (expected "
            f"assets/textures/units/{anim.unit}/animations/{anim.name}/"
            f"<direction>/frame_NNN.png)")
        return None
    if parts[3] != anim.unit:
        report.err(
            where,
            f"cross-unit reference: {declared} is owned by unit "
            f"'{parts[3]}', not '{anim.unit}'")
        return None
    if parts[4] != "animations":
        report.err(
            where,
            f"path is not under {anim.unit}/animations/: {declared}")
        return None
    if parts[5] != anim.name:
        report.err(
            where,
            f"cross-animation reference: {declared} lives in animation "
            f"'{parts[5]}', not '{anim.name}'")
        return None
    if normalise_dir(parts[6]) != direction:
        report.err(
            where,
            f"cross-direction reference: {declared} lives in direction "
            f"'{parts[6]}', but is declared under '{direction}'")
        return None
    if not FRAME_RE.match(parts[7]):
        report.err(
            where,
            f"frame filename must match frame_NNN.png: {parts[7]}")
        return None

    link = check_no_symlink(root, pure)
    if link is not None:
        report.err(where, f"symlink in the ownership path: {link}")
        return None

    abs_p = root / pure
    if not abs_p.is_file():
        if abs_p.is_dir():
            report.err(
                where,
                f"declared frame is a directory, not a file: {declared}")
        else:
            report.err(where, f"missing file: {declared}")
        return None

    expected_dir = (root / PurePosixPath(*parts[:7])).resolve()
    try:
        real = abs_p.resolve(strict=True)
    except OSError as error:
        report.err(where, f"cannot resolve {declared}: {error}")
        return None
    if real.parent != expected_dir:
        report.err(
            where,
            f"resolved path {real} escapes the expected directory "
            f"{expected_dir}")
        return None
    return real


def validate_direction_set(report: Report, anim: AnimDecl) -> None:
    declared = set(anim.frames)
    if not declared:
        return
    expected = CANONICAL_DIRS if anim.flip else ALL_DIRS
    if declared == expected:
        return
    missing = sorted(expected - declared)
    extra = sorted(declared - expected)
    detail = []
    if missing:
        detail.append(f"missing {', '.join(missing)}")
    if extra:
        detail.append(f"unexpected {', '.join(extra)}")
    if anim.flip:
        report.err(
            anim.where,
            f"flip: true requires exactly the canonical five authored "
            f"directions (south, south-east, east, north-east, north): "
            f"{'; '.join(detail)}")
    else:
        report.err(
            anim.where,
            f"flip: false requires exactly all eight compass directions: "
            f"{'; '.join(detail)}")


def validate_numbering(
    report: Report, anim: AnimDecl, direction: str, names: List[str],
) -> None:
    where = f"{anim.where}/{direction}"
    indices: List[int] = []
    for name in names:
        m = FRAME_RE.match(name)
        if m is None:
            return  # already reported by resolve_frame_path
        indices.append(int(m.group(1)))

    counts: Dict[int, int] = {}
    for i in indices:
        counts[i] = counts.get(i, 0) + 1
    dupes = sorted(i for i, c in counts.items() if c > 1)
    if dupes:
        report.err(
            where,
            f"duplicate frame index/indices "
            f"{', '.join(f'{i:03d}' for i in dupes)}")
    # ORDER, not just the set. Runtime playback walks the YAML list in
    # the order it is written (`uyaFrames` is a list per direction), so
    # a contiguous-but-shuffled list plays the animation out of
    # sequence while every set-based check below still passes.
    if indices != sorted(indices):
        first = next(i for i in range(1, len(indices))
                     if indices[i] < indices[i - 1])
        report.err(
            where,
            f"declared frames are out of order: frame_{indices[first]:03d}"
            f".png is listed after frame_{indices[first - 1]:03d}.png. "
            f"Playback follows the declared order, so the list must "
            f"ascend.")

    ordered = sorted(counts)
    if ordered[0] != 0:
        report.err(
            where,
            f"frame sequence must begin at frame_000.png, begins at "
            f"frame_{ordered[0]:03d}.png")
    gaps = [i for i in range(ordered[0], ordered[-1])
            if i not in counts and i >= 0]
    if gaps:
        report.err(
            where,
            f"gap in frame numbering: missing "
            f"{', '.join(f'frame_{i:03d}.png' for i in gaps)}")


class ContentGate:
    """Whether the inventory's content pass can decode, reported once.

    An absent Pillow is an ERROR rather than a skip: a gate that
    quietly stopped opening frames would still print OK while
    validating nothing, which is the exact failure the content checks
    exist to prevent (#1311). It is reported ONCE rather than once per
    frame, because it is one condition about the environment and 4,620
    copies of it would bury every real finding.

    Probing is deferred to first use, so a run with no declared frames
    to decode still needs no decoder installed.
    """

    def __init__(self) -> None:
        self._reason: Optional[str] = None
        self._probed = False
        self._reported = False

    def usable(self, report: Report) -> bool:
        if not self._probed:
            self._probed = True
            try:
                image_module()
            except ImageBackendMissing as error:
                self._reason = str(error)
        if self._reason is None:
            return True
        if not self._reported:
            self._reported = True
            report.err("image decoder", self._reason)
        return False


def validate_animation_contents(
    report: Report, anim: AnimDecl, frames: Sequence[Tuple[str, Path]],
) -> None:
    """Decode one animation's frames and pin them to a single size.

    `frames` is every successfully resolved, singly-owned declared
    frame of this animation, as `(declared path, resolved file)`.

    A frame that fails to decode is reported and SKIPPED rather than
    aborting the animation: three corrupt frames should surface as
    three findings in one run, not across three consecutive re-runs.
    The size comparison then runs over whatever did decode, so an
    undecodable frame is never also reported as a size mismatch.
    """
    sizes: Dict[Tuple[int, int], str] = {}
    for declared, real in frames:
        try:
            size = validate_frame_image(real)
        except ValueError as error:
            report.err(anim.where, f"{declared}: {error}")
            continue
        # The first declaration of each distinct size wins as that
        # size's example, so the diagnostic is stable across runs.
        sizes.setdefault(size, declared)

    if len(sizes) > 1:
        detail = "; ".join(
            f"{width}x{height} (e.g. {example})"
            for (width, height), example in sorted(sizes.items()))
        report.err(
            anim.where,
            f"inconsistent frame dimensions: {detail}. Every frame of one "
            f"animation must decode to one pixel size — the atlas cell IS "
            f"that size and nothing here resamples. Frame COUNTS may still "
            f"differ between directions.")


# --------------------------------------------------------------------
# Filesystem inventory
# --------------------------------------------------------------------


def walk_physical(
    report: Report, root: Path, only_unit: Optional[str],
) -> Set[Path]:
    """Every PNG on disk that the inventory must account for.

    Walks `assets/textures/units/<unit>/animations/` and nothing else.
    Files outside `animations/` — `unknown_unit/rotations/*.png`, the
    per-unit `portrait.png` — are deliberately out of this issue's
    scope: they are referenced from Haskell or from non-animation YAML
    fields and are not animation frames.
    """
    units_root = root.joinpath(*ASSET_PREFIX)
    found: Set[Path] = set()
    if not units_root.is_dir():
        report.err(
            units_root.as_posix(), "unit asset root does not exist")
        return found

    for unit_dir in sorted(units_root.iterdir()):
        unit = unit_dir.name
        if only_unit is not None and unit != only_unit:
            continue
        # A symlink is an ERROR, never a skip. Skipping one was a hole in
        # the no-exemption contract: a symlinked unit tree would evade
        # the whole filesystem-first walk — and with it the identifier,
        # naming and ownership checks — while its frames still shipped.
        # `is_dir()` follows links, so every level below is checked the
        # same way, which is also the rule
        # `Engine.Preview.Unit.resolveUnitDir` applies to a unit
        # directory and its `animations/` root.
        if unit_dir.is_symlink():
            report.err(unit, f"symlinked unit directory: {unit}")
            continue
        if not unit_dir.is_dir():
            continue
        anim_root = unit_dir / "animations"
        if anim_root.is_symlink():
            report.err(unit, f"symlinked animations/ directory: {unit}")
            continue
        if not anim_root.is_dir():
            continue
        if not UNIT_IDENT_RE.match(unit):
            report.err(
                unit, f"unsafe unit directory name '{unit}': a unit "
                f"directory must be one lowercase [a-z0-9_]+ component")
            continue

        for anim_dir in sorted(anim_root.iterdir()):
            rel_anim = f"{unit}/{anim_dir.name}"
            if anim_dir.is_symlink():
                report.err(
                    rel_anim,
                    f"symlinked animation directory: "
                    f"{anim_dir.relative_to(root).as_posix()}")
                continue
            if not anim_dir.is_dir():
                report.err(
                    rel_anim,
                    f"loose file at the animation level: "
                    f"{anim_dir.relative_to(root).as_posix()}")
                continue
            if not ANIM_IDENT_RE.match(anim_dir.name):
                report.err(
                    rel_anim,
                    f"unsafe animation directory name '{anim_dir.name}': an "
                    f"animation directory must be one lowercase [a-z0-9_]+ "
                    f"component")
                continue

            for dir_dir in sorted(anim_dir.iterdir()):
                rel = dir_dir.relative_to(root).as_posix()
                if dir_dir.is_symlink():
                    report.err(
                        rel_anim, f"symlinked direction directory: {rel}")
                    continue
                if not dir_dir.is_dir():
                    report.err(
                        rel_anim, f"loose file at the direction level: {rel}")
                    continue
                if normalise_dir(dir_dir.name) is None:
                    report.err(
                        rel_anim,
                        f"unknown direction directory '{dir_dir.name}' "
                        f"(expected one of {', '.join(sorted(ALL_DIRS))})")
                    continue

                for entry in sorted(dir_dir.iterdir()):
                    erel = entry.relative_to(root).as_posix()
                    if entry.is_dir():
                        report.err(
                            rel_anim,
                            f"unexpected directory below the direction "
                            f"level: {erel}")
                        continue
                    if entry.suffix.lower() != ".png":
                        report.warn(
                            rel_anim,
                            f"non-PNG file in the animation tree: {erel}")
                        continue
                    if entry.is_symlink():
                        report.err(rel_anim, f"symlinked frame: {erel}")
                        continue
                    if not FRAME_RE.match(entry.name):
                        report.err(
                            rel_anim,
                            f"frame filename must match frame_NNN.png: {erel}")
                        continue
                    found.add(entry.resolve())
    return found


# --------------------------------------------------------------------
# The inventory gate
# --------------------------------------------------------------------


def validate_sources(
    root: Path, only_unit: Optional[str], report: Report,
) -> Tuple[Totals, List[UnitDecl]]:
    """The inventory gate: declarations against the filesystem, and
    since #1311 against each declared frame's actual CONTENTS.

    Split out from `validate` because `--compile` needs exactly this
    half — an out-of-date generated index is the very thing a compile
    fixes, so refusing to compile on one would be a deadlock. Compile
    consequently decodes each frame here as well as in `plan_animation`;
    that second read costs roughly half a second over the whole corpus
    and buys one statement — the compiler runs THE inventory gate, not a
    reduced variant of it.
    """
    totals = Totals()
    content = ContentGate()
    decls = load_declarations(report, root / "data" / "units")
    if only_unit is not None:
        decls = [d for d in decls if d.name == only_unit]

    physical = walk_physical(report, root, only_unit)

    # A `--unit` naming nothing at all is a typo, not a clean run of an
    # empty inventory. Without this it exits 0 reporting "0 unit
    # declaration(s), 0 frame(s)" — which reads exactly like a pass.
    if only_unit is not None and not decls and not physical:
        report.err(
            only_unit,
            f"no such unit: '{only_unit}' has neither a declaration in "
            f"data/units/ nor an asset tree under "
            f"{'/'.join(ASSET_PREFIX)}/")
        return totals, decls
    claimed: Dict[Path, str] = {}

    for decl in decls:
        totals.units += 1
        if decl.asset_only:
            totals.asset_only += 1
        for where, p in decl.aux_paths:
            # Existence only. Reuse of an animation frame here is legal
            # and must never register as a duplicate animation claim.
            candidate = root / p
            if ".." in PurePosixPath(p).parts or PurePosixPath(p).is_absolute():
                report.err(where, f"unsafe path: {p}")
            elif not candidate.is_file():
                report.err(where, f"missing file: {p}")

        for anim in decl.anims:
            totals.animations += 1
            validate_direction_set(report, anim)
            # Contents are checked per ANIMATION, not per direction: one
            # pixel size spans the whole animation, so the comparison
            # needs every direction's frames together.
            openable: List[Tuple[str, Path]] = []
            for direction in sorted(anim.frames):
                declared_paths = anim.frames[direction]
                names = [PurePosixPath(p).name for p in declared_paths]
                validate_numbering(report, anim, direction, names)
                for index, declared in enumerate(declared_paths):
                    real = resolve_frame_path(
                        report, root, anim, direction, index, declared)
                    if real is None:
                        continue
                    totals.frames += 1
                    owner = f"{anim.unit}/{anim.name}/{direction}"
                    if real in claimed:
                        report.err(
                            anim.where,
                            f"duplicate animation-frame claim on {declared}: "
                            f"already owned by {claimed[real]}")
                        continue
                    claimed[real] = owner
                    openable.append((declared, real))
            if openable and content.usable(report):
                validate_animation_contents(report, anim, openable)

    for orphan in sorted(physical - set(claimed)):
        rel = orphan.relative_to(root.resolve()).as_posix() \
            if orphan.is_relative_to(root.resolve()) else orphan.as_posix()
        report.err(
            rel.split("/")[3] if rel.startswith("assets/") else "?",
            f"unclassified frame on disk (no animation declaration owns "
            f"it): {rel}")

    return totals, decls
