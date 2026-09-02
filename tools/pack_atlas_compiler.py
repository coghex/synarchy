#!/usr/bin/env python3
"""Atlas compilation for the unit-atlas tool (issue #2054,
requirement 10).

The ONE owner of everything that turns validated declarations into
derived artifacts: animation planning and row geometry
(`plan_animation`), the one-texel extrusion gutter and the transparent
rectangular padding (`extruded_slot`, `compose_atlas`), the
length-prefixed digest stream and the two digests taken over it
(`source_digest`, `content_digest`), canonical index construction and
its one permitted serialization (`build_index_document`,
`canonical_index_bytes`), deterministic PNG encoding, the
compiler-owned output directory's containment and obsolescence sweep
(`resolve_atlas_dir`, `sweep_atlas_dir`), and the incremental
write decisions of `compile_unit`.

Locality is preserved exactly (requirement 11): sources are never
modified, only a changed atlas and its unit index are written, an
unchanged atlas is not rewritten, obsolete files are removed only from
that unit's own `atlas/` directory, and a dry run performs no write or
removal.

Index validation reuses the planning and digest interfaces here rather
than restating them (requirements 12 and 17). Consumes the image owner
and the shared definitions. The public façade is tools/pack_atlas.py.
"""
from __future__ import annotations

import hashlib
import io
import json
import struct
from pathlib import Path, PurePosixPath
from typing import Any, Dict, List, Optional, Sequence, Set, Tuple

from pack_atlas_image import ImageBackendMissing, decode_rgba8, image_module
from pack_atlas_shared import (
    ASSET_PREFIX, ATLAS_DIGEST_TAG, ATLAS_DIRECTION_ORDER, DIGEST_ALGORITHM,
    GENERATOR, INDEX_FILENAME, INDEX_SCHEMA_VERSION, SOURCE_DIGEST_TAG,
    STORAGE_FORMAT, TOOL_VERSION, AnimDecl, AnimPlan, CompileOutcome,
    DirectionPlan, Frame, Report, UnitDecl, atlas_dir_rel, atlas_file_rel,
    check_no_symlink, read_bytes_or_none,
)


# --------------------------------------------------------------------
# Geometry, digests, and the generated index (#1258)
# --------------------------------------------------------------------


def plan_animation(
    report: Report, root: Path, anim: AnimDecl,
) -> Optional[AnimPlan]:
    """Decode one animation's frames and fix its atlas geometry.

    Returns ``None`` — having reported why — if anything about the
    animation makes it uncompilable. A unit with one such animation
    emits NO artifacts at all, so a half-written index can never name
    an atlas that was never produced.
    """
    ordered = [d for d in ATLAS_DIRECTION_ORDER if d in anim.frames]
    if not ordered:
        report.err(anim.where, "no usable direction has any frames")
        return None

    directions: List[DirectionPlan] = []
    cell: Optional[Tuple[int, int]] = None
    cell_source = ""
    for row, direction in enumerate(ordered):
        declared = anim.frames[direction]
        frames: List[Frame] = []
        for declared_path in declared:
            source = root / PurePosixPath(declared_path)
            if not source.is_file():
                # Reported by the inventory gate too, but reaching the
                # decoder with a missing file would report it as an
                # undecodable image, which is a different problem.
                report.err(f"{anim.where}/{direction}",
                           f"missing file: {declared_path}")
                return None
            try:
                frame = decode_rgba8(source)
            except ImageBackendMissing as error:
                report.err(anim.where, str(error))
                return None
            except ValueError as error:
                report.err(f"{anim.where}/{direction}",
                           f"{declared_path}: {error}")
                return None
            if frame.width <= 0 or frame.height <= 0:
                report.err(f"{anim.where}/{direction}",
                           f"{declared_path}: zero-sized image")
                return None
            # Every frame of ONE animation shares a cell. A mismatch is
            # an error rather than an implicit rescale or crop: D-6 keeps
            # nearest mode pixel-identical to the source and makes linear
            # filtering a runtime sampler choice, so the compiler must not
            # alter the authored texels for either mode.
            if cell is None:
                cell = (frame.width, frame.height)
                cell_source = declared_path
            elif (frame.width, frame.height) != cell:
                report.err(
                    f"{anim.where}/{direction}",
                    f"inconsistent frame dimensions: {declared_path} is "
                    f"{frame.width}x{frame.height}, but {cell_source} is "
                    f"{cell[0]}x{cell[1]}. Every frame of one animation "
                    f"must share the atlas cell size.")
                return None
            frames.append(frame)
        directions.append(DirectionPlan(direction, row, list(declared), frames))

    if cell is None:
        report.err(anim.where, "no frames to compile")
        return None

    return AnimPlan(
        unit=anim.unit,
        name=anim.name,
        flip=anim.flip,
        fps=anim.fps,
        loop=anim.loop,
        directions=directions,
        cell_width=cell[0],
        cell_height=cell[1],
        columns=max(len(d.frames) for d in directions),
        atlas_rel=atlas_file_rel(anim.unit, anim.name).as_posix(),
    )


def extruded_slot(plan: AnimPlan, frame: Frame) -> bytes:
    """One frame's PHYSICAL slot: the frame plus its extrusion ring.

    The result is `slot_width` x `slot_height` canonical RGBA8 samples,
    with the frame occupying the inner rect at offset
    `(cell_padding, cell_padding)` and the border filled by copying the
    frame's own outermost texels outward — edges from the adjacent edge
    row or column, and each corner from the single corner texel it
    touches, duplicated into the whole corner square.

    That is the whole of #2076: a bilinear tap taken anywhere inside the
    logical cell reaches at most `cell_padding` texels past its edge and
    so reads a copy of this frame rather than a neighbouring one. The
    samples are COPIED, never blended or resampled — the ring holds
    exact duplicates of real frame texels, which is what keeps the
    nearest-mode picture byte-identical (the UV rect still addresses the
    inner rect alone).
    """
    pad = plan.cell_padding
    src_row = plan.cell_width * 4
    dst_row = plan.slot_width * 4
    slot = bytearray(dst_row * plan.slot_height)

    for line in range(plan.cell_height):
        source = frame.pixels[line * src_row:(line + 1) * src_row]
        left = source[:4] * pad
        right = source[-4:] * pad
        start = (line + pad) * dst_row
        slot[start:start + dst_row] = left + source + right

    # The horizontal gutters are whole padded rows copied from the
    # already-extruded first and last interior rows, which is what makes
    # every corner square a duplicate of the corner texel it touches
    # without a second special case.
    first = bytes(slot[pad * dst_row:(pad + 1) * dst_row])
    last = bytes(slot[(pad + plan.cell_height - 1) * dst_row:
                      (pad + plan.cell_height) * dst_row])
    for line in range(pad):
        slot[line * dst_row:(line + 1) * dst_row] = first
        bottom = pad + plan.cell_height + line
        slot[bottom * dst_row:(bottom + 1) * dst_row] = last

    return bytes(slot)


def compose_atlas(plan: AnimPlan) -> bytes:
    """The atlas's canonical RGBA8 samples.

    The buffer starts as zeroes, so every SLOT a direction does not
    reach stays fully transparent RGBA(0, 0, 0, 0), gutters included.
    That is the ONLY thing a rectangularization slot is for: squaring
    off a row shorter than the animation's maximum authored frame
    count. Nothing addresses one as a frame — the index's per-direction
    `frame_count` is the sole frame authority — and an authored cell
    beside one still reads its OWN extrusion ring, never the transparent
    neighbour, because the gutters belong to the cells they surround.

    Each frame is written as its `extruded_slot`, scanline by scanline
    as raw bytes, so no blending, compositing or resampling can occur.
    """
    stride = plan.atlas_width * 4
    buffer = bytearray(stride * plan.atlas_height)
    slot_row = plan.slot_width * 4
    for direction in plan.directions:
        top = direction.row * plan.slot_height
        for column, frame in enumerate(direction.frames):
            left = column * slot_row
            slot = extruded_slot(plan, frame)
            for line in range(plan.slot_height):
                start = (top + line) * stride + left
                source = line * slot_row
                buffer[start:start + slot_row] = \
                    slot[source:source + slot_row]
    return bytes(buffer)


def digest_stream(tag: bytes, fields: Sequence[Tuple[str, bytes]]) -> str:
    """A canonical, length-prefixed digest over labelled fields.

    Every field carries its label and an explicit byte length, so no
    concatenation of two different field sequences can produce the same
    stream — the failure mode a bare `b"".join` invites, where moving a
    character across a boundary leaves the hash unchanged.
    """
    digest = hashlib.sha256()
    digest.update(struct.pack("<Q", len(tag)))
    digest.update(tag)
    for label, value in fields:
        encoded = label.encode("utf-8")
        digest.update(struct.pack("<Q", len(encoded)))
        digest.update(encoded)
        digest.update(struct.pack("<Q", len(value)))
        digest.update(value)
    return digest.hexdigest()


def source_digest(plan: AnimPlan) -> str:
    """This animation's own source digest.

    PER ANIMATION by design (D-12): the whole point of one atlas per
    animation is that editing one cannot invalidate another, and a
    digest taken over a whole unit would give that back. The inputs are
    everything the compiled artifacts depend on — the animation's
    identity, its mirroring/timing declarations, its cell geometry
    INCLUDING the extrusion gutter, and for each direction in atlas
    order its declared source paths and their canonical decoded
    pixels — in one fixed order.

    `cell_padding` is a digest input because it changes the artifact
    every other input would otherwise describe identically: the same
    frames at a different gutter compile to a different sheet. The
    domain tag carries `v2` for that addition, so no recorded v1 digest
    can collide with a v2 one taken over the same art.
    """
    fields: List[Tuple[str, bytes]] = [
        ("unit", plan.unit.encode("utf-8")),
        ("animation", plan.name.encode("utf-8")),
        ("flip", b"1" if plan.flip else b"0"),
        ("loop", b"1" if plan.loop else b"0"),
        ("fps", repr(plan.fps).encode("ascii")),
        ("cell", f"{plan.cell_width}x{plan.cell_height}".encode("ascii")),
        ("cell_padding", str(plan.cell_padding).encode("ascii")),
        ("columns", str(plan.columns).encode("ascii")),
        ("direction_count", str(len(plan.directions)).encode("ascii")),
    ]
    for direction in plan.directions:
        fields.append(("direction", direction.direction.encode("utf-8")))
        fields.append(("row", str(direction.row).encode("ascii")))
        fields.append(
            ("frame_count", str(len(direction.frames)).encode("ascii")))
        for declared, frame in zip(direction.declared, direction.frames):
            fields.append(("frame_path", declared.encode("utf-8")))
            fields.append(
                ("frame_size", f"{frame.width}x{frame.height}".encode("ascii")))
            fields.append(("frame_pixels", frame.pixels))
    return digest_stream(SOURCE_DIGEST_TAG, fields)


def content_digest(width: int, height: int, pixels: bytes) -> str:
    """A digest over decoded RGBA8 CONTENT, never over file bytes.

    An atlas re-encoded by a different PNG writer holds the same
    picture, so pinning the encoded bytes would report a false staleness
    the moment the toolchain moved. Pinning the samples still catches
    every pixel-level tamper, which is what the guarantee is actually
    about.
    """
    return digest_stream(ATLAS_DIGEST_TAG, [
        ("width", str(width).encode("ascii")),
        ("height", str(height).encode("ascii")),
        ("pixels", pixels),
    ])


def build_index_document(unit: str, plans: Sequence[AnimPlan]) -> Dict[str, Any]:
    """The complete generated index for one unit.

    Key order is the insertion order below and animations are sorted by
    name, so the document is a pure function of its inputs — which is
    what lets validation regenerate it and compare bytes.

    Nobody hand-edits this file (D-11). It carries no field a human
    would need to choose.
    """
    animations: List[Dict[str, Any]] = []
    for plan in sorted(plans, key=lambda p: p.name):
        pixels = compose_atlas(plan)
        animations.append({
            "name": plan.name,
            "storage_format": STORAGE_FORMAT,
            "atlas_path": plan.atlas_rel,
            "atlas_width": plan.atlas_width,
            "atlas_height": plan.atlas_height,
            "cell_width": plan.cell_width,
            "cell_height": plan.cell_height,
            "cell_padding": plan.cell_padding,
            "columns": plan.columns,
            "rows": plan.rows,
            "flip": plan.flip,
            "fps": plan.fps,
            "loop": plan.loop,
            "directions": [
                {
                    "direction": d.direction,
                    "row": d.row,
                    "frame_count": len(d.frames),
                }
                for d in plan.directions
            ],
            "source_digest": source_digest(plan),
            "atlas_digest": content_digest(
                plan.atlas_width, plan.atlas_height, pixels),
        })
    return {
        "schema_version": INDEX_SCHEMA_VERSION,
        "generator": GENERATOR,
        "tool_version": TOOL_VERSION,
        "digest_algorithm": DIGEST_ALGORITHM,
        "unit": unit,
        "direction_order": list(ATLAS_DIRECTION_ORDER),
        "animations": animations,
    }


def canonical_index_bytes(document: Dict[str, Any]) -> bytes:
    """The one permitted serialization of an index document."""
    return (json.dumps(document, indent=2, ensure_ascii=True,
                       sort_keys=False, allow_nan=False) + "\n").encode("utf-8")


def encode_atlas_png(width: int, height: int, pixels: bytes) -> bytes:
    """Encode RGBA8 samples as a lossless PNG.

    `optimize` and `compress_level` are set explicitly rather than left
    to Pillow's defaults so the encode is a fixed function of the
    samples for a given toolchain.
    """
    image_mod = image_module()
    image = image_mod.frombytes("RGBA", (width, height), pixels)
    buffer = io.BytesIO()
    image.save(buffer, format="PNG", optimize=False, compress_level=9)
    return buffer.getvalue()


# --------------------------------------------------------------------
# Compiler-owned output directory
# --------------------------------------------------------------------


def resolve_atlas_dir(
    report: Report, root: Path, unit: str,
) -> Optional[Path]:
    """The unit's own compiler-owned output directory, or ``None``.

    Containment is checked after canonical resolution and every
    component is refused if it is a symlink, so no link can redirect
    writes — or a later obsolescence sweep's DELETES — outside this
    unit's own tree.
    """
    rel = atlas_dir_rel(unit)
    link = check_no_symlink(root, rel)
    if link is not None:
        report.err(unit, f"symlink in the atlas output path: {link}")
        return None

    unit_dir = root.joinpath(*ASSET_PREFIX, unit)
    if not unit_dir.is_dir():
        report.err(unit, f"unit asset directory does not exist: "
                         f"{rel.parent.as_posix()}")
        return None

    atlas_dir = root / rel
    if atlas_dir.exists() and not atlas_dir.is_dir():
        report.err(unit, f"atlas output path is not a directory: "
                         f"{rel.as_posix()}")
        return None
    if atlas_dir.is_dir() and atlas_dir.resolve().parent != unit_dir.resolve():
        report.err(unit, f"atlas output directory escapes the unit tree: "
                         f"{atlas_dir.resolve()}")
        return None
    return atlas_dir


def sweep_atlas_dir(
    report: Report, root: Path, unit: str, atlas_dir: Path,
    keep: Set[str],
) -> Optional[List[str]]:
    """Classify everything in the output directory; list what is obsolete.

    Only ordinary `<animation>.png` files this compiler produces are
    ever removable, and only from this one directory — which sits
    beside `animations/`, never inside it, so source artwork is
    structurally unreachable. Anything unexpected is REPORTED and left
    alone rather than deleted.
    """
    if not atlas_dir.is_dir():
        return []
    obsolete: List[str] = []
    failed = False
    for entry in sorted(atlas_dir.iterdir()):
        rel = f"{atlas_dir_rel(unit).as_posix()}/{entry.name}"
        if entry.is_symlink():
            report.err(unit, f"symlink in the compiler-owned atlas "
                             f"directory: {rel}")
            failed = True
            continue
        if entry.is_dir():
            report.err(unit, f"unexpected directory in the compiler-owned "
                             f"atlas directory: {rel}")
            failed = True
            continue
        if entry.name == INDEX_FILENAME:
            continue
        if not entry.name.endswith(f".{STORAGE_FORMAT}"):
            report.err(unit, f"unexpected file in the compiler-owned atlas "
                             f"directory: {rel}")
            failed = True
            continue
        if entry.name not in keep:
            obsolete.append(rel)
    return None if failed else obsolete


# --------------------------------------------------------------------
# Compile
# --------------------------------------------------------------------


def compile_unit(
    report: Report, root: Path, decl: UnitDecl, dry_run: bool,
) -> Optional[CompileOutcome]:
    """Compile one unit's animations. Nothing is written on any error."""
    output_dir = root / atlas_dir_rel(decl.name)
    if not decl.anims and not (output_dir.exists() or output_dir.is_symlink()):
        # Nothing declared and nothing generated. Most units are here:
        # a unit only acquires an output directory by being compiled.
        return CompileOutcome()

    atlas_dir = resolve_atlas_dir(report, root, decl.name)
    if atlas_dir is None:
        return None

    # A case-insensitive filesystem would silently collapse two
    # animations whose names differ only in case — and `_RH_` makes
    # mixed case reachable — into one atlas file, so the second would
    # overwrite the first while both index entries claimed it.
    by_filename: Dict[str, str] = {}
    for anim in decl.anims:
        filename = f"{anim.name}.{STORAGE_FORMAT}"
        clash = by_filename.get(filename.lower())
        if clash is not None:
            report.err(
                decl.name,
                f"animations '{clash}' and '{anim.name}' would compile to "
                f"the same atlas filename on a case-insensitive filesystem")
            return None
        by_filename[filename.lower()] = anim.name

    plans: List[AnimPlan] = []
    for anim in sorted(decl.anims, key=lambda a: a.name):
        plan = plan_animation(report, root, anim)
        if plan is None:
            return None
        plans.append(plan)

    document = build_index_document(decl.name, plans)
    index_bytes = canonical_index_bytes(document)
    index_path = atlas_dir / INDEX_FILENAME
    index_rel = f"{atlas_dir_rel(decl.name).as_posix()}/{INDEX_FILENAME}"

    obsolete = sweep_atlas_dir(
        report, root, decl.name, atlas_dir,
        {f"{p.name}.{STORAGE_FORMAT}" for p in plans})
    if obsolete is None:
        return None

    if not plans:
        # The unit's LAST animation was deleted or renamed away. Its
        # whole generated set is obsolete, index included: an index
        # describing no animations is a shape nothing downstream should
        # have to interpret, and removing it leaves exactly the
        # index-free state validation already accepts as legacy.
        outcome = CompileOutcome(removed=list(obsolete))
        if index_path.is_file():
            outcome.removed.append(index_rel)
        if not dry_run:
            try:
                for rel in outcome.removed:
                    (root / PurePosixPath(rel)).unlink()
            except OSError as error:
                report.err(decl.name,
                           f"cannot remove obsolete artifacts: {error}")
                return None
            try:
                # `rmdir` refuses a non-empty directory, so this can
                # only ever take away what the compiler put there. An
                # empty output directory left behind would read to
                # validation as an index-less generated set.
                atlas_dir.rmdir()
            except OSError:
                pass
        return outcome

    outcome = CompileOutcome(removed=obsolete)
    pending: List[Tuple[Path, bytes, str]] = []
    for plan in plans:
        pixels = compose_atlas(plan)
        target = root / PurePosixPath(plan.atlas_rel)
        # Compare by decoded CONTENT, not by encoded bytes: an atlas
        # that already holds exactly these samples is correct, and
        # rewriting it only because another PNG encoder produced it
        # would be pure churn against D-12's locality guardrail.
        if atlas_content_matches(target, plan.atlas_width,
                                 plan.atlas_height, pixels):
            outcome.unchanged += 1
            continue
        try:
            encoded = encode_atlas_png(
                plan.atlas_width, plan.atlas_height, pixels)
        except ImageBackendMissing as error:
            report.err(f"{decl.name}/{plan.name}", str(error))
            return None
        pending.append((target, encoded, plan.atlas_rel))

    index_current = read_bytes_or_none(index_path)
    index_changed = index_current != index_bytes
    if index_changed:
        outcome.written.append(index_rel)
    else:
        outcome.unchanged += 1
    outcome.written = [rel for _, _, rel in pending] + outcome.written

    if dry_run:
        return outcome

    try:
        atlas_dir.mkdir(parents=True, exist_ok=True)
        # Atlases first, index second: an interrupted compile then
        # leaves an index OLDER than its atlases, which validation
        # reports as stale — the recoverable direction. The reverse
        # order could leave an index naming an atlas that does not
        # exist yet.
        for target, encoded, _ in pending:
            target.write_bytes(encoded)
        if index_changed:
            index_path.write_bytes(index_bytes)
        # Obsolete output goes last, once the replacement is durable.
        for rel in obsolete:
            (root / PurePosixPath(rel)).unlink()
    except OSError as error:
        report.err(decl.name, f"cannot write compiled artifacts: {error}")
        return None
    return outcome


def atlas_content_matches(
    path: Path, width: int, height: int, pixels: bytes,
) -> bool:
    """Whether ``path`` already decodes to exactly these samples."""
    if not path.is_file() or path.is_symlink():
        return False
    try:
        frame = decode_rgba8(path)
    except ImageBackendMissing:
        raise
    except ValueError:
        return False
    return (frame.width == width and frame.height == height
            and frame.pixels == pixels)
