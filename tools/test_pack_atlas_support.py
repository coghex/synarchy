#!/usr/bin/env python3
"""
test_pack_atlas_support.py — the fixture machinery every
`tools/test_pack_atlas.py` case is built from (#2061 owner split).

One owner for everything a case needs and nothing a case decides: PNG
construction and its exact-offset corruptions, the temporary unit tree
(`Fixture`), the YAML/frame/atlas/index helpers, the compiled-unit
builders the compiler and budget cases share, and the per-owner case
registry the façade assembles.

Nothing here registers a case or runs one. The three case owners —
`test_pack_atlas_validation`, `test_pack_atlas_compiler`, and
`test_pack_atlas_budget` — import this module; this module imports none
of them. It does import the production owners, and must: `Fixture`
reads the shipped budget path, calls `pack_atlas.cmd_validate` and
`cmd_compile` in-process, and decodes through
`pack_atlas_image.image_module()`.
"""
from __future__ import annotations

import binascii
import io
import json
import os
import shutil
import struct
import sys
import zlib
from contextlib import redirect_stdout
from pathlib import Path
from typing import (
    Callable, Dict, List, NamedTuple, Optional, Sequence, Tuple,
)

sys.path.insert(0, str(Path(__file__).resolve().parent))

import pack_atlas  # noqa: E402
# The owners behind the façade (#2054). Each case reads or patches the
# module that actually holds the moved behaviour, so a patch can never
# land on a façade attribute the implementation no longer reads.
# pack_atlas_inventory is reached only through the façade's commands and
# is neither read nor patched directly, so it is not imported here.
import pack_atlas_budget  # noqa: E402
import pack_atlas_image  # noqa: E402
import pack_atlas_shared  # noqa: E402

PNG_SIGNATURE = b"\x89PNG\r\n\x1a\n"


CANON5 = ["south", "south-east", "east", "north-east", "north"]
ALL8 = CANON5 + ["north-west", "west", "south-west"]

# The row order the compiler uses, restricted per animation to the
# authored set. Spelled out here rather than imported so a change to
# `pack_atlas_shared.ATLAS_DIRECTION_ORDER` has to be a deliberate edit in two
# places instead of silently re-blessing itself.
EXPECTED_ROW_ORDER = [
    "south", "south-west", "west", "north-west",
    "north", "north-east", "east", "south-east",
]


# --------------------------------------------------------------------
# Fixture construction
# --------------------------------------------------------------------


def png_chunk(ctype: bytes, body: bytes) -> bytes:
    """One PNG chunk, with a CORRECT CRC over type and payload."""
    return (struct.pack(">I", len(body)) + ctype + body
            + struct.pack(">I", binascii.crc32(ctype + body) & 0xFFFFFFFF))


def png_bytes(width: int = 4, height: int = 4) -> bytes:
    """A real, minimal 8-bit RGBA PNG, built without an encoder.

    Since #1311 the validator opens every declared frame, so these
    bytes must genuinely decode — every positive case in the suite now
    depends on that.

    Hand-built rather than emitted by Pillow because the content cases
    below damage this output at exact chunk offsets: the fixture has to
    be a known quantity, not whatever an encoder happened to choose.
    Pixels vary across the image so the compressed payload is large
    enough to corrupt meaningfully at a chosen offset.
    """
    rows = []
    for y in range(height):
        row = bytearray(b"\x00")           # filter type 0, one per scanline
        for x in range(width):
            row += bytes(((x * 37 + y * 11) % 256, (x * 5 + 32) % 256,
                          (y * 7 + 64) % 256, 255))
        rows.append(bytes(row))
    return (PNG_SIGNATURE
            + png_chunk(b"IHDR",
                        struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0))
            + png_chunk(b"IDAT", zlib.compress(b"".join(rows)))
            + png_chunk(b"IEND", b""))


def find_png_chunk(data: bytes, want: bytes) -> tuple[int, int]:
    """Offset of the first `want` chunk's LENGTH field, and its length."""
    offset = len(PNG_SIGNATURE)
    while offset < len(data):
        length = struct.unpack(">I", data[offset:offset + 4])[0]
        if data[offset + 4:offset + 8] == want:
            return offset, length
        offset += 12 + length
    raise AssertionError(f"fixture PNG has no {want!r} chunk")


def corruptible_png() -> tuple[bytes, int, int]:
    """A PNG big enough to damage precisely, with its IDAT located."""
    data = png_bytes(32, 32)
    offset, length = find_png_chunk(data, b"IDAT")
    assert length > 64, f"fixture IDAT is too small to corrupt: {length} bytes"
    return data, offset, length


def truncated_png() -> bytes:
    """A PNG whose pixel stream stops part-way through.

    The cut is INSIDE the IDAT payload, past the header, so the file
    still identifies as a 32x32 PNG and reports its dimensions
    correctly. Only something that reads the pixel data notices.
    """
    data, offset, length = corruptible_png()
    return data[:offset + 8 + length // 2]


def corrupt_stream_png() -> bytes:
    """A PNG whose compressed data is garbage under a CORRECT checksum.

    The enclosing chunk's CRC is RECOMPUTED over the damaged payload,
    so the container is byte-perfect and only the deflate stream is
    broken. That is the whole point of this fixture: a check that
    validated chunk checksums alone would wave this file through.
    """
    data, offset, length = corruptible_png()
    payload = bytearray(data[offset + 8:offset + 8 + length])
    for i in range(16, 48):
        payload[i] ^= 0xFF
    return (data[:offset] + png_chunk(b"IDAT", bytes(payload))
            + data[offset + 12 + length:])


def bad_checksum_png() -> bytes:
    """A PNG whose IDAT payload is intact but whose CRC is wrong.

    The mirror image of `corrupt_stream_png`, and it needs its own
    fixture: Pillow reads and DISCARDS those four CRC bytes while
    streaming pixel data, so a full decode alone accepts this file
    without complaint.
    """
    data, offset, length = corruptible_png()
    return (data[:offset + 8 + length] + b"\x00\x00\x00\x00"
            + data[offset + 12 + length:])


def tampered_iend_png() -> bytes:
    """A PNG whose TERMINAL chunk carries a wrong checksum.

    Its own fixture because IEND is the one chunk no library pass
    reaches: Pillow's `verify()` breaks on it before checksumming, and
    the decoder stops once it has enough scanlines and never reads that
    far. Everything before it here is byte-perfect.
    """
    data = png_bytes(32, 32)
    assert data[-12:-4] == struct.pack(">I", 0) + b"IEND", "fixture tail moved"
    return data[:-4] + b"\xde\xad\xbe\xef"


def trailing_data_png() -> bytes:
    """A complete PNG with junk appended past IEND.

    The spec makes IEND the final chunk, so this is a structurally
    invalid stream.
    """
    return png_bytes(32, 32) + b"\x00leftover editor metadata"


def duplicate_iend_png() -> bytes:
    """A complete PNG with a SECOND canonical IEND chunk appended.

    The adversarial version of the case above, and the reason the check
    cannot simply compare the file's last bytes: this file's tail IS a
    perfect IEND chunk, while the image it belongs to actually ended
    twelve bytes earlier. Pillow's `verify()` stops at the first IEND
    and the decoder never reads that far, so nothing else notices.
    """
    return png_bytes(32, 32) + pack_atlas_shared.PNG_IEND_CHUNK


def chunk_then_iend_png() -> bytes:
    """A complete PNG with a whole extra chunk before a second IEND.

    Same shape as `duplicate_iend_png`, but with real chunk content in
    between, so a check that only looked one chunk back would miss it.
    """
    return (png_bytes(32, 32) + png_chunk(b"tEXt", b"Comment\x00appended")
            + pack_atlas_shared.PNG_IEND_CHUNK)


def not_an_image() -> bytes:
    """A text file wearing a `.png` name."""
    return b"# notes about this animation, saved to the wrong path\n" * 8


def other_format_png() -> bytes:
    """A perfectly valid BMP, wearing a `.png` name.

    Pillow will happily decode it; the engine's loader will not, so the
    inventory must reject it on FORMAT rather than on readability.
    """
    image_mod = pack_atlas_image.image_module()
    buffer = io.BytesIO()
    image_mod.new("RGBA", (4, 4), (12, 34, 56, 255)).save(buffer, format="BMP")
    return buffer.getvalue()


def png_in_mode(mode: str, size: tuple[int, int] = (4, 4), **save: object) -> bytes:
    """A valid PNG in some colour type other than plain 8-bit RGBA."""
    image_mod = pack_atlas_image.image_module()
    buffer = io.BytesIO()
    image_mod.new(mode, size).save(buffer, format="PNG", **save)
    return buffer.getvalue()


class Fixture:
    """A throwaway repository root holding units and their assets."""

    def __init__(self, root: Path) -> None:
        self.root = root
        (root / "data" / "units").mkdir(parents=True)
        (root / "assets" / "textures" / "units").mkdir(parents=True)
        # The budget (#1262) is part of the tree being validated, and a
        # missing one is a hard error rather than a skipped check — so
        # every fixture gets one. Copied from the SHIPPED document, not
        # a stand-in, which makes "the real policy file still parses"
        # a property every case in this suite exercises.
        self.write_file(
            pack_atlas_budget.BUDGET_REL.as_posix(),
            (pack_atlas.REPO_ROOT / pack_atlas_budget.BUDGET_REL).read_bytes())

    def budget(self) -> dict:
        """The fixture's budget document, as a mutable dict."""
        return json.loads(
            (self.root / pack_atlas_budget.BUDGET_REL)
            .read_text(encoding="utf-8"))

    def write_budget(self, doc: object) -> None:
        self.write_file(
            pack_atlas_budget.BUDGET_REL.as_posix(),
            json.dumps(doc, indent=2).encode("utf-8"))

    # -- assets --------------------------------------------------------
    def frames(
        self,
        unit: str,
        anim: str,
        directions: Sequence[str],
        count: int,
        start: int = 0,
    ) -> None:
        for d in directions:
            target = (self.root / "assets" / "textures" / "units" / unit
                      / "animations" / anim / d)
            target.mkdir(parents=True, exist_ok=True)
            for i in range(start, start + count):
                (target / f"frame_{i:03d}.png").write_bytes(png_bytes())

    def write_file(self, rel: str, content: bytes) -> Path:
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(content)
        return path

    def rm(self, rel: str) -> None:
        (self.root / rel).unlink()

    def symlink(self, rel: str, target: str) -> None:
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        if path.exists() or path.is_symlink():
            if path.is_dir() and not path.is_symlink():
                shutil.rmtree(path)
            else:
                path.unlink()
        os.symlink(target, path)

    # -- declarations --------------------------------------------------
    def yaml(self, name: str, text: str) -> None:
        (self.root / "data" / "units" / f"{name}.yaml").write_text(
            text, encoding="utf-8")

    def frames_rgba(
        self,
        unit: str,
        anim: str,
        directions: Sequence[str],
        counts: Dict[str, int],
        size: tuple[int, int] = (3, 2),
    ) -> None:
        """Frames whose pixels identify their own cell.

        Each frame is a solid colour derived from its direction and
        index, so a misplaced, duplicated or transposed cell in the
        compiled atlas is visible as the WRONG colour rather than
        merely as a different hash.
        """
        for d in directions:
            target = (self.root / "assets" / "textures" / "units" / unit
                      / "animations" / anim / d)
            target.mkdir(parents=True, exist_ok=True)
            for i in range(counts[d]):
                (target / f"frame_{i:03d}.png").write_bytes(
                    rgba_png(size[0], size[1], frame_colour(d, i)))

    # -- compiled artifacts --------------------------------------------
    def atlas_dir(self, unit: str) -> Path:
        return (self.root / "assets" / "textures" / "units" / unit / "atlas")

    def atlas_path(self, unit: str, anim: str) -> Path:
        return self.atlas_dir(unit) / f"{anim}.png"

    def index_path(self, unit: str) -> Path:
        return self.atlas_dir(unit) / "index.json"

    def index(self, unit: str) -> dict:
        return json.loads(self.index_path(unit).read_text(encoding="utf-8"))

    def artifacts(self, *units: str) -> List[Path]:
        out: List[Path] = []
        for unit in units:
            directory = self.atlas_dir(unit)
            if directory.is_dir():
                out.extend(sorted(directory.iterdir()))
        return out

    def run(self, unit: Optional[str] = None, strict: bool = True) -> tuple[int, str]:
        buffer = io.StringIO()
        with redirect_stdout(buffer):
            code = pack_atlas.cmd_validate(self.root, unit, strict)
        return code, buffer.getvalue()

    def compile(
        self, unit: Optional[str] = None, check: bool = False,
    ) -> tuple[int, str]:
        buffer = io.StringIO()
        with redirect_stdout(buffer):
            code = pack_atlas.cmd_compile(self.root, unit, True, check)
        return code, buffer.getvalue()

    def compile_ok(self, unit: Optional[str] = None) -> str:
        code, output = self.compile(unit)
        assert code == 0, f"expected a clean compile, got exit {code}:\n{output}"
        return output

    def validate_ok(self) -> str:
        code, output = self.run()
        assert code == 0, f"expected clean validation, got exit {code}:\n{output}"
        return output

    def validate_fails(self, expect: str) -> str:
        code, output = self.run()
        assert code != 0, f"expected validation to fail:\n{output}"
        assert expect in output, (
            f"validation failed without naming {expect!r}:\n{output}")
        return output


def rgba_png(width: int, height: int, colour: tuple[int, int, int, int]) -> bytes:
    """A solid RGBA PNG, written by the same library the compiler uses."""
    image_mod = pack_atlas_image.image_module()
    buffer = io.BytesIO()
    image_mod.new("RGBA", (width, height), colour).save(buffer, format="PNG")
    return buffer.getvalue()


def frame_colour(direction: str, index: int) -> tuple[int, int, int, int]:
    """A colour unique to one (direction, frame index) pair.

    Alpha stays 255 so it can never be confused with a padding cell,
    which is RGBA(0, 0, 0, 0) exactly.
    """
    slot = EXPECTED_ROW_ORDER.index(direction)
    return (10 + slot * 20, 30 + index * 25, 200 - slot * 10, 255)


def atlas_slots(path: Path, cell_w: int, cell_h: int) -> List[List[List[list]]]:
    """Every physical SLOT's texels, row-major, as decoded RGBA8.

    A slot is the logical cell plus its one-texel extrusion gutter
    (#2076), so `slot[y][x]` is indexed from the slot's own top-left
    corner: `(pad, pad)` is the cell's first texel and `(0, 0)` is the
    top-left corner square.
    """
    pad = pack_atlas_shared.CELL_PADDING
    slot_w = cell_w + 2 * pad
    slot_h = cell_h + 2 * pad
    image_mod = pack_atlas_image.image_module()
    with image_mod.open(path) as handle:
        image = handle.convert("RGBA")
        pixels = image.load()
        rows = image.height // slot_h
        columns = image.width // slot_w
        return [
            [
                [
                    [pixels[c * slot_w + x, r * slot_h + y]
                     for x in range(slot_w)]
                    for y in range(slot_h)
                ]
                for c in range(columns)
            ]
            for r in range(rows)
        ]


def atlas_cells(path: Path, cell_w: int, cell_h: int) -> List[List[tuple]]:
    """Every LOGICAL cell's flat pixel set, row-major, as decoded RGBA8.

    Reads the cell inside its padded slot, which is what `atlas_cell_uv`
    addresses — the gutter is `atlas_slots`' business.
    """
    pad = pack_atlas_shared.CELL_PADDING
    return [
        [
            {slot[pad + y][pad + x]
             for y in range(cell_h) for x in range(cell_w)}
            for slot in row
        ]
        for row in atlas_slots(path, cell_w, cell_h)
    ]


def mtimes(paths: Sequence[Path]) -> Dict[str, int]:
    return {p.name: p.stat().st_mtime_ns for p in paths}


def freeze_mtimes(paths: Sequence[Path]) -> None:
    """Stamp every artifact so a later write is unambiguously visible.

    Write LOCALITY is the property under test, and equal content hashes
    do not establish it: an unrelated atlas rewritten with identical
    bytes still churns the tracked file. Zeroing the timestamps first
    makes "was this file written" an exact observation rather than a
    race against clock resolution.
    """
    for path in paths:
        os.utime(path, (0, 0))


def written_since_freeze(paths: Sequence[Path]) -> List[str]:
    return sorted(p.name for p in paths if p.stat().st_mtime_ns != 0)


def frame_lines(
    unit: str, anim: str, direction: str, count: int, indent: str,
    start: int = 0, name: Optional[Callable[[int], str]] = None,
) -> str:
    fmt = name or (lambda i: f"frame_{i:03d}.png")
    return "".join(
        f'{indent}- "assets/textures/units/{unit}/animations/{anim}/'
        f'{direction}/{fmt(i)}"\n'
        for i in range(start, start + count))


def anim_yaml(
    unit: str, anim: str, directions: Sequence[str], count: int,
    flip: bool, indent: int = 6,
) -> str:
    i = " " * indent
    out = [f"{i}{anim}:\n", f"{i}  fps: 8\n", f"{i}  loop: true\n",
           f"{i}  flip: {'true' if flip else 'false'}\n", f"{i}  frames:\n"]
    for d in directions:
        out.append(f"{i}    {d}:\n")
        out.append(frame_lines(unit, anim, d, count, i + "      "))
    return "".join(out)


def gameplay_yaml(
    unit: str, anims: Sequence[tuple[str, Sequence[str], int, bool]],
    sprite: Optional[str] = None,
) -> str:
    head = (f"units:\n  - name: {unit}\n"
            f'    sprite: "{sprite or f"assets/textures/units/{unit}/idle.png"}"\n'
            f"    animations:\n")
    return head + "".join(anim_yaml(unit, *a) for a in anims)


def asset_only_yaml(
    unit: str, anims: Sequence[tuple[str, Sequence[str], int, bool]],
) -> str:
    head = f"asset_units:\n  - name: {unit}\n    animations:\n"
    return head + "".join(anim_yaml(unit, *a) for a in anims)


def valid_fixture(fx: Fixture) -> None:
    """The baseline every negative case perturbs: one gameplay unit and
    one asset-only unit, both fully declared and fully owned."""
    fx.frames("hero", "idle", CANON5, 3)
    fx.frames("hero", "walk", ALL8, 2)
    fx.write_file("assets/textures/units/hero/idle.png", png_bytes())
    fx.yaml("hero", gameplay_yaml("hero", [
        ("idle", CANON5, 3, True),
        ("walk", ALL8, 2, False),
    ]))
    fx.frames("prop", "spin", CANON5, 2)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)]))


# --------------------------------------------------------------------
# Compiled-unit builders and atlas inspection
# --------------------------------------------------------------------
#
# Shared by the compiler cases (#1258) and the budget cases (#1262):
# both need a unit that actually compiles, and neither owns these.

CELL = (3, 2)


def anim_yaml_ragged(
    unit: str, anim: str, counts: Dict[str, int], flip: bool, indent: int = 6,
) -> str:
    """An animation block whose directions may hold different counts."""
    i = " " * indent
    out = [f"{i}{anim}:\n", f"{i}  fps: 8\n", f"{i}  loop: true\n",
           f"{i}  flip: {'true' if flip else 'false'}\n", f"{i}  frames:\n"]
    for d, n in counts.items():
        out.append(f"{i}    {d}:\n")
        out.append(frame_lines(unit, anim, d, n, i + "      "))
    return "".join(out)


def build_unit(
    fx: Fixture, unit: str,
    anims: Sequence[tuple[str, Dict[str, int], bool]],
    size: tuple[int, int] = CELL,
) -> None:
    """One asset-only unit whose frames identify their own cells."""
    for anim, counts, _flip in anims:
        fx.frames_rgba(unit, anim, list(counts), counts, size)
    fx.yaml(unit, f"asset_units:\n  - name: {unit}\n    animations:\n"
            + "".join(anim_yaml_ragged(unit, a, c, f) for a, c, f in anims))


def uniform(directions: Sequence[str], count: int) -> Dict[str, int]:
    return {d: count for d in directions}


def entry(document: dict, name: str) -> dict:
    matches = [a for a in document["animations"] if a["name"] == name]
    assert len(matches) == 1, f"expected one '{name}' entry, got {matches}"
    return matches[0]


TRANSPARENT = {(0, 0, 0, 0)}


def assert_atlas_matches(
    fx: Fixture, unit: str, anim: str, counts: Dict[str, int],
) -> dict:
    """Every authored cell holds its own frame; every other cell is padding."""
    record = entry(fx.index(unit), anim)
    rows = [d for d in EXPECTED_ROW_ORDER if d in counts]
    assert [d["direction"] for d in record["directions"]] == rows, (
        f"row order is {[d['direction'] for d in record['directions']]}, "
        f"expected {rows}")
    assert [d["row"] for d in record["directions"]] == list(range(len(rows)))
    assert record["columns"] == max(counts.values())
    assert record["rows"] == len(rows)
    assert record["cell_width"] == CELL[0] and record["cell_height"] == CELL[1]
    # The gutter is declared, and the sheet is sized for padded SLOTS.
    pad = pack_atlas_shared.CELL_PADDING
    assert record["cell_padding"] == pad
    assert record["atlas_width"] == record["columns"] * (CELL[0] + 2 * pad)
    assert record["atlas_height"] == record["rows"] * (CELL[1] + 2 * pad)

    slots = atlas_slots(fx.atlas_path(unit, anim), CELL[0], CELL[1])
    cells = atlas_cells(fx.atlas_path(unit, anim), CELL[0], CELL[1])
    assert len(cells) == record["rows"]
    for row, direction in enumerate(rows):
        declared = counts[direction]
        assert record["directions"][row]["frame_count"] == declared, (
            f"{direction} records {record['directions'][row]['frame_count']} "
            f"frames, authored {declared}")
        for column, cell in enumerate(cells[row]):
            slot = slots[row][column]
            if column < declared:
                assert cell == {frame_colour(direction, column)}, (
                    f"{unit}/{anim} cell ({row},{column}) holds {cell}, "
                    f"expected frame {column} of {direction}")
                # #2076: the whole slot — gutter, edges and all four
                # corner squares — is that same frame's own texels
                # extruded outward. These fixture frames are solid, so
                # any texel in the slot that is NOT the frame's colour
                # is a gutter built from something else.
                assert {t for line in slot for t in line} == cell, (
                    f"{unit}/{anim} slot ({row},{column}) carries texels "
                    f"its own cell does not: "
                    f"{ {t for line in slot for t in line} - cell }")
            else:
                # Padding rectangularizes the sheet and nothing else: it
                # is transparent, and `frame_count` above already proves
                # it is unreachable as a frame. Its gutter is
                # transparent too — a rectangularization slot has no art
                # to extrude.
                assert cell == TRANSPARENT, (
                    f"{unit}/{anim} padding cell ({row},{column}) is not "
                    f"transparent: {cell}")
                assert {t for line in slot for t in line} == TRANSPARENT, (
                    f"{unit}/{anim} padding slot ({row},{column}) has a "
                    f"non-transparent gutter")
    return record


def gradient_png(width: int, height: int, seed: int) -> bytes:
    """A PNG whose every texel differs — the opposite of `rgba_png`.

    Solid frames cannot distinguish a gutter built from the RIGHT edge
    row from one built from any other row of the same frame, so the
    extrusion geometry needs art where every position is identifiable.
    """
    image_mod = pack_atlas_image.image_module()
    image = image_mod.new("RGBA", (width, height))
    pixels = image.load()
    for y in range(height):
        for x in range(width):
            pixels[x, y] = ((seed * 37 + x * 29) % 251 + 4,
                            (seed * 53 + y * 41) % 251 + 4,
                            (x * 17 + y * 13 + seed * 7) % 251 + 4,
                            255)
    buffer = io.BytesIO()
    image.save(buffer, format="PNG")
    return buffer.getvalue()


def decode_rgba_texels(png: bytes) -> List[List[tuple]]:
    """A PNG's texels as `[y][x]`, through the compiler's own decoder."""
    image_mod = pack_atlas_image.image_module()
    with image_mod.open(io.BytesIO(png)) as handle:
        image = handle.convert("RGBA")
        pixels = image.load()
        return [[pixels[x, y] for x in range(image.width)]
                for y in range(image.height)]


def budget_unit(fx: Fixture, unit: str = "hero", anims: int = 3) -> None:
    """A compiled unit with `anims` animations and nothing else."""
    build_unit(fx, unit,
               [(f"anim{i}", uniform(CANON5, 2), True) for i in range(anims)])
    fx.compile_ok()


# --------------------------------------------------------------------
# Case records and the per-owner registry
# --------------------------------------------------------------------
#
# Each case owner builds its OWN registry and freezes it into ordered
# tuples at import. There is deliberately no shared module-level list
# here: with one, execution order would be a function of which module
# imported first, and "this owner contributes exactly these cases"
# would not be a checkable statement. The façade assembles the frozen
# collections in a written-down owner order and checks each owner's own
# counts against its floor.

Case = Callable[["Fixture"], None]


class PositiveCase(NamedTuple):
    """A validation fixture that must exit 0."""

    name: str
    build: Case
    # The optional `--unit` argument the validator runs under, so a
    # case can exercise single-unit mode.
    unit: Optional[str]


class NegativeCase(NamedTuple):
    """A validation fixture that must exit non-zero AND print `expect`."""

    name: str
    build: Case
    expect: str
    unit: Optional[str]


class ScenarioCase(NamedTuple):
    """A compiler or budget case, which drives the tool and asserts."""

    name: str
    build: Case


class OwnerCases(NamedTuple):
    """One owner's frozen, ordered contribution to the suite."""

    owner: str
    positive: Tuple[PositiveCase, ...]
    negative: Tuple[NegativeCase, ...]
    scenario: Tuple[ScenarioCase, ...]

    def counts(self) -> Tuple[int, int, int]:
        return len(self.positive), len(self.negative), len(self.scenario)


class CaseRegistry:
    """The decorators one case owner registers through.

    Private to the module that constructs it: two owners never share an
    instance, so `freeze()` returns exactly the cases defined in that
    one file, in definition order.
    """

    def __init__(self, owner: str) -> None:
        self.owner = owner
        self._positive: List[PositiveCase] = []
        self._negative: List[NegativeCase] = []
        self._scenario: List[ScenarioCase] = []

    def positive(
        self, name: str, unit: Optional[str] = None,
    ) -> Callable[[Case], Case]:
        def register(fn: Case) -> Case:
            self._positive.append(PositiveCase(name, fn, unit))
            return fn
        return register

    def negative(
        self, name: str, expect: str, unit: Optional[str] = None,
    ) -> Callable[[Case], Case]:
        def register(fn: Case) -> Case:
            self._negative.append(NegativeCase(name, fn, expect, unit))
            return fn
        return register

    def scenario(self, name: str) -> Callable[[Case], Case]:
        def register(fn: Case) -> Case:
            self._scenario.append(ScenarioCase(name, fn))
            return fn
        return register

    def freeze(self) -> OwnerCases:
        """This owner's cases, in definition order, as immutable tuples."""
        return OwnerCases(self.owner, tuple(self._positive),
                          tuple(self._negative), tuple(self._scenario))
