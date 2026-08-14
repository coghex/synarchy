#!/usr/bin/env python3
"""
test_pack_atlas.py — fixture self-test for tools/pack_atlas.py (#1257).

Every case builds a complete, isolated unit tree in a temporary
directory (`data/units/` + `assets/textures/units/`) and runs the real
validator against it via `--root`. Nothing here reads, writes, or
depends on the shipped asset tree, so the suite keeps passing while the
real corpus grows.

A negative case asserts BOTH halves of the contract: a nonzero exit AND
a diagnostic naming the actual problem, so a check cannot pass by
failing for some unrelated reason.

    python3 tools/test_pack_atlas.py           # run every case
    python3 tools/test_pack_atlas.py -v        # print each case's output
"""
from __future__ import annotations

import argparse
import binascii
import io
import os
import shutil
import struct
import sys
import tempfile
import zlib
from contextlib import redirect_stdout
from pathlib import Path
from typing import Callable, Dict, List, Optional, Sequence

sys.path.insert(0, str(Path(__file__).resolve().parent))

import pack_atlas  # noqa: E402


CANON5 = ["south", "south-east", "east", "north-east", "north"]
ALL8 = CANON5 + ["north-west", "west", "south-west"]


# --------------------------------------------------------------------
# Fixture construction
# --------------------------------------------------------------------

def independent_layout(
    width: int, height: int, interlace: int,
) -> List[tuple[int, int]]:
    """``[(pixels_per_row, row_count), ...]`` for an 8-bit RGBA image.

    Deliberately NOT `pack_atlas.scanline_layout`. The interlaced fixture
    is only evidence about the validator's Adam7 arithmetic if the two
    are computed independently — sharing the helper makes the fixture
    mutate in lockstep with the code under test, and a broken layout then
    produces a file that "matches" the broken expectation. The table
    below is transcribed straight from the PNG specification's Adam7
    pass description.
    """
    if interlace == 0:
        return [(width, height)]
    out: List[tuple[int, int]] = []
    for start_row, start_col, row_step, col_step in (
        (0, 0, 8, 8), (0, 4, 8, 8), (4, 0, 8, 4), (0, 2, 4, 4),
        (2, 0, 4, 2), (0, 1, 2, 2), (1, 0, 2, 1),
    ):
        cols = len(range(start_col, width, col_step))
        rows = len(range(start_row, height, row_step))
        if cols and rows:
            out.append((cols, rows))
    return out


def png_bytes(width: int = 4, height: int = 4, *,
              ihdr_compression: int = 0, ihdr_filter: int = 0,
              scanline_filter: int = 0, interlace: int = 0) -> bytes:
    """A real, minimal 8-bit RGBA PNG of the requested size.

    The malformed-input knobs all keep the file otherwise well-formed —
    correct chunk CRCs, and an IDAT that inflates to exactly the length
    the header implies — so ONLY the field under test is wrong. That is
    what makes the negative cases sharp: a checker that reads a field
    without validating it, or that validates only the inflated SIZE,
    accepts the result.

    `interlace=1` produces a genuine Adam7 layout, so the positive case
    proves the pass-structure arithmetic rather than merely not crashing.
    """
    def chunk(ctype: bytes, body: bytes) -> bytes:
        return (struct.pack(">I", len(body)) + ctype + body
                + struct.pack(">I", binascii.crc32(ctype + body) & 0xFFFFFFFF))

    prefix = bytes([scanline_filter])
    raw = b"".join(prefix + b"\x7f\x20\x40\xff" * row_pixels
                   for row_pixels, rows in independent_layout(
                       width, height, interlace)
                   for _ in range(rows))

    ihdr = struct.pack(">IIBBBBB", width, height, 8, 6,
                       ihdr_compression, ihdr_filter, interlace)
    return (pack_atlas.PNG_SIGNATURE
            + chunk(b"IHDR", ihdr)
            + chunk(b"IDAT", zlib.compress(raw))
            + chunk(b"IEND", b""))


def png_chunk(ctype: bytes, body: bytes) -> bytes:
    """One CRC-correct PNG chunk."""
    return (struct.pack(">I", len(body)) + ctype + body
            + struct.pack(">I", binascii.crc32(ctype + body) & 0xFFFFFFFF))


def png_from_chunks(chunks: Sequence[bytes]) -> bytes:
    """A signature plus exactly the chunks given, each CRC-correct.

    The structural cases build their streams this way rather than by
    tweaking `png_bytes`: chunk PRESENCE and ORDER are the property under
    test, so the fixture should state the chunk list outright instead of
    hiding it behind a flag.
    """
    return pack_atlas.PNG_SIGNATURE + b"".join(chunks)


def ihdr_chunk(width: int, height: int, depth: int = 8,
               colour: int = 6, interlace: int = 0) -> bytes:
    return png_chunk(b"IHDR", struct.pack(
        ">IIBBBBB", width, height, depth, colour, 0, 0, interlace))


def idat_chunk(width: int, height: int, channels: int = 4) -> bytes:
    raw = b"".join(b"\x00" + bytes([0x40]) * (width * channels)
                   for _ in range(height))
    return png_chunk(b"IDAT", zlib.compress(raw))


IEND_CHUNK = png_chunk(b"IEND", b"")


class Fixture:
    """A throwaway repository root holding units and their assets."""

    def __init__(self, root: Path) -> None:
        self.root = root
        (root / "data" / "units").mkdir(parents=True)
        (root / "assets" / "textures" / "units").mkdir(parents=True)

    # -- assets --------------------------------------------------------
    def frames(
        self,
        unit: str,
        anim: str,
        directions: Sequence[str],
        count: int,
        size: tuple[int, int] = (4, 4),
        start: int = 0,
    ) -> None:
        for d in directions:
            target = (self.root / "assets" / "textures" / "units" / unit
                      / "animations" / anim / d)
            target.mkdir(parents=True, exist_ok=True)
            for i in range(start, start + count):
                (target / f"frame_{i:03d}.png").write_bytes(png_bytes(*size))

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

    def run(self, unit: Optional[str] = None, strict: bool = True) -> tuple[int, str]:
        buffer = io.StringIO()
        with redirect_stdout(buffer):
            code = pack_atlas.cmd_validate(self.root, unit, strict)
        return code, buffer.getvalue()


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
# Cases
# --------------------------------------------------------------------

Case = Callable[[Fixture], None]

POSITIVE: List[tuple[str, Case]] = []
NEGATIVE: List[tuple[str, Case, str]] = []


def positive(name: str) -> Callable[[Case], Case]:
    def register(fn: Case) -> Case:
        POSITIVE.append((name, fn))
        return fn
    return register


def negative(name: str, expect: str) -> Callable[[Case], Case]:
    def register(fn: Case) -> Case:
        NEGATIVE.append((name, fn, expect))
        return fn
    return register


# -- positive ---------------------------------------------------------

@positive("a runtime unit and an asset-only unit both validate clean")
def _valid(fx: Fixture) -> None:
    valid_fixture(fx)


@positive("reusing an animation frame as sprite/portrait is not a duplicate claim")
def _reuse(fx: Fixture) -> None:
    fx.frames("hero", "idle", CANON5, 2)
    shared = "assets/textures/units/hero/animations/idle/south/frame_000.png"
    fx.write_file("assets/textures/units/hero/portrait.png", png_bytes())
    fx.yaml("hero", (
        f"units:\n  - name: hero\n    sprite: \"{shared}\"\n"
        f'    portrait: "assets/textures/units/hero/portrait.png"\n'
        f"    directional_sprites:\n      south: \"{shared}\"\n"
        f"    animations:\n") + anim_yaml("hero", "idle", CANON5, 2, True))


@positive("different directions of one animation may hold different frame counts")
def _ragged(fx: Fixture) -> None:
    fx.frames("hero", "idle", ["south"], 4)
    fx.frames("hero", "idle", ["south-east", "east", "north-east", "north"], 2)
    fx.write_file("assets/textures/units/hero/idle.png", png_bytes())
    body = ["    animations:\n", "      idle:\n", "        fps: 8\n",
            "        loop: true\n", "        flip: true\n",
            "        frames:\n", "          south:\n",
            frame_lines("hero", "idle", "south", 4, " " * 12)]
    for d in ["south-east", "east", "north-east", "north"]:
        body.append(f"          {d}:\n")
        body.append(frame_lines("hero", "idle", d, 2, " " * 12))
    fx.yaml("hero", 'units:\n  - name: hero\n    sprite: '
                    '"assets/textures/units/hero/idle.png"\n' + "".join(body))


@positive("a file holding BOTH units: and asset_units: validates")
def _both_keys(fx: Fixture) -> None:
    fx.frames("hero", "idle", CANON5, 2)
    fx.frames("prop", "spin", CANON5, 2)
    fx.write_file("assets/textures/units/hero/idle.png", png_bytes())
    fx.yaml("hero", gameplay_yaml("hero", [("idle", CANON5, 2, True)])
            + asset_only_yaml("prop", [("spin", CANON5, 2, True)]).replace(
                "asset_units:\n", "asset_units:\n", 1))


@positive("the approved <lowercase>_RH_<lowercase> animation name is accepted")
def _rh_exception(fx: Fixture) -> None:
    # The one narrowly matched exception to the lowercase identifier
    # rule: eight shipped acolyte animations use it, and it authors all
    # eight directions (a mirrored right hand would be a left hand).
    fx.frames("hero", "attack_heavy_RH_dagger", ALL8, 2)
    fx.write_file("assets/textures/units/hero/idle.png", png_bytes())
    fx.yaml("hero", gameplay_yaml("hero", [
        ("attack_heavy_RH_dagger", ALL8, 2, False)]))


# An interlaced size whose seven Adam7 passes have distinguishable byte
# totals. Size matters here: at 9x9 several plausible mis-transcriptions
# of the pass table (swapping two start columns, say) happen to preserve
# every pass's dimensions, so the length check cannot see them. 11x7
# separates all of them.
INTERLACED_W, INTERLACED_H = 11, 7


@positive("a genuinely Adam7-interlaced frame validates")
def _interlaced(fx: Fixture) -> None:
    # Exercises the pass-structure arithmetic against an independently
    # computed layout: a wrong Adam7 table makes the length check reject
    # this valid file.
    fx.frames("prop", "spin", CANON5, 2)
    for d in CANON5:
        for i in range(2):
            fx.write_file(
                f"assets/textures/units/prop/animations/spin/{d}/"
                f"frame_{i:03d}.png",
                png_bytes(INTERLACED_W, INTERLACED_H, interlace=1))
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)]))


@positive("structurally legal PNG variants are NOT over-rejected")
def _legal_png_variants(fx: Fixture) -> None:
    # An indexed image WITH its palette, and a truecolour image carrying
    # an ancillary chunk before and after IDAT. Both are valid PNG; the
    # critical-chunk rules must not sweep them up.
    indexed = png_from_chunks([
        ihdr_chunk(4, 4, colour=3),
        png_chunk(b"PLTE", bytes([0x7f, 0x20, 0x40]) * 64),
        idat_chunk(4, 4, channels=1),
        IEND_CHUNK,
    ])
    ancillary = png_from_chunks([
        ihdr_chunk(4, 4),
        png_chunk(b"tEXt", b"Software\x00fixture"),
        # An UNKNOWN ancillary chunk too (lower-case first letter): a
        # decoder skips these, and so must this checker — the
        # unknown-critical rule below must not become "reject anything
        # unfamiliar".
        png_chunk(b"qUvW", b"\x00\x01\x02"),
        idat_chunk(4, 4),
        png_chunk(b"tIME", struct.pack(">HBBBBB", 2026, 8, 13, 0, 0, 0)),
        IEND_CHUNK,
    ])
    fx.frames("prop", "spin", CANON5, 2)
    for d in CANON5:
        fx.write_file(
            f"assets/textures/units/prop/animations/spin/{d}/frame_000.png",
            indexed)
        fx.write_file(
            f"assets/textures/units/prop/animations/spin/{d}/frame_001.png",
            ancillary)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)]))


# -- negative ---------------------------------------------------------

@negative("malformed YAML", "YAML parse error")
def _bad_yaml(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("hero", "units:\n  - name: hero\n   sprite: [unclosed\n")


@negative("a file declaring neither top-level key", "declares neither")
def _no_key(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("stray", "unit:\n  - name: hero\n")


@negative("an asset-only entry carrying gameplay fields",
          "carries gameplay field")
def _asset_only_gameplay(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("    animations:\n",
                     '    sprite: "assets/textures/units/hero/idle.png"\n'
                     "    animations:\n"))


@negative("a gameplay entry with no sprite", "missing required `sprite:`")
def _no_sprite(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("hero", gameplay_yaml("hero", [
        ("idle", CANON5, 3, True), ("walk", ALL8, 2, False)])
        .replace('    sprite: "assets/textures/units/hero/idle.png"\n', ""))


@negative("malformed PNG data", "undecodable PNG")
def _bad_png(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/hero/animations/idle/south/frame_001.png",
        b"\x89PNG\r\n\x1a\nnot really a png at all")


@negative("a PNG whose IDAT is corrupt", "undecodable PNG")
def _corrupt_idat(fx: Fixture) -> None:
    valid_fixture(fx)
    good = png_bytes()
    fx.write_file(
        "assets/textures/units/hero/animations/idle/south/frame_001.png",
        good[:-40])


@negative("an unsafe unit identifier", "unsafe unit identifier")
def _bad_unit_name(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("- name: prop", "- name: ../escape"))


@negative("an unsafe animation identifier", "unsafe animation identifier")
def _bad_anim_name(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("      spin:\n", "      spin/../x:\n"))


@negative("an unknown direction key in a declaration", "unknown direction key")
def _bad_dir_key(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("          north:\n", "          upward:\n"))


@negative("an unknown direction directory on disk",
          "unknown direction directory")
def _bad_dir_dir(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "spin", ["upward"], 1)


@negative("a frame filename that is not frame_NNN.png",
          "must match frame_NNN.png")
def _bad_frame_name(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/sheet.png",
        png_bytes())


@negative("an absolute declared path", "absolute path is not allowed")
def _absolute(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace('"assets/textures/units/prop/animations/spin/south/'
                     'frame_000.png"',
                     '"/assets/textures/units/prop/animations/spin/south/'
                     'frame_000.png"'))


@negative("a declared path with .. traversal", "traversal is not allowed")
def _traversal(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace('"assets/textures/units/prop/animations/spin/south/'
                     'frame_000.png"',
                     '"assets/textures/units/prop/animations/../../../../'
                     'etc/passwd"'))


@negative("a symlink escape in the ownership path",
          "symlink in the ownership path")
def _symlink_escape(fx: Fixture) -> None:
    valid_fixture(fx)
    outside = fx.root.parent / "outside_frames"
    outside.mkdir(exist_ok=True)
    (outside / "frame_000.png").write_bytes(png_bytes())
    (outside / "frame_001.png").write_bytes(png_bytes())
    fx.symlink("assets/textures/units/prop/animations/spin/south",
               str(outside))


@negative("a symlinked frame file on disk", "symlinked frame")
def _symlink_frame(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.symlink("assets/textures/units/prop/animations/spin/south/frame_002.png",
               "frame_000.png")


@negative("a cross-unit reference", "cross-unit reference")
def _cross_unit(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace('"assets/textures/units/prop/animations/spin/south/'
                     'frame_000.png"',
                     '"assets/textures/units/hero/animations/idle/south/'
                     'frame_000.png"'))


@negative("a cross-animation reference", "cross-animation reference")
def _cross_anim(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "other", CANON5, 2)
    fx.yaml("prop", asset_only_yaml(
        "prop", [("spin", CANON5, 2, True), ("other", CANON5, 2, True)])
        .replace('"assets/textures/units/prop/animations/spin/south/'
                 'frame_000.png"',
                 '"assets/textures/units/prop/animations/other/south/'
                 'frame_000.png"', 1))


@negative("a cross-direction reference", "cross-direction reference")
def _cross_dir(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace('"assets/textures/units/prop/animations/spin/south/'
                     'frame_000.png"',
                     '"assets/textures/units/prop/animations/spin/east/'
                     'frame_000.png"', 1))


@negative("a duplicate animation-frame claim",
          "duplicate animation-frame claim")
def _duplicate_claim(fx: Fixture) -> None:
    # Containment already rejects a second slot in another unit,
    # animation, or direction, so the only shape that can reach the
    # claim ledger is one direction list naming the same physical frame
    # twice. It necessarily also trips the numbering rule; this case
    # asserts the CLAIM diagnostic specifically, and `_dupe_index`
    # asserts the numbering one, so neither can mask the other.
    valid_fixture(fx)
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "east", 2, " " * 12),
        frame_lines("prop", "spin", "east", 2, " " * 12)
        + frame_lines("prop", "spin", "east", 1, " " * 12, start=1))
    fx.yaml("prop", body)


@negative("flip: true with an eight-direction set",
          "flip: true requires exactly the canonical five")
def _flip_true_eight(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "spin", ["north-west", "west", "south-west"], 2)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", ALL8, 2, True)]))


@negative("flip: false with a five-direction set",
          "flip: false requires exactly all eight")
def _flip_false_five(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, False)]))


@negative("inconsistent frame dimensions within one animation",
          "inconsistent frame dimensions")
def _bad_dims(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/east/frame_001.png",
        png_bytes(8, 8))


@negative("a frame sequence that does not begin at frame_000",
          "must begin at frame_000.png")
def _missing_zero(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.rm("assets/textures/units/prop/animations/spin/south/frame_000.png")
    fx.frames("prop", "spin", ["south"], 1, start=2)
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "south", 2, " " * 12),
        frame_lines("prop", "spin", "south", 2, " " * 12, start=1))
    fx.yaml("prop", body)


@negative("a gap in frame numbering", "gap in frame numbering")
def _gap(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "spin", ["south"], 1, start=3)
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "south", 2, " " * 12),
        frame_lines("prop", "spin", "south", 2, " " * 12)
        + frame_lines("prop", "spin", "south", 1, " " * 12, start=3))
    fx.yaml("prop", body)


@negative("a duplicate frame index", "duplicate frame index")
def _dupe_index(fx: Fixture) -> None:
    valid_fixture(fx)
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "south", 2, " " * 12),
        frame_lines("prop", "spin", "south", 2, " " * 12)
        + frame_lines("prop", "spin", "south", 1, " " * 12))
    fx.yaml("prop", body)


@negative("a declared frame that is missing from disk", "missing file")
def _missing_declared(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.rm("assets/textures/units/prop/animations/spin/north/frame_001.png")


@negative("an unclassified frame present on disk", "unclassified frame on disk")
def _unclassified(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "spin", CANON5, 1, start=2)


@negative("a whole asset tree with no declaration at all",
          "unclassified frame on disk")
def _undeclared_tree(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("ghost", "idle", CANON5, 2)


@negative("two files declaring the same unit name", "is already declared in")
def _duplicate_unit(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop_copy", asset_only_yaml("prop", [("spin", CANON5, 2, True)]))


@negative("a loose file at the direction level",
          "loose file at the direction level")
def _loose_direction(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file("assets/textures/units/prop/animations/spin/notes.png",
                  png_bytes())


@negative("an arbitrary mixed-case animation name outside the approved "
          "exception", "unsafe animation identifier")
def _mixed_case_anim(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("      spin:\n", "      AnyThing:\n"))


@negative("a near-miss of the approved _RH_ exception (upper-case weapon)",
          "unsafe animation identifier")
def _rh_near_miss(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("      spin:\n", "      attack_RH_Dagger:\n"))


@negative("a mixed-case animation DIRECTORY outside the approved exception",
          "unsafe animation directory name")
def _mixed_case_anim_dir(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "AnyThing", CANON5, 1)


@negative("an unpadded frame filename", "must match frame_NNN.png")
def _unpadded_frame(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_2.png",
        png_bytes())


@negative("an over-padded frame filename", "must match frame_NNN.png")
def _overpadded_frame(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_0002.png",
        png_bytes())


@negative("an unpadded frame DECLARATION", "must match frame_NNN.png")
def _unpadded_declared(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_2.png",
        png_bytes())
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "south", 2, " " * 12),
        frame_lines("prop", "spin", "south", 2, " " * 12)
        + frame_lines("prop", "spin", "south", 1, " " * 12, start=2,
                      name=lambda i: f"frame_{i}.png"))
    fx.yaml("prop", body)


@negative("a PNG header declaring an unknown filter method",
          "unknown filter method")
def _bad_filter_method(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_001.png",
        png_bytes(ihdr_filter=1))


@negative("a PNG header declaring an unknown compression method",
          "unknown compression method")
def _bad_compression_method(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_001.png",
        png_bytes(ihdr_compression=1))


@negative("a scanline declaring an undefined filter type",
          "unknown scanline filter type")
def _bad_scanline_filter(fx: Fixture) -> None:
    # Inflates to EXACTLY the expected length, so only a real
    # filter-byte walk catches it.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_001.png",
        png_bytes(scanline_filter=5))


@negative("an interlaced frame with an undefined filter type in a later "
          "Adam7 pass", "unknown scanline filter type")
def _bad_interlaced_filter(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_001.png",
        png_bytes(INTERLACED_W, INTERLACED_H, scanline_filter=6,
                  interlace=1))


@negative("a symlinked unit directory", "symlinked unit directory")
def _symlinked_unit(fx: Fixture) -> None:
    # The bypass this closes: a skipped symlink meant a whole unit tree
    # could ship without ever entering the filesystem-first walk.
    valid_fixture(fx)
    fx.symlink("assets/textures/units/ghost", "prop")


@negative("a symlinked animations/ root", "symlinked animations/ directory")
def _symlinked_anim_root(fx: Fixture) -> None:
    valid_fixture(fx)
    (fx.root / "assets/textures/units/ghost").mkdir(parents=True)
    fx.symlink("assets/textures/units/ghost/animations",
               str(fx.root / "assets/textures/units/prop/animations"))


@negative("a symlinked animation directory", "symlinked animation directory")
def _symlinked_anim(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.symlink("assets/textures/units/prop/animations/twin", "spin")


@negative("a symlinked direction directory", "symlinked direction directory")
def _symlinked_direction(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.symlink("assets/textures/units/prop/animations/spin/west", "south")


def _replace_frame(fx: Fixture, content: bytes) -> None:
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_001.png",
        content)


@negative("an indexed image with no PLTE chunk", "requires a PLTE chunk")
def _indexed_without_plte(fx: Fixture) -> None:
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk(4, 4, colour=3), idat_chunk(4, 4, channels=1),
        IEND_CHUNK]))


@negative("a chunk after IEND", "appears after IEND")
def _chunk_after_iend(fx: Fixture) -> None:
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk(4, 4), idat_chunk(4, 4), IEND_CHUNK, idat_chunk(4, 4)]))


@negative("an IEND carrying a payload", "IEND is 4 bytes, expected 0")
def _nonempty_iend(fx: Fixture) -> None:
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk(4, 4), idat_chunk(4, 4), png_chunk(b"IEND", b"junk")]))


@negative("IDAT chunks split by another chunk", "not consecutive")
def _split_idat(fx: Fixture) -> None:
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk(4, 4), idat_chunk(4, 4),
        png_chunk(b"tEXt", b"Software\x00fixture"),
        idat_chunk(4, 4), IEND_CHUNK]))


@negative("a PLTE chunk after IDAT", "PLTE chunk appears after IDAT")
def _plte_after_idat(fx: Fixture) -> None:
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk(4, 4, colour=3), idat_chunk(4, 4, channels=1),
        png_chunk(b"PLTE", bytes([0x7f, 0x20, 0x40]) * 64), IEND_CHUNK]))


@negative("a PLTE chunk on a greyscale image", "not allowed for colour type")
def _plte_on_greyscale(fx: Fixture) -> None:
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk(4, 4, colour=0),
        png_chunk(b"PLTE", bytes([0x7f, 0x20, 0x40])),
        idat_chunk(4, 4, channels=1), IEND_CHUNK]))


@negative("a PLTE length that is not a multiple of three",
          "expected a non-zero multiple of 3")
def _ragged_plte(fx: Fixture) -> None:
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk(4, 4, colour=3), png_chunk(b"PLTE", b"\x01\x02\x03\x04"),
        idat_chunk(4, 4, channels=1), IEND_CHUNK]))


@negative("a duplicate IHDR chunk", "duplicate IHDR")
def _duplicate_ihdr(fx: Fixture) -> None:
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk(4, 4), ihdr_chunk(4, 4), idat_chunk(4, 4), IEND_CHUNK]))


@negative("an unknown CRITICAL chunk", "unknown critical chunk")
def _unknown_critical_chunk(fx: Fixture) -> None:
    # Upper-case first letter = critical. A decoder must refuse a
    # critical chunk it does not understand, so accepting one would let
    # a frame no decoder reads pass strict validation.
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk(4, 4), png_chunk(b"ABCD", b"\x00"), idat_chunk(4, 4),
        IEND_CHUNK]))


@negative("an unknown critical chunk after IDAT", "unknown critical chunk")
def _unknown_critical_after_idat(fx: Fixture) -> None:
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk(4, 4), idat_chunk(4, 4), png_chunk(b"ZZZZ", b""),
        IEND_CHUNK]))


@negative("a dimension beyond the PNG maximum", "exceeds the PNG maximum")
def _oversized_dimension(fx: Fixture) -> None:
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk((1 << 31) + 4, 4), idat_chunk(4, 4), IEND_CHUNK]))


@negative("a chunk type that is not four letters", "not four letters")
def _bad_chunk_type(fx: Fixture) -> None:
    valid_fixture(fx)
    _replace_frame(fx, png_from_chunks([
        ihdr_chunk(4, 4), png_chunk(b"12\x00d", b""), idat_chunk(4, 4),
        IEND_CHUNK]))


# --------------------------------------------------------------------
# Runner
# --------------------------------------------------------------------

def run_case(build: Case) -> tuple[int, str]:
    parent = tempfile.mkdtemp(prefix="pack_atlas_test_")
    try:
        fixture = Fixture(Path(parent) / "repo")
        build(fixture)
        return fixture.run()
    finally:
        shutil.rmtree(parent, ignore_errors=True)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    failures: List[str] = []
    total = 0

    for name, build in POSITIVE:
        total += 1
        code, output = run_case(build)
        if args.verbose:
            print(f"--- positive: {name}\n{output}")
        if code != 0:
            failures.append(
                f"positive '{name}': expected exit 0, got {code}\n{output}")

    for name, build, expect in NEGATIVE:
        total += 1
        code, output = run_case(build)
        if args.verbose:
            print(f"--- negative: {name}\n{output}")
        if code == 0:
            failures.append(
                f"negative '{name}': expected a nonzero exit, got 0\n{output}")
        elif expect not in output:
            failures.append(
                f"negative '{name}': exited {code} but no diagnostic matched "
                f"{expect!r}\n{output}")

    # The suite is only meaningful if it actually built fixtures; a
    # refactor that silently emptied a registry must not read as green.
    if len(POSITIVE) < 7 or len(NEGATIVE) < 55:
        failures.append(
            f"case registries look truncated: {len(POSITIVE)} positive, "
            f"{len(NEGATIVE)} negative")

    if failures:
        for failure in failures:
            print(f"FAIL: {failure}", file=sys.stderr)
        print(f"\ntest_pack_atlas: {len(failures)} of {total} case(s) failed",
              file=sys.stderr)
        return 1

    print(f"test_pack_atlas: all {total} case(s) pass "
          f"({len(POSITIVE)} positive, {len(NEGATIVE)} negative)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
