#!/usr/bin/env python3
"""
test_pack_atlas.py — fixture self-test for tools/pack_atlas.py
(#1257 inventory validation, #1258 atlas compilation).

Every case builds a complete, isolated unit tree in a temporary
directory (`data/units/` + `assets/textures/units/`) and runs the real
tool against it via `--root`. Nothing here reads, writes, or depends on
the shipped asset tree, so the suite keeps passing while the real
corpus grows — and no case can leave a production atlas behind.

Three registries:

  POSITIVE   a validation fixture that must exit 0.
  NEGATIVE   a validation fixture that must exit non-zero AND print a
             diagnostic naming the actual problem, so a check cannot
             pass by failing for some unrelated reason. Where a case
             tightens a rule, a positive case pins the other direction,
             so over-rejection fails too.
  SCENARIO   a #1258 compiler case, which needs more than an exit code:
             it inspects the emitted atlas pixels and index document,
             or observes which files a second run actually wrote.

Validation has no image-content cases — the validator never opens a
frame for a unit with no index (#1257), and #1311 tracks lifting that.
Compilation necessarily decodes, so the scenarios do assert pixels.

    python3 tools/test_pack_atlas.py           # run every case
    python3 tools/test_pack_atlas.py -v        # print each case's output
"""
from __future__ import annotations

import argparse
import binascii
import io
import json
import os
import shutil
import struct
import sys
import tempfile
import traceback
import zlib
from contextlib import redirect_stdout
from pathlib import Path
from typing import Callable, Dict, List, Optional, Sequence

sys.path.insert(0, str(Path(__file__).resolve().parent))

import pack_atlas  # noqa: E402

PNG_SIGNATURE = b"\x89PNG\r\n\x1a\n"


CANON5 = ["south", "south-east", "east", "north-east", "north"]
ALL8 = CANON5 + ["north-west", "west", "south-west"]

# The row order the compiler uses, restricted per animation to the
# authored set. Spelled out here rather than imported so a change to
# `pack_atlas.ATLAS_DIRECTION_ORDER` has to be a deliberate edit in two
# places instead of silently re-blessing itself.
EXPECTED_ROW_ORDER = [
    "south", "south-west", "west", "north-west",
    "north", "north-east", "east", "south-east",
]


# --------------------------------------------------------------------
# Fixture construction
# --------------------------------------------------------------------


def png_bytes() -> bytes:
    """A real, minimal 4x4 8-bit RGBA PNG.

    The validator never opens a frame (#1257 dropped image-content
    validation; #1311 tracks adding it), so nothing here depends on
    these bytes being well-formed. They are anyway: a fixture that wrote
    garbage would quietly stop being a realistic asset tree the day
    content checks arrive.
    """
    raw = b"".join(b"\x00" + b"\x7f\x20\x40\xff" * 4 for _ in range(4))

    def chunk(ctype: bytes, body: bytes) -> bytes:
        return (struct.pack(">I", len(body)) + ctype + body
                + struct.pack(">I", binascii.crc32(ctype + body) & 0xFFFFFFFF))

    return (PNG_SIGNATURE
            + chunk(b"IHDR", struct.pack(">IIBBBBB", 4, 4, 8, 6, 0, 0, 0))
            + chunk(b"IDAT", zlib.compress(raw))
            + chunk(b"IEND", b""))


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
    image_mod = pack_atlas.image_module()
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


def atlas_cells(path: Path, cell_w: int, cell_h: int) -> List[List[tuple]]:
    """Every cell's flat pixel set, row-major, as decoded RGBA8."""
    image_mod = pack_atlas.image_module()
    with image_mod.open(path) as handle:
        image = handle.convert("RGBA")
        pixels = image.load()
        rows = image.height // cell_h
        columns = image.width // cell_w
        return [
            [
                {pixels[c * cell_w + x, r * cell_h + y]
                 for y in range(cell_h) for x in range(cell_w)}
                for c in range(columns)
            ]
            for r in range(rows)
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
# Cases
# --------------------------------------------------------------------

Case = Callable[[Fixture], None]

# (name, build, unit) — `unit` is the optional `--unit` argument the
# validator runs under, so a case can exercise single-unit mode.
POSITIVE: List[tuple[str, Case, Optional[str]]] = []
NEGATIVE: List[tuple[str, Case, str, Optional[str]]] = []
# Compiler cases (#1258): each drives the fixture itself and asserts.
SCENARIO: List[tuple[str, Case]] = []


def scenario(name: str) -> Callable[[Case], Case]:
    def register(fn: Case) -> Case:
        SCENARIO.append((name, fn))
        return fn
    return register


def positive(name: str, unit: Optional[str] = None) -> Callable[[Case], Case]:
    def register(fn: Case) -> Case:
        POSITIVE.append((name, fn, unit))
        return fn
    return register


def negative(
    name: str, expect: str, unit: Optional[str] = None,
) -> Callable[[Case], Case]:
    def register(fn: Case) -> Case:
        NEGATIVE.append((name, fn, expect, unit))
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



# -- negative ---------------------------------------------------------

@negative("malformed YAML", "YAML parse error")
def _bad_yaml(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("hero", "units:\n  - name: hero\n   sprite: [unclosed\n")


@negative("a file declaring neither top-level key", "declares neither")
def _no_key(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("stray", "unit:\n  - name: hero\n")


@negative("a file whose only key is an explicit null",
          "present but null")
def _null_units_key(fx: Fixture) -> None:
    # `data.get(key) is None` cannot tell an explicit null from an
    # absent key, so this passed the "declares neither" check and then
    # skipped silently — while the Haskell loader refuses the file.
    valid_fixture(fx)
    fx.yaml("stray", "units: null\n")


@negative("an asset-only key present but null", "present but null")
def _null_asset_units_key(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("stray", "asset_units: null\n")


@negative("a key present with no value at all", "present but null")
def _empty_units_key(fx: Fixture) -> None:
    # `units:` with nothing after it is the same None to PyYAML.
    valid_fixture(fx)
    fx.yaml("stray", "units:\n")


@negative("an asset-only entry carrying gameplay fields",
          "carries gameplay field")
def _asset_only_gameplay(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("    animations:\n",
                     '    sprite: "assets/textures/units/hero/idle.png"\n'
                     "    animations:\n"))


@negative("an asset-only entry carrying an unknown field",
          "unknown field")
def _asset_only_unknown_field(fx: Fixture) -> None:
    # A blacklist of gameplay fields would wave this through; the schema
    # is a whitelist of exactly name + animations.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("    animations:\n", "    typo: true\n    animations:\n"))


@negative("an asset-only entry mixing non-string and string unknown keys",
          "unknown field")
def _mixed_type_unknown_keys(fx: Fixture) -> None:
    # `sorted` over raw YAML keys crashes here: 123 is not orderable
    # against "typo". A crash is not a clear malformed-declaration
    # diagnostic, so the keys are sorted by their rendered form.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("    animations:\n",
                     "    123: true\n    typo: true\n    animations:\n"))


@negative("a declared frame path that is a DIRECTORY",
          "declared frame is a directory")
def _declared_path_is_a_directory(fx: Fixture) -> None:
    # The invariant is "exists as a regular file", not merely "exists" —
    # and a directory deserves its own diagnostic rather than the
    # missing-file one.
    valid_fixture(fx)
    fx.rm("assets/textures/units/prop/animations/spin/south/frame_001.png")
    (fx.root / "assets/textures/units/prop/animations/spin/south"
             / "frame_001.png").mkdir()


@negative("a non-string animation key", "animation key must be a string")
def _numeric_anim_key(fx: Fixture) -> None:
    # YAML resolves an unquoted `123:` to an int, and str(123) == "123"
    # satisfies the identifier rule — so coercion would let a non-string
    # key name a real animation directory.
    valid_fixture(fx)
    fx.frames("prop", "123", CANON5, 2)
    body = asset_only_yaml("prop", [("123", CANON5, 2, True)])
    fx.yaml("prop", body.replace("      123:\n", "      123:\n", 1))


@negative("a non-string direction key", "direction key must be a string")
def _numeric_direction_key(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("          north:\n", "          123:\n"))


@positive("a QUOTED numeric-looking animation name is still accepted")
def _quoted_numeric_anim(fx: Fixture) -> None:
    # The rule is about the KEY's YAML type, not about digits: a
    # deliberately quoted "123" is a string and a legal identifier.
    fx.frames("prop", "123", CANON5, 2)
    body = asset_only_yaml("prop", [("123", CANON5, 2, True)])
    fx.yaml("prop", body.replace("      123:\n", '      "123":\n', 1))


@negative("a gameplay entry with no sprite", "missing required `sprite:`")
def _no_sprite(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("hero", gameplay_yaml("hero", [
        ("idle", CANON5, 3, True), ("walk", ALL8, 2, False)])
        .replace('    sprite: "assets/textures/units/hero/idle.png"\n', ""))


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


@negative("a wrong-case file extension", "must match frame_NNN.png")
def _wrong_case_extension(fx: Fixture) -> None:
    # The extension rule is case-SENSITIVE. `.PNG` is still walked as an
    # image (the suffix test lowercases), so it must be reported as a
    # bad frame name rather than silently ignored as a non-PNG.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_002.PNG",
        png_bytes())


@negative("a frame FILE whose name ends in a newline",
          "must match frame_NNN.png")
def _newline_in_frame_name(fx: Fixture) -> None:
    # A newline is a legal POSIX filename character, and `$` matches
    # just before a trailing one — so with `^...$` this file passes the
    # name rule, exists on disk, and is claimed by its declaration,
    # leaving NO error at all. Only \Z rejects it. The file is both
    # created and declared for exactly that reason: a disk-only version
    # would still fail as "unclassified", masking the bug.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_002.png\n",
        png_bytes())
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "south", 2, " " * 12),
        frame_lines("prop", "spin", "south", 2, " " * 12)
        + '            "assets/textures/units/prop/animations/spin/south/'
          'frame_002.png\\n"\n'.replace('            "', '            - "'))
    fx.yaml("prop", body)


@negative("an animation key ending in a newline",
          "unsafe animation identifier")
def _newline_in_anim_key(fx: Fixture) -> None:
    # `$` matches just BEFORE a trailing newline, so an `^...$` rule
    # used with `match` accepts this; only \Z or fullmatch rejects it.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("      spin:\n", '      "spin\\n":\n'))


@negative("a unit name ending in a newline", "unsafe unit identifier")
def _newline_in_unit_name(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("  - name: prop\n", '  - name: "prop\\n"\n'))


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


@negative("a contiguous but out-of-order frame list", "out of order")
def _out_of_order(fx: Fixture) -> None:
    # Every set-based check passes here: indices are {0, 1}, start at 0,
    # no gap, no duplicate. Only the ORDER is wrong, and playback walks
    # the declared list in order.
    valid_fixture(fx)
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    ordered = frame_lines("prop", "spin", "south", 2, " " * 12)
    reversed_ = "".join(reversed(ordered.splitlines(keepends=True)))
    body = body.replace(ordered, reversed_)
    fx.yaml("prop", body)


@negative("an `fps:` that is not a number", "`fps:` must be a number")
def _fps_not_a_number(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: fast\n"))


@negative("an `fps:` of zero", "`fps:` must be positive")
def _fps_not_positive(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: 0\n"))


@negative("a boolean `fps:`", "`fps:` must be a number")
def _fps_boolean(fx: Fixture) -> None:
    # bool is an int subclass in Python, so a naive isinstance check
    # lets `fps: true` through as the number 1.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: true\n"))


@negative("a NaN `fps:`", "`fps:` must be a finite")
def _fps_nan(fx: Fixture) -> None:
    # `nan <= 0` is False — every NaN comparison is — so a positivity
    # test alone lets this through.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: .nan\n"))


@negative("an infinite `fps:`", "`fps:` must be a finite")
def _fps_infinite(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: .inf\n"))


@negative("a negative-infinity `fps:`", "`fps:` must be a finite")
def _fps_negative_infinite(fx: Fixture) -> None:
    # Caught by the positivity test too, but it must report the FINITE
    # diagnostic: the finiteness check has to run first.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: -.inf\n"))


@negative("an `fps:` integer too large to represent as a float",
          "`fps:` must be a finite")
def _fps_unrepresentable(fx: Fixture) -> None:
    # A Python int has unbounded precision, so this is a perfectly valid
    # YAML integer — and `math.isfinite` RAISES on it rather than
    # answering, which crashed the validator instead of diagnosing it.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: " + "9" * 4000 + "\n"))


@negative("an `fps:` that overflows the engine's 32-bit Float",
          "32-bit Float")
def _fps_overflows_runtime_float(fx: Fixture) -> None:
    # Fits a Python double, so every earlier check passes; loads as
    # Infinity in UnitYamlAnim's Float field.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: 1.0e+100\n"))


@negative("an `fps:` that underflows the engine's 32-bit Float",
          "32-bit Float")
def _fps_underflows_runtime_float(fx: Fixture) -> None:
    # Positive and finite as a double; loads as 0.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: 1.0e-100\n"))


@positive("a large but representable fps is still accepted")
def _fps_large_but_finite(fx: Fixture) -> None:
    # The other direction: the rule is about representability, not about
    # magnitude, so an absurd-but-finite rate must not be rejected here.
    fx.frames("prop", "spin", CANON5, 2)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: 1000000\n"))


@negative("a `loop:` that is not a boolean", "`loop:` must be a boolean")
def _loop_not_a_boolean(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        loop: true\n", "        loop: sometimes\n"))


@positive("a fractional fps and YAML's own boolean spellings are accepted")
def _legal_scalars(fx: Fixture) -> None:
    # The other direction: tightening these types must not reject a
    # legitimate non-integer rate, nor YAML 1.1's `yes`/`no` booleans,
    # which safe_load resolves to real bools before this code sees them.
    fx.frames("prop", "spin", CANON5, 2)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: 7.5\n")
            .replace("        loop: true\n", "        loop: yes\n"))


@negative("an unknown --unit target", "no such unit",
          unit="definitely_not_a_unit")
def _unknown_unit_target(fx: Fixture) -> None:
    # Must not read as a clean run of an empty inventory: before this,
    # a typo exited 0 reporting "0 unit declaration(s), 0 frame(s)".
    valid_fixture(fx)


@positive("a --unit run restricted to a real unit still validates",
          unit="prop")
def _known_unit_target(fx: Fixture) -> None:
    # The other half: narrowing to a unit that DOES exist must stay a
    # pass, so the check above cannot be satisfied by rejecting every
    # --unit invocation.
    valid_fixture(fx)


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


# --------------------------------------------------------------------
# Compiler scenarios (#1258)
# --------------------------------------------------------------------

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
    assert record["atlas_width"] == record["columns"] * CELL[0]
    assert record["atlas_height"] == record["rows"] * CELL[1]

    cells = atlas_cells(fx.atlas_path(unit, anim), CELL[0], CELL[1])
    assert len(cells) == record["rows"]
    for row, direction in enumerate(rows):
        declared = counts[direction]
        assert record["directions"][row]["frame_count"] == declared, (
            f"{direction} records {record['directions'][row]['frame_count']} "
            f"frames, authored {declared}")
        for column, cell in enumerate(cells[row]):
            if column < declared:
                assert cell == {frame_colour(direction, column)}, (
                    f"{unit}/{anim} cell ({row},{column}) holds {cell}, "
                    f"expected frame {column} of {direction}")
            else:
                # Padding rectangularizes the sheet and nothing else: it
                # is transparent, and `frame_count` above already proves
                # it is unreachable as a frame.
                assert cell == TRANSPARENT, (
                    f"{unit}/{anim} padding cell ({row},{column}) is not "
                    f"transparent: {cell}")
    return record


@scenario("a five-direction mirroring layout compiles to five ordered rows")
def _compile_five_direction(fx: Fixture) -> None:
    counts = uniform(CANON5, 3)
    build_unit(fx, "hero", [("idle", counts, True)])
    fx.compile_ok()
    record = assert_atlas_matches(fx, "hero", "idle", counts)
    assert record["flip"] is True
    assert record["rows"] == 5
    assert record["storage_format"] == "png"
    assert record["atlas_path"] == \
        "assets/textures/units/hero/atlas/idle.png"
    fx.validate_ok()


@scenario("eight-direction artwork compiles to all eight rows, flip false")
def _compile_eight_direction(fx: Fixture) -> None:
    counts = uniform(ALL8, 2)
    build_unit(fx, "hero", [("walk", counts, False)])
    fx.compile_ok()
    record = assert_atlas_matches(fx, "hero", "walk", counts)
    assert record["flip"] is False
    assert record["rows"] == 8
    assert [d["direction"] for d in record["directions"]] == EXPECTED_ROW_ORDER
    fx.validate_ok()


@scenario("unequal direction lengths keep real counts and pad the remainder")
def _compile_unequal_lengths(fx: Fixture) -> None:
    # The shipped shape this mirrors: four acolyte animations author
    # more south frames than eastern ones.
    counts = {"south": 4, "south-east": 2, "east": 1, "north-east": 3,
              "north": 2}
    build_unit(fx, "hero", [("idle", counts, True)])
    fx.compile_ok()
    record = assert_atlas_matches(fx, "hero", "idle", counts)
    assert record["columns"] == 4
    assert sum(d["frame_count"] for d in record["directions"]) == 12
    fx.validate_ok()


@scenario("the index carries a schema version distinct from the tool version")
def _index_versions(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    document = fx.index("hero")
    assert document["schema_version"] == pack_atlas.INDEX_SCHEMA_VERSION
    assert document["tool_version"] == pack_atlas.TOOL_VERSION
    assert "schema_version" in document and "tool_version" in document
    assert document["digest_algorithm"] == "sha256"
    assert document["direction_order"] == EXPECTED_ROW_ORDER
    assert document["unit"] == "hero"
    # Per-animation digests, not one unit-wide value: an animation's
    # edit must not invalidate its neighbours.
    digests = {a["name"]: a["source_digest"] for a in document["animations"]}
    assert all(len(d) == 64 for d in digests.values())


@scenario("playback metadata reaches the index as the engine will hold it")
def _playback_metadata(fx: Fixture) -> None:
    counts = uniform(ALL8, 2)
    build_unit(fx, "hero", [("walk", counts, False)])
    fx.yaml("hero", (fx.root / "data/units/hero.yaml").read_text("utf-8")
            .replace("        fps: 8\n", "        fps: 0.1\n")
            .replace("        loop: true\n", "        loop: false\n"))
    fx.compile_ok()

    record = entry(fx.index("hero"), "walk")
    assert record["loop"] is False, record
    # Recorded as the engine's 32-bit `Float` will actually hold it, not
    # as the decimal the file spells: an index promising a rate the
    # runtime rounds would misdescribe playback.
    assert record["fps"] == pack_atlas.narrow_to_runtime_float(0.1), record
    assert record["fps"] != 0.1, (
        "fps was recorded at double precision, not narrowed")
    fx.validate_ok()


@scenario("editing only fps makes the index stale")
def _fps_edit_is_a_change(fx: Fixture) -> None:
    # `fps` is a compiled input even though it moves no pixel: the index
    # is what the runtime reads, so an index still claiming the old rate
    # is stale exactly like a changed frame.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    fx.yaml("hero", (fx.root / "data/units/hero.yaml").read_text("utf-8")
            .replace("        fps: 8\n", "        fps: 12\n"))
    fx.validate_fails("stale atlas")

    fx.compile_ok()
    assert entry(fx.index("hero"), "idle")["fps"] == 12.0
    fx.validate_ok()


@scenario("two clean builds are byte-identical")
def _clean_build_determinism(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 3), True),
                            ("walk", uniform(ALL8, 2), False)])
    fx.compile_ok()
    first = {p.name: p.read_bytes() for p in fx.artifacts("hero")}
    assert set(first) == {"idle.png", "walk.png", "index.json"}

    shutil.rmtree(fx.atlas_dir("hero"))
    fx.compile_ok()
    second = {p.name: p.read_bytes() for p in fx.artifacts("hero")}
    assert first == second, (
        "a clean rebuild from identical sources produced different bytes: "
        + ", ".join(sorted(k for k in set(first) | set(second)
                           if first.get(k) != second.get(k))))


@scenario("an isolated edit rewrites only that atlas and its unit index")
def _incremental_write_locality(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True),
                            ("walk", uniform(ALL8, 2), False)])
    build_unit(fx, "prop", [("spin", uniform(CANON5, 2), True)])
    fx.compile_ok()

    watched = fx.artifacts("hero", "prop")
    freeze_mtimes(watched)
    fx.write_file(
        "assets/textures/units/hero/animations/idle/east/frame_001.png",
        rgba_png(CELL[0], CELL[1], (1, 2, 3, 255)))
    fx.compile_ok()

    written = written_since_freeze(watched)
    hero_written = sorted(
        p.name for p in fx.artifacts("hero") if p.stat().st_mtime_ns != 0)
    prop_written = sorted(
        p.name for p in fx.artifacts("prop") if p.stat().st_mtime_ns != 0)
    assert hero_written == ["idle.png", "index.json"], (
        f"expected only hero's idle atlas and index to be rewritten, got "
        f"{hero_written} (all: {written})")
    assert prop_written == [], (
        f"another unit's artifacts were rewritten: {prop_written}")
    fx.validate_ok()


@scenario("a no-op recompile writes nothing at all")
def _incremental_noop(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    watched = fx.artifacts("hero")
    freeze_mtimes(watched)
    output = fx.compile_ok()
    assert written_since_freeze(watched) == [], (
        f"an up-to-date tree was rewritten:\n{output}")
    assert "wrote 0 artifact(s)" in output, output


@scenario("an mtime-only touch of a frame is not a change")
def _mtime_touch_is_not_a_change(fx: Fixture) -> None:
    # The digest is over CONTENT, so a rebuild trigger keyed on
    # timestamps would rewrite artifacts nothing had actually changed.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    source = (fx.root / "assets/textures/units/hero/animations/idle/south"
              / "frame_000.png")
    os.utime(source, (0, 0))
    code, output = fx.compile(check=True)
    assert code == 0, f"an mtime-only touch was reported as stale:\n{output}"


@scenario("--check reports out-of-date artifacts and writes nothing")
def _check_mode(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    watched = fx.artifacts("hero")
    freeze_mtimes(watched)
    fx.write_file(
        "assets/textures/units/hero/animations/idle/north/frame_000.png",
        rgba_png(CELL[0], CELL[1], (9, 9, 9, 255)))

    code, output = fx.compile(check=True)
    assert code != 0, f"--check passed on a stale tree:\n{output}"
    assert "would write" in output, output
    assert written_since_freeze(watched) == [], (
        "--check wrote artifacts: "
        + ", ".join(written_since_freeze(watched)))


@scenario("a source edit makes validation report that animation stale")
def _stale_source(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True),
                            ("walk", uniform(ALL8, 2), False)])
    fx.compile_ok()
    fx.write_file(
        "assets/textures/units/hero/animations/idle/south/frame_000.png",
        rgba_png(CELL[0], CELL[1], (5, 5, 5, 255)))
    output = fx.validate_fails("stale atlas")
    assert "hero/atlas/idle" in output, output
    assert "/walk" not in output, (
        f"an unrelated animation was reported stale:\n{output}")


@scenario("a tampered atlas is rejected even though its index is untouched")
def _tampered_atlas(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    record = entry(fx.index("hero"), "idle")
    fx.atlas_path("hero", "idle").write_bytes(
        rgba_png(record["atlas_width"], record["atlas_height"],
                 (7, 7, 7, 255)))
    fx.validate_fails("atlas content does not match its sources")


@scenario("an index whose digests were rewritten to match a tampered atlas "
          "is still rejected")
def _forged_index(fx: Fixture) -> None:
    # The check regenerates from sources rather than trusting the
    # numbers the file carries about itself, so making the index
    # self-consistent with a forged atlas does not launder it.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    document = fx.index("hero")
    record = entry(document, "idle")
    forged = rgba_png(record["atlas_width"], record["atlas_height"],
                      (7, 7, 7, 255))
    fx.atlas_path("hero", "idle").write_bytes(forged)
    frame = pack_atlas.decode_rgba8(fx.atlas_path("hero", "idle"))
    record["atlas_digest"] = pack_atlas.content_digest(
        frame.width, frame.height, frame.pixels)
    fx.index_path("hero").write_bytes(
        pack_atlas.canonical_index_bytes(document))
    fx.validate_fails("index entry disagrees with a fresh compile")


@scenario("a hand-edited index value is rejected")
def _hand_edited_index(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    document = fx.index("hero")
    document["tool_version"] = document["tool_version"] + 1
    fx.index_path("hero").write_bytes(
        pack_atlas.canonical_index_bytes(document))
    fx.validate_fails("generated-index metadata mismatch")


@scenario("a reformatted but semantically identical index is rejected")
def _noncanonical_index(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    document = fx.index("hero")
    fx.index_path("hero").write_text(
        json.dumps(document, indent=4) + "\n", encoding="utf-8")
    fx.validate_fails("not canonically serialized")


@scenario("a missing indexed atlas is rejected")
def _missing_indexed_atlas(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    fx.atlas_path("hero", "idle").unlink()
    fx.validate_fails("indexed atlas is missing from disk")


@scenario("an atlas directory with no index is rejected")
def _atlas_dir_without_index(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    fx.index_path("hero").unlink()
    fx.validate_fails("has no index.json")


@scenario("a unit with no generated artifacts stays valid")
def _legacy_unit_without_index(fx: Fixture) -> None:
    # Absence of an index is legitimate until TEX-4 begins production
    # tracking; the freshness checks must not make it an error.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.validate_ok()
    assert not fx.atlas_dir("hero").exists()


@scenario("a generated directory for an undeclared unit is rejected")
def _orphan_atlas_directory(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    shutil.copytree(fx.atlas_dir("hero"), fx.atlas_dir("ghost"))
    fx.validate_fails("no declaration in data/units/")


@scenario("inconsistent frame dimensions refuse to compile")
def _inconsistent_dimensions(fx: Fixture) -> None:
    counts = uniform(CANON5, 2)
    build_unit(fx, "hero", [("idle", counts, True)])
    fx.write_file(
        "assets/textures/units/hero/animations/idle/east/frame_001.png",
        rgba_png(CELL[0] + 1, CELL[1], (4, 4, 4, 255)))
    code, output = fx.compile()
    assert code != 0, f"a ragged cell size compiled:\n{output}"
    assert "inconsistent frame dimensions" in output, output
    assert not fx.atlas_dir("hero").exists(), (
        "a failed compile left artifacts behind")


@scenario("a symlinked atlas output directory refuses to compile")
def _symlinked_output_directory(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    outside = fx.root.parent / "outside_atlas"
    outside.mkdir(exist_ok=True)
    fx.symlink("assets/textures/units/hero/atlas", str(outside))
    code, output = fx.compile()
    assert code != 0, f"a symlinked output directory compiled:\n{output}"
    assert "symlink in the atlas output path" in output, output
    assert list(outside.iterdir()) == [], (
        "the compiler wrote through a symlink: "
        + ", ".join(p.name for p in outside.iterdir()))


@scenario("obsolete output is reported, then removed, and neighbours survive")
def _obsolete_output(fx: Fixture) -> None:
    hero = [("idle", uniform(CANON5, 2), True),
            ("walk", uniform(ALL8, 2), False)]
    build_unit(fx, "hero", hero)
    build_unit(fx, "prop", [("spin", uniform(CANON5, 2), True)])
    fx.compile_ok()
    prop_before = {p.name: p.read_bytes() for p in fx.artifacts("prop")}

    # Delete one animation the way a real rename or removal would:
    # source frames and declaration together.
    shutil.rmtree(fx.root / "assets/textures/units/hero/animations/walk")
    build_unit(fx, "hero", [hero[0]])
    fx.validate_fails("obsolete compiler-owned output")

    fx.compile_ok()
    assert not fx.atlas_path("hero", "walk").exists(), (
        "the obsolete atlas survived a recompile")
    assert fx.atlas_path("hero", "idle").exists(), (
        "a live atlas was removed alongside the obsolete one")
    assert {p.name: p.read_bytes() for p in fx.artifacts("prop")} == \
        prop_before, "another unit's artifacts were touched"
    assert (fx.root / "assets/textures/units/hero/animations/idle").is_dir(), (
        "source artwork was removed")
    fx.validate_ok()


@scenario("deleting a unit's last animation removes its whole generated set")
def _last_animation_removed(fx: Fixture) -> None:
    # The boundary of the obsolescence rule. An index describing no
    # animations is a shape TEX-3 should never have to interpret, so
    # the unit returns to the index-free legacy state instead.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    build_unit(fx, "prop", [("spin", uniform(CANON5, 2), True)])
    fx.compile_ok()
    prop_before = {p.name: p.read_bytes() for p in fx.artifacts("prop")}

    shutil.rmtree(fx.root / "assets/textures/units/hero/animations/idle")
    fx.yaml("hero", "units:\n  - name: hero\n"
                    '    sprite: "assets/textures/units/hero/idle.png"\n')
    fx.write_file("assets/textures/units/hero/idle.png",
                  rgba_png(1, 1, (0, 0, 0, 255)))

    fx.compile_ok()
    assert not fx.index_path("hero").exists(), (
        "an index describing no animations was left behind")
    assert not fx.atlas_path("hero", "idle").exists()
    assert {p.name: p.read_bytes() for p in fx.artifacts("prop")} == \
        prop_before, "another unit's artifacts were touched"
    fx.validate_ok()


@scenario("compiling refuses outright when the inventory does not validate")
def _compile_refuses_invalid_inventory(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    (fx.root / "assets/textures/units/hero/animations/idle/north"
             / "frame_001.png").unlink()
    code, output = fx.compile()
    assert code != 0, f"compiled from a broken inventory:\n{output}"
    assert "refusing to compile" in output, output
    assert not fx.atlas_dir("hero").exists()


@scenario("two animations that differ only in case refuse to compile")
def _case_insensitive_atlas_collision(fx: Fixture) -> None:
    # Driven through `compile_unit` rather than a fixture tree because
    # the hazard is a case-INSENSITIVE filesystem: the two source
    # directories this guards against cannot both exist on the machine
    # where the collision matters, so a filesystem fixture would only
    # ever run on Linux. The `_RH_` exception is what makes mixed case
    # reachable at all.
    counts = uniform(ALL8, 2)
    build_unit(fx, "hero", [("attack_RH_dagger", counts, False)])
    fx.compile_ok()

    report = pack_atlas.Report()
    decls = pack_atlas.load_declarations(report, fx.root / "data" / "units")
    assert not report.errors, report.errors
    decl = next(d for d in decls if d.name == "hero")
    twin = pack_atlas.AnimDecl(
        "hero", "attack_rh_dagger", False, dict(decl.anims[0].frames),
        "hero/attack_rh_dagger")
    clashing = pack_atlas.UnitDecl(
        decl.name, decl.asset_only, decl.source, decl.anims + [twin],
        decl.aux_paths)

    outcome = pack_atlas.compile_unit(report, fx.root, clashing, False)
    assert outcome is None, "a colliding atlas filename compiled"
    assert any("case-insensitive" in issue.msg for issue in report.errors), (
        f"the collision was not named: "
        f"{[issue.msg for issue in report.errors]}")


@scenario("the pinned CI toolchain matches tools/requirements-assets.txt")
def _pinned_toolchain_agrees(_fx: Fixture) -> None:
    # The image tag is the hash of the Dockerfile plus ci-image.yml, so
    # the pins are spelled there rather than COPYed from this file — a
    # copied requirements file could change without producing a new
    # image. That duplication is only safe while something fails on
    # drift, which is this case.
    tools = Path(__file__).resolve().parent
    requirements = (tools / "requirements-assets.txt").read_text(
        encoding="utf-8")
    dockerfile = (tools.parent / ".github" / "ci" / "Dockerfile").read_text(
        encoding="utf-8")
    pins = [line.strip() for line in requirements.splitlines()
            if line.strip() and not line.startswith("#")]
    assert len(pins) >= 2, f"expected pinned requirements, got {pins}"
    for pin in pins:
        assert "==" in pin, f"requirement is not pinned: {pin}"
        assert f'"{pin}"' in dockerfile, (
            f"the CI image does not install the pinned {pin}; update "
            f".github/ci/Dockerfile and tools/requirements-assets.txt "
            f"together")


# --------------------------------------------------------------------
# Runner
# --------------------------------------------------------------------

def run_case(build: Case, unit: Optional[str] = None) -> tuple[int, str]:
    """Run one fixture, reporting a validator CRASH as that case's own
    failure rather than letting it abort the suite.

    A traceback escaping here would kill the run before any case was
    reported, so a checker that raises on malformed input would look
    like a suite-wide breakage instead of one failing case — and a
    negative case whose rule was mutated into a crash would silently
    produce no `FAIL:` line at all.
    """
    parent = tempfile.mkdtemp(prefix="pack_atlas_test_")
    try:
        fixture = Fixture(Path(parent) / "repo")
        build(fixture)
        return fixture.run(unit)
    except Exception:  # noqa: BLE001 - a crash IS the finding here
        return 70, ("the validator raised instead of reporting:\n"
                    + traceback.format_exc())
    finally:
        shutil.rmtree(parent, ignore_errors=True)


def run_scenario(build: Case) -> Optional[str]:
    """Run one compiler scenario; return its failure text, or ``None``.

    A scenario drives the tool itself and asserts on what it produced,
    so its verdict is an exception rather than an exit code.
    """
    parent = tempfile.mkdtemp(prefix="pack_atlas_scenario_")
    try:
        build(Fixture(Path(parent) / "repo"))
        return None
    except AssertionError as error:
        return str(error) or traceback.format_exc()
    except Exception:  # noqa: BLE001 - a crash IS the finding here
        return "the compiler raised instead of reporting:\n" + \
            traceback.format_exc()
    finally:
        shutil.rmtree(parent, ignore_errors=True)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("-v", "--verbose", action="store_true")
    args = ap.parse_args()

    failures: List[str] = []
    total = 0

    for name, build, unit in POSITIVE:
        total += 1
        code, output = run_case(build, unit)
        if args.verbose:
            print(f"--- positive: {name}\n{output}")
        if code != 0:
            failures.append(
                f"positive '{name}': expected exit 0, got {code}\n{output}")

    for name, build, expect, unit in NEGATIVE:
        total += 1
        code, output = run_case(build, unit)
        if args.verbose:
            print(f"--- negative: {name}\n{output}")
        if code == 0:
            failures.append(
                f"negative '{name}': expected a nonzero exit, got 0\n{output}")
        elif expect not in output:
            failures.append(
                f"negative '{name}': exited {code} but no diagnostic matched "
                f"{expect!r}\n{output}")

    for name, build in SCENARIO:
        total += 1
        failure = run_scenario(build)
        if args.verbose:
            print(f"--- scenario: {name}\n{failure or 'ok'}")
        if failure is not None:
            failures.append(f"scenario '{name}': {failure}")

    # The suite is only meaningful if it actually built fixtures; a
    # refactor that silently emptied a registry must not read as green.
    if len(POSITIVE) < 9 or len(NEGATIVE) < 61 or len(SCENARIO) < 27:
        failures.append(
            f"case registries look truncated: {len(POSITIVE)} positive, "
            f"{len(NEGATIVE)} negative, {len(SCENARIO)} scenario")

    if failures:
        for failure in failures:
            print(f"FAIL: {failure}", file=sys.stderr)
        print(f"\ntest_pack_atlas: {len(failures)} of {total} case(s) failed",
              file=sys.stderr)
        return 1

    print(f"test_pack_atlas: all {total} case(s) pass "
          f"({len(POSITIVE)} positive, {len(NEGATIVE)} negative, "
          f"{len(SCENARIO)} scenario)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
