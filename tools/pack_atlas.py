#!/usr/bin/env python3
"""
pack_atlas.py — unit-animation asset inventory validator AND the
deterministic per-animation atlas compiler.

Two modes, one tool:

    --validate-only   the authoritative asset inventory gate (#1257),
                      extended by #1258 to also verify any generated
                      atlas index it finds against its own sources.
    --compile         compile the declared frames into one lossless PNG
                      atlas per ANIMATION plus a generated per-unit
                      index (#1258).

Per `docs/texture_infrastructure.md` (TEX-2), KTX2 encoding stays
deferred to TEX-5 and runtime sampling to TEX-3.

WHAT IT VALIDATES (issue #1257)
-------------------------------

Discovery is FILESYSTEM-FIRST. The physical inventory is every PNG
beneath

    assets/textures/units/<unit>/animations/<animation>/<direction>/

and the declarations under `data/units/*.yaml` are checked AGAINST it,
not used to decide what to look at. That ordering is the whole point:
the previous version harvested unit names from the YAML files and so
never examined the three shipped asset trees that had no YAML at all.

Every committed animation PNG must be owned by exactly one
animation-frame declaration. There is no directory-level or glob-level
exemption mechanism — a file is either declared or it is a failure.

Two declaration forms live under `data/units/`:

  units:        a gameplay unit. Registered by
                `Engine.Asset.YamlUnits.loadUnitYaml`, loads textures,
                spawnable. `name` and `sprite` are mandatory.

  asset_units:  an ASSET-ONLY unit (#1257). Declares animation frames
                for this validator and for the `--preview units/<name>`
                browser, and nothing else — never returned by
                `loadUnitYaml`, so never registered, never loaded into
                the gameplay unit registry, not listable or spawnable.
                `name` and `animations` are mandatory; gameplay fields
                are refused outright rather than ignored.
                The form remains supported for shipped art that must be
                atlas-validated and previewable without registering as a
                gameplay unit. Runtime registration follows the top-level
                key rather than filesystem presence.

A file may hold either key or both. A file holding neither is an error
(that is what a mistyped top-level key looks like), and so is a key
present with an explicit null — the engine's own decoder reads that as
absent and refuses the file, so accepting it here would leave the gate
green while startup failed.

INVARIANTS ENFORCED
-------------------

  * a unit identifier is one lowercase `[a-z0-9_]+` path component; an
    animation identifier is the same, plus the one narrowly matched
    approved exception `<lowercase>_RH_<lowercase>` for the documented
    asymmetric-weapon animations — see ANIM_IDENT_RE;
  * direction names come from the engine's own direction vocabulary and
    sit at the direction level of the tree;
  * frame files are named `frame_NNN.png`, with exactly three digits;
  * a declared path is relative, free of `..`, free of symlinks, and
    resolves inside the exact expected
    `<unit>/animations/<animation>/<direction>/` directory — cross-unit,
    cross-animation and cross-direction references are all named as
    such;
  * every declared frame exists AS A REGULAR FILE, DECODES as a real
    PNG, and every physical frame is declared;
  * every frame of ONE animation decodes to the same pixel size —
    different DIRECTIONS of that animation may still hold different
    frame COUNTS, which is a separate axis;
  * one physical frame is claimed by at most one animation-frame slot
    (reuse as a unit's `sprite`, `directional_sprites` entry or
    `portrait` is deliberately legal and is NOT a duplicate claim);
  * `flip: true` declares exactly the canonical five authored directions
    (south, south-east, east, north-east, north) and `flip: false`
    declares exactly all eight;
  * per direction, frame indices start at 0, ascend in the order they
    are declared (playback follows that order), and have no gaps or
    duplicates — different directions of one animation may still hold
    different counts;
  * an asset-only entry declares exactly `name` and `animations` — a
    whitelist, so an unknown key fails as surely as a gameplay one;
  * animation and direction keys are strings, never coerced (YAML
    resolves an unquoted `123:` to an int that would stringify into a
    valid-looking identifier);
  * `fps` is a positive number that survives the engine's 32-bit
    `Float` — `.nan` and `.inf` are real floats to PyYAML and neither
    fails a positivity test, an unbounded-precision int makes
    `math.isfinite` raise rather than answer, and `1.0e+100` /
    `1.0e-100` fit a double but load as infinity / zero — and `loop` is
    a boolean, rejected rather than coerced when it is not;
  * no symlink appears anywhere in the walk (unit directory,
    `animations/` root, animation directory, direction directory, or
    frame), so nothing can be linked past the inventory.

WHAT IT COMPILES (issue #1258)
------------------------------

`--compile` turns the validated declarations into DERIVED artifacts.
Sources are never touched: individual PNG frames remain the editable
artwork (D-1) and unit YAML remains the only hand-edited semantic
authority (D-11).

    assets/textures/units/<unit>/atlas/<animation>.png   one atlas per
                                                         ANIMATION (D-2)
    assets/textures/units/<unit>/atlas/index.json        the generated
                                                         per-unit index

Layout. One ROW per AUTHORED direction, in `ATLAS_DIRECTION_ORDER` —
the engine's own `Unit.Direction` order `S, SW, W, NW, N, NE, E, SE`,
restricted to the directions this animation actually authors, so a
`flip: true` animation has five rows and a `flip: false` one has eight.
Each direction's row index is nevertheless recorded EXPLICITLY, so the
runtime reads a row rather than re-deriving the order.

Columns are the animation's maximum authored frame count. A shorter row
is rectangularized with transparent RGBA8 zero slots and NOTHING ELSE
(D-5): they exist so the sheet is a rectangle, and the index's
per-direction `frame_count` is the only frame authority — no such slot
is addressable as a frame.

Cell geometry is exact INTEGER pixels, at a stride widened by the
one-texel extrusion gutter (#2076). Each cell occupies a physical SLOT
of `(cell_width + 2 * CELL_PADDING)` x `(cell_height + 2 *
CELL_PADDING)`, and frame `c` of the direction whose row is `r` has its
LOGICAL cell at `x = c * slot_width + CELL_PADDING`,
`y = r * slot_height + CELL_PADDING`, `cell_width` x `cell_height`.
Every frame of one animation must decode to those same cell
dimensions; a mismatch is a compile error, never an implicit rescale or
crop (D-6 — nothing here resamples or blends).

The gutter around each slot is filled by copying that cell's own
outermost texels outward, corners included, so a bilinear tap taken
anywhere inside a logical cell reads only that cell's colours instead
of bleeding into the neighbouring frame. Nearest sampling is unchanged
by construction: the index addresses the INNER cell, so no fragment
centre moves and the picture stays pixel-identical.

Every atlas cell is a byte-for-byte copy of its source frame's
canonical decoded RGBA8 samples, alpha included, and the gutter around
it is a byte-for-byte copy of that frame's own edge texels.
"Byte-for-byte" is about those decoded samples, not about PNG-encoded
file bytes: the engine's own upload path decodes to RGBA8 as well
(`Engine.Scripting.Lua.Message.Texture`'s convertRGBA8).

The index. One JSON document per unit, generated end to end — see
`build_index_document` for the exact schema. It carries a
`schema_version` (the FORMAT contract TEX-3 will parse, bumped when the
document shape changes) separately from `tool_version` (this compiler's
own revision), a documented `direction_order`, and per animation: the
storage format and atlas path, atlas/cell dimensions, the extrusion
gutter (`cell_padding`), columns, rows, each authored direction's row
and REAL frame count, the mirroring declaration, and two digests.

Digests are `sha256` and are named as such in the document:

  * `source_digest` is PER ANIMATION, over a canonically ordered,
    length-prefixed stream of that animation's own inputs — name, flip,
    fps, loop, cell geometry including `cell_padding`, and for every
    direction in atlas order its declared frame paths and their
    canonical decoded RGBA8 pixels. Per-animation
    is the point: one animation's edit must not invalidate an unrelated
    atlas (D-12).
  * `atlas_digest` is over the atlas's decoded RGBA8 CONTENT
    (dimensions + samples), not its file bytes, so it stays meaningful
    across PNG encoders while still pinning every pixel.

Determinism and locality. A clean rebuild from identical sources under
an unchanged toolchain produces identical artifacts. An incremental run
compares each artifact against what it would generate and WRITES ONLY
ON A REAL DIFFERENCE, so editing one animation rewrites that
animation's atlas and its unit index and nothing else — unrelated
atlases are not even opened for writing. Note that an mtime-only touch
of a frame changes nothing: the digest is over content.

Obsolete compiler-owned output — an atlas for an animation that was
deleted or renamed — is removed from the unit's own `atlas/` directory
during a compile of that unit. Nothing outside that directory is ever
removed, so source artwork and other units' artifacts are structurally
out of reach.

STALENESS
---------

`--validate-only` is index-aware. A unit with NO index is valid HERE:
this tool validates declarations against art, and an uncompiled tree is
a legitimate intermediate state of a working copy. The ENGINE is
stricter — since #1261 it refuses to register a unit that declares
animations and ships no compiled artifacts. Every shipped declaration,
gameplay or asset-only, is compiled and tracked. Where an index DOES
exist it is regenerated from the sources and compared, so a stale
source digest, a hand-edited or non-canonically serialized index, a
missing indexed atlas, and an atlas whose pixels do not match its
sources are all reported — and a tampered index cannot certify a
tampered atlas, because the comparison is against a fresh regeneration
rather than against the numbers the file itself carries.

USAGE
-----

    python3 tools/pack_atlas.py --validate-only
        Validate the whole unit corpus. Exit 0 on success, non-zero
        with a report on any issue.

    python3 tools/pack_atlas.py --validate-only --unit acolyte
        Restrict both the declarations and the filesystem walk to one
        unit. A name with neither a declaration nor an asset tree is an
        error, not an empty success.

    python3 tools/pack_atlas.py --validate-only --strict
        Also treat warnings as errors. Every inventory violation above
        is an ERROR regardless of this flag, and so is every
        frame-content finding and every image/slot budget breach;
        `--strict` runs no extra checks, it only promotes the warnings.
        Two things warn: non-PNG debris in the animation tree, and the
        resident-memory budget below. The latter is why CI and
        `make ci` pass `--strict` — a breach is a project decision to
        make, and this is what stops it passing unnoticed.

    python3 tools/pack_atlas.py --compile [--unit acolyte]
        Compile atlases and indices. Refuses to run at all if the
        inventory does not validate.

    python3 tools/pack_atlas.py --compile --check
        Report what a compile WOULD change and change nothing. Exits
        non-zero if anything is out of date — the shape a CI freshness
        gate wants.

    python3 tools/pack_atlas.py {--validate-only|--compile} --root <dir>
        Operate on an alternative tree holding `data/units/` and
        `assets/textures/units/`. Used by tools/test_pack_atlas.py so
        its fixtures never touch the shipped assets.

Exactly one of `--validate-only` / `--compile` is required: writing
derived artifacts is never something this tool does by default.

FRAME CONTENTS (issue #1311)
----------------------------

Every DECLARED frame is opened and decoded, not merely stat-ed. #1257
stopped at the file boundary, so a truncated, corrupt or mislabelled
frame passed the gate and failed later — at texture-upload time, or
visibly in game. `validate_frame_image` closes that gap in three
checks, because each one covers ground the others cannot:

  * a full `decode_rgba8` runs the filters, interlace passes and colour
    conversion — catching a truncated stream, corrupt compressed data,
    a non-image, and (through its own format check) a valid image of
    some other format renamed to `.png` — but never looks at an IDAT
    chunk's checksum, because Pillow reads and discards those four
    bytes while streaming pixel data;
  * Pillow's own `verify()` then CRCs the chunks, which is what catches
    an intact payload under a wrong checksum. It never decompresses, so
    it could not have replaced the pass above;
  * and `locate_png_stream_end` covers the terminal chunk, which
    `verify()` breaks ON without checksumming and the decoder never
    reads at all, plus anything appended after the image ends. It walks
    chunk FRAMING only — length, type, payload, CRC — decoding nothing
    and knowing no chunk type but IEND, and it runs only after Pillow
    has CRC-validated that sequence, so it cannot disagree with the
    real decoder about where a chunk lies. Its answer feeds two
    constant comparisons: that IEND's own 12 bytes are the fixed
    constant its empty payload makes them, and that the file ends
    there. Checking the FILE's last bytes would not do: appending a
    second canonical IEND leaves a perfect tail while the real image
    ended 12 bytes earlier.

Together they reject a truncated file, corrupt compressed data, a bad
chunk checksum anywhere including the terminal one, a structurally
invalid stream, a non-image wearing a `.png` name, and a valid image of
some OTHER format renamed to `.png` (the engine's loader is a PNG
loader). Every legitimate PNG colour type
— paletted, greyscale, greyscale+alpha, 16-bit, interlaced — is
accepted: the rule is "decodes as a PNG", never "is already RGBA8".

The pixel size each frame decodes to is then compared ACROSS one
animation, which is the constraint the atlas cell geometry rests on
(D-6 forbids resampling, so the compiler has no way to reconcile a
mismatch). This says nothing about frame COUNTS: different directions
of one animation legitimately hold different numbers of frames.

Content findings are ERRORS, in plain `--validate-only` as much as
under `--strict`.

WHAT IT STILL DOES NOT VALIDATE
-------------------------------

Non-animation unit textures. `portrait.png`, `directional_sprites`
entries, `sprite`, and `unknown_unit/rotations/*.png` are checked for
EXISTENCE only: the inventory's scope is `animations/`, and those files
are referenced from hard-coded Haskell or non-animation YAML fields.

REQUIREMENTS
------------

PyYAML for the declarations, Pillow for image decode/encode. Both are
now load-bearing for `--validate-only`: since #1311 the inventory gate
decodes every declared frame, so an absent Pillow is a loud ERROR
naming the install command, never a silent skip of the content checks
— a gate that skipped them would print OK while validating nothing.

Pillow is still imported LAZILY, but that now only spares a run with no
declared frames to decode (`--help`, an empty root, an argument error).

`tools/requirements-assets.txt` pins both, is what the CI image
installs, and is therefore the reference toolchain for byte-identical
output. Install it with:

    python3 -m pip install --user -r tools/requirements-assets.txt

Validation does NOT require that exact toolchain: every recorded digest
is over canonical decoded RGBA8, so a different Pillow build verifies a
committed atlas just as well as the one that wrote it.

"""
from __future__ import annotations

import argparse
import hashlib
import io
import json
import math
import re
import struct
import sys
import zlib
from dataclasses import dataclass, field
from pathlib import Path, PurePosixPath
from typing import Any, Dict, List, Optional, Sequence, Set, Tuple

try:
    import yaml  # PyYAML
except ImportError:
    sys.stderr.write(
        "error: PyYAML is required. Install with:\n"
        "    python3 -m pip install --user -r tools/requirements-assets.txt\n"
    )
    sys.exit(2)


REPO_ROOT = Path(__file__).resolve().parent.parent

# Direction-key aliases the engine accepts. Keep in sync with
# Engine.Scripting.Lua.API.Units.Yaml.parseDirKey and
# Engine.Preview.Unit.parseDirectionDirName on the Haskell side. We
# normalise to the long form for reporting.
DIR_ALIASES: Dict[str, str] = {
    "s": "south", "south": "south",
    "sw": "south-west", "south-west": "south-west", "south_west": "south-west",
    "w": "west", "west": "west",
    "nw": "north-west", "north-west": "north-west", "north_west": "north-west",
    "n": "north", "north": "north",
    "ne": "north-east", "north-east": "north-east", "north_east": "north-east",
    "e": "east", "east": "east",
    "se": "south-east", "south-east": "south-east", "south_east": "south-east",
}

ALL_DIRS = {
    "south", "south-west", "west", "north-west",
    "north", "north-east", "east", "south-east",
}

# The canonical five authored directions of a bilaterally-symmetric
# animation: the eastern half, with SW/W/NW mirrored at draw time.
CANONICAL_DIRS = {"south", "south-east", "east", "north-east", "north"}

# A unit identifier is exactly one lowercase path component.
#
# All three use \Z, never $: `$` also matches just BEFORE a trailing
# newline, so `re.match(r"^[a-z0-9_]+$", "unit\n")` succeeds and a
# directory or key whose name ends in a newline would pass a rule that
# demands one path component.
UNIT_IDENT_RE = re.compile(r"\A[a-z0-9_]+\Z")

# An ANIMATION identifier is the same lowercase rule, plus ONE narrowly
# matched approved exception: the documented asymmetric-weapon infix
# `_RH_` (`docs/asset_generation.md` — a mirrored right hand would become
# a left hand, so those animations author all eight directions). Eight
# shipped acolyte animations use it, and `scripts/acolyte_combat.lua` and
# `scripts/unit_ai_combat_attack.lua` concatenate the suffix at runtime
# from the equipped item, so lowercasing them is a gameplay-data rename
# #1257 puts out of scope.
#
# The exception is deliberately a SHAPE, not a blanket allowance of upper
# case: both halves around `_RH_` must still be lowercase, so
# `attack_heavy_RH_dagger` is accepted while `AnyThing`,
# `attack_heavy_RH_Dagger`, `attack_LH_dagger` and a bare `RH_dagger` are
# all rejected. The safety property the rule exists for — ONE path
# component, ASCII word characters only, so nothing carries a separator, a
# dot, or traversal — holds either way.
ANIM_IDENT_RE = re.compile(r"\A[a-z0-9_]+\Z|\A[a-z0-9_]+_RH_[a-z0-9_]+\Z")

# Exactly three digits: `frame_NNN.png` is the asset format, and the
# numbering rule below counts from `frame_000.png`. `\d+` would also
# admit `frame_1.png`, `frame_01.png` and `frame_0000.png`, which are
# three different spellings of one index and would let a directory hold
# apparent duplicates that the gap/duplicate check could not see.
FRAME_RE = re.compile(r"\Aframe_(\d{3})\.png\Z")

# Relative to the validation root.
ASSET_PREFIX: Tuple[str, ...] = ("assets", "textures", "units")

PNG_SIGNATURE = b"\x89PNG\r\n\x1a\n"

# The terminal PNG chunk, in full. IEND's payload is EMPTY by
# specification, so its length, type and CRC are all fixed: this is a
# constant to compare against, not a chunk to parse. `verify_png_container`
# needs it because Pillow's `verify()` breaks ON this chunk before
# checksumming it, leaving the terminal CRC the one thing no library pass
# validates.
PNG_IEND_CHUNK = (struct.pack(">I", 0) + b"IEND"
                  + struct.pack(">I", zlib.crc32(b"IEND") & 0xFFFFFFFF))

# --- compiler constants (#1258) --------------------------------------

# The FORMAT contract. TEX-3 parses this document, and
# `docs/texture_infrastructure.md` requires an incompatible index to be
# rejectable, so this number is the runtime's compatibility gate and
# changes only when the document SHAPE does.
#
# v2 (#2076) added the REQUIRED per-animation `cell_padding`. A v1
# document describes edge-adjacent cells at a different stride, so
# reading one under v2 geometry would sample the wrong texels — the
# runtime rejects it on the version alone, before it can miss the field.
INDEX_SCHEMA_VERSION = 2

# This compiler's own revision, deliberately separate from the schema
# version: a change in how artifacts are produced (encoder settings,
# padding, digest inputs) that leaves the document shape alone bumps
# only this, and every index carrying an older value is regenerated.
#
# v2 (#2076): cells are extruded by one texel per side.
TOOL_VERSION = 2

# The extrusion gutter, in texels per side (#2076, epic #2072 TSR-3's
# precondition). Every cell occupies a `(cell_width + 2 * CELL_PADDING)`
# x `(cell_height + 2 * CELL_PADDING)` slot whose border is a copy of
# the cell's own edge texels, so a bilinear tap anywhere inside the
# logical cell reads only that cell's colours. Nearest sampling is
# untouched: the UV rect still addresses the INNER cell exactly, so no
# fragment centre moves.
#
# One texel is the supported layout and the runtime validates for
# exactly it. Widening this is a schema change, not a constant edit: the
# stride, the digest, and every recorded index would all move with it.
CELL_PADDING = 1

GENERATOR = "tools/pack_atlas.py"
DIGEST_ALGORITHM = "sha256"
STORAGE_FORMAT = "png"

# The compiler-owned output directory, a SIBLING of `animations/`. The
# inventory walk descends `animations/` and nothing else, so generated
# artifacts are structurally outside it and can never read as an
# unclassified source frame.
ATLAS_DIR_NAME = "atlas"
INDEX_FILENAME = "index.json"

# Row order: the engine's own `Unit.Direction` constructor order
# (`DirS | DirSW | DirW | DirNW | DirN | DirNE | DirE | DirSE`),
# restricted per animation to the directions it actually authors. Each
# row index is recorded explicitly in the index, so the runtime never
# has to reproduce this list to read an atlas — it is here to make the
# layout deterministic, not to be re-derived downstream.
ATLAS_DIRECTION_ORDER: Tuple[str, ...] = (
    "south", "south-west", "west", "north-west",
    "north", "north-east", "east", "south-east",
)

# `Engine.Asset.YamlUnits.UnitYamlAnim`'s own decoder defaults. The
# index records EFFECTIVE values, so an animation that omits a field
# must record exactly what the engine would hold for it.
DEFAULT_FPS = 8.0
DEFAULT_LOOP = True
DEFAULT_FLIP = False

# Digest domain tags. Each carries its own version so a change to what
# goes INTO a digest invalidates every recorded one rather than
# silently producing a colliding value from different inputs.
SOURCE_DIGEST_TAG = b"synarchy-atlas-source-v2"
ATLAS_DIGEST_TAG = b"synarchy-atlas-content-v1"


class ImageBackendMissing(Exception):
    """Pillow is needed for this operation and is not installed."""


_IMAGE_MODULE: Any = None


def image_module() -> Any:
    """Import Pillow on demand.

    Still lazy, but no longer optional in practice: since #1311 the
    inventory gate decodes every declared frame, so any run with frames
    to check needs it. Laziness now spares only the runs that decode
    nothing at all — `--help`, an argument error, a root with no
    declarations — and keeps the import failure reportable as one
    finding rather than an import-time crash.
    """
    global _IMAGE_MODULE
    if _IMAGE_MODULE is None:
        try:
            from PIL import Image  # type: ignore[import-not-found]
        except ImportError as error:
            raise ImageBackendMissing(
                "Pillow is required to validate frame contents and to "
                "compile or verify atlases. Install the pinned toolchain "
                "with:\n"
                "    python3 -m pip install --user -r "
                "tools/requirements-assets.txt"
            ) from error
        _IMAGE_MODULE = Image
    return _IMAGE_MODULE


@dataclass
class Frame:
    """One decoded source frame: canonical RGBA8 samples plus size."""
    width: int
    height: int
    pixels: bytes


def decode_rgba8(path: Path) -> Frame:
    """Decode one PNG to canonical 8-bit RGBA samples.

    Decoding goes through Pillow rather than a hand-rolled parser: PNG
    has paletted, greyscale, 16-bit and interlaced forms plus ancillary
    colour-management chunks, and every one of them has to arrive at
    the same canonical RGBA8 the engine's upload path produces.
    `convert("RGBA")` is that normalisation; it is not a resample.
    """
    image_mod = image_module()
    try:
        with image_mod.open(path) as handle:
            if handle.format != "PNG":
                found = handle.format or "an unrecognised format"
                raise ValueError(f"expected a PNG, got {found}")
            converted = handle.convert("RGBA")
            return Frame(converted.width, converted.height,
                         converted.tobytes())
    except ImageBackendMissing:
        raise
    except Exception as error:  # noqa: BLE001 - any decode failure is one finding
        raise ValueError(f"cannot decode as an image: {error}") from error


def locate_png_stream_end(path: Path) -> Tuple[int, int, bytes]:
    """Find where the PNG datastream ends: `(end, file size, IEND bytes)`.

    `end` is the offset just past the FIRST IEND chunk — the one that
    terminates the image per the specification. `IEND bytes` are that
    chunk's own 12 bytes, for the caller to compare against
    `PNG_IEND_CHUNK`. Raises `ValueError` when no IEND terminates the
    stream, rather than returning a sentinel the caller would have to
    re-check on a path Pillow has already made unreachable.

    This is chunk FRAMING and nothing else: a four-byte big-endian
    length, a four-byte type, the payload, a four-byte CRC. It decodes
    nothing, checksums nothing, and knows nothing about any chunk type
    beyond the four letters that end the stream.

    That narrowness is deliberate and must be preserved (#1311 —
    hand-rolling a second PNG parser is what sank the previous
    attempt). Two things keep it honest: it runs only AFTER Pillow has
    CRC-validated the chunk sequence, which fixes the framing uniquely,
    so this cannot disagree with the real decoder about where a chunk
    lies; and its entire output is one offset, which the caller uses to
    ask a single question the library will not answer — is there
    anything after the image?
    """
    with path.open("rb") as handle:
        size = handle.seek(0, io.SEEK_END)
        handle.seek(len(PNG_SIGNATURE))
        while True:
            header = handle.read(8)
            if len(header) < 8:
                break
            (length,) = struct.unpack(">I", header[:4])
            chunk_start = handle.tell() - 8
            end = handle.seek(length + 4, io.SEEK_CUR)
            if end > size:
                # A chunk claiming more bytes than the file holds.
                break
            if header[4:] == b"IEND":
                handle.seek(chunk_start)
                return end, size, handle.read(end - chunk_start)
    raise ValueError(
        "corrupt PNG structure: no IEND chunk terminates the stream")


def verify_png_container(path: Path) -> None:
    """Check the chunk stream's checksums, without decompressing.

    Pillow's `verify()` walks the chunks and CRCs each one, which is
    the ONLY thing here that looks at an IDAT checksum — the decoder
    reads and discards those four bytes while streaming pixel data, so
    a correct payload under a wrong checksum decodes happily.

    It stops ON the terminal chunk without checksumming it, though, and
    the decoder never reads that far, so IEND is the one chunk neither
    library pass covers — nor does either notice bytes appended after
    it. `locate_png_stream_end` supplies the offset both questions need.

    Deliberately does NOT re-check the format: this only ever runs on a
    file `decode_rgba8` has already accepted as a decodable PNG, and a
    second copy of that rule would mean neither copy could be removed
    detectably.
    """
    image_mod = image_module()
    try:
        with image_mod.open(path) as handle:
            handle.verify()
    except ImageBackendMissing:
        raise
    except Exception as error:  # noqa: BLE001 - any rejection is one finding
        raise ValueError(f"corrupt PNG structure: {error}") from error

    try:
        end, size, terminal = locate_png_stream_end(path)
    except OSError as error:
        raise ValueError(f"cannot read: {error}") from error

    # Trailing data is not part of the image. Comparing the FILE's last
    # bytes would not establish this: appending a second canonical IEND
    # leaves a perfect tail while the real image ended 12 bytes earlier.
    if end != size:
        raise ValueError(
            f"corrupt PNG structure: {size - end} byte(s) follow the IEND "
            f"chunk that ends the image")
    # And the terminal chunk itself, which needs no parsing to check:
    # IEND's payload is EMPTY by specification, so its length, type and
    # CRC together are a fixed 12-byte constant.
    if terminal != PNG_IEND_CHUNK:
        raise ValueError(
            "corrupt PNG structure: the IEND chunk that ends the image "
            "carries a wrong terminal checksum")


def validate_frame_image(path: Path) -> Tuple[int, int]:
    """Prove one declared frame is real art, and report its pixel size.

    TWO passes, neither sufficient alone — see the module docstring's
    FRAME CONTENTS section. The decode comes first: it settles the
    format question and covers the compressed pixel stream, so the
    container pass that follows is reached only for a genuine,
    decodable PNG and has exactly one job left, the checksums.

    Raises `ValueError` naming the reason for any unreadable frame.
    `ImageBackendMissing` propagates untouched: an absent decoder is one
    condition for the whole run, not a finding about this file.
    """
    frame = decode_rgba8(path)
    verify_png_container(path)
    return frame.width, frame.height


@dataclass
class Issue:
    severity: str  # "error" | "warning"
    where: str
    msg: str


@dataclass
class Report:
    errors: List[Issue] = field(default_factory=list)
    warnings: List[Issue] = field(default_factory=list)

    def err(self, where: str, msg: str) -> None:
        self.errors.append(Issue("error", where, msg))

    def warn(self, where: str, msg: str) -> None:
        self.warnings.append(Issue("warning", where, msg))

    def has_failures(self, strict: bool) -> bool:
        return bool(self.errors) or (strict and bool(self.warnings))


def normalise_dir(key: str) -> Optional[str]:
    return DIR_ALIASES.get(key.lower())


def is_representable_number(value: object) -> bool:
    """Whether ``value`` is a finite number this tool can reason about.

    ``math.isfinite`` RAISES ``OverflowError`` on an int too large to
    convert to float, so it cannot be used as a bare predicate against
    YAML input: an integer literal has unbounded precision and a
    four-thousand-digit one parses perfectly well.
    """
    try:
        return math.isfinite(value)  # type: ignore[arg-type]
    except (OverflowError, TypeError, ValueError):
        return False


def narrow_to_runtime_float(value: float) -> float:
    """``value`` as the engine's 32-bit `Float` will actually hold it.

    The generated index records the EFFECTIVE frame rate, so `fps: 8`
    and `fps: 8.0` must produce one identical document — and a rate the
    engine will round has to be recorded rounded, or the index would
    promise a timing the runtime cannot reproduce. Only ever called
    after `fits_runtime_float` has accepted the value.
    """
    return float(struct.unpack("<f", struct.pack("<f", float(value)))[0])


def fits_runtime_float(value: float) -> bool:
    """Whether ``value`` survives the engine's single-precision `Float`.

    `Engine.Asset.YamlUnits.UnitYamlAnim` stores `uyaFps` as a Haskell
    `Float`, which is 32-bit. Python's own check only proves the value
    fits a 64-bit double, so `1.0e+100` and `1.0e-100` pass it and then
    become `Infinity` and `0` respectively at load time — neither is a
    frame rate. Round-tripping through a 32-bit pack reproduces exactly
    what the engine will hold.
    """
    try:
        narrowed = struct.unpack("<f", struct.pack("<f", float(value)))[0]
    except (OverflowError, ValueError):
        return False
    return math.isfinite(narrowed) and narrowed != 0.0


def render_scalar(value: object, limit: int = 40) -> str:
    """``repr`` of a YAML scalar, truncated.

    A malformed value can be arbitrarily long — the very input that
    motivated 'is_representable_number' is four thousand digits — and a
    diagnostic that pastes it whole is unreadable.
    """
    text = repr(value)
    if len(text) <= limit:
        return text
    return f"{text[:limit]}... ({len(text)} chars)"


# --------------------------------------------------------------------
# Declarations
# --------------------------------------------------------------------

@dataclass
class AnimDecl:
    """One declared animation, already bound to its declaring unit.

    `fps` and `loop` are the EFFECTIVE values — the declared ones where
    they are valid, `UnitYamlAnim`'s own decoder defaults otherwise —
    because the generated index records what the engine will hold, not
    what the file happened to spell.
    """
    unit: str
    name: str
    flip: bool
    frames: Dict[str, List[str]]  # normalised direction -> declared paths
    where: str
    fps: float = DEFAULT_FPS
    loop: bool = DEFAULT_LOOP


@dataclass
class UnitDecl:
    name: str
    asset_only: bool
    source: str            # YAML file name, for diagnostics
    anims: List[AnimDecl]
    aux_paths: List[Tuple[str, str]]   # (where, path) — sprite/portrait/…


# The COMPLETE schema of an asset-only entry. A whitelist, not a
# blacklist of gameplay fields: an entry declares animation frames and
# nothing else, so `typo: true` has to fail here just as `sprite:` does.
# A blacklist only catches the fields someone thought to enumerate.
ASSET_ONLY_KEYS = {"name", "animations"}

# The subset worth naming specifically, because the mistake is a
# recognisable one: an entry that was meant to be a spawnable unit and
# landed in the wrong list.
GAMEPLAY_ONLY_KEYS = {
    "sprite", "directional_sprites", "portrait", "state_animations",
    "stats", "skills", "knowledge", "body", "body_parts", "eager_stats",
    "starting_inventory", "starting_equipment", "starting_accessories",
    "equipment_class", "natural_resistance", "natural_weapon",
    "modifiers", "name_pool", "display_name", "base_width", "max_speed",
    "run_threshold",
}


def parse_animations(
    report: Report,
    unit_name: str,
    source: str,
    raw_anims: object,
) -> List[AnimDecl]:
    where_unit = f"{source}:{unit_name}"
    if raw_anims is None:
        return []
    if not isinstance(raw_anims, dict):
        report.err(where_unit, "`animations:` is not a mapping")
        return []

    out: List[AnimDecl] = []
    for raw_name, raw_anim in raw_anims.items():
        # No str() coercion. YAML resolves an unquoted `123:` to an int,
        # and stringifying it yields "123", which satisfies the
        # identifier rule — so a non-string key would name a real
        # animation directory rather than being rejected as malformed.
        if not isinstance(raw_name, str):
            report.err(
                where_unit,
                f"animation key must be a string, got "
                f"{type(raw_name).__name__} {raw_name!r} (quote it if the "
                f"name is meant to be literal)")
            continue
        anim_name = raw_name
        where = f"{unit_name}/{anim_name}"
        if not ANIM_IDENT_RE.match(anim_name):
            report.err(
                where_unit,
                f"unsafe animation identifier '{anim_name}': an animation "
                f"name must be one lowercase [a-z0-9_]+ path component")
            continue
        if not isinstance(raw_anim, dict):
            report.err(where, "animation entry is not a mapping")
            continue

        flip = raw_anim.get("flip", False)
        if not isinstance(flip, bool):
            report.err(
                where,
                f"`flip:` must be a boolean, got {render_scalar(flip)}")
            flip = False

        # `fps` and `loop` are inventory-relevant: the preview reads them
        # and the atlas compiler will. A wrong scalar type must be
        # REJECTED here rather than coerced or ignored — Aeson's decoder
        # on the Haskell side fails the whole file on one, so a value
        # this tool waves through would take the unit's real animations
        # down with it at load time.
        fps_value = DEFAULT_FPS
        if "fps" in raw_anim:
            fps = raw_anim["fps"]
            # bool is an int subclass in Python; `fps: true` is not a
            # frame rate.
            if isinstance(fps, bool) or not isinstance(fps, (int, float)):
                report.err(
                    where,
                    f"`fps:` must be a number, got {render_scalar(fps)}")
            elif not is_representable_number(fps):
                # Two distinct ways to get here, both of which a bare
                # positivity test misses:
                #
                #  * PyYAML resolves `.nan` and `.inf` to real floats.
                #    `nan <= 0` is False because every NaN comparison
                #    is, and `inf <= 0` is False because infinity really
                #    is greater.
                #  * a Python int has unbounded precision, so a
                #    thousand-digit `fps:` is a perfectly valid YAML
                #    integer that no float can hold — and asking
                #    `math.isfinite` about it raises rather than
                #    answering.
                #
                # Either way it is not a frame rate any clock can
                # advance against, so check this BEFORE positivity.
                report.err(
                    where,
                    f"`fps:` must be a finite, representable number, got "
                    f"{render_scalar(fps)}")
            elif fps <= 0:
                report.err(
                    where, f"`fps:` must be positive, got {render_scalar(fps)}")
            elif not fits_runtime_float(fps):
                report.err(
                    where,
                    f"`fps:` does not survive the engine's 32-bit Float, got "
                    f"{render_scalar(fps)} (it would load as infinity or "
                    f"zero)")
            else:
                fps_value = narrow_to_runtime_float(fps)
        loop_value = DEFAULT_LOOP
        if "loop" in raw_anim:
            if not isinstance(raw_anim["loop"], bool):
                report.err(
                    where,
                    f"`loop:` must be a boolean, got "
                    f"{render_scalar(raw_anim['loop'])}")
            else:
                loop_value = raw_anim["loop"]

        raw_frames = raw_anim.get("frames")
        if not isinstance(raw_frames, dict) or not raw_frames:
            report.err(where, "no `frames:` block")
            continue

        frames: Dict[str, List[str]] = {}
        seen_spelling: Dict[str, str] = {}
        for raw_dir, paths in raw_frames.items():
            if not isinstance(raw_dir, str):
                report.err(
                    where,
                    f"direction key must be a string, got "
                    f"{type(raw_dir).__name__} {raw_dir!r}")
                continue
            norm = normalise_dir(raw_dir)
            if norm is None:
                report.err(where, f"unknown direction key '{raw_dir}'")
                continue
            if norm in seen_spelling:
                report.err(
                    where,
                    f"duplicate direction '{raw_dir}' (already had "
                    f"'{seen_spelling[norm]}')")
                continue
            seen_spelling[norm] = raw_dir
            if not isinstance(paths, list):
                report.err(where, f"direction '{raw_dir}' is not a list")
                continue
            if not paths:
                report.err(where, f"direction '{raw_dir}' has zero frames")
                continue
            frames[norm] = [str(p) for p in paths]

        out.append(AnimDecl(unit_name, anim_name, flip, frames, where,
                            fps_value, loop_value))
    return out


def collect_aux_paths(unit: dict, unit_name: str) -> List[Tuple[str, str]]:
    """Non-animation texture references. Checked for existence, never
    counted as an animation-frame claim (#1257: reuse is legal)."""
    out: List[Tuple[str, str]] = []
    if unit.get("sprite"):
        out.append((f"{unit_name}/sprite", str(unit["sprite"])))
    dir_sprites = unit.get("directional_sprites") or {}
    if isinstance(dir_sprites, dict):
        for raw_dir, p in dir_sprites.items():
            out.append(
                (f"{unit_name}/directional_sprites/{raw_dir}", str(p)))
    if unit.get("portrait"):
        out.append((f"{unit_name}/portrait", str(unit["portrait"])))
    return out


def parse_unit_entry(
    report: Report,
    source: str,
    entry: object,
    asset_only: bool,
) -> Optional[UnitDecl]:
    key = "asset_units" if asset_only else "units"
    if not isinstance(entry, dict):
        report.err(source, f"non-mapping entry in `{key}:`")
        return None

    raw_name = entry.get("name")
    if not isinstance(raw_name, str) or not raw_name:
        report.err(source, f"entry in `{key}:` has no `name:`")
        return None
    unit_name = raw_name
    if not UNIT_IDENT_RE.match(unit_name):
        report.err(
            source,
            f"unsafe unit identifier '{unit_name}': a unit name must be one "
            f"lowercase [a-z0-9_]+ path component")
        return None

    if asset_only:
        # `sorted` on raw YAML keys is a crash waiting to happen: a
        # mapping may mix types, and `123` is not orderable against
        # `"typo"`. Sort by the rendered form instead, which is what the
        # diagnostic shows anyway.
        stray = sorted((k for k in entry if k not in ASSET_ONLY_KEYS),
                       key=repr)
        gameplay = [k for k in stray if k in GAMEPLAY_ONLY_KEYS]
        if gameplay:
            report.err(
                f"{source}:{unit_name}",
                f"asset-only declaration carries gameplay field(s) "
                f"{', '.join(str(k) for k in gameplay)}. An `asset_units:` "
                f"entry declares animation frames and nothing else; move it "
                f"to `units:` if it is meant to be a real unit.")
        unknown = [k for k in stray if k not in GAMEPLAY_ONLY_KEYS]
        if unknown:
            report.err(
                f"{source}:{unit_name}",
                f"asset-only declaration carries unknown field(s) "
                f"{', '.join(repr(k) for k in unknown)}. "
                f"The schema is exactly "
                f"{', '.join(sorted(ASSET_ONLY_KEYS))}.")
        if not entry.get("animations"):
            report.err(
                f"{source}:{unit_name}",
                "asset-only declaration has no `animations:` block — it "
                "would own no assets at all")
    else:
        if not entry.get("sprite"):
            report.err(unit_name, "missing required `sprite:` path")

    anims = parse_animations(report, unit_name, source, entry.get("animations"))
    aux = [] if asset_only else collect_aux_paths(entry, unit_name)
    return UnitDecl(unit_name, asset_only, source, anims, aux)


def load_declarations(
    report: Report, data_units: Path,
) -> List[UnitDecl]:
    if not data_units.is_dir():
        report.err(str(data_units), "unit data directory does not exist")
        return []

    decls: List[UnitDecl] = []
    seen: Dict[str, str] = {}
    for yaml_path in sorted(data_units.glob("*.yaml")):
        source = yaml_path.name
        try:
            data = yaml.safe_load(yaml_path.read_text(encoding="utf-8"))
        except (yaml.YAMLError, OSError, UnicodeDecodeError) as error:
            report.err(source, f"YAML parse error: {error}")
            continue
        if data is None:
            report.err(source, "file is empty")
            continue
        if not isinstance(data, dict):
            report.err(source, "top level is not a mapping")
            continue
        if "units" not in data and "asset_units" not in data:
            report.err(
                source,
                "declares neither `units:` nor `asset_units:` — a unit file "
                "must use one of those two top-level keys (a mistyped key "
                "looks exactly like this)")
            continue

        for asset_only, key in ((False, "units"), (True, "asset_units")):
            if key not in data:
                continue
            raw = data[key]
            # An explicit null is NOT the same as an absent key, and
            # `data.get(key) is None` cannot tell them apart. Aeson's
            # `.:?` reads a present null as Nothing, so `units: null`
            # alone makes the Haskell loader fail with "declares
            # neither" — a file that skipped this check would leave the
            # gate green while startup logged a parse failure, which is
            # exactly the divergence this tool exists to prevent.
            if raw is None:
                report.err(
                    source,
                    f"`{key}:` is present but null. Give it a list of "
                    f"entries, or remove the key.")
                continue
            if not isinstance(raw, list):
                report.err(source, f"`{key}:` is not a list")
                continue
            for entry in raw:
                decl = parse_unit_entry(report, source, entry, asset_only)
                if decl is None:
                    continue
                if decl.name in seen:
                    report.err(
                        source,
                        f"unit '{decl.name}' is already declared in "
                        f"{seen[decl.name]}")
                    continue
                seen[decl.name] = source
                decls.append(decl)
    return decls


# --------------------------------------------------------------------
# Path ownership
# --------------------------------------------------------------------

def check_no_symlink(root: Path, rel: PurePosixPath) -> Optional[str]:
    """Reject a symlink anywhere in the ownership path, root-downwards.

    A symlinked component is refused unconditionally rather than only
    when it escapes: the resolved-containment check below cannot tell a
    link that happens to stay inside from one that was retargeted after
    the fact, and the engine's own preview loader refuses links the same
    way.
    """
    current = root
    for part in rel.parts:
        current = current / part
        if current.is_symlink():
            return current.relative_to(root).as_posix()
    return None


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
# Compilation: geometry, digests, and the generated index (#1258)
# --------------------------------------------------------------------

@dataclass
class DirectionPlan:
    """One authored direction's row in an animation's atlas."""
    direction: str
    row: int
    declared: List[str]   # declared POSIX paths, in playback order
    frames: List[Frame]   # decoded, same order


@dataclass
class AnimPlan:
    """Everything needed to emit one animation's atlas and index entry."""
    unit: str
    name: str
    flip: bool
    fps: float
    loop: bool
    directions: List[DirectionPlan]
    cell_width: int
    cell_height: int
    columns: int
    atlas_rel: str        # POSIX path relative to the operating root

    @property
    def rows(self) -> int:
        return len(self.directions)

    @property
    def cell_padding(self) -> int:
        """The extrusion gutter this plan compiles with, per side."""
        return CELL_PADDING

    @property
    def slot_width(self) -> int:
        """One cell's PHYSICAL width, gutters included."""
        return self.cell_width + 2 * self.cell_padding

    @property
    def slot_height(self) -> int:
        return self.cell_height + 2 * self.cell_padding

    @property
    def atlas_width(self) -> int:
        return self.columns * self.slot_width

    @property
    def atlas_height(self) -> int:
        return self.rows * self.slot_height


def atlas_dir_rel(unit: str) -> PurePosixPath:
    return PurePosixPath(*ASSET_PREFIX, unit, ATLAS_DIR_NAME)


def atlas_file_rel(unit: str, anim: str) -> PurePosixPath:
    return atlas_dir_rel(unit) / f"{anim}.{STORAGE_FORMAT}"


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

@dataclass
class CompileOutcome:
    written: List[str] = field(default_factory=list)
    removed: List[str] = field(default_factory=list)
    unchanged: int = 0


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


def read_bytes_or_none(path: Path) -> Optional[bytes]:
    try:
        return path.read_bytes()
    except OSError:
        return None


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


# --------------------------------------------------------------------
# Index validation
# --------------------------------------------------------------------

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


# --------------------------------------------------------------------
# The unit-animation texture budget (#1262)
# --------------------------------------------------------------------

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


@dataclass
class BudgetTally:
    """What the corpus actually costs, as the budget measures it."""
    units: int = 0
    animations: int = 0
    images: int = 0
    frames: int = 0
    resident_bytes: int = 0


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
    """The per-unit image/slot budget, and the TEX-5 memory trigger.

    Two independent budgets, one policy document
    (`tools/unit_texture_budget.json`):

    **Images and bindless slots.** D-2 fixes atlas granularity at one
    image per ANIMATION, and the runtime turns each indexed atlas into
    exactly one texture handle and one bindless registration
    (`Unit.Atlas.Load`). So a unit's compiler-owned `atlas/` directory
    must hold exactly `max_per_animation * (animations the index
    declares)` images — a bound DERIVED from the authoritative index,
    never a frozen roster total and never a frame count, so it keeps
    holding as animations are added. Reintroducing per-frame
    registrations means putting one image per FRAME where one per
    animation belongs, which fails this immediately and by path.

    Non-animation textures are excluded by construction rather than by
    an exemption list: portraits, the direct `sprite`, its
    `directional_sprites` T-pose overrides and `unknown_unit/rotations`
    all live OUTSIDE `atlas/` and are named by no index, and D-8 leaves
    them on ordinary single-texture loading.

    **Resident bytes.** The decoded RGBA8 footprint every session pays —
    `scripts/startup_loader.lua` feeds every `data/units/*.yaml` to the
    loader at boot, so the whole tracked roster is resident regardless
    of what spawns. Measured, projected at the recorded growth factor,
    and compared against the owner-confirmed threshold: exceeding it is
    D-10's precondition for resuming deferred TEX-5. That one is a
    WARNING — a breach is a project decision to make, not a broken tree
    — so a plain run reports it and `--strict` (CI, `make ci`) fails on
    it rather than letting the trigger pass unnoticed.

    Note this is NOT D-12's guardrail, which caps tracked derived
    artifact bytes ON DISK at two times their source frames. That is
    repository size; this is resident memory.
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

        # Two animations naming ONE image would keep the file count
        # right while halving the registrations, so it is reported on
        # its own terms rather than through the count below.
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
                f"budget: expected {expected} resident animation image(s) "
                f"({len(anims)} animation(s) x {budget.max_per_animation}), "
                f"found {len(present)}. Unclaimed by any animation: {shown}. "
                f"One image per animation is D-2's contract and is what "
                f"keeps registrations off the frame count.")

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
                f"{mib(tally.resident_bytes)} measured x "
                f"{budget.growth_factor:g} projected = {mib(projected)} > "
                f"{mib(budget.threshold_bytes)} threshold. This is D-10's "
                f"precondition for resuming deferred TEX-5 (KTX2 atlas "
                f"loading) — resume it, or have the owner re-confirm a new "
                f"threshold in {BUDGET_REL.as_posix()}.")
    return tally


def mib(value: float) -> str:
    return f"{value / (1024 * 1024):.2f} MiB"


# --------------------------------------------------------------------
# Driver
# --------------------------------------------------------------------

@dataclass
class Totals:
    units: int = 0
    asset_only: int = 0
    animations: int = 0
    frames: int = 0


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


def validate(
    root: Path, only_unit: Optional[str], report: Report,
) -> Tuple[Totals, Optional[BudgetTally]]:
    """The full `--validate-only` gate: inventory, freshness, budget."""
    totals, decls = validate_sources(root, only_unit, report)
    # Index checks run even when the inventory already failed: a stale
    # artifact and a broken declaration are independent findings, and
    # reporting only the first would hide the other behind a fix. The
    # budget follows for the same reason: it reads the STORED index, so
    # it stays meaningful — and stays reported — while a freshness
    # failure is being fixed.
    validate_indices(report, root, decls, only_unit)
    budget = validate_budget(report, root, decls, only_unit)
    return totals, budget


def print_report(report: Report) -> None:
    def fmt(issue: Issue) -> str:
        return f"  [{issue.where}] {issue.msg}"

    if report.errors:
        print(f"ERRORS ({len(report.errors)}):")
        for e in report.errors:
            print(fmt(e))
    if report.warnings:
        print(f"WARNINGS ({len(report.warnings)}):")
        for w in report.warnings:
            print(fmt(w))


def cmd_validate(
    root: Path, target_unit: Optional[str], strict: bool,
) -> int:
    report = Report()
    totals, budget = validate(root, target_unit, report)
    print_report(report)

    if not report.errors and not report.warnings:
        print(
            f"OK — {totals.units} unit declaration(s) "
            f"({totals.asset_only} asset-only), {totals.animations} "
            f"animation(s), {totals.frames} frame(s); every animation PNG "
            f"on disk is owned exactly once.")
        # Printed on success, not only on breach: the budget's whole
        # value is that the numbers are visible while they are still
        # healthy, so a regression shows up as a moved number in a diff
        # rather than only as a threshold crossing years later.
        if budget is not None and budget.units:
            print(
                f"BUDGET — {budget.images} resident animation image(s) for "
                f"{budget.animations} animation(s) across {budget.units} "
                f"compiled unit(s) ({budget.frames} logical frames); "
                f"{mib(budget.resident_bytes)} decoded RGBA8 resident.")

    return 1 if report.has_failures(strict) else 0


def cmd_compile(
    root: Path, target_unit: Optional[str], strict: bool, dry_run: bool,
) -> int:
    """Compile atlases and indices — or, with `dry_run`, report the work.

    Compilation never runs against an inventory that does not validate:
    the declarations ARE the compiler's contract, so producing derived
    artifacts from a corpus that fails it would launder a broken
    declaration into a tracked artifact.
    """
    report = Report()
    _, decls = validate_sources(root, target_unit, report)
    if report.has_failures(strict):
        print_report(report)
        print("refusing to compile: the source inventory does not validate.")
        return 1

    written: List[str] = []
    removed: List[str] = []
    unchanged = 0
    for decl in sorted(decls, key=lambda d: d.name):
        outcome = compile_unit(report, root, decl, dry_run)
        if outcome is None:
            continue
        written.extend(outcome.written)
        removed.extend(outcome.removed)
        unchanged += outcome.unchanged

    print_report(report)
    verb = "would write" if dry_run else "wrote"
    scrub = "would remove" if dry_run else "removed"
    for path in written:
        print(f"  {verb}: {path}")
    for path in removed:
        print(f"  {scrub}: {path}")

    tally = (f"{verb} {len(written)} artifact(s), {scrub} {len(removed)}, "
             f"{unchanged} already current.")
    if report.has_failures(strict):
        return 1
    if dry_run and (written or removed):
        print(f"OUT OF DATE — {tally}")
        return 1
    print(f"OK — {tally}")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument(
        "--validate-only",
        action="store_true",
        help="Validate the inventory and any generated atlas index; write "
             "nothing.",
    )
    ap.add_argument(
        "--compile",
        action="store_true",
        help="Compile one lossless PNG atlas per animation plus the "
             "generated per-unit index.",
    )
    ap.add_argument(
        "--check",
        action="store_true",
        help="With --compile: report what would change and change nothing, "
             "exiting non-zero if any artifact is out of date.",
    )
    ap.add_argument(
        "--unit",
        help="Restrict the run to a single unit by name (e.g. 'acolyte').",
    )
    ap.add_argument(
        "--strict",
        action="store_true",
        help="Also treat warnings as errors. Inventory violations are errors "
             "either way.",
    )
    ap.add_argument(
        "--root",
        default=str(REPO_ROOT),
        help="Tree holding data/units/ and assets/textures/units/ "
             "(default: the repository root).",
    )
    args = ap.parse_args()

    # Writing derived artifacts is never a default. Requiring the mode
    # explicitly also keeps every existing `--validate-only` call site
    # meaning exactly what it did before the compiler existed.
    if args.validate_only == args.compile:
        sys.stderr.write(
            "error: pass exactly one of --validate-only or --compile\n")
        return 2
    if args.check and not args.compile:
        sys.stderr.write("error: --check applies to --compile\n")
        return 2

    root = Path(args.root).resolve()
    if not root.is_dir():
        sys.stderr.write(f"error: --root is not a directory: {root}\n")
        return 2
    if args.unit is not None and not UNIT_IDENT_RE.match(args.unit):
        sys.stderr.write(f"error: not a unit name: {args.unit}\n")
        return 2

    if args.compile:
        return cmd_compile(root, args.unit, args.strict, args.check)
    return cmd_validate(root, args.unit, args.strict)


if __name__ == "__main__":
    sys.exit(main())
