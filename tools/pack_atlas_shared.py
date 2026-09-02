#!/usr/bin/env python3
"""Shared constants and data models of the unit-atlas tool (issue #2054,
requirement 4).

The LEAF of the split (requirement 16): it imports nothing from the
other `pack_atlas_*` owners, and every one of them imports from here.
What lives here is exactly what more than one owner reads —

  * the identifier and filename rules (`UNIT_IDENT_RE`, `ANIM_IDENT_RE`,
    `FRAME_RE`), the direction vocabulary and the atlas row order;
  * the atlas/index format constants (`INDEX_SCHEMA_VERSION`,
    `TOOL_VERSION`, `CELL_PADDING`, the compiler-owned `atlas/`
    directory and index names) and the digest domain tags;
  * the records that cross an owner boundary: `Frame`, `Issue`/`Report`,
    the declarations (`AnimDecl`/`UnitDecl`), the compile plans
    (`DirectionPlan`/`AnimPlan`), `CompileOutcome`, `BudgetTally` and
    `Totals`. A record only one owner ever holds — the parsed budget
    policy — stays with that owner;
  * the scalar rules the declaration parser, the index diagnostics and
    the budget all apply (`normalise_dir`, `is_representable_number`,
    `render_scalar`);
  * the path helpers the inventory, the compiler, index validation and
    the budget share: `check_no_symlink`, `atlas_dir_rel`,
    `atlas_file_rel` and `read_bytes_or_none`. They are here rather than
    with the compiler precisely so the budget can read a stored index
    without depending on the compiler (requirement 16), and without a
    second copy (requirement 17).

Nothing here decodes an image, reads YAML, walks the asset tree, writes
an artifact or parses a command line. The public façade is
tools/pack_atlas.py.
"""
from __future__ import annotations

import math
import re
import struct
import zlib
from dataclasses import dataclass, field
from pathlib import Path, PurePosixPath
from typing import Dict, List, Optional, Tuple


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


# --------------------------------------------------------------------
# Records that cross an owner boundary
# --------------------------------------------------------------------


@dataclass
class Frame:
    """One decoded source frame: canonical RGBA8 samples plus size."""
    width: int
    height: int
    pixels: bytes


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


@dataclass
class CompileOutcome:
    written: List[str] = field(default_factory=list)
    removed: List[str] = field(default_factory=list)
    unchanged: int = 0


@dataclass
class BudgetTally:
    """What the corpus actually costs, as the budget measures it."""
    units: int = 0
    animations: int = 0
    images: int = 0
    frames: int = 0
    resident_bytes: int = 0


@dataclass
class Totals:
    units: int = 0
    asset_only: int = 0
    animations: int = 0
    frames: int = 0


# --------------------------------------------------------------------
# Scalar rules
# --------------------------------------------------------------------


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
# Path helpers shared by the inventory, the compiler, index validation
# and the budget
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


def atlas_dir_rel(unit: str) -> PurePosixPath:
    return PurePosixPath(*ASSET_PREFIX, unit, ATLAS_DIR_NAME)


def atlas_file_rel(unit: str, anim: str) -> PurePosixPath:
    return atlas_dir_rel(unit) / f"{anim}.{STORAGE_FORMAT}"


def read_bytes_or_none(path: Path) -> Optional[bytes]:
    try:
        return path.read_bytes()
    except OSError:
        return None
