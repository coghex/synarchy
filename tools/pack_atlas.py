#!/usr/bin/env python3
"""
pack_atlas.py — authoritative unit-animation asset inventory validator.

Per the texture-infrastructure plan in `docs/texture_infrastructure.md`,
this script will eventually pack per-direction PNG frames into atlas
sheets, optionally compressing to KTX2. For now only the
`--validate-only` mode is implemented.

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

A file may hold either key or both. A file holding neither is an error
(that is what a mistyped top-level key looks like).

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
  * every declared frame exists and every physical frame is declared;
  * one physical frame is claimed by at most one animation-frame slot
    (reuse as a unit's `sprite`, `directional_sprites` entry or
    `portrait` is deliberately legal and is NOT a duplicate claim);
  * `flip: true` declares exactly the canonical five authored directions
    (south, south-east, east, north-east, north) and `flip: false`
    declares exactly all eight;
  * per direction, frame indices start at 0 with no gaps or duplicates
    (different directions of one animation may hold different counts);
  * no symlink appears anywhere in the walk (unit directory,
    `animations/` root, animation directory, direction directory, or
    frame), so nothing can be linked past the inventory.

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
        Also treat warnings as errors. Warnings are advisory only
        (non-PNG debris in the animation tree); every inventory
        violation above is an ERROR regardless of this flag.

    python3 tools/pack_atlas.py --validate-only --root <dir>
        Validate an alternative tree holding `data/units/` and
        `assets/textures/units/`. Used by tools/test_pack_atlas.py so
        its fixtures never touch the shipped assets.

WHAT IT DOES NOT VALIDATE
-------------------------

This tool never OPENS a frame. It establishes that each declared frame
exists and is a regular file, and asserts nothing about its contents:
not that it decodes, not its pixel dimensions, not its colour type, and
not that one animation's frames agree on a size.

That boundary is deliberate (#1257). Validating a real binary format
here is its own piece of work with its own cost, tracked as #1311, and
it will depend on a maintained decoding library rather than a
hand-rolled parser.

REQUIREMENTS
------------

PyYAML — install with:

    python3 -m pip install --user pyyaml

That is the only third-party dependency; deliberately no image package.

"""
from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass, field
from pathlib import Path, PurePosixPath
from typing import Dict, List, Optional, Set, Tuple

try:
    import yaml  # PyYAML
except ImportError:
    sys.stderr.write(
        "error: PyYAML is required. Install with:\n"
        "    python3 -m pip install --user pyyaml\n"
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
UNIT_IDENT_RE = re.compile(r"^[a-z0-9_]+$")

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
ANIM_IDENT_RE = re.compile(r"^[a-z0-9_]+$|^[a-z0-9_]+_RH_[a-z0-9_]+$")

# Exactly three digits: `frame_NNN.png` is the asset format, and the
# numbering rule below counts from `frame_000.png`. `\d+` would also
# admit `frame_1.png`, `frame_01.png` and `frame_0000.png`, which are
# three different spellings of one index and would let a directory hold
# apparent duplicates that the gap/duplicate check could not see.
FRAME_RE = re.compile(r"^frame_(\d{3})\.png$")

# Relative to the validation root.
ASSET_PREFIX: Tuple[str, ...] = ("assets", "textures", "units")

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


# --------------------------------------------------------------------
# Declarations
# --------------------------------------------------------------------

@dataclass
class AnimDecl:
    """One declared animation, already bound to its declaring unit."""
    unit: str
    name: str
    flip: bool
    frames: Dict[str, List[str]]  # normalised direction -> declared paths
    where: str


@dataclass
class UnitDecl:
    name: str
    asset_only: bool
    source: str            # YAML file name, for diagnostics
    anims: List[AnimDecl]
    aux_paths: List[Tuple[str, str]]   # (where, path) — sprite/portrait/…


# Gameplay-only keys. An asset-only entry carrying one of these is a
# mistake worth naming: it looks like a unit that was meant to be
# spawnable and landed in the wrong list.
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
        anim_name = str(raw_name)
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
            report.err(where, f"`flip:` must be a boolean, got {flip!r}")
            flip = False

        raw_frames = raw_anim.get("frames")
        if not isinstance(raw_frames, dict) or not raw_frames:
            report.err(where, "no `frames:` block")
            continue

        frames: Dict[str, List[str]] = {}
        seen_spelling: Dict[str, str] = {}
        for raw_dir, paths in raw_frames.items():
            norm = normalise_dir(str(raw_dir))
            if norm is None:
                report.err(where, f"unknown direction key '{raw_dir}'")
                continue
            if norm in seen_spelling:
                report.err(
                    where,
                    f"duplicate direction '{raw_dir}' (already had "
                    f"'{seen_spelling[norm]}')")
                continue
            seen_spelling[norm] = str(raw_dir)
            if not isinstance(paths, list):
                report.err(where, f"direction '{raw_dir}' is not a list")
                continue
            if not paths:
                report.err(where, f"direction '{raw_dir}' has zero frames")
                continue
            frames[norm] = [str(p) for p in paths]

        out.append(AnimDecl(unit_name, anim_name, flip, frames, where))
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
        stray = sorted(set(entry) & GAMEPLAY_ONLY_KEYS)
        if stray:
            report.err(
                f"{source}:{unit_name}",
                f"asset-only declaration carries gameplay field(s) "
                f"{', '.join(stray)}. An `asset_units:` entry declares "
                f"animation frames and nothing else; move it to `units:` "
                f"if it is meant to be a real unit.")
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
            raw = data.get(key)
            if raw is None:
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
# Driver
# --------------------------------------------------------------------

@dataclass
class Totals:
    units: int = 0
    asset_only: int = 0
    animations: int = 0
    frames: int = 0


def validate(
    root: Path, only_unit: Optional[str], report: Report,
) -> Totals:
    totals = Totals()
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
        return totals
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

    for orphan in sorted(physical - set(claimed)):
        rel = orphan.relative_to(root.resolve()).as_posix() \
            if orphan.is_relative_to(root.resolve()) else orphan.as_posix()
        report.err(
            rel.split("/")[3] if rel.startswith("assets/") else "?",
            f"unclassified frame on disk (no animation declaration owns "
            f"it): {rel}")

    return totals


def cmd_validate(
    root: Path, target_unit: Optional[str], strict: bool,
) -> int:
    report = Report()
    totals = validate(root, target_unit, report)

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

    if not report.errors and not report.warnings:
        print(
            f"OK — {totals.units} unit declaration(s) "
            f"({totals.asset_only} asset-only), {totals.animations} "
            f"animation(s), {totals.frames} frame(s); every animation PNG "
            f"on disk is owned exactly once.")

    return 1 if report.has_failures(strict) else 0


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument(
        "--validate-only",
        action="store_true",
        help="Only validate; do not pack atlases (packing not yet implemented).",
    )
    ap.add_argument(
        "--unit",
        help="Restrict validation to a single unit by name (e.g. 'acolyte').",
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

    if not args.validate_only:
        sys.stderr.write(
            "error: packing not yet implemented; pass --validate-only\n"
        )
        return 2

    root = Path(args.root).resolve()
    if not root.is_dir():
        sys.stderr.write(f"error: --root is not a directory: {root}\n")
        return 2
    if args.unit is not None and not UNIT_IDENT_RE.match(args.unit):
        sys.stderr.write(f"error: not a unit name: {args.unit}\n")
        return 2

    return cmd_validate(root, args.unit, args.strict)


if __name__ == "__main__":
    sys.exit(main())
