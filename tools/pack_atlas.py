#!/usr/bin/env python3
"""
pack_atlas.py — unit / animation asset tool.

Per the texture-infrastructure plan in `docs/texture_infrastructure.md`,
this script will eventually pack per-direction PNG frames into atlas
sheets, optionally compressing to KTX2. For now only the
`--validate-only` mode is implemented; it walks the unit YAMLs and
checks that every referenced PNG actually exists on disk, that no
orphan PNGs are sitting around, and that `flip` declarations are
consistent with which directions are listed.

USAGE
-----

    python3 tools/pack_atlas.py --validate-only
        Validate every unit YAML in data/units/. Exit 0 on success,
        non-zero with a report on any issue.

    python3 tools/pack_atlas.py --validate-only --unit acolyte
        Validate a single unit.

    python3 tools/pack_atlas.py --validate-only --strict
        Treat warnings (orphan files, flip inconsistencies) as errors.

REQUIREMENTS
------------

PyYAML — install with:

    python3 -m pip install --user pyyaml

"""
from __future__ import annotations

import argparse
import os
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional

try:
    import yaml  # PyYAML
except ImportError:
    sys.stderr.write(
        "error: PyYAML is required. Install with:\n"
        "    python3 -m pip install --user pyyaml\n"
    )
    sys.exit(2)


REPO_ROOT = Path(__file__).resolve().parent.parent
DATA_UNITS = REPO_ROOT / "data" / "units"

# Direction-key aliases the engine accepts. Keep in sync with
# Engine.Scripting.Lua.API.Units.parseDirKey on the Haskell side. We
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

# Directions that are produced by horizontal mirror when flip: true.
# These must NOT be authored when flip is set — the mirror gives them.
MIRRORED_DIRS = {"south-west", "west", "north-west"}

# Directions that are the canonical source (kept) when flip: true.
CANONICAL_DIRS = {"south", "south-east", "east", "north-east", "north"}


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


def check_path(report: Report, where: str, p: str) -> None:
    """Resolve a project-relative path and verify the file exists."""
    abs_p = REPO_ROOT / p
    if not abs_p.is_file():
        report.err(where, f"missing file: {p}")


def validate_animation(
    report: Report,
    unit_name: str,
    anim_name: str,
    anim: dict,
) -> None:
    where_anim = f"{unit_name}/{anim_name}"

    flip = bool(anim.get("flip", False))
    frames = anim.get("frames")
    if not isinstance(frames, dict) or not frames:
        report.err(where_anim, "no `frames:` block")
        return

    # Normalise direction keys and detect duplicates / unknowns.
    seen_dirs: Dict[str, str] = {}  # normalised → original key
    for raw_dir_key, paths in frames.items():
        norm = normalise_dir(str(raw_dir_key))
        if norm is None:
            report.err(where_anim, f"unknown direction key '{raw_dir_key}'")
            continue
        if norm in seen_dirs:
            report.err(
                where_anim,
                f"duplicate direction '{raw_dir_key}' (already had "
                f"'{seen_dirs[norm]}')"
            )
            continue
        seen_dirs[norm] = str(raw_dir_key)

        if not isinstance(paths, list):
            report.err(where_anim, f"direction '{raw_dir_key}' is not a list")
            continue
        if len(paths) == 0:
            report.warn(where_anim, f"direction '{raw_dir_key}' has zero frames")
            continue

        for i, p in enumerate(paths):
            check_path(report, f"{where_anim}/{norm}/#{i}", str(p))

    # flip / direction-set consistency
    declared = set(seen_dirs.keys())
    if flip:
        # Should be exactly the 5 canonical dirs. Mirrored dirs must NOT
        # be listed (they'll be produced at runtime).
        present_mirrored = declared & MIRRORED_DIRS
        if present_mirrored:
            for d in sorted(present_mirrored):
                report.warn(
                    where_anim,
                    f"flip: true but '{d}' is listed — these assets are "
                    f"unused (the renderer mirrors them from the eastern "
                    f"counterpart). Either delete them or set flip: false.",
                )
        missing_canon = CANONICAL_DIRS - declared
        if missing_canon:
            report.warn(
                where_anim,
                f"flip: true but missing canonical direction(s): "
                f"{', '.join(sorted(missing_canon))}. The renderer will "
                f"fall back to T-pose for those facings.",
            )
    else:
        # flip: false → expect all 8 dirs (or at least, be explicit about
        # missing ones — those will silently fall back to T-pose).
        missing_all = ALL_DIRS - declared
        if missing_all:
            report.warn(
                where_anim,
                f"flip: false but missing direction(s): "
                f"{', '.join(sorted(missing_all))}. These facings will "
                f"render as the T-pose. Either supply them or set flip: true.",
            )


def validate_unit(
    report: Report,
    yaml_path: Path,
    unit: dict,
) -> None:
    unit_name = unit.get("name", "<no name>")

    # Default sprite (required)
    sprite = unit.get("sprite")
    if not sprite:
        report.err(unit_name, "missing required `sprite:` path")
    else:
        check_path(report, f"{unit_name}/sprite", str(sprite))

    # Directional T-pose sprites (optional)
    dir_sprites = unit.get("directional_sprites") or {}
    if isinstance(dir_sprites, dict):
        for raw_dir, p in dir_sprites.items():
            norm = normalise_dir(str(raw_dir))
            if norm is None:
                report.err(
                    unit_name,
                    f"unknown direction key '{raw_dir}' in directional_sprites",
                )
                continue
            check_path(
                report, f"{unit_name}/directional_sprites/{norm}", str(p),
            )

    # Portrait (optional)
    portrait = unit.get("portrait")
    if portrait:
        check_path(report, f"{unit_name}/portrait", str(portrait))

    # Animations
    animations = unit.get("animations") or {}
    if isinstance(animations, dict):
        for anim_name, anim in animations.items():
            if not isinstance(anim, dict):
                report.err(
                    f"{unit_name}/animations/{anim_name}",
                    "animation entry is not a mapping",
                )
                continue
            validate_animation(report, unit_name, str(anim_name), anim)


def find_orphan_pngs(
    report: Report,
    unit_name: str,
    referenced_paths: set,
) -> None:
    """
    Walk assets/textures/units/<unit_name>/animations/ and report any
    PNG files not referenced by the YAML. Catches forgotten files left
    over from refactors.
    """
    anim_root = REPO_ROOT / "assets" / "textures" / "units" / unit_name / "animations"
    if not anim_root.is_dir():
        return

    for p in anim_root.rglob("*.png"):
        rel = p.relative_to(REPO_ROOT).as_posix()
        if rel not in referenced_paths:
            report.warn(unit_name, f"orphan PNG (not referenced by YAML): {rel}")


def collect_referenced_paths(unit: dict) -> set:
    """Gather every PNG path the YAML references for this unit."""
    paths: set = set()
    if unit.get("sprite"):
        paths.add(str(unit["sprite"]))
    for p in (unit.get("directional_sprites") or {}).values():
        paths.add(str(p))
    if unit.get("portrait"):
        paths.add(str(unit["portrait"]))
    for anim in (unit.get("animations") or {}).values():
        if not isinstance(anim, dict):
            continue
        for frame_list in (anim.get("frames") or {}).values():
            if isinstance(frame_list, list):
                for p in frame_list:
                    paths.add(str(p))
    return paths


def validate_yaml_file(report: Report, yaml_path: Path) -> List[str]:
    """Validate one unit YAML. Returns names of units processed."""
    try:
        data = yaml.safe_load(yaml_path.read_text())
    except yaml.YAMLError as e:
        report.err(yaml_path.name, f"YAML parse error: {e}")
        return []

    units = (data or {}).get("units") or []
    if not isinstance(units, list):
        report.err(yaml_path.name, "`units:` is not a list")
        return []

    processed = []
    for unit in units:
        if not isinstance(unit, dict):
            report.err(yaml_path.name, "non-mapping entry in `units:`")
            continue
        validate_unit(report, yaml_path, unit)
        name = unit.get("name")
        if name:
            referenced = collect_referenced_paths(unit)
            find_orphan_pngs(report, str(name), referenced)
            processed.append(str(name))

    return processed


def cmd_validate(target_unit: Optional[str], strict: bool) -> int:
    report = Report()

    yaml_files: List[Path] = []
    if target_unit:
        target = DATA_UNITS / f"{target_unit}.yaml"
        if not target.is_file():
            sys.stderr.write(f"error: unit YAML not found: {target}\n")
            return 2
        yaml_files = [target]
    else:
        yaml_files = sorted(DATA_UNITS.glob("*.yaml"))

    if not yaml_files:
        sys.stderr.write(f"error: no unit YAMLs found under {DATA_UNITS}\n")
        return 2

    all_units: List[str] = []
    for yp in yaml_files:
        all_units.extend(validate_yaml_file(report, yp))

    # Report
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
        print(f"OK — validated {len(all_units)} unit(s): {', '.join(all_units)}")

    if report.has_failures(strict):
        return 1
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument(
        "--validate-only",
        action="store_true",
        help="Only validate; do not pack atlases (packing not yet implemented).",
    )
    ap.add_argument(
        "--unit",
        help="Validate a single unit by name (e.g. 'acolyte').",
    )
    ap.add_argument(
        "--strict",
        action="store_true",
        help="Treat warnings as errors (exit non-zero on any warning).",
    )
    args = ap.parse_args()

    if not args.validate_only:
        sys.stderr.write(
            "error: packing not yet implemented; pass --validate-only\n"
        )
        return 2

    return cmd_validate(args.unit, args.strict)


if __name__ == "__main__":
    sys.exit(main())
