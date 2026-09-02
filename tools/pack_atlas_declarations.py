#!/usr/bin/env python3
"""YAML declaration parsing for the unit-atlas tool (issue #2054,
requirement 7).

The ONE owner of how `data/units/*.yaml` is read: recognition of the
two entry forms (`units:` gameplay entries and `asset_units:` asset-only
entries, the latter against an explicit whitelist), animation,
direction, frame, `fps`, `loop` and `flip` validation, auxiliary
non-animation path collection, the mixed-key and explicit-null
rejections, and the complete loader (`load_declarations`).

It is also where PyYAML is required. The import guard below keeps the
diagnostic and exit status the command has always had: the façade
imports this owner at load, so a missing PyYAML still exits 2 with the
install command before any argument is parsed.

Consumes the shared definitions owner and the image owner's
runtime-`Float` helpers. The public façade is tools/pack_atlas.py.
"""
from __future__ import annotations

import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple


try:
    import yaml  # PyYAML
except ImportError:
    sys.stderr.write(
        "error: PyYAML is required. Install with:\n"
        "    python3 -m pip install --user -r tools/requirements-assets.txt\n"
    )
    sys.exit(2)


from pack_atlas_image import fits_runtime_float, narrow_to_runtime_float
from pack_atlas_shared import (
    ANIM_IDENT_RE, DEFAULT_FPS, DEFAULT_LOOP, UNIT_IDENT_RE, AnimDecl,
    Report, UnitDecl, is_representable_number, normalise_dir, render_scalar,
)


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
