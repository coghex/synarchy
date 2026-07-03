#!/usr/bin/env python3
"""Texture subset completeness audit (issue #478).

A texture "subset" is a group of entities that share one visual fallback
(units, buildings, flora, vegetation, world materials, icon families,
equipment/items). For each subset this reports:

  - whether its canonical unknown/placeholder texture exists on disk
  - which yaml-declared per-entity texture paths in that subset don't
    resolve to a real file (would hit the fallback at runtime, or crash
    the engine pre-#478 wiring)

Subsets that intentionally reuse an EXISTING generic asset (vegetation ->
utility/blanktexture.png, the established "unmapped id" convention; world
materials -> utility/notexture.png, the established "no material"
convention; items -> ui/placeholders/missing_equipment.png, already wired
in Engine.Scripting.Lua.API.Items.resolveSpritePath; equipment classes ->
ui/placeholders/humanoid_silhouette_blank.png, a pre-existing dimension-
matched blank silhouette wired in
Engine.Scripting.Lua.API.Equipment.loadEquipmentYamlFn) are reported
against that shared asset rather than a new per-subset one — no new art
needed for those. Items (data/items, `sprite:` icons) and equipment
classes (data/equipment, `silhouette:` body outlines) are two distinct
runtime loaders with two distinct fallbacks, so they're audited as
separate subsets even though they share a data domain.

Icon families (skill/stat/status/injury/infection/knowledge) reference
icons by bare basename resolved at runtime via a directory-listing index
(scripts/unit_info_v2.lua buildIconIndex), not literal yaml paths, so
per-entity missing-texture checking is out of scope here (same carve-out
tools/check_texture_paths.py already makes) — only unknown-asset presence
is checked per family.

Exit 0 when every subset has its unknown asset AND every yaml-declared
per-entity texture resolves. Exit 1 otherwise (offenders listed).
"""
import os
import re
import sys

import yaml

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PAT = re.compile(r"assets/textures/[A-Za-z0-9_./-]+\.png")


def ap(rel):
    return os.path.join(ROOT, rel)


def exists(rel):
    return os.path.isfile(ap(rel))


def read(rel):
    return open(ap(rel), encoding="utf-8", errors="ignore").read()


def regex_missing(yaml_dir):
    """Literal `assets/textures/...png` refs under yaml_dir that don't resolve."""
    missing = []
    d = ap(yaml_dir)
    if not os.path.isdir(d):
        return missing
    for fn in sorted(os.listdir(d)):
        if not fn.endswith((".yaml", ".yml")):
            continue
        rel = f"{yaml_dir}/{fn}"
        for m in sorted(set(PAT.findall(read(rel)))):
            if not exists(m):
                missing.append(f"{rel}: {m}")
    return missing


def flora_missing():
    """Flora splits texDir (base) + per-entry texture (relative filename),
    so literal-path regex can't catch it — reconstruct paths per the
    schema in Engine.Asset.YamlFlora."""
    missing = []
    d = ap("data/flora")
    if not os.path.isdir(d):
        return missing
    for fn in sorted(os.listdir(d)):
        if not fn.endswith((".yaml", ".yml")):
            continue
        rel = f"data/flora/{fn}"
        doc = yaml.safe_load(read(rel)) or {}
        for entry in doc.get("flora", []):
            tex_dir = entry.get("texDir", "")
            name = entry.get("name", "?")
            candidates = []
            phases = entry.get("phases") or []
            for p in phases:
                candidates.append((f"phase:{p.get('tag', '?')}", p.get("texture")))
            if not phases:
                candidates.append(("base", "matured.png"))
            for c in entry.get("annualCycle") or []:
                candidates.append((f"cycle:{c.get('tag', '?')}", c.get("texture")))
            for o in entry.get("cycleOverrides") or []:
                candidates.append(("override", o.get("texture")))
            harvest = entry.get("harvestable") or {}
            if harvest.get("harvested_texture"):
                candidates.append(("harvested", harvest.get("harvested_texture")))
            for label, tex in candidates:
                if not tex:
                    continue
                relpath = f"{tex_dir}/{tex}"
                if not exists(relpath):
                    missing.append(f"{rel}:{name}:{label}: {relpath}")
    return missing


SUBSETS = [
    {
        "name": "units",
        # Direction-aware fallback (#478): one static pose per compass
        # direction, since a unit's facing is known at load time. All 8
        # must be present — a partial set would leave some directions
        # crash-prone.
        "unknown": [
            f"assets/textures/units/unknown_unit/rotations/{d}.png"
            for d in ("north", "north-east", "east", "south-east",
                      "south", "south-west", "west", "north-west")
        ],
        "missing_entities": lambda: regex_missing("data/units"),
    },
    {
        "name": "buildings",
        "unknown": "assets/textures/buildings/unknown_building.png",
        "missing_entities": lambda: regex_missing("data/buildings"),
    },
    {
        "name": "flora",
        "unknown": "assets/textures/flora/unknown_flora.png",
        "missing_entities": flora_missing,
    },
    {
        "name": "vegetation",
        "unknown": "assets/textures/utility/blanktexture.png",
        "shared_fallback": "reuses the existing 'unmapped veg id' blank-texture convention",
        "missing_entities": lambda: regex_missing("data/vegetation"),
    },
    {
        "name": "world materials (tile/zoom/bg)",
        "unknown": "assets/textures/utility/notexture.png",
        "shared_fallback": "reuses the existing 'no material' notexture convention",
        "missing_entities": lambda: regex_missing("data/materials"),
    },
    {
        "name": "items",
        "unknown": "assets/textures/ui/placeholders/missing_equipment.png",
        "shared_fallback": "already wired: Engine.Scripting.Lua.API.Items.resolveSpritePath",
        "missing_entities": lambda: regex_missing("data/items"),
    },
    {
        "name": "equipment (silhouettes)",
        "unknown": "assets/textures/ui/placeholders/humanoid_silhouette_blank.png",
        "shared_fallback": "already wired: Engine.Scripting.Lua.API.Equipment.loadEquipmentYamlFn",
        "missing_entities": lambda: regex_missing("data/equipment"),
    },
]

ICON_KINDS = ["skill", "stat", "status", "injury", "infection", "knowledge"]
for _kind in ICON_KINDS:
    SUBSETS.append(
        {
            "name": f"icons/{_kind}",
            "unknown": f"assets/textures/icons/{_kind}/{_kind}_unknown.png",
            "missing_entities": lambda: [],
            "note": "bare-name, runtime-resolved (unit_info_v2.lua buildIconIndex) — "
                    "per-entity check out of scope, see module docstring",
        }
    )


def main():
    ok = True
    print(f"Texture subset audit — {len(SUBSETS)} subsets\n")
    for s in SUBSETS:
        unknown_paths = s["unknown"] if isinstance(s["unknown"], list) else [s["unknown"]]
        missing_unknown = [p for p in unknown_paths if not exists(p)]
        has_unknown = not missing_unknown
        missing = s["missing_entities"]()
        status = "OK" if has_unknown else "MISSING UNKNOWN ASSET"
        if not has_unknown:
            ok = False
        extra = s.get("shared_fallback") or s.get("note")
        suffix = f"  ({extra})" if extra else ""
        shown = unknown_paths[0] if len(unknown_paths) == 1 else f"{len(unknown_paths)} paths (see below)"
        print(f"[{status:22}] {s['name']:32} unknown={shown}{suffix}")
        if missing_unknown and len(unknown_paths) > 1:
            for p in unknown_paths:
                mark = "MISSING" if p in missing_unknown else "ok"
                print(f"      [{mark:7}] {p}")
        if missing:
            ok = False
            print(f"    {len(missing)} yaml-declared texture(s) missing on disk:")
            for m in missing[:10]:
                print(f"      - {m}")
            if len(missing) > 10:
                print(f"      ... and {len(missing) - 10} more")
    print()
    if ok:
        print("OK — every subset has its unknown-texture asset and all yaml-declared textures resolve")
        return 0
    print("INCOMPLETE — see MISSING items above")
    return 1


if __name__ == "__main__":
    sys.exit(main())
