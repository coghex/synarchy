#!/usr/bin/env python3
"""Audit the signed-off Dungeon pilot; optionally rebuild its shading masks.

Requires Pillow and PyYAML. Colour sprites are approved source art, never
regenerated here. Masks keep the existing face lighting where defined and
use the shader's top-light fallback for newly exposed rubble. Their alpha
matches each sprite, so static rendering cannot clip its approved silhouette.
"""
import argparse
import hashlib
import json
from pathlib import Path

from PIL import Image
import yaml

ROOT = Path(__file__).resolve().parents[1]
ARCHIVE = ROOT / "docs/art/dungeon_lifecycle"
BASE = "assets/textures/buildings/dungeon_1/"
VARIANTS = ("weathered", "broken", "ruined")


def rgba(path):
    with Image.open(ROOT / path) as image:
        return image.convert("RGBA")


def pixels(image):
    return (image.getpixel((x, y)) for y in range(image.height) for x in range(image.width))


def facemap(sprite, original):
    assert sprite.size == original.size
    result = Image.new("RGBA", sprite.size)
    result.putdata([
        ((face[:3] if face[3] and sum(face[:3]) else (0, 255, 0)) + (255,)
         if pixel[3] else (0, 0, 0, 0))
        for pixel, face in zip(pixels(sprite), pixels(original))
    ])
    return result


def audit(write_facemaps=False):
    manifest = json.loads((ARCHIVE / "manifest.json").read_text())
    pack = yaml.safe_load((ROOT / "data/structure_packs/dungeon_1.yaml").read_text())
    for family, entry in manifest.items():
        kind, phase = family.split("_")
        states = entry["states"]
        hashes = []
        for state in states:
            path = state["path"]
            image = rgba(path)
            assert image.size == ((96, 64) if kind == "floor" else (32, 32)), path
            assert set(image.getchannel("A").tobytes()) <= {0, 255}, path
            assert image.getbbox(), path
            digest = hashlib.sha256(image.tobytes()).hexdigest()
            assert digest == state["rgba_sha256"], path
            assert hashlib.sha256((ROOT / path).read_bytes()).hexdigest() == state["file_sha256"], path
            hashes.append(digest)
        assert len(set(hashes)) == len(states), family
        if phase == "construction":
            assert len(states) == (4 if kind == "floor" else 2), family
            assert pack["pieces"][kind]["construction"] == [s["path"] for s in states], family
            assert rgba(states[-1]["path"]).tobytes() == rgba(pack["pieces"][kind]["texture"]).tobytes(), family
        else:
            assert len(states) == 4, family
            assert states[0]["path"] == pack["pieces"][kind]["texture"], family
            original = rgba(pack["pieces"][kind]["facemap"])
            for variant, state in zip(VARIANTS, states[1:]):
                appearance = pack["variants"][variant]["pieces"][kind]
                assert appearance["texture"] == state["path"], (kind, variant)
                expected = facemap(rgba(state["path"]), original)
                path = ROOT / appearance["facemap"]
                if write_facemaps:
                    path.parent.mkdir(parents=True, exist_ok=True)
                    expected.save(path)
                actual = rgba(appearance["facemap"])
                assert actual.size == expected.size and actual.tobytes() == expected.tobytes(), path
                assert "construction" not in appearance and "destruction" not in appearance
    # These damage stages are persistent variants, not a transient teardown clip.
    assert all("destruction" not in p for p in pack["pieces"].values())
    broken = rgba(BASE + "broken/floor.png")
    ruined = rgba(BASE + "ruined/floor.png")
    mask = Image.open(ARCHIVE / "floor_damage_mask.png").convert("L")
    assert broken.size == ruined.size == mask.size
    changed = [m for a, b, m in zip(pixels(broken), pixels(ruined), pixels(mask)) if a != b]
    assert len(changed) == 481 and all(changed), "final floor repair escaped its approved mask"
    print("PASS: 14 approved states, exact construction handoffs, 6 unclipped static variants, frozen floor pattern")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write-facemaps", action="store_true")
    audit(parser.parse_args().write_facemaps)
