#!/usr/bin/env python3
"""Asset validation for the location map-icon pair (#781).

Standalone, no engine needed — just Pillow against the two PNGs on disk:

  - both assets/textures/icons/location/ruin_hidden.png and
    ruin_discovered.png exist and decode successfully
  - both share identical dimensions
  - both carry real transparency (a transparent background around an
    opaque silhouette, not a fully-opaque rectangle)
  - neither is completely empty (fully transparent)
  - the two are not byte-identical (a real undiscovered/discovered pair,
    not one file copy-pasted under two names)
  - both silhouettes' visible (non-transparent) content falls inside the
    same expected anchor envelope, so swapping textures at the same
    world position doesn't visually jump

Usage: python3 tools/location_map_icon_asset_check.py
"""
from __future__ import annotations

import os
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
HIDDEN = os.path.join(ROOT, "assets/textures/icons/location/ruin_hidden.png")
DISCOVERED = os.path.join(ROOT, "assets/textures/icons/location/ruin_discovered.png")

# The shared anchor envelope both icons' visible content must fit inside
# (canvas-relative pixel bounds, inclusive) — generous enough to allow
# each icon its own silhouette shape while keeping both grounded/centred
# consistently so swapping textures at the same annotation position
# never visibly jumps.
ENVELOPE = (0, 4, 31, 31)  # (left, top, right, bottom)

failures = 0


def check(name: str, ok: bool, detail: str = "") -> bool:
    global failures
    print(f"  [{'PASS' if ok else 'FAIL'}] {name}"
          + (f" — {detail}" if detail and not ok else ""))
    failures += not ok
    return ok


def main() -> int:
    try:
        from PIL import Image
    except ImportError:
        print("location_map_icon_asset_check: Pillow (PIL) is required")
        return 1

    print("== location map icon assets (#781) ==")

    hidden_exists = check("ruin_hidden.png exists", os.path.isfile(HIDDEN))
    discovered_exists = check("ruin_discovered.png exists", os.path.isfile(DISCOVERED))
    if not (hidden_exists and discovered_exists):
        print(f"location_map_icon_asset_check: {failures} check(s) FAILED")
        return 1

    try:
        hidden = Image.open(HIDDEN).convert("RGBA")
        hidden.load()
    except Exception as e:
        check("ruin_hidden.png decodes", False, str(e))
        print(f"location_map_icon_asset_check: {failures} check(s) FAILED")
        return 1
    check("ruin_hidden.png decodes", True)

    try:
        discovered = Image.open(DISCOVERED).convert("RGBA")
        discovered.load()
    except Exception as e:
        check("ruin_discovered.png decodes", False, str(e))
        print(f"location_map_icon_asset_check: {failures} check(s) FAILED")
        return 1
    check("ruin_discovered.png decodes", True)

    check("dimensions match", hidden.size == discovered.size,
          f"hidden={hidden.size} discovered={discovered.size}")

    for name, im in (("ruin_hidden.png", hidden), ("ruin_discovered.png", discovered)):
        alpha_hist = im.getchannel("A").histogram()
        transparent_px = alpha_hist[0]
        opaque_ish_px = sum(alpha_hist[1:])
        check(f"{name} has real transparency (background transparent, "
              f"silhouette opaque)",
              transparent_px > 0 and opaque_ish_px > 0,
              f"transparent={transparent_px} opaque={opaque_ish_px}")
        check(f"{name} is not completely empty", opaque_ish_px > 0)

    with open(HIDDEN, "rb") as f:
        hidden_bytes = f.read()
    with open(DISCOVERED, "rb") as f:
        discovered_bytes = f.read()
    check("the two assets are not byte-identical", hidden_bytes != discovered_bytes)

    left, top, right, bottom = ENVELOPE
    for name, im in (("ruin_hidden.png", hidden), ("ruin_discovered.png", discovered)):
        bbox = im.getbbox()  # (left, upper, right, lower) of non-zero-alpha content
        if bbox is None:
            check(f"{name} visible content fits the shared anchor envelope",
                  False, "image has no visible content")
            continue
        bl, bt, br, bb = bbox[0], bbox[1], bbox[2] - 1, bbox[3] - 1
        fits = bl >= left and bt >= top and br <= right and bb <= bottom
        check(f"{name} visible content fits the shared anchor envelope "
              f"{ENVELOPE}", fits, f"content bbox=({bl},{bt},{br},{bb})")

    if failures:
        print(f"location_map_icon_asset_check: {failures} check(s) FAILED")
        return 1
    print("location_map_icon_asset_check: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
