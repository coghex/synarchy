#!/usr/bin/env python3
"""Asset validation for the location map icons (#781, reshaped by #1230).

Standalone, no engine needed — just Pillow against the PNGs on disk.

#1230 replaced the per-definition (undiscovered, discovered) PAIR with
two structurally different assets, and this check follows that split:

  - the ONE shared unknown marker,
    assets/textures/icons/location/location_unknown.png, which belongs
    to no location type and which every annotated location draws while
    its type is unknown; and
  - one TYPE icon per definition — today only
    assets/textures/icons/location/ruin.png — which a location draws
    once a unit has seen it, and which the renderer darkens through the
    icon quad's own colour for the cleared/depleted states rather than
    loading a third bitmap.

What is checked:

  - both assets exist and decode successfully
  - the shared unknown marker is exactly 32x32 RGBA (#1230 requirement 1
    states the canonical size; the renderer scales to
    locationIconTargetPixels, so a mismatch is a silent resample)
  - both share identical dimensions, so swapping one for the other at
    the same annotation position never changes the drawn footprint
  - both carry real transparency (a transparent background around an
    opaque silhouette, not a fully-opaque rectangle)
  - neither is completely empty (fully transparent)
  - the two are not byte-identical — the whole point of the unknown
    marker is that it does NOT look like the ruin it hides
  - both silhouettes' visible (non-transparent) content lies inside the
    canvas (never a silhouette clipped by the edge), and both rest on
    the SAME baseline row — the actual "swapping textures at the same
    annotation position doesn't visually jump" property. A shared
    baseline is what the renderer needs: both quads are drawn centred on
    the identical anchor, so content that sits at different heights
    within the canvas appears to hop when the marker resolves into a
    type icon. Their HEIGHTS are deliberately free to differ, since the
    two silhouettes are different shapes.
  - the retired ruin_hidden.png is GONE: #1230 removed the
    per-definition unknown icon, and a stale file left behind would
    still be shipped while nothing loads it

Usage: python3 tools/location_map_icon_asset_check.py
"""
from __future__ import annotations

import os
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ICON_DIR = os.path.join(ROOT, "assets/textures/icons/location")
UNKNOWN = os.path.join(ICON_DIR, "location_unknown.png")
RUIN = os.path.join(ICON_DIR, "ruin.png")
RETIRED = os.path.join(ICON_DIR, "ruin_hidden.png")

# The canonical size #1230 requirement 1 states for the shared marker.
CANONICAL_SIZE = (32, 32)

# How far apart the two icons' content baselines (bottom-most visible
# row) may sit before a texture swap reads as a vertical hop. Zero would
# be ideal but is needlessly brittle against a one-pixel antialiased
# edge; one row is below the threshold of a visible jump at the icon's
# 32-logical-pixel draw size.
BASELINE_TOLERANCE_PX = 1

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

    print("== location map icon assets (#781/#1230) ==")

    unknown_exists = check("location_unknown.png exists", os.path.isfile(UNKNOWN))
    ruin_exists = check("ruin.png exists", os.path.isfile(RUIN))
    # The per-definition unknown icon #1230 retired. Its absence is a
    # real assertion, not tidiness: nothing loads it any more, so a copy
    # left on disk would ship dead weight and invite a definition to
    # start declaring it again.
    check("the retired per-definition ruin_hidden.png is gone",
          not os.path.exists(RETIRED))
    if not (unknown_exists and ruin_exists):
        print(f"location_map_icon_asset_check: {failures} check(s) FAILED")
        return 1

    images = {}
    for label, path in (("location_unknown.png", UNKNOWN), ("ruin.png", RUIN)):
        try:
            im = Image.open(path).convert("RGBA")
            im.load()
        except Exception as e:
            check(f"{label} decodes", False, str(e))
            print(f"location_map_icon_asset_check: {failures} check(s) FAILED")
            return 1
        check(f"{label} decodes", True)
        images[label] = im

    unknown = images["location_unknown.png"]
    ruin = images["ruin.png"]

    check(f"location_unknown.png is exactly {CANONICAL_SIZE[0]}x"
          f"{CANONICAL_SIZE[1]} (#1230 requirement 1)",
          unknown.size == CANONICAL_SIZE, f"size={unknown.size}")

    check("dimensions match", unknown.size == ruin.size,
          f"unknown={unknown.size} ruin={ruin.size}")

    for name, im in images.items():
        alpha_hist = im.getchannel("A").histogram()
        transparent_px = alpha_hist[0]
        opaque_ish_px = sum(alpha_hist[1:])
        check(f"{name} has real transparency (background transparent, "
              f"silhouette opaque)",
              transparent_px > 0 and opaque_ish_px > 0,
              f"transparent={transparent_px} opaque={opaque_ish_px}")
        check(f"{name} is not completely empty", opaque_ish_px > 0)

    with open(UNKNOWN, "rb") as f:
        unknown_bytes = f.read()
    with open(RUIN, "rb") as f:
        ruin_bytes = f.read()
    # The marker exists to NOT reveal the type. Two identical files would
    # satisfy every other check here while leaking exactly what #1230
    # hides.
    check("the unknown marker and the ruin type icon are not byte-identical",
          unknown_bytes != ruin_bytes)

    baselines = {}
    for name, im in images.items():
        bbox = im.getbbox()  # (left, upper, right, lower) of non-zero-alpha content
        if bbox is None:
            check(f"{name} has visible content inside the canvas",
                  False, "image has no visible content")
            continue
        bl, bt, br, bb = bbox[0], bbox[1], bbox[2] - 1, bbox[3] - 1
        w, h = im.size
        inside = bl >= 0 and bt >= 0 and br <= w - 1 and bb <= h - 1
        check(f"{name} visible content lies inside the canvas", inside,
              f"content bbox=({bl},{bt},{br},{bb}) canvas={w}x{h}")
        baselines[name] = bb

    if len(baselines) == 2:
        (n1, b1), (n2, b2) = baselines.items()
        check("both icons rest on the same baseline row, so a texture swap "
              "at one annotation position never hops vertically",
              abs(b1 - b2) <= BASELINE_TOLERANCE_PX,
              f"{n1} baseline={b1}, {n2} baseline={b2} "
              f"(tolerance {BASELINE_TOLERANCE_PX}px)")

    if failures:
        print(f"location_map_icon_asset_check: {failures} check(s) FAILED")
        return 1
    print("location_map_icon_asset_check: all checks passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
