#!/usr/bin/env python3
"""Generate the "damaged" structure-pack variant art (#91).

Reads the intact dungeon_1 pieces and emits weathered copies under
assets/textures/buildings/dungeon_1/damaged/ — the texture side of the
structure tile variant system. The output is DETERMINISTIC (seeded per
file), so re-running the script reproduces the committed art bit-for-bit;
regenerate after changing the intact art or the damage parameters here.

Damage passes (pixel-art safe — hard pixels, palette-disciplined):
  * chip: bite brick-sized chunks off the sprite's top silhouette edge
    (walls/post), re-outlining the cut in the art's darkest color. Depth
    is bounded so a wall still reads as a wall. The floor's silhouette is
    NOT carved — its diamond must keep covering the tile exactly (grid
    alignment), and the intact facemaps are reused by the variant.
  * cracks: 1px meandering dark fissures across the faces.
  * weather: blotchy clusters demoting highlight/light pixels to the base
    color (erosion of the dressed face).
  * moss: sparse green-grey clusters, biased toward mortar lines and the
    lower half (where damp sits).
  * pits (floor only): a few flagstones sunk to the darkest color — reads
    as missing/broken stones without punching alpha holes.

Usage: python3 tools/make_damaged_structures.py
"""
from __future__ import annotations

import random
from pathlib import Path

from PIL import Image

SRC = Path("assets/textures/buildings/dungeon_1")
DST = SRC / "damaged"

# The intact art's 4-color palette.
OUTLINE = (31, 37, 38)     # darkest: outlines + mortar
BASE = (41, 53, 59)        # brick base
LIGHT = (70, 84, 91)       # dressed face
HILITE = (73, 89, 106)     # top-face highlight

# Two new colors for the damaged variant, same cool value range.
MOSS = (56, 74, 55)        # damp green-grey
MOSS_HI = (74, 94, 66)     # lit moss speck


def opaque(px, x, y, w, h):
    return 0 <= x < w and 0 <= y < h and px[x, y][3] > 0


def top_edge(px, w, h):
    """x -> topmost opaque y (columns with any opaque pixel)."""
    tops = {}
    for x in range(w):
        for y in range(h):
            if px[x, y][3] > 0:
                tops[x] = y
                break
    return tops


def chip_top(px, w, h, rng, max_depth):
    """Bite brick-sized chunks off the top silhouette edge."""
    tops = top_edge(px, w, h)
    if not tops:
        return
    xs = sorted(tops)
    x = xs[0]
    while x <= xs[-1]:
        run = rng.randint(3, 8)          # chunk width ~ a brick
        depth = rng.choice([0, 0, 1, 1, 2, 2, 3, max_depth])
        for cx in range(x, min(x + run, xs[-1] + 1)):
            if cx not in tops:
                continue
            ty = tops[cx]
            for d in range(depth):
                if opaque(px, cx, ty + d, w, h):
                    px[cx, ty + d] = (0, 0, 0, 0)
        x += run
    # re-outline the new top edge so the cut reads like the art's style
    for cx, ty in top_edge(px, w, h).items():
        px[cx, ty] = (*OUTLINE, 255)


def cracks(px, w, h, rng, n):
    """1px meandering dark fissures, drawn downward through the faces."""
    bbox_xs = [x for x in range(w) for y in range(h) if px[x, y][3] > 0]
    if not bbox_xs:
        return
    x0, x1 = min(bbox_xs), max(bbox_xs)
    for _ in range(n):
        x = rng.randint(x0, x1)
        ys = [y for y in range(h) if px[x, y][3] > 0]
        if len(ys) < 6:
            continue
        y = ys[rng.randint(0, max(0, len(ys) // 3))]
        length = rng.randint(4, 9)
        for _ in range(length):
            if opaque(px, x, y, w, h):
                px[x, y] = (*OUTLINE, 255)
            x += rng.choice([-1, 0, 0, 1])
            y += 1
            if y >= h:
                break


def weather(px, w, h, rng, n):
    """Blotches demoting highlight/light pixels to the base color."""
    spots = [(x, y) for x in range(w) for y in range(h)
             if px[x, y][:3] in (LIGHT, HILITE) and px[x, y][3] > 0]
    rng.shuffle(spots)
    for x, y in spots[:n]:
        for dx, dy in ((0, 0), (1, 0), (0, 1), (1, 1)):
            if opaque(px, x + dx, y + dy, w, h) \
                    and px[x + dx, y + dy][:3] in (LIGHT, HILITE):
                px[x + dx, y + dy] = (*BASE, 255)


def moss(px, w, h, rng, n):
    """Green-grey clusters, biased to mortar lines and the lower half."""
    spots = [(x, y) for x in range(w) for y in range(h)
             if px[x, y][3] > 0 and px[x, y][:3] == OUTLINE and y > h // 3]
    rng.shuffle(spots)
    for x, y in spots[:n]:
        px[x, y] = (*MOSS, 255)
        for dx, dy in ((1, 0), (0, 1), (-1, 0), (1, 1)):
            if opaque(px, x + dx, y + dy, w, h) and rng.random() < 0.5:
                c = MOSS_HI if rng.random() < 0.25 else MOSS
                px[x + dx, y + dy] = (*c, 255)


def pits(px, w, h, rng, n):
    """Floor only: sink whole small patches to the darkest color."""
    for _ in range(n):
        # find a light-face seed pixel to sink around
        for _ in range(200):
            x, y = rng.randint(0, w - 1), rng.randint(0, h - 1)
            if px[x, y][3] > 0 and px[x, y][:3] in (LIGHT, HILITE, BASE):
                break
        else:
            return
        rw, rh = rng.randint(3, 6), rng.randint(2, 4)
        for dx in range(rw):
            for dy in range(rh):
                if opaque(px, x + dx, y + dy, w, h):
                    px[x + dx, y + dy] = (*OUTLINE, 255)


def damage(name, im, rng):
    px = im.load()
    w, h = im.size
    if name.startswith("wall"):
        chip_top(px, w, h, rng, max_depth=5)
        cracks(px, w, h, rng, n=5)
        weather(px, w, h, rng, n=26)
        moss(px, w, h, rng, n=10)
    elif name == "post":
        chip_top(px, w, h, rng, max_depth=4)
        cracks(px, w, h, rng, n=2)
        weather(px, w, h, rng, n=8)
        moss(px, w, h, rng, n=4)
    elif name == "floor":
        pits(px, w, h, rng, n=5)
        cracks(px, w, h, rng, n=6)
        weather(px, w, h, rng, n=40)
        moss(px, w, h, rng, n=18)
    return im


def main():
    DST.mkdir(exist_ok=True)
    for f in ("floor", "post", "wall_ne", "wall_nw", "wall_se", "wall_sw"):
        src = SRC / f"{f}.png"
        im = Image.open(src).convert("RGBA")
        rng = random.Random(f"dungeon_1/{f}")   # deterministic per file
        out = damage(f, im, rng)
        out.save(DST / f"{f}.png")
        print(f"wrote {DST / f'{f}.png'}")


if __name__ == "__main__":
    main()
