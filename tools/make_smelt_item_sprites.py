#!/usr/bin/env python3
"""Generate the smelting-tier item sprites (#327).

Derives the ore-chunk / coal-chunk / bronze-bar item art from the two
existing hand-quality templates, keeping their silhouette, outline and
dither structure:

  * granite_chunk.png → {iron,copper,tin}_ore_chunk.png and
    {lignite,bituminous_coal,anthracite}_chunk.png — each opaque
    non-outline pixel's luminance (normalized over the lump) is mapped
    through a per-material color ramp, then seeded mineral speckles are
    sprinkled on the interior (rust flecks on iron, oxidised blue-green
    on copper, cassiterite glints on tin, sheen on the harder coals).
  * bar_steel.png → bar_bronze.png — same luminance→ramp remap, no
    speckles (the ingot reads through its facet shading alone).

Output is DETERMINISTIC (seeded per sprite), same policy as
make_damaged_structures.py: re-running reproduces the committed art
bit-for-bit. Regenerate after changing the templates or the palettes.

Usage: python3 tools/make_smelt_item_sprites.py
"""
from __future__ import annotations

import random
from pathlib import Path

from PIL import Image

DIR = Path("assets/textures/items/material")

# Luminance below this (0..255) counts as outline and is left untouched;
# the templates draw outlines in near-black.
OUTLINE_LUM = 24


def lum(px):
    r, g, b = px[0], px[1], px[2]
    return 0.299 * r + 0.587 * g + 0.114 * b


def ramp_color(stops, t):
    """Piecewise-linear ramp over [0,1]; stops = [(t, (r,g,b)), ...]."""
    t = max(0.0, min(1.0, t))
    for (t0, c0), (t1, c1) in zip(stops, stops[1:]):
        if t <= t1:
            f = 0.0 if t1 == t0 else (t - t0) / (t1 - t0)
            return tuple(round(a + (b - a) * f) for a, b in zip(c0, c1))
    return stops[-1][1]


def remap(template, stops):
    """Luminance→ramp remap of every opaque non-outline pixel."""
    im = template.copy()
    px = im.load()
    w, h = im.size
    lums = [lum(px[x, y]) for y in range(h) for x in range(w)
            if px[x, y][3] > 0 and lum(px[x, y]) >= OUTLINE_LUM]
    lo, hi = min(lums), max(lums)
    span = max(1.0, hi - lo)
    for y in range(h):
        for x in range(w):
            p = px[x, y]
            if p[3] == 0 or lum(p) < OUTLINE_LUM:
                continue
            r, g, b = ramp_color(stops, (lum(p) - lo) / span)
            px[x, y] = (r, g, b, p[3])
    return im


def interior(im):
    """Opaque non-outline pixels whose 4-neighbourhood is also opaque —
    speckle candidates that can't bleed into the outline or the edge."""
    px = im.load()
    w, h = im.size
    out = []
    for y in range(1, h - 1):
        for x in range(1, w - 1):
            if px[x, y][3] == 0 or lum(px[x, y]) < OUTLINE_LUM:
                continue
            if all(px[x + dx, y + dy][3] > 0
                   for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1))):
                out.append((x, y))
    return out


def speckle(im, colors, count, seed):
    """Seeded single-pixel mineral flecks on the lump's interior."""
    rng = random.Random(seed)
    px = im.load()
    spots = interior(im)
    rng.shuffle(spots)
    for i, (x, y) in enumerate(spots[:count]):
        c = colors[i % len(colors)]
        px[x, y] = (c[0], c[1], c[2], px[x, y][3])
    return im


# name → (ramp stops dark→light, speckle colors, speckle count, seed)
CHUNKS = {
    # Dark rust-purple rock, hematite-red flecks (world iron_ore tile).
    "iron_ore_chunk": (
        [(0.0, (30, 20, 22)), (0.45, (78, 48, 42)),
         (0.75, (118, 68, 52)), (1.0, (156, 96, 66))],
        [(186, 92, 48), (150, 60, 40)], 26, 8027),
    # Tan host rock, oxidised blue-green flecks (malachite/azurite).
    "copper_ore_chunk": (
        [(0.0, (48, 34, 24)), (0.45, (108, 78, 52)),
         (0.75, (146, 110, 74)), (1.0, (180, 142, 100))],
        [(64, 148, 138), (96, 182, 164)], 26, 8427),
    # Dark grey rock, bright cassiterite glints (world tin_ore tile).
    "tin_ore_chunk": (
        [(0.0, (24, 24, 28)), (0.45, (64, 64, 72)),
         (0.75, (98, 98, 108)), (1.0, (136, 136, 148))],
        [(212, 212, 222), (176, 176, 188)], 22, 8527),
    # Matte brown-black, woody texture, no sheen.
    "lignite_chunk": (
        [(0.0, (16, 12, 9)), (0.45, (42, 32, 23)),
         (0.75, (64, 50, 36)), (1.0, (88, 70, 50))],
        [(54, 40, 26)], 10, 7027),
    # Grey-black, a few dull sheen spots.
    "bituminous_coal_chunk": (
        [(0.0, (11, 11, 13)), (0.45, (33, 33, 37)),
         (0.75, (54, 54, 60)), (1.0, (78, 78, 86))],
        [(104, 104, 116)], 12, 7127),
    # Glossy black, bright conchoidal glints.
    "anthracite_chunk": (
        [(0.0, (7, 7, 9)), (0.45, (22, 22, 28)),
         (0.75, (40, 42, 52)), (1.0, (66, 70, 84))],
        [(138, 148, 168), (100, 108, 126)], 14, 7227),
}

BRONZE_BAR = [(0.0, (48, 28, 16)), (0.35, (104, 60, 30)),
              (0.65, (160, 102, 48)), (0.85, (204, 148, 82)),
              (1.0, (236, 198, 132))]


def main():
    chunk = Image.open(DIR / "granite_chunk.png").convert("RGBA")
    for name, (stops, colors, count, seed) in CHUNKS.items():
        out = speckle(remap(chunk, stops), colors, count, seed)
        out.save(DIR / f"{name}.png")
        print(f"wrote {DIR / f'{name}.png'}")

    bar = Image.open(DIR / "bar_steel.png").convert("RGBA")
    remap(bar, BRONZE_BAR).save(DIR / "bar_bronze.png")
    print(f"wrote {DIR / 'bar_bronze.png'}")


if __name__ == "__main__":
    main()
