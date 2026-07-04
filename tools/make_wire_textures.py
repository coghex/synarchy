#!/usr/bin/env python3
"""Generate the power-grid wire connection sprites (#359).

The wire structure piece (Structure.Types.SWire) is an autotile: its
rendered variant is picked from the 4-neighbour wire adjacency (see
scripts/wire.lua `shapeFor`), so 16 sprites cover every N/E/S/W
combination — 1 isolated + 4 ends + 2 straights + 4 corners + 4 tees +
1 cross. Each is a thin insulated cable laid across the tile diamond,
drawn as straight segments from the tile centre out to the connected
edge vertex/vertices (so any combination reads as one continuous run
where tiles meet).

Canvas + diamond geometry matches the existing floor/wall pieces
(Structure.Render's postCornerOffset comment): 96x64 canvas, diamond
corners N=(48,12) E=(96,36) S=(48,60) W=(0,36), centre=(48,36). A
cardinal NEIGHBOUR connects through the diamond EDGE between two
corners, not a corner itself (scripts/structures.lua's WALL_ENDS /
quarterEdge: the NE edge borders the north(gy-1) neighbour, SE borders
east(gx+1), SW borders south(gy+1), NW borders west(gx-1)) — so each
spoke is drawn from the centre to that edge's MIDPOINT, giving the
usual isometric-pipe diagonal cross rather than an axis-aligned plus.

Output is DETERMINISTIC (no randomness) — re-running reproduces the
committed art bit-for-bit.

Usage: python3 tools/make_wire_textures.py
"""
from __future__ import annotations

from pathlib import Path

from PIL import Image, ImageDraw

DST = Path("assets/textures/structures/wire")

W, H = 96, 64
CENTER = (48, 36)
CORNER = {"n": (48, 12), "e": (96, 36), "s": (48, 60), "w": (0, 36)}


def midpoint(a, b):
    return ((a[0] + b[0]) / 2.0, (a[1] + b[1]) / 2.0)


# Edge midpoint a cardinal neighbour's spoke points toward.
EDGE_MID = {
    "n": midpoint(CORNER["n"], CORNER["e"]),   # NE edge -> north neighbour
    "e": midpoint(CORNER["e"], CORNER["s"]),   # SE edge -> east neighbour
    "s": midpoint(CORNER["s"], CORNER["w"]),   # SW edge -> south neighbour
    "w": midpoint(CORNER["w"], CORNER["n"]),   # NW edge -> west neighbour
}

OUTLINE = (35, 27, 20, 255)     # dark brown-black cable jacket
CABLE = (196, 108, 44, 255)     # copper-orange insulation
HILITE = (230, 150, 84, 255)    # top highlight stripe

OUTLINE_W = 8
CABLE_W = 5
HILITE_W = 2

# The 16 connection shapes: name -> set of connected cardinal directions.
SHAPES = {
    "isolated": set(),
    "end_n": {"n"}, "end_e": {"e"}, "end_s": {"s"}, "end_w": {"w"},
    "straight_ns": {"n", "s"}, "straight_ew": {"e", "w"},
    "corner_ne": {"n", "e"}, "corner_nw": {"n", "w"},
    "corner_se": {"s", "e"}, "corner_sw": {"s", "w"},
    "tee_n": {"e", "s", "w"}, "tee_e": {"n", "s", "w"},
    "tee_s": {"n", "e", "w"}, "tee_w": {"n", "e", "s"},
    "cross": {"n", "e", "s", "w"},
}


def draw_pass(draw, dirs, color, width):
    if not dirs:
        # Isolated node: a small pad at the centre so a lone wire tile
        # still reads as "something is here", not an empty sprite.
        r = width
        draw.ellipse([CENTER[0] - r, CENTER[1] - r,
                      CENTER[0] + r, CENTER[1] + r], fill=color)
        return
    for d in dirs:
        draw.line([CENTER, EDGE_MID[d]], fill=color, width=width)
    # Round the centre joint so multi-spoke shapes (tee/cross) don't show
    # a miter gap between segments.
    r = width // 2
    draw.ellipse([CENTER[0] - r, CENTER[1] - r,
                  CENTER[0] + r, CENTER[1] + r], fill=color)


def make_sprite(dirs):
    im = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    draw = ImageDraw.Draw(im)
    draw_pass(draw, dirs, OUTLINE, OUTLINE_W)
    draw_pass(draw, dirs, CABLE, CABLE_W)
    # Highlight stripe: same segments, thinner, nudged up 1px to read as a
    # lit top face on the cable (cheap fake-AO, matches the flat-shaded
    # style of the rest of the structure art).
    hi = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    hidraw = ImageDraw.Draw(hi)
    draw_pass(hidraw, dirs, HILITE, HILITE_W)
    im.alpha_composite(hi, (0, -1))
    return im


def main():
    DST.mkdir(parents=True, exist_ok=True)
    for name, dirs in SHAPES.items():
        out = make_sprite(dirs)
        path = DST / f"{name}.png"
        out.save(path)
        print(f"wrote {path}")


if __name__ == "__main__":
    main()
