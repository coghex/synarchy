#!/usr/bin/env python3
"""Generate the power-grid wire art: flat colour sprites, dedicated
facemaps, and the construction/destruction sequences (#2516, from #359).

The wire structure piece (Structure.Types.SWire) is an autotile: its
rendered variant is picked from the 4-neighbour wire adjacency (see
scripts/wire.lua `shapeFor`), so 16 appearances cover every N/E/S/W
combination — 1 isolated + 4 ends + 2 straights + 4 corners + 4 tees +
1 cross. Every appearance has FOUR kinds of image, each a colour/facemap
pair:

  static        1 per appearance   structures/wire/<shape>.png
  construction  CONSTRUCTION_FRAMES  structures/wire/build/<shape>_<k>.png
  destruction   DESTRUCTION_FRAMES   structures/wire/break/<shape>_<k>.png

and the facemap of each lives at the same relative path under
assets/textures/facemap/wire/.

Flat colour, lighting from the facemap
--------------------------------------
The colour images carry MATERIAL colour only: one flat dark-orange
insulation colour, no outline. No highlight stripe, no ambient shading,
no directional shadow, no lighting tint (the old generator painted a
highlight and a jacket outline into the colour texture; both are gone and
must not come back). The power cable is THIN: one to two pixels wide.
The volume of the cable is carried by the facemap instead: the renderer
lights each pixel by its facemap channel (docs/structure_pack_schema.md),
red = right face, green = top face, blue = left face.

The cable is modelled as a ribbon laid on the tile, raised `CABLE_HEIGHT`
pixels. Its footprint is built in GROUND coordinates (U east, V north,
in tile units), which is what makes a ribbon look like a ribbon in the
2:1 isometric projection. A pixel is

  * TOP   if the footprint covers it at the raised height  -> green
  * SIDE  if it lies in the extrusion below the top face   -> red for a
          ribbon along the NE-SW axis (its visible side faces down-right),
          blue for one along the NW-SE axis (down-left).

Facemap alpha equals colour alpha exactly (binary), so the renderer's
`color.a * faceAlpha` rule never clips the sprite and every visible pixel
has nonzero facemap RGB: no pixel relies on the top-light fallback.

Canvas + diamond geometry matches the existing floor/wall pieces
(Structure.Render's postCornerOffset comment): 96x64 canvas, diamond
corners N=(48,12) E=(96,36) S=(48,60) W=(0,36), centre=(48,36). A
cardinal NEIGHBOUR connects through the diamond EDGE between two
corners, so each spoke runs from the centre to that edge's MIDPOINT and
meets the neighbour's spoke exactly at the tile seam.

Lifecycles
----------
Construction lays the cable outward from the centre toward each
connected edge in CONSTRUCTION_FRAMES steps; the unfinished run ends in
a stripped-metal tip. The last frame IS the static pair, pixel for
pixel. Destruction is authored separately and is not the construction
sequence run backwards: frame 0 is the static pair, then the cable is
cut and sparks, breaks into lifted fragments, scatters into debris, and
ends on a few soot specks whose positions depend on the shape, so the
sixteen endpoints are visibly distinct without being empty.

Output is DETERMINISTIC (pure arithmetic and a hash-seeded generator,
no PIL drawing primitives) — re-running reproduces the committed art
bit-for-bit.

Usage: python3 tools/make_wire_textures.py [--yaml] [--pins]
  (default) write every colour image and facemap
  --yaml    also (re)write data/structure_packs/wire.yaml, the pack
            declaration that names them. Off by default: the declaration's
            schema (per-connection facemaps, paired lifecycle lists) needs
            the loader that reads it, so a checkout whose loader predates it
            must not have its tracked pack replaced by a plain rerun.
  --pins    also (re)write the approved-content digest manifest
            (test-headless/fixtures/wire_art_pins.txt); run it only after
            the owner has approved the art.
"""
from __future__ import annotations

import argparse
import hashlib
from pathlib import Path

from PIL import Image

YAML = Path("data/structure_packs/wire.yaml")
DESTRUCTION_FPS = 10
COLOR_DIR = Path("assets/textures/structures/wire")
FACE_DIR = Path("assets/textures/facemap/wire")
PINS = Path("test-headless/fixtures/wire_art_pins.txt")

W, H = 96, 64
CX, CY = 48, 36

CONSTRUCTION_FRAMES = 4
DESTRUCTION_FRAMES = 5

# Flat material colours. No highlight, no shade: the facemap lights these.
CABLE = (166, 78, 22, 255)      # dark orange insulation
TIP = (168, 170, 178, 255)      # stripped metal at an unfinished end
SPARK = (255, 226, 130, 255)    # a spark off a cut
SOOT = (58, 30, 14, 255)        # residue

# Facemap channels (docs/structure_pack_schema.md): R right, G top, B left.
FACE_RIGHT = (255, 0, 0, 255)
FACE_TOP = (0, 255, 0, 255)
FACE_LEFT = (0, 0, 255, 255)

CABLE_HEIGHT = 1        # px the cable stands above the floor
HALF_WIDTH = 0.0056     # ground half-width, tile units: a top face one
                        # pixel tall (96 * 2 * HALF_WIDTH = 1.07 px)
PAD_RADIUS = 0.035      # isolated pad radius, tile units
SPOKE_LENGTH = 0.5      # centre -> edge midpoint, tile units

# The 16 connection shapes: name -> connected cardinal directions.
SHAPES = {
    "isolated": "",
    "end_n": "n", "end_e": "e", "end_s": "s", "end_w": "w",
    "straight_ns": "ns", "straight_ew": "ew",
    "corner_ne": "ne", "corner_nw": "nw",
    "corner_se": "se", "corner_sw": "sw",
    "tee_n": "esw", "tee_e": "nsw",
    "tee_s": "new", "tee_w": "nes",
    "cross": "nesw",
}

# The four RISERS (#2516 requirement 11): a cable that runs up one tile
# side's wall face to the level above and joins a flat run at its foot,
# named after the wall face it climbs (the same cardinal as the flat
# shapes: n = the NE edge / gy-1, e = SE / gx+1, s = SW / gy+1,
# w = NW / gx-1). Art and declaration only: nothing places them and the
# autotile never selects them.
RISERS = {"riser_n": "n", "riser_e": "e", "riser_s": "s", "riser_w": "w"}
ALL_SHAPES = {**SHAPES, **RISERS}

# The two risers whose wall faces the camera. At the default camera
# (FaceSouth) the screen-front edges are SE and SW
# (Structure.Render.isScreenFrontWall), which are the east (gx+1) and south
# (gy+1) sides, so riser_e and riser_s are the owner's "two risers running
# up the south walls". The camera sees their cable's UNDERSIDE, painted a
# slightly darker dark orange into the colour texture (not a tint); the
# facemap still marks those pixels as a side face.
UNDERSIDE_RISERS = ("riser_e", "riser_s")
CABLE_UNDER = (134, 62, 16, 255)

RISE_HEIGHT = 16        # px: one level, the same as a wall (tileSideHeight)
# Which column of the two-pixel rise is the SIDE face, and its facemap.
# The other column is the top face. Column offsets are from the edge
# midpoint's x.
RISE_SIDE = {
    "e": (0, FACE_RIGHT),    # SE wall: the outer face looks right
    "w": (0, FACE_RIGHT),    # NW wall: its inner face looks right
    "s": (-1, FACE_LEFT),    # SW wall: the outer face looks left
    "n": (-1, FACE_LEFT),    # NE wall: its inner face looks left
}

# Spoke geometry. `axis` is the ground axis the spoke runs along ("u" =
# east/west, "v" = north/south) and `sign` its direction; `side` is the
# facemap of the extrusion under it.
SPOKES = {
    "n": ("v", +1, FACE_RIGHT),   # NE-SW axis on screen
    "s": ("v", -1, FACE_RIGHT),
    "e": ("u", +1, FACE_LEFT),    # NW-SE axis on screen
    "w": ("u", -1, FACE_LEFT),
}


def ground(px: float, py: float) -> tuple[float, float]:
    """Screen point -> ground (U east, V north), tile units, from the
    tile centre. East = (+48,+24) per tile, north = (+48,-24)."""
    dx, dy = px - CX, py - CY
    return dx / 96.0 + dy / 48.0, dx / 96.0 - dy / 48.0


class Part:
    """One piece of cable: a ground footprint plus where it is drawn.

    kind     "spoke" | "disc" | "speck"
    lift     extra pixels the part floats above the floor
    off      (dx, dy) screen displacement
    open_end the spoke's far end sits on the tile seam
    """

    def __init__(self, kind, d=None, a=0.0, b=0.0, radius=0.0, lift=0,
                 off=(0, 0), open_end=False, tip=False, colour=None,
                 side=None, at=None):
        self.kind, self.d = kind, d
        self.a, self.b, self.radius = a, b, radius
        self.lift, self.off = lift, off
        self.open_end, self.tip = open_end, tip
        self.colour, self.side, self.at = colour, side, at

    # Does the footprint cover ground point (u, v)?
    def covers(self, u: float, v: float) -> bool:
        if self.kind == "disc":
            return u * u + v * v <= self.radius * self.radius
        if self.kind == "spoke":
            axis, sign, _ = SPOKES[self.d]
            along, across = (v, u) if axis == "v" else (u, v)
            along *= sign
            return (self.a - 1e-9 <= along <= self.b + 1e-9
                    and abs(across) <= HALF_WIDTH + 1e-9)
        return False

    def side_face(self, px: float) -> tuple[int, int, int, int]:
        if self.kind == "spoke":
            return SPOKES[self.d][2]
        return FACE_LEFT if px < CX + self.off[0] else FACE_RIGHT


def _top_hit(part: Part, x: int, y: int) -> bool:
    ox, oy = part.off
    u, v = ground(x + 0.5 - ox, y + 0.5 - oy + CABLE_HEIGHT + part.lift)
    return part.covers(u, v)


def _side_hit(part: Part, x: int, y: int) -> bool:
    ox, oy = part.off
    for t in range(CABLE_HEIGHT + 1):
        u, v = ground(x + 0.5 - ox, y + 0.5 - oy + part.lift + t)
        if part.covers(u, v):
            return True
    return False


def _rise_cells(part: Part):
    """((x, y), is_side) for a rise part: a two-pixel-wide vertical cable
    on the wall plane at the edge midpoint, from `a` to `b` of its
    height (0 = foot, 1 = the level above)."""
    ex, ey = _spoke_point(part.d, 1.0)
    side_dx, _ = RISE_SIDE[part.d]
    ox, oy = part.off
    y_hi = ey - round(part.b * RISE_HEIGHT)
    y_lo = ey - round(part.a * RISE_HEIGHT)
    for y in range(y_hi, y_lo + (1 if part.b >= 1.0 and part.a <= 0.0 else 0)):
        for dx in (-1, 0):
            yield (ex + dx + ox, y - part.lift + oy), dx == side_dx


def render(parts: list[Part], under: bool = False) -> tuple[Image.Image, Image.Image]:
    """Rasterise parts (painter order) into a colour/facemap pair. Every
    visible pixel is flat cable colour (or a tip, spark or soot override);
    there is no outline."""
    cells: dict[tuple[int, int], tuple[tuple, tuple | None]] = {}
    for part in parts:
        if part.kind == "speck":
            x, y = part.at
            if 0 <= x < W and 0 <= y < H:
                cells[(x, y)] = (FACE_TOP, part.colour)
            continue
        if part.kind == "rise":
            face_side = RISE_SIDE[part.d][1]
            for (x, y), is_side in _rise_cells(part):
                if 0 <= x < W and 0 <= y < H:
                    cells[(x, y)] = (face_side if is_side else FACE_TOP, None)
            if part.tip:
                tx, ty = min(_rise_cells(part), key=lambda c: c[0][1])[0]
                if 0 <= tx < W and 0 <= ty < H:
                    cells[(tx, ty)] = (FACE_TOP, TIP)
            continue
        for y in range(H):
            for x in range(W):
                if _top_hit(part, x, y):
                    attr = (FACE_TOP, None)
                elif _side_hit(part, x, y):
                    attr = (part.side_face(x + 0.5), None)
                else:
                    continue
                # Later parts overdraw earlier ones, except that a side
                # face of a part resting on the floor never covers
                # another floor part's top face (ribbons crossing at a
                # tee or cross share one plane). A lifted part is above
                # whatever it overlaps.
                prev = cells.get((x, y))
                if not (attr[0] != FACE_TOP and part.lift == 0
                        and prev is not None and prev[0] == FACE_TOP):
                    cells[(x, y)] = attr
                if part.tip and attr[0] == FACE_TOP \
                        and _along(part, x, y) >= part.b - 0.03:
                    cells[(x, y)] = (FACE_TOP, TIP)
    colour = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    face = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    cpx, fpx = colour.load(), face.load()
    for (x, y), (fmap, override) in cells.items():
        if override is not None:
            cpx[x, y] = override
        elif under and fmap != FACE_TOP:
            cpx[x, y] = CABLE_UNDER
        else:
            cpx[x, y] = CABLE
        fpx[x, y] = fmap
    return colour, face


def _along(part: Part, x: int, y: int) -> float:
    ox, oy = part.off
    u, v = ground(x + 0.5 - ox, y + 0.5 - oy + CABLE_HEIGHT + part.lift)
    axis, sign, _ = SPOKES[part.d]
    return (v if axis == "v" else u) * sign


# -- sequences ---------------------------------------------------------

def _hash_ints(*key) -> list[int]:
    h = hashlib.sha256(":".join(str(k) for k in key).encode()).digest()
    return list(h)


def static_parts(dirs: str, scale: float = 1.0, tip: bool = False) -> list[Part]:
    if not dirs:
        return [Part("disc", radius=PAD_RADIUS * scale)]
    parts = [Part("disc", radius=HALF_WIDTH)]
    for d in dirs:
        parts.append(Part("spoke", d=d, a=0.0, b=SPOKE_LENGTH * scale,
                          open_end=scale >= 1.0, tip=tip and scale < 1.0))
    return parts


def construction_parts(dirs: str, k: int) -> list[Part]:
    """Frame k of CONSTRUCTION_FRAMES; the last is the static art."""
    scale = (k + 1) / CONSTRUCTION_FRAMES
    if k == CONSTRUCTION_FRAMES - 1:
        return static_parts(dirs)
    if not dirs:
        # A lone node: the cable end is lowered onto the pad. The pad
        # grows and the stripped end comes down with it.
        lift = CONSTRUCTION_FRAMES - 1 - k
        return [Part("disc", radius=PAD_RADIUS * (0.55 + 0.45 * scale),
                     lift=lift),
                Part("speck", at=(CX + 3, CY - 2 - lift), colour=TIP)]
    return static_parts(dirs, scale=0.18 + 0.82 * scale, tip=True)


def destruction_parts(name: str, dirs: str, k: int) -> list[Part]:
    """Frame k of DESTRUCTION_FRAMES. Frame 0 is the static art."""
    if k == 0:
        return static_parts(dirs)
    rng = _hash_ints("wire-break", name, k)
    pick = iter(rng * 8)

    def jitter(n: int) -> int:
        return (next(pick) % (2 * n + 1)) - n

    parts: list[Part] = []
    if k == 1:
        # Cut: a gap opens mid-run, the far half lifts, sparks fly.
        parts.append(Part("disc", radius=(PAD_RADIUS if not dirs
                                          else HALF_WIDTH) * 0.85))
        if not dirs:
            for i in range(3):
                parts.append(Part("speck", at=(CX + jitter(5), CY - 3 + jitter(2)),
                                  colour=SPARK))
        for d in dirs:
            parts.append(Part("spoke", d=d, a=0.0, b=0.26 * SPOKE_LENGTH * 2))
            parts.append(Part("spoke", d=d, a=0.70 * SPOKE_LENGTH,
                              b=SPOKE_LENGTH, lift=1, off=(0, -1),
                              open_end=True))
            sx, sy = _spoke_point(d, 0.55)
            for i in range(3):
                parts.append(Part("speck", at=(sx + jitter(3), sy - 2 + jitter(2)),
                                  colour=SPARK))
        return parts
    if k == 2:
        # Fragments: the run is in lifted, displaced pieces.
        if not dirs:
            parts.append(Part("disc", radius=PAD_RADIUS * 0.6, lift=1))
            parts.append(Part("disc", radius=PAD_RADIUS * 0.3, lift=3,
                              off=(jitter(4) + 4, -2)))
        for d in dirs:
            for i, (a, b) in enumerate(((0.05, 0.17), (0.24, 0.36),
                                        (0.40, 0.49))):
                lift = 1 + 2 * i
                parts.append(Part("spoke", d=d, a=a, b=b, lift=lift,
                                  off=(jitter(2), -lift + jitter(1))))
        return parts
    if k == 3:
        # Debris: short stubs and dust flung outward and up.
        if not dirs:
            parts.append(Part("disc", radius=PAD_RADIUS * 0.28, lift=4,
                              off=(-5, -3)))
            parts.append(Part("speck", at=(CX + 7, CY - 6), colour=SOOT))
        for d in dirs:
            parts.append(Part("spoke", d=d, a=0.18, b=0.26, lift=5,
                              off=(jitter(3), -5)))
            sx, sy = _spoke_point(d, 0.7)
            parts.append(Part("speck", at=(sx + jitter(3), sy - 7 + jitter(2)),
                              colour=SOOT))
            parts.append(Part("speck", at=(sx + jitter(5), sy - 3 + jitter(3)),
                              colour=CABLE))
        return parts
    # Final frame: a few soot specks, placed by shape so the sixteen
    # endpoints differ. Never empty.
    if not dirs:
        for dx, dy in ((0, 1), (-2, 0), (3, 2)):
            parts.append(Part("speck", at=(CX + dx, CY + dy), colour=SOOT))
        return parts
    parts.append(Part("speck", at=(CX + jitter(2), CY + 1 + jitter(1)),
                      colour=SOOT))
    for d in dirs:
        sx, sy = _spoke_point(d, 0.5)
        parts.append(Part("speck", at=(sx + jitter(2), sy + jitter(1)),
                          colour=SOOT))
        parts.append(Part("speck", at=(sx + jitter(4), sy - 2 + jitter(1)),
                          colour=SOOT))
    return parts


def _spoke_point(d: str, frac: float) -> tuple[int, int]:
    """Screen pixel `frac` of the way (0..1) along spoke `d`."""
    axis, sign, _ = SPOKES[d]
    u = v = 0.0
    if axis == "v":
        v = sign * SPOKE_LENGTH * frac
    else:
        u = sign * SPOKE_LENGTH * frac
    return (round(CX + (u + v) * 48), round(CY + (u - v) * 24))


# -- risers ------------------------------------------------------------

def riser_parts(d: str, frac_rise: float = 1.0, tip: bool = False) -> list[Part]:
    """A riser is ONLY the vertical cable on its wall face, one level high:
    no flat foot and no pad (owner verdict, #2516). A flat run on the tile
    comes from the existing flat appearances."""
    return [Part("rise", d=d, a=0.0, b=frac_rise, tip=tip and frac_rise < 1.0)]


def riser_construction_parts(d: str, k: int) -> list[Part]:
    """Run the cable up the wall: a quarter, half, three quarters, done."""
    if k == CONSTRUCTION_FRAMES - 1:
        return riser_parts(d)
    return riser_parts(d, (k + 1) / CONSTRUCTION_FRAMES, tip=True)


def riser_destruction_parts(name: str, d: str, k: int) -> list[Part]:
    """Independently authored teardown of a riser: the cable is cut on the
    wall and sparks, the upper run drops away from the wall in pieces,
    debris scatters, and soot specks mark where it ran."""
    if k == 0:
        return riser_parts(d)
    rng = _hash_ints("wire-break", name, k)
    pick = iter(rng * 8)

    def jitter(n: int) -> int:
        return (next(pick) % (2 * n + 1)) - n

    ex, ey = _spoke_point(d, 1.0)
    parts: list[Part] = []
    if k == 1:
        # Cut about halfway up the wall: the upper half sags sideways.
        parts.append(Part("rise", d=d, a=0.0, b=0.4))
        parts.append(Part("rise", d=d, a=0.55, b=1.0,
                          off=(1 if jitter(1) >= 0 else -1, 1)))
        for _ in range(3):
            parts.append(Part("speck", at=(ex + jitter(3), ey - 7 + jitter(2)),
                              colour=SPARK))
        return parts
    if k == 2:
        # The upper run has fallen and broken into pieces.
        parts.append(Part("rise", d=d, a=0.0, b=0.2, off=(jitter(1), 0)))
        parts.append(Part("rise", d=d, a=0.3, b=0.45,
                          off=(jitter(2) + 3, 5 + jitter(1))))
        parts.append(Part("rise", d=d, a=0.6, b=0.75,
                          off=(jitter(2) - 3, 9 + jitter(1))))
        parts.append(Part("rise", d=d, a=0.85, b=1.0,
                          off=(jitter(2) + 5, 12)))
        return parts
    if k == 3:
        parts.append(Part("rise", d=d, a=0.0, b=0.1, off=(jitter(3), 4)))
        parts.append(Part("rise", d=d, a=0.5, b=0.6, off=(jitter(3), 10)))
        for i in range(3):
            parts.append(Part("speck", at=(ex + jitter(5), ey - 4 + jitter(4)),
                              colour=SOOT if i else CABLE))
        return parts
    # Final frame: soot specks up the wall where the cable ran.
    for i in range(4):
        parts.append(Part("speck", at=(ex + jitter(2), ey - 2 - 4 * i + jitter(1)),
                          colour=SOOT))
    return parts


# -- output ------------------------------------------------------------

def paths(shape: str) -> list[tuple[str, int | None, Path, Path]]:
    """(stage, index, colour path, facemap path) for every image of one
    shape, in the order the pack declares them."""
    out = [("static", None, COLOR_DIR / f"{shape}.png",
            FACE_DIR / f"{shape}.png")]
    for k in range(CONSTRUCTION_FRAMES):
        out.append(("build", k, COLOR_DIR / "build" / f"{shape}_{k}.png",
                    FACE_DIR / "build" / f"{shape}_{k}.png"))
    for k in range(DESTRUCTION_FRAMES):
        out.append(("break", k, COLOR_DIR / "break" / f"{shape}_{k}.png",
                    FACE_DIR / "break" / f"{shape}_{k}.png"))
    return out


def images(shape: str):
    if shape in RISERS:
        d = RISERS[shape]
        under = shape in UNDERSIDE_RISERS
        yield "static", None, render(riser_parts(d), under)
        for k in range(CONSTRUCTION_FRAMES):
            yield "build", k, render(riser_construction_parts(d, k), under)
        for k in range(DESTRUCTION_FRAMES):
            yield "break", k, render(riser_destruction_parts(shape, d, k), under)
        return
    dirs = SHAPES[shape]
    yield "static", None, render(static_parts(dirs))
    for k in range(CONSTRUCTION_FRAMES):
        yield "build", k, render(construction_parts(dirs, k))
    for k in range(DESTRUCTION_FRAMES):
        yield "break", k, render(destruction_parts(shape, dirs, k))


def posix(p: Path) -> str:
    return p.as_posix()


def yaml_text() -> str:
    """data/structure_packs/wire.yaml: the header, the build cost and one
    declaring entry per connection. Generated, so the declared paths can
    never drift from what this tool writes."""
    out = [
        "# Structure pack: wire (#359, power epic #357; art #2516)",
        "#",
        "# Power-grid wire. Unlike dungeon_1's floor/wall/post pieces (one fixed",
        "# texture per piece), wire is a single piece KIND (\"wire\") whose rendered",
        "# texture is picked at placement time from 16 CONNECTION variants — an",
        "# autotile keyed by which of the tile's 4 cardinal neighbours also carry",
        "# wire (scripts/wire.lua `shapeFor`). Read directly by scripts/wire.lua",
        "# (not scripts/structures.lua, which only knows the pieces/walls schema).",
        "#",
        "# GENERATED by tools/make_wire_textures.py together with every image it",
        "# names; edit the generator and re-run it, never this file by hand.",
        "#",
        "# Every connection declares its OWN facemap, and every lifecycle frame",
        "# is paired with one (docs/structure_pack_schema.md): the colour images",
        "# are flat material colour and the facemaps carry the lighting.",
        "",
        "name: wire",
        "",
        "# Build-job cost (#96), read by the build AI (scripts/unit_ai.lua",
        "# construct_job) exactly like dungeon_1's `build:` block.",
        "build:",
        "  wire: { build_work: 1.5, materials: { wiring: 1 } }",
        "",
        "# Connection variant name -> its colour sprite, facemap, construction",
        "# sequence and destruction clip, each frame paired with its facemap.",
        "connections:",
    ]
    for shape in ALL_SHAPES:
        out.append(f"  {shape}:")
        out.append(f"    texture: {posix(COLOR_DIR / (shape + '.png'))}")
        out.append(f"    facemap: {posix(FACE_DIR / (shape + '.png'))}")
        out.append("    construction:")
        out += [f"      - {posix(COLOR_DIR / 'build' / f'{shape}_{k}.png')}"
                for k in range(CONSTRUCTION_FRAMES)]
        out.append("    construction_facemaps:")
        out += [f"      - {posix(FACE_DIR / 'build' / f'{shape}_{k}.png')}"
                for k in range(CONSTRUCTION_FRAMES)]
        out.append("    destruction:")
        out.append(f"      fps: {DESTRUCTION_FPS}")
        out.append("      frames:")
        out += [f"        - {posix(COLOR_DIR / 'break' / f'{shape}_{k}.png')}"
                for k in range(DESTRUCTION_FRAMES)]
        out.append("      facemaps:")
        out += [f"        - {posix(FACE_DIR / 'break' / f'{shape}_{k}.png')}"
                for k in range(DESTRUCTION_FRAMES)]
    return "\n".join(out) + "\n"


def pixel_digest(img: Image.Image) -> str:
    """SHA-256 over size + raw RGBA: the approved CONTENT, independent of
    the PNG container."""
    h = hashlib.sha256()
    h.update(f"{img.width}x{img.height}\n".encode())
    h.update(img.convert("RGBA").tobytes())
    return h.hexdigest()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--yaml", action="store_true",
                    help="also write data/structure_packs/wire.yaml")
    ap.add_argument("--pins", action="store_true",
                    help="write the approved-content digest manifest")
    args = ap.parse_args()
    pins = []
    for shape in ALL_SHAPES:
        by_key = {(s, i): (c, f) for s, i, (c, f) in images(shape)}
        for stage, idx, cpath, fpath in paths(shape):
            colour, face = by_key[(stage, idx)]
            for p in (cpath, fpath):
                p.parent.mkdir(parents=True, exist_ok=True)
            colour.save(cpath)
            face.save(fpath)
            print(f"wrote {cpath} + {fpath}")
            tag = shape if idx is None and stage == "static" \
                else f"{shape}/{stage}{idx}"
            pins.append(f"{tag}\tcolour\t{pixel_digest(colour)}")
            pins.append(f"{tag}\tfacemap\t{pixel_digest(face)}")
    if args.yaml:
        YAML.write_text(yaml_text())
        print(f"wrote {YAML}")
    if args.pins:
        PINS.parent.mkdir(parents=True, exist_ok=True)
        PINS.write_text(
            "# Approved decoded-content digests for the Wire art (#2516).\n"
            "# <appearance>[/<stage><frame>]<TAB>colour|facemap<TAB>sha256\n"
            "# Regenerate ONLY after owner approval:\n"
            "#   python3 tools/make_wire_textures.py --pins\n"
            + "\n".join(pins) + "\n")
        print(f"wrote {PINS}")


if __name__ == "__main__":
    main()
