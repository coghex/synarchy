#!/usr/bin/env python3
"""Schematic fluid-mask review using production material/tint/lighting inputs."""
from __future__ import annotations

import argparse
import math
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont

from fluid_level_masks import MASKS, ROOT, SIZE, read_masks, validate

# World.Render.TileQuads: River/Lake/Ocean share matOcean; Lava uses matLava.
APPEARANCES = (
    ("River", "ocean", (0.6, 0.85, 0.95)),
    ("Lake", "ocean", (0.5, 0.8, 0.9)),
    ("Ocean", "ocean", (0.7, 0.8, 1.0)),
    ("Lava", "lava", (1.0, 0.6, 0.2)),
)
FACINGS = ("South", "West", "North", "East")
PHASES = (("Day", 0.125), ("Night", 0.75))
TERRAINS = (("Light terrain", "sand"), ("Dark terrain", "basalt"))


def face_brightness(phase: float, facing: int) -> tuple[float, float, float]:
    """bindlessFragmentShaderCode, including camera-relative sun direction."""
    angle = phase * 6.28318530718
    height = math.sin(angle)
    sun_dir = math.cos(angle + facing * 1.5707963)
    ambient = 0.5 + 0.2 * height if height >= 0 else 0.15 + 0.35 * (1 + height)
    direct = (1 - ambient) * max(0, height)
    top = ambient + direct
    left = ambient * 0.85 + direct * (0.4 + 0.6 * max(0, sun_dir))
    right = ambient * 0.85 + direct * (0.4 + 0.6 * max(0, -sun_dir))
    return (left, top, right) if facing % 2 else (right, top, left)


def material(name: str) -> Image.Image:
    with Image.open(ROOT / f"assets/textures/world/{name}/{name}.png") as image:
        if image.size != SIZE:
            raise ValueError(f"unexpected material canvas: {name} {image.size}")
        return image.convert("RGBA")


def shade(texture: Image.Image, mask: Image.Image, tint: tuple[float, float, float],
          phase: float, facing: int, brightness: float = 1.0) -> Image.Image:
    """UNORM texture + UNORM target: no sRGB conversion; tileAlpha=1.

    CPU swatch of the fragment formula, not a full renderer or scene capture.
    Samples texels at native resolution; masks and material multiply alpha.
    """
    lights = face_brightness(phase, facing)
    out = Image.new("RGBA", SIZE)
    for y in range(64):
        for x in range(96):
            r, g, b, a = texture.getpixel((x, y))
            mr, mg, mb, ma = mask.getpixel((x, y))
            total = mr + mg + mb
            light = sum(c * v for c, v in zip((mr, mg, mb), lights)) / total if total else lights[1]
            rgb = tuple(max(0, min(255, round(c * t * light * brightness)))
                        for c, t in zip((r, g, b), tint))
            out.putpixel((x, y), (*rgb, round(a * ma / 255)))
    return out


def panel(masks: list[Image.Image], flat: Image.Image, facing: int, terrain: str) -> Image.Image:
    label_w, cell_w, row_h, header_h = 120, 152, 104, 55
    out = Image.new("RGBA", (label_w + 8 * cell_w, header_h + 8 * row_h), (23, 26, 31, 255))
    draw = ImageDraw.Draw(out)
    font = ImageFont.load_default(size=13)
    draw.text((12, 8), f"{FACINGS[facing]} facing | {terrain} | native pixels | brightness 1.0", fill="white", font=font)
    for level in range(1, 9):
        draw.text((label_w + (level - 1) * cell_w + 55, 32), f"{level}/8", fill="white", font=font)
    ground = material(terrain)
    for appearance, (title, tex_name, tint) in enumerate(APPEARANCES):
        texture = material(tex_name)
        for lighting, (light_name, phase) in enumerate(PHASES):
            row = 2 * appearance + lighting
            y0 = header_h + row * row_h
            draw.text((10, y0 + 25), title, fill="white", font=font)
            draw.text((10, y0 + 44), f"{light_name} {phase}", fill=(190, 195, 205), font=font)
            dry = shade(ground, flat, (1, 1, 1), phase, facing)
            # The real terrain material fills the background; the adjacent
            # dry tile is explicitly face-lit. Swatches show a shared bed:
            # low fluid tops move down by 16 - 2*level, keeping the base fixed.
            for level, mask in enumerate(masks, 1):
                cell = Image.new("RGBA", (cell_w, row_h), (0, 0, 0, 255))
                bg = shade(ground, Image.new("RGBA", SIZE, (0, 255, 0, 255)),
                           (0.45, 0.45, 0.45), phase, facing)
                for bx in range(0, cell_w, 96):
                    for by in range(0, row_h, 64):
                        cell.alpha_composite(bg, (bx, by))
                cell.alpha_composite(dry, (50, 2))
                cell.alpha_composite(shade(texture, mask, tint, phase, facing),
                                     (2, 26 + 16 - 2 * level))
                ImageDraw.Draw(cell).rectangle((0, 0, cell_w - 1, row_h - 1), outline=(56, 60, 65))
                out.alpha_composite(cell, (label_w + (level - 1) * cell_w, y0))
    return out


def self_test() -> None:
    mask = Image.new("RGBA", SIZE, (255, 0, 0, 255))
    texture = Image.new("RGBA", SIZE, (200, 100, 50, 128))
    light = face_brightness(0.125, 0)[0]
    assert shade(texture, mask, (0.6, 0.85, 0.95), 0.125, 0).getpixel((0, 0)) == (
        round(200 * 0.6 * light), round(100 * 0.85 * light), round(50 * 0.95 * light), 128)
    mask.putpixel((0, 0), (255, 0, 0, 0))
    assert shade(texture, mask, (1, 1, 1), 0.125, 0).getpixel((0, 0))[3] == 0
    mask.putpixel((0, 0), (255, 0, 0, 128))
    assert shade(texture, mask, (1, 1, 1), 0.125, 0).getpixel((0, 0))[3] == 64
    for facing in range(4):
        assert abs(face_brightness(0.75, facing)[1] - 0.15) < 1e-6
    assert face_brightness(0.125, 0) != face_brightness(0.125, 2)
    assert face_brightness(0.125, 1) != face_brightness(0.125, 0)
    print("OK: tint, camera lighting, night ambient, material/mask alpha")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", type=Path)
    parser.add_argument("--panels", type=Path, help="also write eight more legible individual panels")
    parser.add_argument("--self-test", action="store_true")
    args = parser.parse_args()
    if args.self_test:
        self_test()
        return
    if args.out is None:
        parser.error("--out is required")
    masks = read_masks(MASKS)
    with Image.open(MASKS / "isoface.png") as image:
        flat = image.copy()
    errors = validate(masks, flat)
    if errors:
        raise SystemExit("\n".join(errors))
    panels = [panel(masks, flat, facing, terrain) for facing in range(4) for _, terrain in TERRAINS]
    width, height = panels[0].size
    sheet = Image.new("RGBA", (2 * width, 4 * height + 50), (23, 26, 31, 255))
    ImageDraw.Draw(sheet).text((12, 12),
        "Fluid levels 1-8 | River, Lake, Ocean, Lava | day/night | sand/basalt | four facings | CPU shader swatches",
        fill="white", font=ImageFont.load_default(size=18))
    for i, im in enumerate(panels):
        sheet.alpha_composite(im, ((i % 2) * width, 50 + (i // 2) * height))
        if args.panels:
            args.panels.mkdir(parents=True, exist_ok=True)
            im.save(args.panels / f"{FACINGS[i // 2].lower()}_{TERRAINS[i % 2][1]}.png")
    args.out.parent.mkdir(parents=True, exist_ok=True)
    sheet.save(args.out)
    print(f"OK: {args.out} ({sheet.width}x{sheet.height}), 512 fluid swatches")


if __name__ == "__main__":
    main()
