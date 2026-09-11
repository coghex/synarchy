"""Validate the approved pair; optionally render supplementary 1x/6x evidence."""

import argparse
import hashlib
import json
from pathlib import Path

from PIL import Image, ImageChops, ImageDraw


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--sheet", action="store_true")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[3]
    evidence = json.loads((root / "docs/art/saguaro_sprout_dead_evidence.json").read_text())
    sprites = []
    for filename, key in [("sprout.png", "reference_sha256"),
                          ("sprout_dead.png", "delivered_sha256")]:
        path = root / "assets/textures/flora/saguaro" / filename
        sprite = Image.open(path)
        assert sprite.format == "PNG" and sprite.mode == "RGBA"
        assert sprite.size == (48, 48)
        alpha = sprite.getchannel("A")
        assert set(alpha.get_flattened_data()) == {0, 255}
        assert alpha.getbbox() == (18, 5, 30, 47)
        assert alpha.crop((0, 47, 48, 48)).getbbox() is None
        assert hashlib.sha256(path.read_bytes()).hexdigest() == evidence[key]
        sprites.append(sprite)
    band = evidence["lower_band_start_row"]
    assert type(band) is int and 5 <= band <= 46
    difference = ImageChops.difference(*(s.getchannel("A") for s in sprites))
    assert difference.crop((0, band, 48, 48)).getbbox() is None
    assert difference.getbbox() is None
    assert (root / evidence["delivered_path"]).read_bytes() == (root / evidence["raw_output_path"]).read_bytes()
    print("PASS: dimensions, RGBA, binary alpha, hashes, ground row, lower band, complete silhouette, raw output identity")
    if args.sheet:
        sheet = Image.new("RGB", (640, 420), (48, 48, 48))
        draw = ImageDraw.Draw(sheet)
        for x, label, sprite in zip((16, 336), ("Approved living", "Approved dead"), sprites):
            draw.text((x, 12), label, fill="white")
            sheet.paste(sprite, (x, 34), sprite)
            enlarged = sprite.resize((288, 288), Image.Resampling.NEAREST)
            sheet.paste(enlarged, (x, 104), enlarged)
            draw.text((x, 399), "1x above; nearest-neighbour 6x below", fill="white")
        path = Path(__file__).with_name("approved_pair.png")
        sheet.save(path)
        print("Supplementary sheet:", path)


if __name__ == "__main__":
    main()
