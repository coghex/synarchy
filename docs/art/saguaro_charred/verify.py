"""Validate the owner-approved charred saguaro and its retained evidence."""

import hashlib
import json
from pathlib import Path

from PIL import Image, ImageChops


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def lowest_opaque_row(alpha: Image.Image) -> int:
    return max(
        y for y in range(alpha.height)
        if alpha.crop((0, y, alpha.width, y + 1)).getbbox()
    )


def main() -> None:
    root = Path(__file__).resolve().parents[3]
    evidence = json.loads(
        (root / "docs/art/saguaro_charred_evidence.json").read_text()
    )
    reference_path = root / evidence["reference_path"]
    delivered_path = root / evidence["delivered_path"]
    raw_path = root / evidence["raw_output_path"]

    reference = Image.open(reference_path).convert("RGBA")
    delivered_file = Image.open(delivered_path)
    assert delivered_file.format == "PNG" and delivered_file.mode == "RGBA"
    delivered = delivered_file.convert("RGBA")
    assert reference.size == delivered.size == tuple(evidence["dimensions"])

    reference_alpha = reference.getchannel("A")
    delivered_alpha = delivered.getchannel("A")
    assert sorted(set(delivered_alpha.get_flattened_data())) == evidence["alpha_values"]
    assert list(delivered_alpha.getbbox()) == evidence["alpha_bounds"]

    differences = ImageChops.difference(reference_alpha, delivered_alpha)
    difference_count = sum(
        1 for value in differences.get_flattened_data() if value
    )
    assert difference_count == evidence["complete_alpha_difference_pixels"]

    outside_count = sum(
        1
        for reference_value, delivered_value in zip(
            reference_alpha.get_flattened_data(),
            delivered_alpha.get_flattened_data(),
        )
        if delivered_value and not reference_value
    )
    assert outside_count == evidence["opaque_pixels_outside_reference"] == 0

    removed = [
        [index % delivered.width, index // delivered.width]
        for index, (reference_value, delivered_value) in enumerate(
            zip(
                reference_alpha.get_flattened_data(),
                delivered_alpha.get_flattened_data(),
            )
        )
        if reference_value and not delivered_value
    ]
    assert removed == evidence["removed_reference_alpha_pixels"]

    band = evidence["lower_band_start_row"]
    lower_difference_count = sum(
        1
        for value in differences.crop(
            (0, band, delivered.width, delivered.height)
        ).get_flattened_data()
        if value
    )
    assert lower_difference_count == evidence["lower_band_alpha_difference_pixels"] == 0
    assert lowest_opaque_row(reference_alpha) == lowest_opaque_row(delivered_alpha)
    assert lowest_opaque_row(delivered_alpha) == evidence["lowest_opaque_row"]

    assert digest(reference_path) == evidence["reference_sha256"]
    assert digest(delivered_path) == evidence["delivered_sha256"]
    assert digest(raw_path) == evidence["raw_output_sha256"]
    assert delivered_path.read_bytes() == raw_path.read_bytes()
    for key in ("preview_capture", "comparison"):
        path = root / evidence[key]
        Image.open(path).verify()
        assert digest(path) == evidence[f"{key}_sha256"]

    print(
        "PASS: dimensions, RGBA, binary alpha, hashes, raw identity, "
        "alpha subset, lower band, and ground row"
    )


if __name__ == "__main__":
    main()
