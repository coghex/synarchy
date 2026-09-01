#!/usr/bin/env python3
"""Self-test for vegetation_variant_audit.py (#1782).

The negative fixture uses two PNGs with different encoded bytes but identical
decoded RGBA pixels.  This pins the visible-image contract and prevents a raw
file hash from being substituted for the real guard.  A separate single-frame
fixture proves the deliberate tilled_soil-style exception remains accepted.

Usage:
  python3 tools/test_vegetation_variant_audit.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import contextlib
import io
import sys
import tempfile
from pathlib import Path

from PIL import Image, PngImagePlugin

sys.path.insert(0, str(Path(__file__).resolve().parent))
from vegetation_variant_audit import main as audit_main  # type: ignore

import selftest  # noqa: E402
from selftest import FAILURES, expect  # noqa: E402


def _write_registry(root: Path, name: str, variants: list[str]) -> None:
    lines = ["vegetation:", "  - id_start: 1", f"    name: {name}", "    variants:"]
    lines.extend(f'      - "{variant}"' for variant in variants)
    path = root / "data/vegetation/fixture.yaml"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def _write_png(
    root: Path, rel: str, pixels: list[tuple[int, int, int, int]], *, tagged: bool = False
) -> Path:
    path = root / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    image = Image.new("RGBA", (2, 2))
    image.putdata(pixels)
    pnginfo = None
    if tagged:
        pnginfo = PngImagePlugin.PngInfo()
        pnginfo.add_text("fixture", "different encoded bytes, same decoded pixels")
    image.save(path, format="PNG", pnginfo=pnginfo)
    return path


def _run(root: Path) -> tuple[int, str]:
    output = io.StringIO()
    with contextlib.redirect_stdout(output):
        code = audit_main(repo_root=root)
    return code, output.getvalue()


def test_encoded_different_decoded_duplicate_fails() -> None:
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        left_rel = "assets/textures/vegetation/same_pixels/frame_000.png"
        right_rel = "assets/textures/vegetation/same_pixels/frame_001.png"
        pixels = [(12, 34, 56, 255)] * 4
        left = _write_png(root, left_rel, pixels)
        right = _write_png(root, right_rel, pixels, tagged=True)
        _write_registry(root, "same_pixels", [left_rel, right_rel])

        expect(
            left.read_bytes() != right.read_bytes(),
            "negative fixture PNGs must have different encoded bytes",
        )
        with Image.open(left) as left_image, Image.open(right) as right_image:
            expect(
                left_image.convert("RGBA").tobytes()
                == right_image.convert("RGBA").tobytes(),
                "negative fixture PNGs must decode to identical RGBA pixels",
            )

        code, output = _run(root)
        expect(code != 0, "a decoded-identical multi-variant family must fail")
        for needle in ("same_pixels", left_rel, right_rel):
            expect(
                needle in output,
                f"duplicate diagnostic must name {needle!r}; got {output!r}",
            )


def test_single_frame_family_passes() -> None:
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        frame_rel = "assets/textures/vegetation/tilled_soil/frame_000.png"
        _write_png(root, frame_rel, [(70, 50, 30, 255)] * 4)
        _write_registry(root, "tilled_soil", [frame_rel])

        code, output = _run(root)
        expect(code == 0, f"a single-frame family must pass; got {output!r}")
        expect(
            "1 YAML-declared vegetation families checked (0 multi-variant)" in output,
            f"single-frame pass must report its audited scope; got {output!r}",
        )


def test_visibly_distinct_multi_variant_family_passes() -> None:
    with tempfile.TemporaryDirectory() as directory:
        root = Path(directory)
        left_rel = "assets/textures/vegetation/distinct/frame_000.png"
        right_rel = "assets/textures/vegetation/distinct/frame_001.png"
        _write_png(root, left_rel, [(10, 20, 30, 255)] * 4)
        _write_png(root, right_rel, [(10, 20, 31, 255)] * 4)
        _write_registry(root, "distinct", [left_rel, right_rel])

        code, output = _run(root)
        expect(code == 0, f"decoded-distinct variants must pass; got {output!r}")
        expect(
            "no decoded-RGBA duplicate frames" in output,
            f"clean multi-variant scan must report success; got {output!r}",
        )


def main() -> int:
    selftest.parse_verbose()
    tests = [
        test_encoded_different_decoded_duplicate_fails,
        test_single_frame_family_passes,
        test_visibly_distinct_multi_variant_family_passes,
    ]
    for test in tests:
        print(test.__name__)
        try:
            test()
        except Exception as exc:  # keep running so every fixture reports
            selftest.record_fail(
                f"{test.__name__} raised {type(exc).__name__}: {exc}")

    if FAILURES:
        print(f"\nFAILED — {len(FAILURES)} assertion(s)")
        return selftest.concluded(1)
    return selftest.concluded(
        0, f"\nOK — {len(tests)} vegetation variant audit tests passed")


if __name__ == "__main__":
    sys.exit(main())
