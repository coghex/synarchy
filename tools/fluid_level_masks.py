#!/usr/bin/env python3
"""Generate from World.Slope.FaceMaps, or independently validate its PNGs (#2525)."""
from __future__ import annotations

import argparse
import contextlib
import io
from pathlib import Path
import subprocess
import tempfile
import time

from PIL import Image

ROOT = Path(__file__).resolve().parent.parent
MASKS = ROOT / "assets/textures/facemap"
SIZE = (96, 64)
CLEAR = (0, 0, 0, 0)
FACES = {(255, 0, 0, 255), (0, 255, 0, 255), (0, 0, 255, 255)}


def name(level: int) -> str:
    return f"isoface_level_{level}.png"


def expected(reference: Image.Image, level: int) -> Image.Image:
    """Independent oracle: trim ONLY the shipped flat map's lower extrusion."""
    result = reference.copy()
    for x in range(96):
        end = 24 + min(x, 95 - x) // 2 + 2 * level
        for y in range(end + 1, 64):
            result.putpixel((x, y), CLEAR)
    return result


def validate(images: list[Image.Image], reference: Image.Image) -> list[str]:
    errors = []
    if reference.mode != "RGBA" or reference.size != SIZE:
        return ["invalid reference"]
    if len(images) != 8:
        return ["expected eight masks"]
    for level, im in enumerate(images, 1):
        prefix = f"level {level}: "
        if im.mode != "RGBA" or im.size != SIZE:
            errors.append(prefix + "dimensions/mode")
            continue
        wanted = expected(reference, level)
        colors = set(im.getdata())
        if any(c[3] not in (0, 255) for c in colors):
            errors.append(prefix + "binary alpha")
        if any(c[3] == 255 and c not in FACES for c in colors):
            errors.append(prefix + "opaque channel purity")
        if any(c[3] == 0 and c != CLEAR for c in colors):
            errors.append(prefix + "transparent black")
        if im.getchannel("A").tobytes() != wanted.getchannel("A").tobytes():
            errors.append(prefix + "exact silhouette (including interior)")
        if im.tobytes() != wanted.tobytes():
            errors.append(prefix + "footprint/face assignment")
        for x in (0, 95):
            for y in (23, 24):
                if im.getpixel((x, y)) != reference.getpixel((x, y)):
                    errors.append(prefix + "fixed corner")
        if level == 8 and im.tobytes() != reference.tobytes():
            errors.append(prefix + "full-level equality")
    for level, (low, high) in enumerate(zip(images, images[1:]), 1):
        if any(im.mode != "RGBA" or im.size != SIZE for im in (low, high)):
            continue
        for x in range(96):
            end = 24 + min(x, 95 - x) // 2 + 2 * level
            changed = [y for y in range(64) if low.getpixel((x, y)) != high.getpixel((x, y))]
            if changed != [end + 1, end + 2]:
                errors.append(f"levels {level}/{level + 1}: two-pixel progression at x={x}")
                break
    return errors


def read_masks(directory: Path) -> list[Image.Image]:
    images = []
    for level in range(1, 9):
        with Image.open(directory / name(level)) as image:
            if image.format != "PNG":
                raise ValueError(f"{name(level)} is not PNG")
            images.append(image.copy())
    return images


@contextlib.contextmanager
def build_hold():
    import probe_resource_lock as lock
    start = time.monotonic()
    while True:
        try:
            hold = lock.acquire(exclusive={"cabal-build"},
                                namespace=lock.repository_namespace(ROOT),
                                purpose="fluid-level mask exporter #2525")
            break
        except lock.ResourceBusy as busy:
            print(busy, flush=True)
            if time.monotonic() - start >= 1800:
                raise RuntimeError("30-minute build-lock timeout") from busy
            time.sleep(60)
    with hold:
        yield


def production_images() -> list[Image.Image]:
    """Freshness-build and execute the real library, never a Python generator."""
    with build_hold(), tempfile.TemporaryDirectory(prefix="fluid-mask-export-") as tmp:
        subprocess.run(["cabal", "build", "lib:synarchy"], cwd=ROOT, check=True)
        executable = ROOT / "dist-newstyle/fluid-mask-export"
        subprocess.run(["cabal", "exec", "--", "ghc", "-Wall", "-Werror",
                        "-XGHC2024", "-package", "synarchy",
                        "-outputdir", "dist-newstyle/fluid-mask-export-build",
                        "tools/fluid_masks/Main.hs", "-o", str(executable)],
                       cwd=ROOT, check=True)
        subprocess.run([str(executable), tmp], cwd=ROOT, check=True)
        return [Image.frombytes("RGBA", SIZE,
                                (Path(tmp) / f"isoface_level_{n}.rgba").read_bytes())
                for n in range(1, 9)]


def encode(image: Image.Image) -> bytes:
    out = io.BytesIO()
    image.save(out, format="PNG", optimize=False, compress_level=9)
    return out.getvalue()


def self_test(reference: Image.Image) -> None:
    valid = [expected(reference, n) for n in range(1, 9)]
    assert not validate(valid, reference)
    mutations = [
        ("dimensions/mode", 1, lambda im: im.crop((0, 0, 95, 64))),
        ("dimensions/mode", 1, lambda im: im.convert("RGB")),
        ("binary alpha", 1, ((47, 10), (0, 255, 0, 127))),
        ("opaque channel purity", 1, ((47, 10), (1, 254, 0, 255))),
        ("transparent black", 1, ((0, 0), (1, 0, 0, 0))),
        ("exact silhouette", 1, ((0, 0), (0, 255, 0, 255))),
        ("exact silhouette", 1, ((47, 10), CLEAR)),
        ("footprint/face assignment", 1, ((10, 31), (255, 0, 0, 255))),
        ("fixed corner", 1, ((0, 23), CLEAR)),
        ("fixed corner", 1, ((95, 24), (0, 0, 255, 255))),
        ("two-pixel progression", 2, ((47, 51), CLEAR)),
        ("full-level equality", 8, ((47, 63), CLEAR)),
    ]
    for rule, level, mutation in mutations:
        images = [im.copy() for im in valid]
        if callable(mutation):
            images[level - 1] = mutation(images[level - 1])
        else:
            images[level - 1].putpixel(*mutation)
        errors = validate(images, reference)
        assert any(rule in error for error in errors), (rule, errors)
    assert validate(valid[:-1], reference) == ["expected eight masks"]
    assert encode(valid[0]) == encode(valid[0])
    print(f"OK: {len(mutations)} rule mutations rejected; eight-mask count checked")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--generate", action="store_true", help="freshness-build Haskell and write PNGs")
    mode.add_argument("--check-generated", action="store_true", help="compare Haskell RGBA and encoded file bytes")
    mode.add_argument("--self-test", action="store_true")
    parser.add_argument("--directory", type=Path, default=MASKS)
    args = parser.parse_args()
    with Image.open(MASKS / "isoface.png") as image:
        reference = image.copy()
    if args.self_test:
        self_test(reference)
        return
    if args.generate or args.check_generated:
        images = production_images()
        errors = validate(images, reference)
        if errors:
            raise SystemExit("\n".join(errors))
        # Generation pins the encoder; validation only decodes, so is unpinned.
        import PIL
        if PIL.__version__ != "11.3.0":
            raise SystemExit("Generation requires Pillow==11.3.0 (tools/requirements-assets.txt)")
        for level, im in enumerate(images, 1):
            path = args.directory / name(level)
            encoded = encode(im)
            if args.check_generated:
                if path.read_bytes() != encoded:
                    raise SystemExit(f"encoded PNG differs: {path}")
                with Image.open(path) as tracked:
                    if tracked.mode != "RGBA" or tracked.tobytes() != im.tobytes():
                        raise SystemExit(f"Haskell RGBA differs: {path}")
            elif not path.exists() or path.read_bytes() != encoded:
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(encoded)
    errors = validate(read_masks(args.directory), reference)
    if errors:
        raise SystemExit("\n".join(errors))
    print("OK: eight masks; fixed footprint, exact silhouettes/channels, two-pixel progression, level-8 equality")
    if args.check_generated:
        print("OK: all eight match Haskell RGBA and regenerated PNG bytes")


if __name__ == "__main__":
    main()
