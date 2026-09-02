#!/usr/bin/env python3
"""Image and PNG handling for the unit-atlas tool (issue #2054,
requirement 5).

The ONE owner of everything that touches image bytes: Pillow's lazy
import and its install diagnostic (`image_module`, whose `_IMAGE_MODULE`
memo lives here and nowhere else — a test that blocks the decoder
patches THIS module), canonical RGBA8 decoding (`decode_rgba8`), the
PNG container checks (`locate_png_stream_end`, `verify_png_container`),
the frame-content check that combines them and reports a frame's pixel
size (`validate_frame_image`), and the runtime-`Float` representability
helpers the declaration parser applies to `fps`.

The two distinct image checks are preserved as two functions on
purpose (requirement 6): `decode_rgba8` is the maintained-library decode
that catches corrupt, truncated, non-image and wrong-format content,
and `verify_png_container` is the container pass that catches appended
bytes and a malformed terminal chunk. Neither is a header-only
inspection or a hand-written general PNG decoder, and neither may
replace the other — the module docstring of tools/pack_atlas.py's
FRAME CONTENTS section says why, and tools/test_pack_atlas.py pins each
with a fixture the other accepts.

Consumes only the shared definitions owner. The public façade is
tools/pack_atlas.py.
"""
from __future__ import annotations

import io
import math
import struct
from pathlib import Path
from typing import Any, Tuple

from pack_atlas_shared import PNG_IEND_CHUNK, PNG_SIGNATURE, Frame


class ImageBackendMissing(Exception):
    """Pillow is needed for this operation and is not installed."""


_IMAGE_MODULE: Any = None


def image_module() -> Any:
    """Import Pillow on demand.

    Still lazy, but no longer optional in practice: since #1311 the
    inventory gate decodes every declared frame, so any run with frames
    to check needs it. Laziness now spares only the runs that decode
    nothing at all — `--help`, an argument error, a root with no
    declarations — and keeps the import failure reportable as one
    finding rather than an import-time crash.
    """
    global _IMAGE_MODULE
    if _IMAGE_MODULE is None:
        try:
            from PIL import Image  # type: ignore[import-not-found]
        except ImportError as error:
            raise ImageBackendMissing(
                "Pillow is required to validate frame contents and to "
                "compile or verify atlases. Install the pinned toolchain "
                "with:\n"
                "    python3 -m pip install --user -r "
                "tools/requirements-assets.txt"
            ) from error
        _IMAGE_MODULE = Image
    return _IMAGE_MODULE


def decode_rgba8(path: Path) -> Frame:
    """Decode one PNG to canonical 8-bit RGBA samples.

    Decoding goes through Pillow rather than a hand-rolled parser: PNG
    has paletted, greyscale, 16-bit and interlaced forms plus ancillary
    colour-management chunks, and every one of them has to arrive at
    the same canonical RGBA8 the engine's upload path produces.
    `convert("RGBA")` is that normalisation; it is not a resample.
    """
    image_mod = image_module()
    try:
        with image_mod.open(path) as handle:
            if handle.format != "PNG":
                found = handle.format or "an unrecognised format"
                raise ValueError(f"expected a PNG, got {found}")
            converted = handle.convert("RGBA")
            return Frame(converted.width, converted.height,
                         converted.tobytes())
    except ImageBackendMissing:
        raise
    except Exception as error:  # noqa: BLE001 - any decode failure is one finding
        raise ValueError(f"cannot decode as an image: {error}") from error


def locate_png_stream_end(path: Path) -> Tuple[int, int, bytes]:
    """Find where the PNG datastream ends: `(end, file size, IEND bytes)`.

    `end` is the offset just past the FIRST IEND chunk — the one that
    terminates the image per the specification. `IEND bytes` are that
    chunk's own 12 bytes, for the caller to compare against
    `PNG_IEND_CHUNK`. Raises `ValueError` when no IEND terminates the
    stream, rather than returning a sentinel the caller would have to
    re-check on a path Pillow has already made unreachable.

    This is chunk FRAMING and nothing else: a four-byte big-endian
    length, a four-byte type, the payload, a four-byte CRC. It decodes
    nothing, checksums nothing, and knows nothing about any chunk type
    beyond the four letters that end the stream.

    That narrowness is deliberate and must be preserved (#1311 —
    hand-rolling a second PNG parser is what sank the previous
    attempt). Two things keep it honest: it runs only AFTER Pillow has
    CRC-validated the chunk sequence, which fixes the framing uniquely,
    so this cannot disagree with the real decoder about where a chunk
    lies; and its entire output is one offset, which the caller uses to
    ask a single question the library will not answer — is there
    anything after the image?
    """
    with path.open("rb") as handle:
        size = handle.seek(0, io.SEEK_END)
        handle.seek(len(PNG_SIGNATURE))
        while True:
            header = handle.read(8)
            if len(header) < 8:
                break
            (length,) = struct.unpack(">I", header[:4])
            chunk_start = handle.tell() - 8
            end = handle.seek(length + 4, io.SEEK_CUR)
            if end > size:
                # A chunk claiming more bytes than the file holds.
                break
            if header[4:] == b"IEND":
                handle.seek(chunk_start)
                return end, size, handle.read(end - chunk_start)
    raise ValueError(
        "corrupt PNG structure: no IEND chunk terminates the stream")


def verify_png_container(path: Path) -> None:
    """Check the chunk stream's checksums, without decompressing.

    Pillow's `verify()` walks the chunks and CRCs each one, which is
    the ONLY thing here that looks at an IDAT checksum — the decoder
    reads and discards those four bytes while streaming pixel data, so
    a correct payload under a wrong checksum decodes happily.

    It stops ON the terminal chunk without checksumming it, though, and
    the decoder never reads that far, so IEND is the one chunk neither
    library pass covers — nor does either notice bytes appended after
    it. `locate_png_stream_end` supplies the offset both questions need.

    Deliberately does NOT re-check the format: this only ever runs on a
    file `decode_rgba8` has already accepted as a decodable PNG, and a
    second copy of that rule would mean neither copy could be removed
    detectably.
    """
    image_mod = image_module()
    try:
        with image_mod.open(path) as handle:
            handle.verify()
    except ImageBackendMissing:
        raise
    except Exception as error:  # noqa: BLE001 - any rejection is one finding
        raise ValueError(f"corrupt PNG structure: {error}") from error

    try:
        end, size, terminal = locate_png_stream_end(path)
    except OSError as error:
        raise ValueError(f"cannot read: {error}") from error

    # Trailing data is not part of the image. Comparing the FILE's last
    # bytes would not establish this: appending a second canonical IEND
    # leaves a perfect tail while the real image ended 12 bytes earlier.
    if end != size:
        raise ValueError(
            f"corrupt PNG structure: {size - end} byte(s) follow the IEND "
            f"chunk that ends the image")
    # And the terminal chunk itself, which needs no parsing to check:
    # IEND's payload is EMPTY by specification, so its length, type and
    # CRC together are a fixed 12-byte constant.
    if terminal != PNG_IEND_CHUNK:
        raise ValueError(
            "corrupt PNG structure: the IEND chunk that ends the image "
            "carries a wrong terminal checksum")


def validate_frame_image(path: Path) -> Tuple[int, int]:
    """Prove one declared frame is real art, and report its pixel size.

    TWO passes, neither sufficient alone — see the module docstring's
    FRAME CONTENTS section. The decode comes first: it settles the
    format question and covers the compressed pixel stream, so the
    container pass that follows is reached only for a genuine,
    decodable PNG and has exactly one job left, the checksums.

    Raises `ValueError` naming the reason for any unreadable frame.
    `ImageBackendMissing` propagates untouched: an absent decoder is one
    condition for the whole run, not a finding about this file.
    """
    frame = decode_rgba8(path)
    verify_png_container(path)
    return frame.width, frame.height


def narrow_to_runtime_float(value: float) -> float:
    """``value`` as the engine's 32-bit `Float` will actually hold it.

    The generated index records the EFFECTIVE frame rate, so `fps: 8`
    and `fps: 8.0` must produce one identical document — and a rate the
    engine will round has to be recorded rounded, or the index would
    promise a timing the runtime cannot reproduce. Only ever called
    after `fits_runtime_float` has accepted the value.
    """
    return float(struct.unpack("<f", struct.pack("<f", float(value)))[0])


def fits_runtime_float(value: float) -> bool:
    """Whether ``value`` survives the engine's single-precision `Float`.

    `Engine.Asset.YamlUnits.UnitYamlAnim` stores `uyaFps` as a Haskell
    `Float`, which is 32-bit. Python's own check only proves the value
    fits a 64-bit double, so `1.0e+100` and `1.0e-100` pass it and then
    become `Infinity` and `0` respectively at load time — neither is a
    frame rate. Round-tripping through a 32-bit pack reproduces exactly
    what the engine will hold.
    """
    try:
        narrowed = struct.unpack("<f", struct.pack("<f", float(value)))[0]
    except (OverflowError, ValueError):
        return False
    return math.isfinite(narrowed) and narrowed != 0.0
