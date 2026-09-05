#!/usr/bin/env python3
"""
test_pack_atlas_validation.py — the inventory, declaration, path, and
image-content cases of `tools/test_pack_atlas.py` (#2061 owner split).

Two registries, both about `pack_atlas --validate-only`:

  POSITIVE   a validation fixture that must exit 0.
  NEGATIVE   a validation fixture that must exit non-zero AND print a
             diagnostic naming the actual problem, so a check cannot
             pass by failing for some unrelated reason. Where a case
             tightens a rule, a positive case pins the other direction,
             so over-rejection fails too.

Validation opens every declared frame (#1311), so the content cases
below corrupt real PNG bytes at exact offsets — a truncated stream, a
garbled payload under a correct checksum, a correct payload under a
wrong checksum, a non-image, a valid image of another format — and pair
each with a positive that would fail on over-rejection.

Not runnable on its own: it parses no arguments and executes no case at
import. `tools/test_pack_atlas.py` imports `CASES` and runs it.
"""
from __future__ import annotations

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from test_pack_atlas_support import (  # noqa: E402
    ALL8, CANON5, CaseRegistry, Fixture, OwnerCases, anim_yaml,
    asset_only_yaml, bad_checksum_png, chunk_then_iend_png,
    corrupt_stream_png, duplicate_iend_png, frame_lines, gameplay_yaml,
    not_an_image, other_format_png, png_bytes, png_in_mode,
    tampered_iend_png, trailing_data_png, truncated_png, valid_fixture,
)

_CASES = CaseRegistry("validation")
positive = _CASES.positive
negative = _CASES.negative

# -- positive ---------------------------------------------------------

@positive("a runtime unit and an asset-only unit both validate clean")
def _valid(fx: Fixture) -> None:
    valid_fixture(fx)


@positive("reusing an animation frame as sprite/portrait is not a duplicate claim")
def _reuse(fx: Fixture) -> None:
    fx.frames("hero", "idle", CANON5, 2)
    shared = "assets/textures/units/hero/animations/idle/south/frame_000.png"
    fx.write_file("assets/textures/units/hero/portrait.png", png_bytes())
    fx.yaml("hero", (
        f"units:\n  - name: hero\n    sprite: \"{shared}\"\n"
        f'    portrait: "assets/textures/units/hero/portrait.png"\n'
        f"    directional_sprites:\n      south: \"{shared}\"\n"
        f"    animations:\n") + anim_yaml("hero", "idle", CANON5, 2, True))


@positive("different directions of one animation may hold different frame counts")
def _ragged(fx: Fixture) -> None:
    fx.frames("hero", "idle", ["south"], 4)
    fx.frames("hero", "idle", ["south-east", "east", "north-east", "north"], 2)
    fx.write_file("assets/textures/units/hero/idle.png", png_bytes())
    body = ["    animations:\n", "      idle:\n", "        fps: 8\n",
            "        loop: true\n", "        flip: true\n",
            "        frames:\n", "          south:\n",
            frame_lines("hero", "idle", "south", 4, " " * 12)]
    for d in ["south-east", "east", "north-east", "north"]:
        body.append(f"          {d}:\n")
        body.append(frame_lines("hero", "idle", d, 2, " " * 12))
    fx.yaml("hero", 'units:\n  - name: hero\n    sprite: '
                    '"assets/textures/units/hero/idle.png"\n' + "".join(body))


@positive("a file holding BOTH units: and asset_units: validates")
def _both_keys(fx: Fixture) -> None:
    fx.frames("hero", "idle", CANON5, 2)
    fx.frames("prop", "spin", CANON5, 2)
    fx.write_file("assets/textures/units/hero/idle.png", png_bytes())
    fx.yaml("hero", gameplay_yaml("hero", [("idle", CANON5, 2, True)])
            + asset_only_yaml("prop", [("spin", CANON5, 2, True)]).replace(
                "asset_units:\n", "asset_units:\n", 1))


@positive("the approved <lowercase>_RH_<lowercase> animation name is accepted")
def _rh_exception(fx: Fixture) -> None:
    # The one narrowly matched exception to the lowercase identifier
    # rule: eight shipped acolyte animations use it, and it authors all
    # eight directions (a mirrored right hand would be a left hand).
    fx.frames("hero", "attack_heavy_RH_dagger", ALL8, 2)
    fx.write_file("assets/textures/units/hero/idle.png", png_bytes())
    fx.yaml("hero", gameplay_yaml("hero", [
        ("attack_heavy_RH_dagger", ALL8, 2, False)]))


@positive("every legitimate PNG colour type decodes")
def _png_colour_types(fx: Fixture) -> None:
    # The content rule (#1311) is "decodes as a PNG", NOT "is already
    # 8-bit RGBA". Paletted, greyscale, greyscale+alpha, 16-bit and
    # interlaced frames are all valid art the engine's own upload path
    # normalises, so rejecting any of them would be over-rejection —
    # and each is a distinct decode path inside the library.
    valid_fixture(fx)
    base = "assets/textures/units/prop/animations/spin"
    variants = [
        ("south/frame_000.png", png_in_mode("P")),
        ("south/frame_001.png", png_in_mode("L")),
        ("south-east/frame_000.png", png_in_mode("LA")),
        ("east/frame_000.png", png_in_mode("I;16")),
        ("north/frame_000.png", png_in_mode("RGBA", interlace=True)),
    ]
    for rel, data in variants:
        fx.write_file(f"{base}/{rel}", data)


@positive("two ANIMATIONS of one unit may use different pixel sizes")
def _per_animation_size(fx: Fixture) -> None:
    # One size per ANIMATION is the atlas cell constraint; it says
    # nothing across animations, which get their own atlases. A rule
    # applied per unit would reject this legitimate tree.
    fx.frames("hero", "idle", CANON5, 2)
    fx.write_file("assets/textures/units/hero/idle.png", png_bytes())
    fx.yaml("hero", gameplay_yaml("hero", [
        ("idle", CANON5, 2, True), ("walk", CANON5, 2, True)]))
    for direction in CANON5:
        for index in range(2):
            fx.write_file(
                f"assets/textures/units/hero/animations/walk/{direction}/"
                f"frame_{index:03d}.png", png_bytes(9, 5))


@positive("a corrupt NON-animation texture is outside the inventory")
def _aux_contents_out_of_scope(fx: Fixture) -> None:
    # `sprite`, `directional_sprites` and `portrait` are checked for
    # EXISTENCE only: the inventory's scope is `animations/`, and these
    # files are also reached from hard-coded Haskell. Widening the
    # content pass to them is a separate decision, not a side effect.
    valid_fixture(fx)
    fx.write_file("assets/textures/units/hero/idle.png", not_an_image())


# -- negative ---------------------------------------------------------

@negative("a truncated frame",
          "spin/south/frame_000.png: cannot decode as an image")
def _truncated_frame(fx: Fixture) -> None:
    # Reading a header alone would accept this: the IHDR is intact and
    # the dimensions it reports are correct. Only reading the pixel
    # stream reaches the missing bytes.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_000.png",
        truncated_png())


@negative("a frame whose compressed data is corrupt under a valid checksum",
          "idle/north/frame_002.png: cannot decode as an image")
def _corrupt_stream_frame(fx: Fixture) -> None:
    # The chunk CRC is recomputed over the damaged payload, so the
    # container is byte-perfect. This is the case a checksum-only check
    # cannot see, and it is why the pass actually DECODES.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/hero/animations/idle/north/frame_002.png",
        corrupt_stream_png())


@negative("a frame with a bad chunk checksum",
          "bad header checksum")
def _bad_checksum_frame(fx: Fixture) -> None:
    # And the converse: the payload here is intact, so a full decode
    # accepts it (Pillow discards IDAT CRCs while streaming). Only the
    # container pass rejects it. `_both_decode_passes_earn_their_keep`
    # pins that asymmetry directly.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/east/frame_001.png",
        bad_checksum_png())


@negative("a frame whose TERMINAL chunk checksum is wrong",
          "wrong terminal checksum")
def _tampered_iend_frame(fx: Fixture) -> None:
    # Neither library pass reaches IEND — `verify()` breaks on it and
    # the decoder never gets there — so without the terminal check this
    # file validates clean. `_both_decode_passes_earn_their_keep` pins
    # that both of them really do accept it.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/hero/animations/walk/north/frame_000.png",
        tampered_iend_png())


@negative("a frame with data appended past IEND",
          "byte(s) follow the IEND chunk")
def _trailing_data_frame(fx: Fixture) -> None:
    # IEND is the final chunk by specification, so anything after it is
    # not part of the image.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/hero/animations/walk/west/frame_000.png",
        trailing_data_png())


@negative("a frame with a SECOND canonical IEND appended",
          "byte(s) follow the IEND chunk")
def _duplicate_iend_frame(fx: Fixture) -> None:
    # The adversarial trailing-data case: the file's last twelve bytes
    # are a perfect IEND chunk, so comparing the FILE's tail accepts it
    # while the real image ended twelve bytes earlier.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/hero/animations/walk/south-west/frame_000.png",
        duplicate_iend_png())


@negative("a frame with an extra chunk before a second IEND",
          "byte(s) follow the IEND chunk")
def _chunk_then_iend_frame(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/hero/animations/walk/north-west/frame_000.png",
        chunk_then_iend_png())


@negative("a non-image file wearing a .png name",
          "walk/east/frame_001.png: cannot decode as an image")
def _non_image_frame(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/hero/animations/walk/east/frame_001.png",
        not_an_image())


@negative("a valid image of another format renamed to .png",
          "expected a PNG, got BMP")
def _other_format_frame(fx: Fixture) -> None:
    # Decodable is not enough: the engine's loader is a PNG loader, so
    # a readable BMP under a `.png` name is still a broken asset.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/north/frame_000.png",
        other_format_png())


@negative("frames of one animation disagreeing on size ACROSS directions",
          "inconsistent frame dimensions")
def _size_mismatch_across_directions(fx: Fixture) -> None:
    # EVERY frame of the odd direction is resized, so that direction is
    # internally consistent and only the animation-wide comparison can
    # see the problem. A check that reset per direction would pass.
    valid_fixture(fx)
    for index in range(2):
        fx.write_file(
            f"assets/textures/units/prop/animations/spin/east/"
            f"frame_{index:03d}.png", png_bytes(8, 8))


@negative("frames of one animation disagreeing on size WITHIN a direction",
          "inconsistent frame dimensions")
def _size_mismatch_within_direction(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/hero/animations/idle/south/frame_001.png",
        png_bytes(6, 3))


@negative("malformed YAML", "YAML parse error")
def _bad_yaml(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("hero", "units:\n  - name: hero\n   sprite: [unclosed\n")


@negative("a file declaring neither top-level key", "declares neither")
def _no_key(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("stray", "unit:\n  - name: hero\n")


@negative("a file whose only key is an explicit null",
          "present but null")
def _null_units_key(fx: Fixture) -> None:
    # `data.get(key) is None` cannot tell an explicit null from an
    # absent key, so this passed the "declares neither" check and then
    # skipped silently — while the Haskell loader refuses the file.
    valid_fixture(fx)
    fx.yaml("stray", "units: null\n")


@negative("an asset-only key present but null", "present but null")
def _null_asset_units_key(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("stray", "asset_units: null\n")


@negative("a key present with no value at all", "present but null")
def _empty_units_key(fx: Fixture) -> None:
    # `units:` with nothing after it is the same None to PyYAML.
    valid_fixture(fx)
    fx.yaml("stray", "units:\n")


@negative("an asset-only entry carrying gameplay fields",
          "carries gameplay field")
def _asset_only_gameplay(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("    animations:\n",
                     '    sprite: "assets/textures/units/hero/idle.png"\n'
                     "    animations:\n"))


@negative("an asset-only entry carrying an unknown field",
          "unknown field")
def _asset_only_unknown_field(fx: Fixture) -> None:
    # A blacklist of gameplay fields would wave this through; the schema
    # is a whitelist of exactly name + animations.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("    animations:\n", "    typo: true\n    animations:\n"))


@negative("an asset-only entry mixing non-string and string unknown keys",
          "unknown field")
def _mixed_type_unknown_keys(fx: Fixture) -> None:
    # `sorted` over raw YAML keys crashes here: 123 is not orderable
    # against "typo". A crash is not a clear malformed-declaration
    # diagnostic, so the keys are sorted by their rendered form.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("    animations:\n",
                     "    123: true\n    typo: true\n    animations:\n"))


@negative("a declared frame path that is a DIRECTORY",
          "declared frame is a directory")
def _declared_path_is_a_directory(fx: Fixture) -> None:
    # The invariant is "exists as a regular file", not merely "exists" —
    # and a directory deserves its own diagnostic rather than the
    # missing-file one.
    valid_fixture(fx)
    fx.rm("assets/textures/units/prop/animations/spin/south/frame_001.png")
    (fx.root / "assets/textures/units/prop/animations/spin/south"
             / "frame_001.png").mkdir()


@negative("a non-string animation key", "animation key must be a string")
def _numeric_anim_key(fx: Fixture) -> None:
    # YAML resolves an unquoted `123:` to an int, and str(123) == "123"
    # satisfies the identifier rule — so coercion would let a non-string
    # key name a real animation directory.
    valid_fixture(fx)
    fx.frames("prop", "123", CANON5, 2)
    body = asset_only_yaml("prop", [("123", CANON5, 2, True)])
    fx.yaml("prop", body.replace("      123:\n", "      123:\n", 1))


@negative("a non-string direction key", "direction key must be a string")
def _numeric_direction_key(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("          north:\n", "          123:\n"))


@positive("a QUOTED numeric-looking animation name is still accepted")
def _quoted_numeric_anim(fx: Fixture) -> None:
    # The rule is about the KEY's YAML type, not about digits: a
    # deliberately quoted "123" is a string and a legal identifier.
    fx.frames("prop", "123", CANON5, 2)
    body = asset_only_yaml("prop", [("123", CANON5, 2, True)])
    fx.yaml("prop", body.replace("      123:\n", '      "123":\n', 1))


@negative("a gameplay entry with no sprite", "missing required `sprite:`")
def _no_sprite(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("hero", gameplay_yaml("hero", [
        ("idle", CANON5, 3, True), ("walk", ALL8, 2, False)])
        .replace('    sprite: "assets/textures/units/hero/idle.png"\n', ""))


@negative("an unsafe unit identifier", "unsafe unit identifier")
def _bad_unit_name(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("- name: prop", "- name: ../escape"))


@negative("an unsafe animation identifier", "unsafe animation identifier")
def _bad_anim_name(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("      spin:\n", "      spin/../x:\n"))


@negative("an unknown direction key in a declaration", "unknown direction key")
def _bad_dir_key(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("          north:\n", "          upward:\n"))


@negative("an unknown direction directory on disk",
          "unknown direction directory")
def _bad_dir_dir(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "spin", ["upward"], 1)


@negative("a frame filename that is not frame_NNN.png",
          "must match frame_NNN.png")
def _bad_frame_name(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/sheet.png",
        png_bytes())


@negative("a wrong-case file extension", "must match frame_NNN.png")
def _wrong_case_extension(fx: Fixture) -> None:
    # The extension rule is case-SENSITIVE. `.PNG` is still walked as an
    # image (the suffix test lowercases), so it must be reported as a
    # bad frame name rather than silently ignored as a non-PNG.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_002.PNG",
        png_bytes())


@negative("a frame FILE whose name ends in a newline",
          "must match frame_NNN.png")
def _newline_in_frame_name(fx: Fixture) -> None:
    # A newline is a legal POSIX filename character, and `$` matches
    # just before a trailing one — so with `^...$` this file passes the
    # name rule, exists on disk, and is claimed by its declaration,
    # leaving NO error at all. Only \Z rejects it. The file is both
    # created and declared for exactly that reason: a disk-only version
    # would still fail as "unclassified", masking the bug.
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_002.png\n",
        png_bytes())
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "south", 2, " " * 12),
        frame_lines("prop", "spin", "south", 2, " " * 12)
        + '            "assets/textures/units/prop/animations/spin/south/'
          'frame_002.png\\n"\n'.replace('            "', '            - "'))
    fx.yaml("prop", body)


@negative("an animation key ending in a newline",
          "unsafe animation identifier")
def _newline_in_anim_key(fx: Fixture) -> None:
    # `$` matches just BEFORE a trailing newline, so an `^...$` rule
    # used with `match` accepts this; only \Z or fullmatch rejects it.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("      spin:\n", '      "spin\\n":\n'))


@negative("a unit name ending in a newline", "unsafe unit identifier")
def _newline_in_unit_name(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("  - name: prop\n", '  - name: "prop\\n"\n'))


@negative("an absolute declared path", "absolute path is not allowed")
def _absolute(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace('"assets/textures/units/prop/animations/spin/south/'
                     'frame_000.png"',
                     '"/assets/textures/units/prop/animations/spin/south/'
                     'frame_000.png"'))


@negative("a declared path with .. traversal", "traversal is not allowed")
def _traversal(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace('"assets/textures/units/prop/animations/spin/south/'
                     'frame_000.png"',
                     '"assets/textures/units/prop/animations/../../../../'
                     'etc/passwd"'))


@negative("a symlink escape in the ownership path",
          "symlink in the ownership path")
def _symlink_escape(fx: Fixture) -> None:
    valid_fixture(fx)
    outside = fx.root.parent / "outside_frames"
    outside.mkdir(exist_ok=True)
    (outside / "frame_000.png").write_bytes(png_bytes())
    (outside / "frame_001.png").write_bytes(png_bytes())
    fx.symlink("assets/textures/units/prop/animations/spin/south",
               str(outside))


@negative("a symlinked frame file on disk", "symlinked frame")
def _symlink_frame(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.symlink("assets/textures/units/prop/animations/spin/south/frame_002.png",
               "frame_000.png")


@negative("a cross-unit reference", "cross-unit reference")
def _cross_unit(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace('"assets/textures/units/prop/animations/spin/south/'
                     'frame_000.png"',
                     '"assets/textures/units/hero/animations/idle/south/'
                     'frame_000.png"'))


@negative("a cross-animation reference", "cross-animation reference")
def _cross_anim(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "other", CANON5, 2)
    fx.yaml("prop", asset_only_yaml(
        "prop", [("spin", CANON5, 2, True), ("other", CANON5, 2, True)])
        .replace('"assets/textures/units/prop/animations/spin/south/'
                 'frame_000.png"',
                 '"assets/textures/units/prop/animations/other/south/'
                 'frame_000.png"', 1))


@negative("a cross-direction reference", "cross-direction reference")
def _cross_dir(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace('"assets/textures/units/prop/animations/spin/south/'
                     'frame_000.png"',
                     '"assets/textures/units/prop/animations/spin/east/'
                     'frame_000.png"', 1))


@negative("a duplicate animation-frame claim",
          "duplicate animation-frame claim")
def _duplicate_claim(fx: Fixture) -> None:
    # Containment already rejects a second slot in another unit,
    # animation, or direction, so the only shape that can reach the
    # claim ledger is one direction list naming the same physical frame
    # twice. It necessarily also trips the numbering rule; this case
    # asserts the CLAIM diagnostic specifically, and `_dupe_index`
    # asserts the numbering one, so neither can mask the other.
    valid_fixture(fx)
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "east", 2, " " * 12),
        frame_lines("prop", "spin", "east", 2, " " * 12)
        + frame_lines("prop", "spin", "east", 1, " " * 12, start=1))
    fx.yaml("prop", body)


@negative("flip: true with an eight-direction set",
          "flip: true requires exactly the canonical five")
def _flip_true_eight(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "spin", ["north-west", "west", "south-west"], 2)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", ALL8, 2, True)]))


@negative("flip: false with a five-direction set",
          "flip: false requires exactly all eight")
def _flip_false_five(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, False)]))


@negative("a frame sequence that does not begin at frame_000",
          "must begin at frame_000.png")
def _missing_zero(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.rm("assets/textures/units/prop/animations/spin/south/frame_000.png")
    fx.frames("prop", "spin", ["south"], 1, start=2)
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "south", 2, " " * 12),
        frame_lines("prop", "spin", "south", 2, " " * 12, start=1))
    fx.yaml("prop", body)


@negative("a gap in frame numbering", "gap in frame numbering")
def _gap(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "spin", ["south"], 1, start=3)
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "south", 2, " " * 12),
        frame_lines("prop", "spin", "south", 2, " " * 12)
        + frame_lines("prop", "spin", "south", 1, " " * 12, start=3))
    fx.yaml("prop", body)


@negative("a duplicate frame index", "duplicate frame index")
def _dupe_index(fx: Fixture) -> None:
    valid_fixture(fx)
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "south", 2, " " * 12),
        frame_lines("prop", "spin", "south", 2, " " * 12)
        + frame_lines("prop", "spin", "south", 1, " " * 12))
    fx.yaml("prop", body)


@negative("a declared frame that is missing from disk", "missing file")
def _missing_declared(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.rm("assets/textures/units/prop/animations/spin/north/frame_001.png")


@negative("an unclassified frame present on disk", "unclassified frame on disk")
def _unclassified(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "spin", CANON5, 1, start=2)


@negative("a whole asset tree with no declaration at all",
          "unclassified frame on disk")
def _undeclared_tree(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("ghost", "idle", CANON5, 2)


@negative("two files declaring the same unit name", "is already declared in")
def _duplicate_unit(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop_copy", asset_only_yaml("prop", [("spin", CANON5, 2, True)]))


@negative("a contiguous but out-of-order frame list", "out of order")
def _out_of_order(fx: Fixture) -> None:
    # Every set-based check passes here: indices are {0, 1}, start at 0,
    # no gap, no duplicate. Only the ORDER is wrong, and playback walks
    # the declared list in order.
    valid_fixture(fx)
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    ordered = frame_lines("prop", "spin", "south", 2, " " * 12)
    reversed_ = "".join(reversed(ordered.splitlines(keepends=True)))
    body = body.replace(ordered, reversed_)
    fx.yaml("prop", body)


@negative("an `fps:` that is not a number", "`fps:` must be a number")
def _fps_not_a_number(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: fast\n"))


@negative("an `fps:` of zero", "`fps:` must be positive")
def _fps_not_positive(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: 0\n"))


@negative("a boolean `fps:`", "`fps:` must be a number")
def _fps_boolean(fx: Fixture) -> None:
    # bool is an int subclass in Python, so a naive isinstance check
    # lets `fps: true` through as the number 1.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: true\n"))


@negative("a NaN `fps:`", "`fps:` must be a finite")
def _fps_nan(fx: Fixture) -> None:
    # `nan <= 0` is False — every NaN comparison is — so a positivity
    # test alone lets this through.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: .nan\n"))


@negative("an infinite `fps:`", "`fps:` must be a finite")
def _fps_infinite(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: .inf\n"))


@negative("a negative-infinity `fps:`", "`fps:` must be a finite")
def _fps_negative_infinite(fx: Fixture) -> None:
    # Caught by the positivity test too, but it must report the FINITE
    # diagnostic: the finiteness check has to run first.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: -.inf\n"))


@negative("an `fps:` integer too large to represent as a float",
          "`fps:` must be a finite")
def _fps_unrepresentable(fx: Fixture) -> None:
    # A Python int has unbounded precision, so this is a perfectly valid
    # YAML integer — and `math.isfinite` RAISES on it rather than
    # answering, which crashed the validator instead of diagnosing it.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: " + "9" * 4000 + "\n"))


@negative("an `fps:` that overflows the engine's 32-bit Float",
          "32-bit Float")
def _fps_overflows_runtime_float(fx: Fixture) -> None:
    # Fits a Python double, so every earlier check passes; loads as
    # Infinity in UnitYamlAnim's Float field.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: 1.0e+100\n"))


@negative("an `fps:` that underflows the engine's 32-bit Float",
          "32-bit Float")
def _fps_underflows_runtime_float(fx: Fixture) -> None:
    # Positive and finite as a double; loads as 0.
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: 1.0e-100\n"))


@positive("a large but representable fps is still accepted")
def _fps_large_but_finite(fx: Fixture) -> None:
    # The other direction: the rule is about representability, not about
    # magnitude, so an absurd-but-finite rate must not be rejected here.
    fx.frames("prop", "spin", CANON5, 2)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: 1000000\n"))


@negative("a `loop:` that is not a boolean", "`loop:` must be a boolean")
def _loop_not_a_boolean(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        loop: true\n", "        loop: sometimes\n"))


@positive("a fractional fps and YAML's own boolean spellings are accepted")
def _legal_scalars(fx: Fixture) -> None:
    # The other direction: tightening these types must not reject a
    # legitimate non-integer rate, nor YAML 1.1's `yes`/`no` booleans,
    # which safe_load resolves to real bools before this code sees them.
    fx.frames("prop", "spin", CANON5, 2)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("        fps: 8\n", "        fps: 7.5\n")
            .replace("        loop: true\n", "        loop: yes\n"))


@negative("an unknown --unit target", "no such unit",
          unit="definitely_not_a_unit")
def _unknown_unit_target(fx: Fixture) -> None:
    # Must not read as a clean run of an empty inventory: before this,
    # a typo exited 0 reporting "0 unit declaration(s), 0 frame(s)".
    valid_fixture(fx)


@positive("a --unit run restricted to a real unit still validates",
          unit="prop")
def _known_unit_target(fx: Fixture) -> None:
    # The other half: narrowing to a unit that DOES exist must stay a
    # pass, so the check above cannot be satisfied by rejecting every
    # --unit invocation.
    valid_fixture(fx)


@negative("a loose file at the direction level",
          "loose file at the direction level")
def _loose_direction(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file("assets/textures/units/prop/animations/spin/notes.png",
                  png_bytes())


@negative("an arbitrary mixed-case animation name outside the approved "
          "exception", "unsafe animation identifier")
def _mixed_case_anim(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("      spin:\n", "      AnyThing:\n"))


@negative("a near-miss of the approved _RH_ exception (upper-case weapon)",
          "unsafe animation identifier")
def _rh_near_miss(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.yaml("prop", asset_only_yaml("prop", [("spin", CANON5, 2, True)])
            .replace("      spin:\n", "      attack_RH_Dagger:\n"))


@negative("a mixed-case animation DIRECTORY outside the approved exception",
          "unsafe animation directory name")
def _mixed_case_anim_dir(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.frames("prop", "AnyThing", CANON5, 1)


@negative("an unpadded frame filename", "must match frame_NNN.png")
def _unpadded_frame(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_2.png",
        png_bytes())


@negative("an over-padded frame filename", "must match frame_NNN.png")
def _overpadded_frame(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_0002.png",
        png_bytes())


@negative("an unpadded frame DECLARATION", "must match frame_NNN.png")
def _unpadded_declared(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.write_file(
        "assets/textures/units/prop/animations/spin/south/frame_2.png",
        png_bytes())
    body = asset_only_yaml("prop", [("spin", CANON5, 2, True)])
    body = body.replace(
        frame_lines("prop", "spin", "south", 2, " " * 12),
        frame_lines("prop", "spin", "south", 2, " " * 12)
        + frame_lines("prop", "spin", "south", 1, " " * 12, start=2,
                      name=lambda i: f"frame_{i}.png"))
    fx.yaml("prop", body)


@negative("a symlinked unit directory", "symlinked unit directory")
def _symlinked_unit(fx: Fixture) -> None:
    # The bypass this closes: a skipped symlink meant a whole unit tree
    # could ship without ever entering the filesystem-first walk.
    valid_fixture(fx)
    fx.symlink("assets/textures/units/ghost", "prop")


@negative("a symlinked animations/ root", "symlinked animations/ directory")
def _symlinked_anim_root(fx: Fixture) -> None:
    valid_fixture(fx)
    (fx.root / "assets/textures/units/ghost").mkdir(parents=True)
    fx.symlink("assets/textures/units/ghost/animations",
               str(fx.root / "assets/textures/units/prop/animations"))


@negative("a symlinked animation directory", "symlinked animation directory")
def _symlinked_anim(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.symlink("assets/textures/units/prop/animations/twin", "spin")


@negative("a symlinked direction directory", "symlinked direction directory")
def _symlinked_direction(fx: Fixture) -> None:
    valid_fixture(fx)
    fx.symlink("assets/textures/units/prop/animations/spin/west", "south")


# The façade's single entry point into this owner. Frozen here, after
# every decorator above has run, so the tuples are this file's complete
# contribution in definition order.
CASES: OwnerCases = _CASES.freeze()
