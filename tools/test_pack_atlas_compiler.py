#!/usr/bin/env python3
"""
test_pack_atlas_compiler.py — the compiler and stored-index scenarios of
`tools/test_pack_atlas.py` (#2061 owner split).

A SCENARIO is a #1258 compiler case, which needs more than an exit
code: it inspects the emitted atlas pixels and index document, or
observes which files a second run actually wrote. Deterministic
compilation, incremental write locality, atlas layout and digests,
`--check`, stored-index and generated-artifact freshness, and the
decoder and CI toolchain contracts all live here.

Compilation necessarily decodes, so its scenarios assert pixels. Two
scenarios pin why BOTH of validation's decode passes exist by showing a
file that each one alone accepts.

Not runnable on its own: it parses no arguments and executes no case at
import. `tools/test_pack_atlas.py` imports `CASES` and runs it.
"""
from __future__ import annotations

import json
import os
import shutil
import struct
import sys
from pathlib import Path
from typing import Dict

sys.path.insert(0, str(Path(__file__).resolve().parent))

import pack_atlas_compiler  # noqa: E402
import pack_atlas_declarations  # noqa: E402
import pack_atlas_image  # noqa: E402
import pack_atlas_index  # noqa: E402
import pack_atlas_shared  # noqa: E402
from test_pack_atlas_support import (  # noqa: E402
    ALL8, CANON5, CELL, EXPECTED_ROW_ORDER, CaseRegistry, Fixture,
    OwnerCases, anim_yaml_ragged, assert_atlas_matches, atlas_slots,
    bad_checksum_png, build_unit, corrupt_stream_png,
    decode_rgba_texels, duplicate_iend_png, entry, freeze_mtimes,
    gradient_png, png_bytes, rgba_png, tampered_iend_png, uniform,
    valid_fixture, written_since_freeze,
)

_CASES = CaseRegistry("compiler")
scenario = _CASES.scenario

@scenario("each cell's gutter is its OWN edge texels, corners included")
def _compile_extrusion_ring(fx: Fixture) -> None:
    """#2076's core guarantee, measured texel by texel.

    Two directions of two frames each, every frame a distinct gradient,
    so a gutter sourced from the wrong row, the wrong column, the
    neighbouring cell, or the wrong corner is a different colour than
    the one this asserts.
    """
    unit, anim = "hero", "idle"
    width, height = 5, 4
    pad = pack_atlas_shared.CELL_PADDING
    # `flip: true` over the canonical five, so the layout is legal with
    # only a couple of rows of art to reason about.
    directions = CANON5
    counts = {d: 2 for d in directions}
    art: Dict[tuple[str, int], bytes] = {}
    for index, direction in enumerate(directions):
        target = (fx.root / "assets" / "textures" / "units" / unit
                  / "animations" / anim / direction)
        target.mkdir(parents=True, exist_ok=True)
        for i in range(counts[direction]):
            seed = index * 11 + i * 3 + 1
            art[(direction, i)] = gradient_png(width, height, seed)
            (target / f"frame_{i:03d}.png").write_bytes(art[(direction, i)])
    fx.yaml(unit, f"asset_units:\n  - name: {unit}\n    animations:\n"
            + anim_yaml_ragged(unit, anim, counts, True))
    fx.compile_ok()

    record = entry(fx.index(unit), anim)
    assert record["cell_padding"] == pad
    assert record["cell_width"] == width and record["cell_height"] == height
    assert record["atlas_width"] == 2 * (width + 2 * pad)
    assert record["atlas_height"] == len(directions) * (height + 2 * pad)

    slots = atlas_slots(fx.atlas_path(unit, anim), width, height)
    ordered = [d for d in EXPECTED_ROW_ORDER if d in counts]
    for row, direction in enumerate(ordered):
        for column in range(counts[direction]):
            frame = decode_rgba_texels(art[(direction, column)])
            slot = slots[row][column]
            for sy in range(height + 2 * pad):
                for sx in range(width + 2 * pad):
                    # The extrusion rule, stated independently of the
                    # compiler: clamp the slot coordinate into the cell
                    # on BOTH axes. A side takes its adjacent edge
                    # texel; a corner square takes the one corner texel
                    # it touches.
                    cx = min(max(sx - pad, 0), width - 1)
                    cy = min(max(sy - pad, 0), height - 1)
                    assert slot[sy][sx] == frame[cy][cx], (
                        f"{unit}/{anim} slot ({row},{column}) texel "
                        f"({sx},{sy}) is {slot[sy][sx]}, expected the "
                        f"cell's ({cx},{cy}) = {frame[cy][cx]}")

    # And the cells really are distinguishable, so the sweep above is
    # not passing on frames that happen to agree.
    distinct = {tuple(tuple(line) for line in
                      decode_rgba_texels(art[(d, i)]))
                for d in directions for i in range(counts[d])}
    assert len(distinct) == sum(counts.values())


@scenario("a five-direction mirroring layout compiles to five ordered rows")
def _compile_five_direction(fx: Fixture) -> None:
    counts = uniform(CANON5, 3)
    build_unit(fx, "hero", [("idle", counts, True)])
    fx.compile_ok()
    record = assert_atlas_matches(fx, "hero", "idle", counts)
    assert record["flip"] is True
    assert record["rows"] == 5
    assert record["storage_format"] == "png"
    assert record["atlas_path"] == \
        "assets/textures/units/hero/atlas/idle.png"
    fx.validate_ok()


@scenario("eight-direction artwork compiles to all eight rows, flip false")
def _compile_eight_direction(fx: Fixture) -> None:
    counts = uniform(ALL8, 2)
    build_unit(fx, "hero", [("walk", counts, False)])
    fx.compile_ok()
    record = assert_atlas_matches(fx, "hero", "walk", counts)
    assert record["flip"] is False
    assert record["rows"] == 8
    assert [d["direction"] for d in record["directions"]] == EXPECTED_ROW_ORDER
    fx.validate_ok()


@scenario("unequal direction lengths keep real counts and pad the remainder")
def _compile_unequal_lengths(fx: Fixture) -> None:
    # The shipped shape this mirrors: four acolyte animations author
    # more south frames than eastern ones.
    counts = {"south": 4, "south-east": 2, "east": 1, "north-east": 3,
              "north": 2}
    build_unit(fx, "hero", [("idle", counts, True)])
    fx.compile_ok()
    record = assert_atlas_matches(fx, "hero", "idle", counts)
    assert record["columns"] == 4
    assert sum(d["frame_count"] for d in record["directions"]) == 12
    fx.validate_ok()


@scenario("the index carries a schema version distinct from the tool version")
def _index_versions(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    document = fx.index("hero")
    assert document["schema_version"] == pack_atlas_shared.INDEX_SCHEMA_VERSION
    assert document["tool_version"] == pack_atlas_shared.TOOL_VERSION
    assert "schema_version" in document and "tool_version" in document
    assert document["digest_algorithm"] == "sha256"
    assert document["direction_order"] == EXPECTED_ROW_ORDER
    assert document["unit"] == "hero"
    # Per-animation digests, not one unit-wide value: an animation's
    # edit must not invalidate its neighbours.
    digests = {a["name"]: a["source_digest"] for a in document["animations"]}
    assert all(len(d) == 64 for d in digests.values())


@scenario("playback metadata reaches the index as the engine will hold it")
def _playback_metadata(fx: Fixture) -> None:
    counts = uniform(ALL8, 2)
    build_unit(fx, "hero", [("walk", counts, False)])
    fx.yaml("hero", (fx.root / "data/units/hero.yaml").read_text("utf-8")
            .replace("        fps: 8\n", "        fps: 0.1\n")
            .replace("        loop: true\n", "        loop: false\n"))
    fx.compile_ok()

    record = entry(fx.index("hero"), "walk")
    assert record["loop"] is False, record
    # Recorded as the engine's 32-bit `Float` will actually hold it, not
    # as the decimal the file spells: an index promising a rate the
    # runtime rounds would misdescribe playback.
    assert record["fps"] == pack_atlas_image.narrow_to_runtime_float(0.1), (
        record)
    assert record["fps"] != 0.1, (
        "fps was recorded at double precision, not narrowed")
    fx.validate_ok()


@scenario("editing only fps makes the index stale")
def _fps_edit_is_a_change(fx: Fixture) -> None:
    # `fps` is a compiled input even though it moves no pixel: the index
    # is what the runtime reads, so an index still claiming the old rate
    # is stale exactly like a changed frame.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    fx.yaml("hero", (fx.root / "data/units/hero.yaml").read_text("utf-8")
            .replace("        fps: 8\n", "        fps: 12\n"))
    fx.validate_fails("stale atlas")

    fx.compile_ok()
    assert entry(fx.index("hero"), "idle")["fps"] == 12.0
    fx.validate_ok()


@scenario("two clean builds are byte-identical")
def _clean_build_determinism(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 3), True),
                            ("walk", uniform(ALL8, 2), False)])
    fx.compile_ok()
    first = {p.name: p.read_bytes() for p in fx.artifacts("hero")}
    assert set(first) == {"idle.png", "walk.png", "index.json"}

    shutil.rmtree(fx.atlas_dir("hero"))
    fx.compile_ok()
    second = {p.name: p.read_bytes() for p in fx.artifacts("hero")}
    assert first == second, (
        "a clean rebuild from identical sources produced different bytes: "
        + ", ".join(sorted(k for k in set(first) | set(second)
                           if first.get(k) != second.get(k))))


@scenario("an isolated edit rewrites only that atlas and its unit index")
def _incremental_write_locality(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True),
                            ("walk", uniform(ALL8, 2), False)])
    build_unit(fx, "prop", [("spin", uniform(CANON5, 2), True)])
    fx.compile_ok()

    watched = fx.artifacts("hero", "prop")
    freeze_mtimes(watched)
    fx.write_file(
        "assets/textures/units/hero/animations/idle/east/frame_001.png",
        rgba_png(CELL[0], CELL[1], (1, 2, 3, 255)))
    fx.compile_ok()

    written = written_since_freeze(watched)
    hero_written = sorted(
        p.name for p in fx.artifacts("hero") if p.stat().st_mtime_ns != 0)
    prop_written = sorted(
        p.name for p in fx.artifacts("prop") if p.stat().st_mtime_ns != 0)
    assert hero_written == ["idle.png", "index.json"], (
        f"expected only hero's idle atlas and index to be rewritten, got "
        f"{hero_written} (all: {written})")
    assert prop_written == [], (
        f"another unit's artifacts were rewritten: {prop_written}")
    fx.validate_ok()


@scenario("a no-op recompile writes nothing at all")
def _incremental_noop(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    watched = fx.artifacts("hero")
    freeze_mtimes(watched)
    output = fx.compile_ok()
    assert written_since_freeze(watched) == [], (
        f"an up-to-date tree was rewritten:\n{output}")
    assert "wrote 0 artifact(s)" in output, output


@scenario("an mtime-only touch of a frame is not a change")
def _mtime_touch_is_not_a_change(fx: Fixture) -> None:
    # The digest is over CONTENT, so a rebuild trigger keyed on
    # timestamps would rewrite artifacts nothing had actually changed.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    source = (fx.root / "assets/textures/units/hero/animations/idle/south"
              / "frame_000.png")
    os.utime(source, (0, 0))
    code, output = fx.compile(check=True)
    assert code == 0, f"an mtime-only touch was reported as stale:\n{output}"


@scenario("--check reports out-of-date artifacts and writes nothing")
def _check_mode(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    watched = fx.artifacts("hero")
    freeze_mtimes(watched)
    fx.write_file(
        "assets/textures/units/hero/animations/idle/north/frame_000.png",
        rgba_png(CELL[0], CELL[1], (9, 9, 9, 255)))

    code, output = fx.compile(check=True)
    assert code != 0, f"--check passed on a stale tree:\n{output}"
    assert "would write" in output, output
    assert written_since_freeze(watched) == [], (
        "--check wrote artifacts: "
        + ", ".join(written_since_freeze(watched)))


@scenario("a source edit makes validation report that animation stale")
def _stale_source(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True),
                            ("walk", uniform(ALL8, 2), False)])
    fx.compile_ok()
    fx.write_file(
        "assets/textures/units/hero/animations/idle/south/frame_000.png",
        rgba_png(CELL[0], CELL[1], (5, 5, 5, 255)))
    output = fx.validate_fails("stale atlas")
    assert "hero/atlas/idle" in output, output
    assert "/walk" not in output, (
        f"an unrelated animation was reported stale:\n{output}")


@scenario("a tampered atlas is rejected even though its index is untouched")
def _tampered_atlas(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    record = entry(fx.index("hero"), "idle")
    fx.atlas_path("hero", "idle").write_bytes(
        rgba_png(record["atlas_width"], record["atlas_height"],
                 (7, 7, 7, 255)))
    fx.validate_fails("atlas content does not match its sources")


@scenario("an index whose digests were rewritten to match a tampered atlas "
          "is still rejected")
def _forged_index(fx: Fixture) -> None:
    # The check regenerates from sources rather than trusting the
    # numbers the file carries about itself, so making the index
    # self-consistent with a forged atlas does not launder it.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    document = fx.index("hero")
    record = entry(document, "idle")
    forged = rgba_png(record["atlas_width"], record["atlas_height"],
                      (7, 7, 7, 255))
    fx.atlas_path("hero", "idle").write_bytes(forged)
    frame = pack_atlas_image.decode_rgba8(fx.atlas_path("hero", "idle"))
    record["atlas_digest"] = pack_atlas_compiler.content_digest(
        frame.width, frame.height, frame.pixels)
    fx.index_path("hero").write_bytes(
        pack_atlas_compiler.canonical_index_bytes(document))
    fx.validate_fails("index entry disagrees with a fresh compile")


@scenario("a hand-edited index value is rejected")
def _hand_edited_index(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    document = fx.index("hero")
    document["tool_version"] = document["tool_version"] + 1
    fx.index_path("hero").write_bytes(
        pack_atlas_compiler.canonical_index_bytes(document))
    fx.validate_fails("generated-index metadata mismatch")


@scenario("a duplicated animation entry in the index is rejected")
def _duplicate_index_entry(fx: Fixture) -> None:
    # Keying the stored entries by name to diagnose them is exactly
    # what swallows this: two copies of one VALID entry collapse to a
    # dict identical to a fresh compile's, so every per-entry check
    # passes while the file plainly differs.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True),
                            ("walk", uniform(ALL8, 2), False)])
    fx.compile_ok()
    document = fx.index("hero")
    document["animations"].append(document["animations"][0])
    fx.index_path("hero").write_text(
        json.dumps(document, indent=2) + "\n", encoding="utf-8")
    fx.validate_fails("duplicate entry for animation 'idle'")


@scenario("a reordered animations list is rejected")
def _reordered_index_entries(fx: Fixture) -> None:
    # Same class as the duplicate above: the set of names is right and
    # every entry is byte-correct, so only the ORDER differs.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True),
                            ("walk", uniform(ALL8, 2), False)])
    fx.compile_ok()
    document = fx.index("hero")
    document["animations"].reverse()
    fx.index_path("hero").write_text(
        json.dumps(document, indent=2) + "\n", encoding="utf-8")
    fx.validate_fails("not in canonical name-sorted order")


@scenario("a mismatch the diagnostics cannot name still fails")
def _unnamed_mismatch_backstop(fx: Fixture) -> None:
    # The whole-document comparison is the authority; the drill-down
    # only says WHERE. This pins the backstop directly by blinding the
    # drill-down, so a future edit shape nobody anticipated cannot be
    # accepted merely because no diagnostic recognised it.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    document = fx.index("hero")
    document["animations"].append(document["animations"][0])
    fx.index_path("hero").write_text(
        json.dumps(document, indent=2) + "\n", encoding="utf-8")

    # Patched on the INDEX owner: `validate_unit_index` looks the name
    # up on its own module at call time, so a patch on the façade would
    # be a no-op and this case would pass vacuously.
    blinded = pack_atlas_index.report_index_mismatch
    pack_atlas_index.report_index_mismatch = lambda *a, **k: None
    try:
        fx.validate_fails("does not match a fresh compile")
    finally:
        pack_atlas_index.report_index_mismatch = blinded


@scenario("a reformatted but semantically identical index is rejected")
def _noncanonical_index(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    document = fx.index("hero")
    fx.index_path("hero").write_text(
        json.dumps(document, indent=4) + "\n", encoding="utf-8")
    fx.validate_fails("not canonically serialized")


@scenario("a missing indexed atlas is rejected")
def _missing_indexed_atlas(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    fx.atlas_path("hero", "idle").unlink()
    fx.validate_fails("indexed atlas is missing from disk")


@scenario("an atlas directory with no index is rejected")
def _atlas_dir_without_index(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    fx.index_path("hero").unlink()
    fx.validate_fails("has no index.json")


@scenario("a unit with no generated artifacts stays valid")
def _uncompiled_unit_without_index(fx: Fixture) -> None:
    # Absence of an index is legitimate to THIS tool: an uncompiled
    # tree is an ordinary working-copy state between adding art and
    # running --compile, so the freshness checks must not make it an
    # error. The ENGINE is stricter — since #1261 it refuses to
    # register a unit that declares animations and ships no artifacts,
    # and every shipped unit is compiled and tracked.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.validate_ok()
    assert not fx.atlas_dir("hero").exists()


@scenario("a generated directory for an undeclared unit is rejected")
def _orphan_atlas_directory(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    fx.compile_ok()
    shutil.copytree(fx.atlas_dir("hero"), fx.atlas_dir("ghost"))
    fx.validate_fails("no declaration in data/units/")


@scenario("inconsistent frame dimensions refuse to compile")
def _inconsistent_dimensions(fx: Fixture) -> None:
    counts = uniform(CANON5, 2)
    build_unit(fx, "hero", [("idle", counts, True)])
    fx.write_file(
        "assets/textures/units/hero/animations/idle/east/frame_001.png",
        rgba_png(CELL[0] + 1, CELL[1], (4, 4, 4, 255)))
    code, output = fx.compile()
    assert code != 0, f"a ragged cell size compiled:\n{output}"
    assert "inconsistent frame dimensions" in output, output
    assert not fx.atlas_dir("hero").exists(), (
        "a failed compile left artifacts behind")


@scenario("a symlinked atlas output directory refuses to compile")
def _symlinked_output_directory(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    outside = fx.root.parent / "outside_atlas"
    outside.mkdir(exist_ok=True)
    fx.symlink("assets/textures/units/hero/atlas", str(outside))
    code, output = fx.compile()
    assert code != 0, f"a symlinked output directory compiled:\n{output}"
    assert "symlink in the atlas output path" in output, output
    assert list(outside.iterdir()) == [], (
        "the compiler wrote through a symlink: "
        + ", ".join(p.name for p in outside.iterdir()))


@scenario("obsolete output is reported, then removed, and neighbours survive")
def _obsolete_output(fx: Fixture) -> None:
    hero = [("idle", uniform(CANON5, 2), True),
            ("walk", uniform(ALL8, 2), False)]
    build_unit(fx, "hero", hero)
    build_unit(fx, "prop", [("spin", uniform(CANON5, 2), True)])
    fx.compile_ok()
    prop_before = {p.name: p.read_bytes() for p in fx.artifacts("prop")}

    # Delete one animation the way a real rename or removal would:
    # source frames and declaration together.
    shutil.rmtree(fx.root / "assets/textures/units/hero/animations/walk")
    build_unit(fx, "hero", [hero[0]])
    fx.validate_fails("obsolete compiler-owned output")

    fx.compile_ok()
    assert not fx.atlas_path("hero", "walk").exists(), (
        "the obsolete atlas survived a recompile")
    assert fx.atlas_path("hero", "idle").exists(), (
        "a live atlas was removed alongside the obsolete one")
    assert {p.name: p.read_bytes() for p in fx.artifacts("prop")} == \
        prop_before, "another unit's artifacts were touched"
    assert (fx.root / "assets/textures/units/hero/animations/idle").is_dir(), (
        "source artwork was removed")
    fx.validate_ok()


@scenario("deleting a unit's last animation removes its whole generated set")
def _last_animation_removed(fx: Fixture) -> None:
    # The boundary of the obsolescence rule. An index describing no
    # animations is a shape TEX-3 should never have to interpret, so
    # the unit returns to the index-free legacy state instead.
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    build_unit(fx, "prop", [("spin", uniform(CANON5, 2), True)])
    fx.compile_ok()
    prop_before = {p.name: p.read_bytes() for p in fx.artifacts("prop")}

    shutil.rmtree(fx.root / "assets/textures/units/hero/animations/idle")
    fx.yaml("hero", "units:\n  - name: hero\n"
                    '    sprite: "assets/textures/units/hero/idle.png"\n')
    fx.write_file("assets/textures/units/hero/idle.png",
                  rgba_png(1, 1, (0, 0, 0, 255)))

    fx.compile_ok()
    assert not fx.index_path("hero").exists(), (
        "an index describing no animations was left behind")
    assert not fx.atlas_path("hero", "idle").exists()
    assert {p.name: p.read_bytes() for p in fx.artifacts("prop")} == \
        prop_before, "another unit's artifacts were touched"
    fx.validate_ok()


@scenario("compiling refuses outright when the inventory does not validate")
def _compile_refuses_invalid_inventory(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    (fx.root / "assets/textures/units/hero/animations/idle/north"
             / "frame_001.png").unlink()
    code, output = fx.compile()
    assert code != 0, f"compiled from a broken inventory:\n{output}"
    assert "refusing to compile" in output, output
    assert not fx.atlas_dir("hero").exists()


@scenario("two animations that differ only in case refuse to compile")
def _case_insensitive_atlas_collision(fx: Fixture) -> None:
    # Driven through `compile_unit` rather than a fixture tree because
    # the hazard is a case-INSENSITIVE filesystem: the two source
    # directories this guards against cannot both exist on the machine
    # where the collision matters, so a filesystem fixture would only
    # ever run on Linux. The `_RH_` exception is what makes mixed case
    # reachable at all.
    counts = uniform(ALL8, 2)
    build_unit(fx, "hero", [("attack_RH_dagger", counts, False)])
    fx.compile_ok()

    report = pack_atlas_shared.Report()
    decls = pack_atlas_declarations.load_declarations(
        report, fx.root / "data" / "units")
    assert not report.errors, report.errors
    decl = next(d for d in decls if d.name == "hero")
    twin = pack_atlas_shared.AnimDecl(
        "hero", "attack_rh_dagger", False, dict(decl.anims[0].frames),
        "hero/attack_rh_dagger")
    clashing = pack_atlas_shared.UnitDecl(
        decl.name, decl.asset_only, decl.source, decl.anims + [twin],
        decl.aux_paths)

    outcome = pack_atlas_compiler.compile_unit(
        report, fx.root, clashing, False)
    assert outcome is None, "a colliding atlas filename compiled"
    assert any("case-insensitive" in issue.msg for issue in report.errors), (
        f"the collision was not named: "
        f"{[issue.msg for issue in report.errors]}")


@scenario("locating the end of a PNG stream is framing-only and total")
def _stream_end_contract(fx: Fixture) -> None:
    """`locate_png_stream_end` answers where the image ends, or says why not.

    Covered directly rather than only through validation because two of
    its branches are unreachable from there — Pillow rejects an
    IEND-less or overlong-chunk stream first — and an uncovered branch
    is exactly where a crash hides instead of a diagnostic.
    """
    good = png_bytes(32, 32)
    clean = fx.write_file("loose/good.png", good)
    end, size, terminal = pack_atlas_image.locate_png_stream_end(clean)
    assert (end, size) == (len(good), len(good)), (
        f"a clean PNG should end where the file does, got {end}/{size}")
    assert terminal == pack_atlas_shared.PNG_IEND_CHUNK

    # Trailing data moves the FILE's end, never the stream's.
    doubled = fx.write_file("loose/doubled.png", duplicate_iend_png())
    end, size, terminal = pack_atlas_image.locate_png_stream_end(doubled)
    assert end == len(good), f"the stream end moved with the appended data: {end}"
    assert size == len(good) + len(pack_atlas_shared.PNG_IEND_CHUNK)
    assert terminal == pack_atlas_shared.PNG_IEND_CHUNK

    for label, data in [
        ("no IEND at all", good[:-12]),
        ("a chunk claiming more bytes than the file holds",
         good[:-12] + struct.pack(">I", 1 << 30) + b"IDAT" + b"\x00" * 8),
        # The overlong chunk is itself labelled IEND. Without the
        # bounds guard the walk accepts it and reports a NEGATIVE count
        # of trailing bytes, so this is the case that guard exists for.
        ("a TERMINAL chunk claiming more bytes than the file holds",
         good[:-12] + struct.pack(">I", 1 << 30) + b"IEND" + b"\x00" * 8),
    ]:
        broken = fx.write_file("loose/broken.png", data)
        try:
            pack_atlas_image.locate_png_stream_end(broken)
        except ValueError as error:
            assert "IEND" in str(error), (
                f"{label}: rejected for the wrong reason: {error}")
        else:
            raise AssertionError(f"{label}: accepted, or returned a sentinel")


@scenario("every content check earns its keep")
def _both_decode_passes_earn_their_keep(fx: Fixture) -> None:
    """No part of `validate_frame_image` is redundant.

    Each fixture here is accepted by every check except one, which is
    the only evidence that removing that check would let a broken frame
    through. Without this, a later "simplification" down to a single
    call would keep the whole suite green.
    """
    image_mod = pack_atlas_image.image_module()

    # A correct payload under a wrong CRC: the full decode accepts it,
    # because Pillow never checks an IDAT checksum.
    checksum = fx.write_file("loose/bad_checksum.png", bad_checksum_png())
    frame = pack_atlas_image.decode_rgba8(checksum)
    assert (frame.width, frame.height) == (32, 32), (
        f"the bad-checksum fixture should still DECODE, got "
        f"{frame.width}x{frame.height}")
    try:
        pack_atlas_image.validate_frame_image(checksum)
    except ValueError as error:
        assert "checksum" in str(error), (
            f"rejected for the wrong reason: {error}")
    else:
        raise AssertionError(
            "the container pass accepted a wrong IDAT checksum, so the "
            "decode pass alone is doing all the work")

    # Garbage payload under a correct CRC: the container pass accepts
    # it, because verify() never decompresses.
    stream = fx.write_file("loose/corrupt_stream.png", corrupt_stream_png())
    with image_mod.open(stream) as handle:
        handle.verify()          # raises if the container pass would object
    try:
        pack_atlas_image.validate_frame_image(stream)
    except ValueError as error:
        assert "decode" in str(error), f"rejected for the wrong reason: {error}"
    else:
        raise AssertionError(
            "a corrupt compressed stream passed the full check, so the "
            "content pass is not decoding pixel data at all")

    # A wrong TERMINAL checksum: both library passes accept it, because
    # verify() breaks on IEND before checksumming and the decoder never
    # reads that far. Only the terminal comparison sees it.
    terminal = fx.write_file("loose/tampered_iend.png", tampered_iend_png())
    frame = pack_atlas_image.decode_rgba8(terminal)
    assert (frame.width, frame.height) == (32, 32), (
        "the tampered-IEND fixture should still DECODE")
    with image_mod.open(terminal) as handle:
        handle.verify()          # raises if Pillow objected to the tail
    try:
        pack_atlas_image.validate_frame_image(terminal)
    except ValueError as error:
        assert "terminal checksum" in str(error), (
            f"rejected for the wrong reason: {error}")
    else:
        raise AssertionError(
            "a wrong IEND checksum passed the full check: no part of the "
            "content pass validates the terminal chunk")

    # A SECOND canonical IEND appended: both library passes accept it,
    # AND so would a comparison of the file's own last bytes. Only
    # locating the real end of the stream catches it — which is why
    # that walk exists rather than a tail constant.
    doubled = fx.write_file("loose/duplicate_iend.png", duplicate_iend_png())
    frame = pack_atlas_image.decode_rgba8(doubled)
    assert (frame.width, frame.height) == (32, 32), (
        "the duplicate-IEND fixture should still DECODE")
    with image_mod.open(doubled) as handle:
        handle.verify()
    assert doubled.read_bytes()[-12:] == pack_atlas_shared.PNG_IEND_CHUNK, (
        "this fixture only means something while its TAIL is a valid IEND")
    try:
        pack_atlas_image.validate_frame_image(doubled)
    except ValueError as error:
        assert "follow the IEND chunk" in str(error), (
            f"rejected for the wrong reason: {error}")
    else:
        raise AssertionError(
            "data after a complete image passed the full check: the "
            "terminal check is comparing the file's tail rather than "
            "locating where the stream actually ends")


@scenario("a missing image decoder fails validation with an install hint")
def _missing_decoder(fx: Fixture) -> None:
    """An absent Pillow is a loud error, never a silent skip.

    Skipping would be the worst possible outcome: the run would print
    OK while checking no contents at all. Deterministic because it
    blocks the import rather than depending on what is installed.
    """
    valid_fixture(fx)
    fx.validate_ok()             # the same tree passes with a decoder present

    # The memo lives on the IMAGE owner, which is the module
    # `image_module()` reads it from; clearing a copy on the façade would
    # leave the real memo populated and the case vacuous.
    saved_memo = pack_atlas_image._IMAGE_MODULE
    sentinel = object()
    saved = {name: sys.modules.get(name, sentinel)
             for name in ("PIL", "PIL.Image")}
    pack_atlas_image._IMAGE_MODULE = None
    # A None entry in sys.modules makes `from PIL import Image` raise
    # ImportError, which is exactly what an uninstalled Pillow does.
    for name in saved:
        sys.modules[name] = None            # type: ignore[assignment]
    try:
        code, output = fx.run()
    finally:
        pack_atlas_image._IMAGE_MODULE = saved_memo
        for name, module in saved.items():
            if module is sentinel:
                del sys.modules[name]
            else:
                sys.modules[name] = module  # type: ignore[assignment]

    assert code != 0, f"validation passed without a decoder:\n{output}"
    assert "Pillow is required" in output, (
        f"the missing decoder was not named:\n{output}")
    assert "requirements-assets.txt" in output, (
        f"no install hint was printed:\n{output}")
    # One condition about the environment, not one finding per frame.
    assert output.count("Pillow is required") == 1, (
        f"the missing decoder was reported more than once:\n{output}")

    # And the tree still validates once the decoder is back, so the
    # case cannot pass by having corrupted global state.
    fx.validate_ok()


@scenario("the pinned CI toolchain matches tools/requirements-assets.txt")
def _pinned_toolchain_agrees(_fx: Fixture) -> None:
    # The image tag is the hash of the Dockerfile plus ci-image.yml, so
    # the pins are spelled there rather than COPYed from this file — a
    # copied requirements file could change without producing a new
    # image. That duplication is only safe while something fails on
    # drift, which is this case.
    tools = Path(__file__).resolve().parent
    requirements = (tools / "requirements-assets.txt").read_text(
        encoding="utf-8")
    dockerfile = (tools.parent / ".github" / "ci" / "Dockerfile").read_text(
        encoding="utf-8")
    pins = [line.strip() for line in requirements.splitlines()
            if line.strip() and not line.startswith("#")]
    assert len(pins) >= 2, f"expected pinned requirements, got {pins}"
    for pin in pins:
        assert "==" in pin, f"requirement is not pinned: {pin}"
        assert f'"{pin}"' in dockerfile, (
            f"the CI image does not install the pinned {pin}; update "
            f".github/ci/Dockerfile and tools/requirements-assets.txt "
            f"together")


# The façade's single entry point into this owner. Frozen here, after
# every decorator above has run, so the tuple is this file's complete
# contribution in definition order.
CASES: OwnerCases = _CASES.freeze()
