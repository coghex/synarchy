#!/usr/bin/env python3
"""
test_pack_atlas_budget.py — the unit-animation texture budget scenarios
of `tools/test_pack_atlas.py` (#1262, #2061 owner split).

Two budgets, one policy document. The image/slot budget is a hard error
and is what catches a reintroduced per-frame registration; the
resident-byte budget is a warning carrying D-10's TEX-5 activation
trigger, so `--strict` is what makes it blocking. Policy schema and
value validation, image/frame/decoded-byte accounting, both thresholds,
and the shipped budget document all live here.

Not runnable on its own: it parses no arguments and executes no case at
import. `tools/test_pack_atlas.py` imports `CASES` and runs it.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import pack_atlas  # noqa: E402
import pack_atlas_budget  # noqa: E402
import pack_atlas_shared  # noqa: E402
from test_pack_atlas_support import (  # noqa: E402
    CANON5, CELL, CaseRegistry, Fixture, OwnerCases, anim_yaml_ragged,
    budget_unit, build_unit, entry, rgba_png, uniform,
)

_CASES = CaseRegistry("budget")
scenario = _CASES.scenario

@scenario("the budget reports one resident image per animation")
def _budget_baseline(fx: Fixture) -> None:
    budget_unit(fx, anims=3)
    output = fx.validate_ok()
    assert "BUDGET — 3 resident animation image(s) for 3 animation(s)" \
        in output, output
    # Derived from the index, not from a roster constant: growing the
    # unit must move the expected count with it.
    build_unit(fx, "hero", [(f"anim{i}", uniform(CANON5, 2), True)
                            for i in range(4)])
    fx.compile_ok()
    assert "BUDGET — 4 resident animation image(s) for 4 animation(s)" \
        in fx.validate_ok()


@scenario("a per-frame regression fails the image budget by path")
def _budget_per_frame_regression(fx: Fixture) -> None:
    # THE case requirement 3 names. A regression that puts one image
    # per FRAME where D-2 wants one per animation lands extra files in
    # the compiler-owned directory; the budget must name the unit, the
    # expected count, the actual count, and the offending records.
    budget_unit(fx, anims=2)
    source = fx.atlas_path("hero", "anim0").read_bytes()
    for direction in CANON5:
        for index in range(2):
            fx.write_file(
                f"assets/textures/units/hero/atlas/"
                f"anim0_{direction}_{index:03d}.png", source)
    output = fx.validate_fails("budget: expected 2 resident animation image(s)")
    assert "found 12" in output, output
    assert "anim0_east_000.png" in output, output
    assert "(+" in output and "more)" in output, (
        "the offending list was not capped:\n" + output)
    # A per-frame regression is a BUDGET finding, not an incidental
    # freshness one: the sweep reports the same files, and both are
    # useful, but the budget line has to be there on its own.
    assert output.count("budget:") >= 1, output


@scenario("removing an atlas fails the image budget in the other direction")
def _budget_missing_image(fx: Fixture) -> None:
    # The bound is an equality, so under-count fails too — otherwise a
    # unit whose atlas silently vanished would satisfy "at most one per
    # animation" while rendering nothing.
    budget_unit(fx, anims=3)
    fx.rm("assets/textures/units/hero/atlas/anim1.png")
    output = fx.validate_fails("budget: expected 3 resident animation image(s)")
    assert "found 2" in output, output


@scenario("two animations sharing one atlas is its own budget finding")
def _budget_shared_atlas(fx: Fixture) -> None:
    # File COUNT alone would still be right here while the second
    # animation registered no image of its own, so this is reported on
    # its own terms rather than through the count.
    budget_unit(fx, anims=2)
    doc = fx.index("hero")
    entry(doc, "anim1")["atlas_path"] = entry(doc, "anim0")["atlas_path"]
    fx.index_path("hero").write_text(json.dumps(doc, indent=2) + "\n",
                                     encoding="utf-8")
    output = fx.validate_fails("is claimed by 2 animations")
    assert "anim0, anim1" in output, output


@scenario("non-animation textures are outside the budget entirely")
def _budget_excludes_non_animation(fx: Fixture) -> None:
    # Portraits, the direct sprite and its directional overrides are
    # excluded BY CONSTRUCTION — they live outside atlas/ and no index
    # names them. This pins that, so a future "count every texture
    # under the unit" reading of the budget fails here.
    counts = uniform(CANON5, 2)
    fx.frames_rgba("hero", "idle", CANON5, counts, CELL)
    for extra in ("portrait.png", "sprite.png", "sprite_south.png"):
        fx.write_file(f"assets/textures/units/hero/{extra}",
                      rgba_png(CELL[0], CELL[1], (1, 2, 3, 255)))
    fx.yaml("hero", (
        "units:\n  - name: hero\n"
        '    sprite: "assets/textures/units/hero/sprite.png"\n'
        '    portrait: "assets/textures/units/hero/portrait.png"\n'
        "    directional_sprites:\n"
        '      south: "assets/textures/units/hero/sprite_south.png"\n'
        "    animations:\n") + anim_yaml_ragged("hero", "idle", counts, True))
    fx.compile_ok()
    assert "BUDGET — 1 resident animation image(s) for 1 animation(s)" \
        in fx.validate_ok()


@scenario("an uncompiled unit is weighed by neither budget")
def _budget_skips_uncompiled(fx: Fixture) -> None:
    build_unit(fx, "hero", [("idle", uniform(CANON5, 2), True)])
    output = fx.validate_ok()
    assert "BUDGET" not in output, (
        "an uncompiled tree reported a budget it cannot measure:\n" + output)


@scenario("resident bytes are the index's own decoded RGBA8 footprint")
def _budget_resident_bytes(fx: Fixture) -> None:
    budget_unit(fx, anims=2)
    doc = fx.index("hero")
    expected = sum(a["atlas_width"] * a["atlas_height"] * 4
                   for a in doc["animations"])
    assert f"{expected / (1024 * 1024):.2f} MiB decoded RGBA8 resident" \
        in fx.validate_ok(), (expected, fx.validate_ok())


@scenario("crossing the memory threshold reports D-10's TEX-5 trigger")
def _budget_memory_trigger(fx: Fixture) -> None:
    budget_unit(fx, anims=2)
    doc = fx.budget()
    measured = sum(a["atlas_width"] * a["atlas_height"] * 4
                   for a in fx.index("hero")["animations"])
    factor = doc["resident_bytes"]["projection"]["roster_growth_factor"]
    # Exactly at the threshold must NOT fire — the rule is a strict
    # `>` and a budget that fails on its own limit is off by one.
    doc["resident_bytes"]["threshold"] = int(measured * factor)
    fx.write_budget(doc)
    fx.validate_ok()

    doc["resident_bytes"]["threshold"] = int(measured * factor) - 1
    fx.write_budget(doc)
    output = fx.validate_fails("unit-texture memory budget exceeded")
    assert "TEX-5" in output, output
    # Advisory without --strict, blocking with it: the breach is a
    # project decision, and CI is where it has to stop being ignorable.
    code, plain = fx.run(strict=False)
    assert code == 0, f"a budget breach blocked a non-strict run:\n{plain}"
    assert "unit-texture memory budget exceeded" in plain, plain


@scenario("a single-unit run does not evaluate the roster-wide trigger")
def _budget_single_unit_scope(fx: Fixture) -> None:
    # The memory budget aggregates the WHOLE tracked roster, so `--unit`
    # can only ever see part of it. Reporting a breach — or an all-clear
    # — from a fraction would be a measurement of nothing.
    budget_unit(fx, "hero", anims=2)
    budget_unit(fx, "prop", anims=2)
    doc = fx.budget()
    doc["resident_bytes"]["threshold"] = 1
    fx.write_budget(doc)
    code, output = fx.run(unit="hero", strict=True)
    assert code == 0, f"--unit evaluated the roster-wide trigger:\n{output}"
    assert "memory budget exceeded" not in output, output
    assert fx.run(strict=True)[0] != 0, "the whole-roster run did not fire"


@scenario("a missing or malformed budget document is a hard error")
def _budget_document_required(fx: Fixture) -> None:
    # Never a skipped check: a budget that silently disappears would
    # print a clean run while enforcing nothing, which is worse than
    # having no guardrail at all.
    budget_unit(fx, anims=2)
    fx.validate_ok()

    fx.rm(pack_atlas_budget.BUDGET_REL.as_posix())
    fx.validate_fails("cannot read the unit texture budget")

    fx.write_file(pack_atlas_budget.BUDGET_REL.as_posix(), b"{not json")
    fx.validate_fails("not valid JSON")

    fx.write_budget({"schema_version": 99})
    fx.validate_fails("unsupported schema_version 99")

    # `True == 1` in Python, so an equality test alone would read this
    # as version 1 and validate the document against a version it never
    # declared.
    for version in (True, "1", 1.0, None, [1]):
        fx.write_budget({"schema_version": version,
                         "animation_images": {}, "resident_bytes": {}})
        fx.validate_fails("unsupported schema_version")


@scenario("a required budget field must SAY something, not merely exist")
def _budget_fields_typed(fx: Fixture) -> None:
    # Presence alone is not enough. These fields carry the comparison
    # rule and the owner's confirmation of a number nobody may raise
    # unilaterally, so a numeric confirmed_on or a boolean
    # comparison_rule has to fail as loudly as a missing one.
    budget_unit(fx, anims=2)
    pristine = fx.budget()
    prose = [
        ("animation_images", "measure"),
        ("animation_images", "aggregation_scope"),
        ("animation_images", "comparison_rule"),
        ("animation_images", "rationale"),
        ("resident_bytes", "measure"),
        ("resident_bytes", "aggregation_scope"),
        ("resident_bytes", "comparison_rule"),
        ("resident_bytes", "distinct_from"),
        ("resident_bytes", "derivation"),
        ("resident_bytes", "confirmed_by"),
        ("resident_bytes", "confirmed_on"),
    ]
    for block, key in prose:
        for value, expect in ((42, f"{key} must be str"),
                              (True, f"{key} must be str"),
                              (None, f"{key} must be str"),
                              ({"a": 1}, f"{key} must be str"),
                              ("", f"{key} must not be empty"),
                              ("   ", f"{key} must not be empty")):
            doc = json.loads(json.dumps(pristine))
            doc[block][key] = value
            fx.write_budget(doc)
            fx.validate_fails(expect)

    # `excluded` documents what the image budget deliberately does NOT
    # count, so an empty or non-textual list is the same defect.
    for value, expect in (([], "excluded must not be empty"),
                          ("portraits", "excluded must be list"),
                          ([1, 2], "excluded must hold non-empty strings"),
                          (["ok", ""], "excluded must hold non-empty strings")):
        doc = json.loads(json.dumps(pristine))
        doc["animation_images"]["excluded"] = value
        fx.write_budget(doc)
        fx.validate_fails(expect)

    # projection is the other structured field: it has to be an object
    # carrying the factor, not a bare number that looks like one.
    for value in (2.0, "2.0", [2.0], None, True):
        doc = json.loads(json.dumps(pristine))
        doc["resident_bytes"]["projection"] = value
        fx.write_budget(doc)
        fx.validate_fails("projection must be dict")

    # ...and a well-formed document still passes, so none of the above
    # is over-rejection.
    fx.write_budget(pristine)
    fx.validate_ok()


@scenario("every documented budget field is required, not defaulted")
def _budget_fields_required(fx: Fixture) -> None:
    # A bare threshold with no stated unit, scope or comparison rule is
    # exactly the artifact this document exists to avoid.
    budget_unit(fx, anims=2)
    # Each perturbation starts from the pristine document: re-reading
    # the file would accumulate the previous iteration's deletion and
    # stop isolating the field under test.
    pristine = fx.budget()
    for block, key in (("animation_images", "max_per_animation"),
                       ("animation_images", "aggregation_scope"),
                       ("animation_images", "comparison_rule"),
                       ("resident_bytes", "threshold"),
                       ("resident_bytes", "unit"),
                       ("resident_bytes", "projection"),
                       ("resident_bytes", "comparison_rule"),
                       ("resident_bytes", "derivation"),
                       ("resident_bytes", "confirmed_by")):
        doc = json.loads(json.dumps(pristine))
        del doc[block][key]
        fx.write_budget(doc)
        output = fx.validate_fails(f"missing field: {key}")
        # Exactly one diagnostic per absent key: a presence sweep and a
        # type check both reporting it would double every finding.
        assert output.count(f"missing field: {key}") == 1, output


@scenario("an out-of-range budget value is rejected rather than coerced")
def _budget_values_checked(fx: Fixture) -> None:
    budget_unit(fx, anims=2)
    bad = [
        (("animation_images", "max_per_animation"), 0,
         "max_per_animation must be at least 1"),
        (("animation_images", "max_per_animation"), True,
         "max_per_animation must be int"),
        (("resident_bytes", "threshold"), 0,
         "threshold must be a positive byte count"),
        (("resident_bytes", "threshold"), "384 MiB",
         "threshold must be int"),
        (("resident_bytes", "unit"), "MiB",
         "unit must be 'bytes'"),
    ]
    pristine = fx.budget()
    for (block, key), value, expect in bad:
        doc = json.loads(json.dumps(pristine))
        doc[block][key] = value
        fx.write_budget(doc)
        fx.validate_fails(expect)

    for value, expect in ((0.5, "must be at least 1"),
                          ("2", "must be a number"),
                          (float("inf"), "is not finite")):
        doc = json.loads(json.dumps(pristine))
        doc["resident_bytes"]["projection"]["roster_growth_factor"] = value
        fx.write_budget(doc)
        fx.validate_fails(expect)


@scenario("the shipped budget document is the one the shipped corpus meets")
def _budget_shipped_document(_fx: Fixture) -> None:
    # The fixtures above prove the CHECK works; this proves the real
    # policy file parses through the real loader and that the shipped
    # roster is inside it. Reading the numbers rather than asserting
    # them keeps this from becoming a second place the threshold lives.
    report = pack_atlas_shared.Report()
    budget = pack_atlas_budget.load_budget(report, pack_atlas.REPO_ROOT)
    assert budget is not None, (
        "the shipped budget document does not load: "
        + "; ".join(i.msg for i in report.errors))
    assert not report.errors and not report.warnings
    assert budget.max_per_animation == 1, (
        f"D-2 is one atlas per animation, the document says "
        f"{budget.max_per_animation}")
    doc = json.loads(
        (pack_atlas.REPO_ROOT / pack_atlas_budget.BUDGET_REL)
        .read_text("utf-8"))
    confirmed = doc["resident_bytes"]
    assert confirmed["confirmed_by"] and confirmed["confirmed_on"], (
        "the recorded threshold carries no owner confirmation")


# The façade's single entry point into this owner. Frozen here, after
# every decorator above has run, so the tuple is this file's complete
# contribution in definition order.
CASES: OwnerCases = _CASES.freeze()
