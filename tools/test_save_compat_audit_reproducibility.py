#!/usr/bin/env python3
"""The save-compat self-test's fixture-reproducibility owner (issue
#2073; converted off GHCi by issue #2273).

`test_normalize_fixture_timestamp_makes_generation_reproducible` is the
member `--only-reproducibility` selects, and the façade derives
`REPRODUCIBILITY_TESTS` from this module's `TESTS` so the selected set
and the run order can never disagree about which member that is.

Until #2273 this member also built its two envelope variants by feeding
a fourth GHCi program to `cabal repl test:synarchy-test-headless` -- an
invocation the issue that split this owner out did not count, and the
reason `--only-reproducibility` cost 64-88 s on the CI critical path.
Both variants are now written by the compiled
`exe:synarchy-save-codec`, through `codec.set_fixture_timestamp`: the
same production re-encode `normalize_fixture_timestamp` itself performs,
differing only in which timestamp it stamps and where it writes.

That makes this owner cheap, and the path selection around it a matter of
WHICH inputs can move the result rather than of cost. It is kept
selective anyway: `tools/ci_parity_audit.py` pins both command spellings,
and CI and `make ci` run the same two save-compatibility commands
(issue #2273 requirement 7). A second member added here still joins
`REPRODUCIBILITY_TESTS` automatically, so put a case whose subject is the
codec bridge itself in `test_save_compat_audit_codec.py` instead.
"""
from __future__ import annotations

import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import save_compat_audit_codec as codec  # noqa: E402
import save_compat_audit_common as common  # noqa: E402

from selftestlib import expect  # noqa: E402


# A real, checked-in CURRENT-FORMAT fixture -- a genuine modern-shaped
# envelope with the SAME component set a real --generate-session run
# would also produce -- used below to build two envelopes that differ
# ONLY in their "metadata" component's smTimestamp (simulating what two
# engine.saveWorld calls at different wall-clock moments actually
# produce), so normalize_fixture_timestamp's reproducibility guarantee
# can be exercised against genuine envelope bytes rather than a
# hand-rolled binary fixture.
#
# It must track the CURRENT "metadata" schema version, because building
# a variant decodes that payload as the live SaveMetadata: an older
# baseline (c3, d1, e1 ...) carries a HISTORICAL metadata shape that only
# decodes through its frozen compat mirror, which is a different concern
# from the timestamp reproducibility this test is about. Re-point this at
# the newest current-format baseline whenever the metadata component's
# version is bumped again.
_CURRENT_FORMAT_FIXTURE_PATH = (
    common.FIXTURE_DATA_DIR / "u1-generated-world-identity.bin")

#: The two wall-clock moments the variants stand in for. Deliberately far
#: apart and neither equal to `common.FIXED_GENERATED_TIMESTAMP`, so the
#: convergence below is normalization's doing rather than either variant
#: having started out already normalized.
_VARIANT_A_TIMESTAMP = "2020-01-01T00:00:00.000000Z"
_VARIANT_B_TIMESTAMP = "2099-12-31T23:59:59.999999Z"


def test_normalize_fixture_timestamp_makes_generation_reproducible() -> None:
    print("round-11 review: two envelopes differing ONLY in engine.saveWorld's "
          "wall-clock smTimestamp converge to byte-identical fixtures after "
          "normalize_fixture_timestamp, proving --generate-session's output no "
          "longer depends on when the command happened to run")
    if not _CURRENT_FORMAT_FIXTURE_PATH.exists():
        expect(False, f"expected the tracked fixture to exist at {_CURRENT_FORMAT_FIXTURE_PATH}")
        return
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        variant_a = tmp / "variant_a.bin"
        variant_b = tmp / "variant_b.bin"
        for variant, timestamp in ((variant_a, _VARIANT_A_TIMESTAMP),
                                   (variant_b, _VARIANT_B_TIMESTAMP)):
            ok, tail = codec.set_fixture_timestamp(
                _CURRENT_FORMAT_FIXTURE_PATH, timestamp, variant)
            if not ok or not variant.exists():
                expect(False,
                       f"expected timestamp-variant setup for {variant.name} "
                       f"to succeed, got: {tail}")
                return
        expect(variant_a.read_bytes() != variant_b.read_bytes(),
               "expected the two variants to genuinely differ before "
               "normalization (otherwise this test proves nothing)")

        ok_a, tail_a = codec.normalize_fixture_timestamp(variant_a)
        expect(ok_a, f"expected normalization of variant A to succeed, got: {tail_a}")
        ok_b, tail_b = codec.normalize_fixture_timestamp(variant_b)
        expect(ok_b, f"expected normalization of variant B to succeed, got: {tail_b}")

        expect(variant_a.read_bytes() == variant_b.read_bytes(),
               "expected both variants to be byte-identical after "
               "normalize_fixture_timestamp, proving repeat generation over "
               "identical inputs is now reproducible regardless of wall-clock "
               "drift between runs")


#: The tracked pre-#2701 fixture (units v4, buildings v2, world-pages v13)
#: and its nonempty, current-version counterpart.
_PRE_TAG_FIXTURE_PATH = common.FIXTURE_DATA_DIR / "z8-units-v4-physiology-overrides.bin"
_TAGGED_FIXTURE_PATH = common.FIXTURE_DATA_DIR / "z9-gameplay-tags.bin"
#: #2703: the tracked fixture carrying nonempty physical-item tags
#: (core-session v2).
_ITEM_TAGGED_FIXTURE_PATH = common.FIXTURE_DATA_DIR / "z10-item-tags.bin"

#: One assignment per owned category, on z8's own page and ids.
_TAG_SEED = ('{"units":[{"page":"generated_page","id":1,"tags":["defender"]}],'
             '"buildings":[],'
             '"locations":[{"page":"generated_page","id":1,"tags":["landmark"]}],'
             '"items":[{"id":2,"tags":["food"]},{"id":3,"tags":["food","spare"]}]}')


def _summary_tags(fixture: Path, out: Path) -> dict | None:
    ok, tail = codec.dump_canonical_summary(fixture, out)
    if not ok:
        expect(False, f"expected a canonical summary of {fixture.name}, got: {tail}")
        return None
    import json
    summary = json.loads(out.read_text())
    tags = {p["pageId"]: p.get("gameplayTags", {}) for p in summary["pages"]}
    # #2703: the session-scoped item assignments, under their own key.
    tags["<items>"] = summary.get("itemTags", [])
    return tags


def test_seeded_tag_fixture_generation_is_reproducible() -> None:
    print("#2701/#2703: tag seeding (synarchy-save-codec seed-tags, the one way a "
          "generated fixture carries general gameplay tags) is as reproducible "
          "as generation itself -- two timestamp variants seeded with the same "
          "spec converge to byte-identical files after normalization -- and the "
          "canonical summary reads NO assignments off the pre-tag fixture and "
          "nonempty ones, with their target ids, off the seeded and tracked "
          "tagged fixtures")
    for path in (_PRE_TAG_FIXTURE_PATH, _TAGGED_FIXTURE_PATH, _ITEM_TAGGED_FIXTURE_PATH):
        if not path.exists():
            expect(False, f"expected the tracked fixture to exist at {path}")
            return
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        spec = tmp / "seed.json"
        spec.write_text(_TAG_SEED)
        variants = []
        for name, timestamp in (("a", _VARIANT_A_TIMESTAMP), ("b", _VARIANT_B_TIMESTAMP)):
            variant = tmp / f"seeded_{name}.bin"
            ok, tail = codec.set_fixture_timestamp(_PRE_TAG_FIXTURE_PATH, timestamp, variant)
            expect(ok, f"expected timestamp variant {name} to be written, got: {tail}")
            ok, tail = codec.seed_fixture_tags(variant, spec, variant)
            expect(ok, f"expected variant {name} to be seeded, got: {tail}")
            ok, tail = codec.normalize_fixture_timestamp(variant)
            expect(ok, f"expected variant {name} to normalize, got: {tail}")
            variants.append(variant)
        if not all(v.exists() for v in variants):
            return
        expect(variants[0].read_bytes() == variants[1].read_bytes(),
               "expected the two seeded variants to be byte-identical after "
               "normalization")

        before = _summary_tags(_PRE_TAG_FIXTURE_PATH, tmp / "before.json")
        after = _summary_tags(variants[0], tmp / "after.json")
        tracked = _summary_tags(_TAGGED_FIXTURE_PATH, tmp / "tracked.json")
        tracked_items = _summary_tags(_ITEM_TAGGED_FIXTURE_PATH, tmp / "tracked_items.json")
        if before is None or after is None or tracked is None or tracked_items is None:
            return
        empty = {"units": [], "buildings": [], "locations": []}
        expect(before.pop("<items>") == [] and tracked.pop("<items>") == [],
               "expected the pre-tag and the core-session v1 tagged fixtures to "
               "read no item assignments")
        expect(after.pop("<items>") == [{"id": 2, "tags": ["food"]},
                                        {"id": 3, "tags": ["food", "spare"]}],
               "expected the seeded variant to carry exactly the seeded item "
               "assignments, by instance id")
        expect(bool(tracked_items.pop("<items>")),
               "expected the tracked item-tag fixture to carry nonempty item "
               "assignments")
        expect(all(tags == empty for tags in before.values()),
               f"expected the pre-tag fixture to read no assignments, got {before}")
        expect(after.get("generated_page") ==
               {"units": [{"id": 1, "tags": ["defender"]}], "buildings": [],
                "locations": [{"id": 1, "tags": ["landmark"]}]},
               f"expected the seeded variant to carry exactly the seeded "
               f"assignments, got {after}")
        page = tracked.get("generated_page", {})
        expect(all(page.get(cat) for cat in ("units", "buildings", "locations")),
               f"expected the tracked tagged fixture to carry nonempty unit, "
               f"building and location assignments, got {tracked}")


#: This owner's members, in the run order the façade concatenates
#: (issue #2073 requirement 12). The façade also takes this list AS
#: `REPRODUCIBILITY_TESTS`, so requirement 13's "exactly the one
#: reproducibility test" is a property of where the member lives rather
#: than of a second list that could drift from it.
TESTS = [
    test_normalize_fixture_timestamp_makes_generation_reproducible,
    test_seeded_tag_fixture_generation_is_reproducible,
]
