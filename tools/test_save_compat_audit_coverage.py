#!/usr/bin/env python3
"""Manifest-coverage and compatibility-policy cases of the save-compat
self-test (issue #2073).

The sixteen members asserting what the manifest must COVER rather than
how it is shaped: accepted and required component coverage, oldest and
current version coverage, modern-baseline completeness, the B1
migration-helper policy and its two legitimate absence cases, orphan
detection in the fixture directory, the setup-Lua substitution that has
to survive Lua table braces, and the two guards that face real tracked
data -- the real manifest passing the audit, and a version claim no real
fixture's bytes actually back.

Those last two read tracked files and never write them, which is what
requirement 15 asks of this owner: every synthetic manifest below is
built in a temporary directory, and nothing here edits the manifest, the
fixtures or the canonical summaries.
"""
from __future__ import annotations

import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import save_compat_audit_common as common  # noqa: E402
import save_compat_audit_components as components  # noqa: E402
import save_compat_audit_generate as generate  # noqa: E402
import save_compat_audit_manifest as manifest_audit  # noqa: E402

from selftestlib import expect  # noqa: E402
from test_save_compat_audit_support import (  # noqa: E402
    base_manifest, make_fixture,
)


def test_detects_unknown_component_id_in_baseline() -> None:
    print("a baseline declares a component id the real registry doesn't know")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["baselines"][0]["components"].append(
            {"id": "totally-made-up-component", "version": 1, "required": True})
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("no longer exists in the real component registry" in v
                    for v in violations),
               f"expected an unknown-component violation, got {violations}")


def test_detects_removed_input_version() -> None:
    print("a baseline declares a version the real codec no longer accepts")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        # craft-bills really accepts {1, 2} -- 99 has never existed.
        manifest["baselines"][0]["components"].append(
            {"id": "craft-bills", "version": 99, "required": True})
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("currently accepted input versions" in v
                    and "craft-bills" in v for v in violations),
               f"expected a removed-decoder violation, got {violations}")


def test_detects_untracked_oldest_version() -> None:
    print("a real multi-version component is tracked, but not at its oldest version")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        # craft-bills really accepts {1, 2}; bump its ONE tracked entry to
        # the current version (2) instead of the oldest (1) -- still
        # "tracked" (so the separate REQUIRED-with-zero-coverage check
        # below doesn't also fire), but its real v1 migration is now
        # unvalidated by any baseline.
        for c in manifest["baselines"][0]["components"]:
            if c["id"] == "craft-bills":
                c["version"] = 2
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("craft-bills" in v and "no manifest baseline declares" in v
                    for v in violations),
               f"expected an untracked-oldest-version violation, got {violations}")


def test_detects_untracked_current_version() -> None:
    print("round-10 review: a component's OLDEST version is tracked, but "
          "its CURRENT (bumped) version has no fixture coverage at all")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        # craft-bills really accepts {1, 2} and its current version is 2;
        # drop every entry tracking it at its CURRENT version, leaving
        # only its oldest (1) -- simulates a version bump (e.g. to a
        # hypothetical v3) that shipped with no fixture ever exercising
        # the new shape, even though the OLD historical migration is
        # still validly tracked.
        real = components.real_component_registry()
        craft_bills_current = real["craft-bills"]["currentVersion"]
        manifest["baselines"][0]["components"] = [
            c for c in manifest["baselines"][0]["components"]
            if not (c["id"] == "craft-bills" and c["version"] == craft_bills_current)
        ]
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("craft-bills" in v and "CURRENT version" in v
                    for v in violations),
               f"expected an untracked-current-version violation, got {violations}")
        expect(not any("craft-bills" in v and "a migration exists from" in v
                       for v in violations),
               f"did not expect an untracked-OLDEST-version violation too "
               f"(the oldest version 1 is still tracked), got {violations}")


def test_detects_required_component_with_zero_coverage() -> None:
    print("a required component (even single-version) has no baseline tracking it at all")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        # core-session is required and single-version (inputVersions=[1]) --
        # the PRIOR audit never looked at it at all (">1 input version"
        # was the only case it checked). Drop its coverage entirely.
        manifest["baselines"][0]["components"] = [
            c for c in manifest["baselines"][0]["components"]
            if c["id"] != "core-session"]
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("core-session" in v and "is REQUIRED" in v
                    and "not tracked by ANY" in v for v in violations),
               f"expected a required-zero-coverage violation, got {violations}")


def test_detects_modern_baseline_missing_required_component() -> None:
    print("a modern-shaped (non-session) baseline omits a required component from components[]")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        # Drop "session" (making this baseline unambiguously modern-shaped,
        # not b1-style) and drop coverage for one real required component
        # ("world-pages") entirely -- round-6 review's exact scenario: a
        # baseline's components[] under-declaring what a valid modern
        # fixture structurally must contain.
        manifest["baselines"][0]["components"] = [
            c for c in manifest["baselines"][0]["components"]
            if c["id"] not in ("session", "world-pages")]
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("is modern-shaped" in v and "world-pages" in v
                    for v in violations),
               f"expected a modern-baseline-incomplete violation, got {violations}")


def test_modern_baseline_check_skips_b1_shaped_baselines() -> None:
    print("a baseline that DOES declare session is exempt from the modern-completeness check")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        # base_manifest's components[] already includes "session" (every
        # required component, via _oldest_version_components) -- this is
        # the b1-shaped case, which can never declare the full modern set
        # and must not be flagged for that.
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(not any("is modern-shaped" in v for v in violations),
               f"expected no modern-shape violation for a session-shaped baseline, got {violations}")


def test_detects_b1_migration_missing_apply_helper() -> None:
    print("migrateSessionV90's source no longer references a required apply* helper")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        p = tmp / "SessionV90.hs"
        # Every REAL helper name except applyCraftBills -- simulating a
        # rename/removal that silently drops that component's b1 coverage.
        p.write_text(
            "afterEdits <- applyWorldEdits 1 (...) base\n"
            "afterActivity <- applyWorldActivity 1 (...) afterEdits\n"
            "afterBuildings <- applyBuildings 1 nextBuildingId (...) afterActivity\n"
            "afterUnits <- applyUnits 1 nextUnitId (...) afterBuildings\n"
            "afterSim <- applyUnitSim 1 (...) afterUnits\n"
            "afterPower <- applyPowerNodes 1 (...) afterSim\n")
        violations = manifest_audit.audit_b1_migration_covers_page_scoped_components(
            components.real_component_registry(), p)
        expect(any("applyCraftBills" in v and "craft-bills" in v for v in violations),
               f"expected a missing-apply-helper violation, got {violations}")
        expect(len(violations) == 1,
               f"expected exactly one violation (only craft-bills' helper is missing), got {violations}")


def test_detects_unclassified_new_required_component_for_b1() -> None:
    print("round-13 review: a brand-new REQUIRED Haskell component that nobody "
          "added to SESSION_V90_APPLY_HELPER_FOR_COMPONENT or "
          "SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS is its own violation, not a "
          "silent gap in B1 compatibility coverage")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        p = tmp / "SessionV90.hs"
        # The real source text, unmodified -- every REAL known component's
        # helper genuinely IS referenced here. The only injected fault is
        # a brand-new REQUIRED registry entry this dict/exemption set was
        # never told about.
        p.write_text(common.SESSION_V90_SOURCE_PATH.read_text(encoding="utf-8"))
        registry = dict(components.real_component_registry())
        registry["future-thing"] = {
            "currentVersion": 1, "inputVersions": [1], "required": True}
        violations = manifest_audit.audit_b1_migration_covers_page_scoped_components(registry, p)
        expect(any("future-thing" in v and "NO known migration-helper" in v
                   for v in violations),
               f"expected an unclassified-required-component violation, got {violations}")


def test_b1_migration_check_ignores_unrequired_new_component() -> None:
    print("a brand-new OPTIONAL Haskell component needs no B1 migration policy "
          "at all (requirement 9's legitimate absence case)")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        p = tmp / "SessionV90.hs"
        p.write_text(common.SESSION_V90_SOURCE_PATH.read_text(encoding="utf-8"))
        registry = dict(components.real_component_registry())
        registry["future-optional-thing"] = {
            "currentVersion": 1, "inputVersions": [1], "required": False}
        violations = manifest_audit.audit_b1_migration_covers_page_scoped_components(registry, p)
        expect(not any("future-optional-thing" in v for v in violations),
               f"expected no violation for an optional new component, got {violations}")


def test_detects_orphaned_fixture_file() -> None:
    print("round-19 (post-approval) review: a file exists under the "
          "fixture directory but is not referenced by any baseline's "
          "fixture path or expectedCanonicalSummary")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        make_fixture(tmp, "orphaned.bin", b"nobody references me")
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("orphaned.bin" in v and "not referenced" in v
                   for v in violations),
               f"expected an orphaned-fixture violation, got {violations}")


def test_no_orphan_violation_when_every_file_is_referenced() -> None:
    print("a fixture's own path AND its expectedCanonicalSummary both "
          "count as references -- neither is misclassified as an orphan")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        summary_path = make_fixture(tmp, "fixture.expected.json", b"{}")
        manifest["baselines"][0]["fixtures"][0]["expectedCanonicalSummary"] = \
            str(summary_path.relative_to(common.REPO_ROOT))
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(not any("not referenced" in v for v in violations),
               f"expected no orphan violation, got {violations}")


def test_orphan_check_is_skipped_when_fixture_dir_does_not_exist() -> None:
    print("a fixture_dir that doesn't exist yet (e.g. a from-scratch "
          "synthetic manifest with no directory at all) is not itself a "
          "violation -- the check has nothing to scan, not a missing dir")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        violations = manifest_audit.audit(manifest, fixture_dir=tmp / "does-not-exist")
        expect(not any("not referenced" in v for v in violations),
               f"expected no orphan violation, got {violations}")


def test_render_setup_lua_survives_lua_table_braces() -> None:
    """#1087 (PR review round 2): --setup-lua statements were rendered
    with str.format, which treats ORDINARY Lua table braces as format
    fields -- so a perfectly valid statement containing a table raised
    KeyError before the engine ever saw it. Only the two documented
    placeholders may be substituted."""
    table_stmt = ("local t = {bandage = 1, [2] = 'x'}; "
                  "return unit.depositToCargo({uid}, {bid}, 'bandage')")
    rendered = generate.render_setup_lua(table_stmt, 7, 3)
    expect(rendered == ("local t = {bandage = 1, [2] = 'x'}; "
                        "return unit.depositToCargo(3, 7, 'bandage')"),
           f"the table literal survives verbatim and only {{bid}}/{{uid}} "
           f"are substituted, got {rendered!r}")

    nested = generate.render_setup_lua("return f({a = {b = 1}})", 1, 2)
    expect(nested == "return f({a = {b = 1}})",
           f"a statement with NO placeholders is returned unchanged, "
           f"got {nested!r}")

    unspawned = generate.render_setup_lua("return g({bid}, {uid})", None, None)
    expect(unspawned == "return g(nil, nil)",
           f"an unspawned side substitutes the Lua literal nil, never the "
           f"Python string 'None', got {unspawned!r}")


def test_real_manifest_passes_the_audit() -> None:
    print("the real, checked-in manifest currently passes (regression guard)")
    manifest = manifest_audit.load_manifest()
    violations = manifest_audit.audit(manifest)
    expect(violations == [],
           f"expected the real manifest to be clean, got {violations}")


def test_detects_manifest_version_claim_not_backed_by_real_fixture_bytes() -> None:
    print("round-12 review: a baseline's declared components[] version bump "
          "is rejected when NO real, tracked fixture's own decoded envelope "
          "actually carries a matching descriptor -- catches a manifest-only "
          "edit with no fixture ever re-encoded at the claimed version")
    manifest = manifest_audit.load_manifest()
    for baseline in manifest["baselines"]:
        # b2-split-haskell-lua-state has exactly ONE fixture (unlike
        # c3-raw-reference-v1, whose OTHER fixtures happen to carry
        # craft-bills at both v1 and v2 already) -- its single real
        # fixture genuinely carries craft-bills at v1 (round-10/11
        # review fixed it FROM the wrong v2), so claiming v2 here is
        # backed by NO real fixture at all in this baseline.
        if baseline["id"] == "b2-split-haskell-lua-state":
            for comp in baseline["components"]:
                if comp["id"] == "craft-bills":
                    comp["version"] = 2
    violations = manifest_audit.audit(manifest)
    expect(any("craft-bills" in v and "not backed by any tracked fixture's bytes" in v
               for v in violations),
           f"expected a fixture-backed-claim violation, got {violations}")


#: This owner's members, in the run order the façade concatenates
#: (issue #2073 requirement 12).
TESTS = [
    test_detects_unknown_component_id_in_baseline,
    test_detects_removed_input_version,
    test_detects_untracked_oldest_version,
    test_detects_untracked_current_version,
    test_detects_required_component_with_zero_coverage,
    test_detects_modern_baseline_missing_required_component,
    test_modern_baseline_check_skips_b1_shaped_baselines,
    test_detects_b1_migration_missing_apply_helper,
    test_detects_unclassified_new_required_component_for_b1,
    test_b1_migration_check_ignores_unrequired_new_component,
    test_detects_orphaned_fixture_file,
    test_no_orphan_violation_when_every_file_is_referenced,
    test_orphan_check_is_skipped_when_fixture_dir_does_not_exist,
    test_render_setup_lua_survives_lua_table_braces,
    test_real_manifest_passes_the_audit,
    test_detects_manifest_version_claim_not_backed_by_real_fixture_bytes,
]
