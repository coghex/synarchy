#!/usr/bin/env python3
"""Unit tests for save_compat_audit.py (issue #766, save-overhaul C4).

Feeds `audit()` synthetic manifests over a temporary directory tree --
never touches the real docs/save_compat/manifest.json or tracked
fixtures -- so these tests stay stable regardless of how the real
manifest grows, and prove the audit actually detects each violation
class it claims to (a "the audit detects an intentionally introduced
violation" gate, mirroring tools/test_persistence_inventory_audit.py's
own convention).

Usage:
  python3 tools/test_save_compat_audit.py
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import hashlib
import json
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import save_compat_audit as sca  # type: ignore

FAILURES: list[str] = []


def expect(cond: bool, msg: str) -> None:
    if not cond:
        FAILURES.append(msg)
        print(f"  FAIL: {msg}")
    else:
        print(f"  OK:   {msg}")


def make_fixture(tmp: Path, name: str, content: bytes) -> Path:
    p = tmp / name
    p.write_bytes(content)
    return p


def _oldest_version_components() -> list[dict]:
    """A components[] list covering every REAL required component's
    oldest accepted version (round-3 review: required components need
    coverage regardless of how many versions they accept, not just
    multi-version ones), satisfying audit_component_versions' full
    coverage check with entries that are trivially true of THIS repo's
    actual registry. audit_component_versions cross-checks against the
    real source unconditionally (there is no "test mode" -- that's the
    whole point), so a synthetic manifest aimed at ONE specific,
    unrelated violation class must still declare this or it would
    incidentally also fail on every real component's coverage check,
    which has nothing to do with what that test is exercising."""
    registry = sca.real_component_registry()
    return [
        {"id": cid, "version": min(info["inputVersions"]), "required": True}
        for cid, info in registry.items()
        if info.get("required")
    ]


def base_manifest(tmp: Path, fixture_path: Path, content: bytes) -> dict:
    return {
        "envelopeFramingVersion": sca.current_envelope_version(),
        "frozenDtoFingerprint": sca.frozen_dto_fingerprint(),
        "baselines": [
            {
                "id": "test-baseline",
                "components": _oldest_version_components(),
                "fixtures": [
                    {
                        "id": "test-fixture",
                        "path": str(fixture_path.relative_to(sca.REPO_ROOT))
                            if fixture_path.is_relative_to(sca.REPO_ROOT)
                            else str(fixture_path),
                        "sha256": hashlib.sha256(content).hexdigest(),
                        "sizeBytes": len(content),
                    }
                ],
            }
        ],
    }


def test_clean_manifest_has_no_violations() -> None:
    print("clean manifest with a matching fixture")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        violations = sca.audit(manifest)
        expect(violations == [], f"expected no violations, got {violations}")


def test_detects_missing_fixture_file() -> None:
    print("missing fixture path")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = tmp / "does_not_exist.bin"
        manifest = base_manifest(tmp, fpath, content)
        violations = sca.audit(manifest)
        expect(any("does not exist" in v for v in violations),
               f"expected a missing-path violation, got {violations}")


def test_detects_checksum_drift() -> None:
    print("fixture bytes changed after being recorded")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        original = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", original)
        manifest = base_manifest(tmp, fpath, original)
        fpath.write_bytes(b"HELLO WORLD -- tampered")
        violations = sca.audit(manifest)
        expect(any("drifted" in v for v in violations),
               f"expected a drift violation, got {violations}")


def test_detects_size_mismatch_alone() -> None:
    print("recorded size disagrees even when sha256 is absent (n/a case skipped)")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["baselines"][0]["fixtures"][0]["sizeBytes"] = len(content) + 1
        violations = sca.audit(manifest)
        expect(any("size" in v for v in violations),
               f"expected a size-mismatch violation, got {violations}")


def test_decode_only_fixture_skips_checksum() -> None:
    print("a fixture with sha256=null (decode-only/inline-source evidence) is not checksummed")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        fpath = make_fixture(tmp, "fixture.hs", b"-- source file, not a binary blob")
        manifest = base_manifest(tmp, fpath, b"unused")
        manifest["baselines"][0]["fixtures"][0]["sha256"] = None
        manifest["baselines"][0]["fixtures"][0]["sizeBytes"] = None
        violations = sca.audit(manifest)
        expect(violations == [], f"expected no violations, got {violations}")


def test_detects_framing_version_mismatch() -> None:
    print("manifest envelopeFramingVersion disagrees with the real source")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["envelopeFramingVersion"] = sca.current_envelope_version() + 1
        violations = sca.audit(manifest)
        expect(any("envelopeFramingVersion" in v for v in violations),
               f"expected a framing-version violation, got {violations}")


def test_detects_frozen_dto_fingerprint_mismatch() -> None:
    print("manifest frozenDtoFingerprint disagrees with the real source")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["frozenDtoFingerprint"] = "0" * 64
        violations = sca.audit(manifest)
        expect(any("frozenDtoFingerprint" in v for v in violations),
               f"expected a fingerprint violation, got {violations}")


def test_detects_baseline_with_no_fixtures() -> None:
    print("a declared baseline with zero fixtures")
    manifest = {
        "envelopeFramingVersion": sca.current_envelope_version(),
        "frozenDtoFingerprint": sca.frozen_dto_fingerprint(),
        "baselines": [{"id": "empty-baseline", "components": _oldest_version_components(), "fixtures": []}],
    }
    violations = sca.audit(manifest)
    expect(any("has no fixtures" in v for v in violations),
           f"expected a no-fixtures violation, got {violations}")


def test_frozen_dto_fingerprint_is_comment_insensitive() -> None:
    print("fingerprint ignores whitespace/comment-only changes to the source")
    with tempfile.TemporaryDirectory() as d:
        p = Path(d) / "SessionV90.hs"
        p.write_text(
            "-- a comment\n"
            "data Foo = Foo\n"
            "    { fooA ∷ !Int\n"
            "    , fooB ∷ !Text\n"
            "    } deriving (Show, Generic, Serialize)\n")
        fp1 = sca.frozen_dto_fingerprint(p)
        p.write_text(
            "-- a DIFFERENT comment, much longer, explaining fooA in depth\n"
            "data Foo = Foo\n"
            "    { fooA ∷ !Int    -- extra trailing comment\n"
            "    , fooB ∷ !Text\n"
            "    } deriving (Show, Generic, Serialize)\n")
        fp2 = sca.frozen_dto_fingerprint(p)
        expect(fp1 == fp2,
               "expected fingerprint to ignore comment-only changes")


def test_frozen_dto_fingerprint_changes_on_field_reorder() -> None:
    print("fingerprint changes when a frozen DTO's fields are reordered")
    with tempfile.TemporaryDirectory() as d:
        p = Path(d) / "SessionV90.hs"
        p.write_text(
            "data Foo = Foo\n"
            "    { fooA ∷ !Int\n"
            "    , fooB ∷ !Text\n"
            "    } deriving (Show, Generic, Serialize)\n")
        fp1 = sca.frozen_dto_fingerprint(p)
        p.write_text(
            "data Foo = Foo\n"
            "    { fooB ∷ !Text\n"
            "    , fooA ∷ !Int\n"
            "    } deriving (Show, Generic, Serialize)\n")
        fp2 = sca.frozen_dto_fingerprint(p)
        expect(fp1 != fp2,
               "expected fingerprint to change on field reorder")


class _Args:
    """A minimal stand-in for argparse.Namespace -- only the attributes
    cmd_add_baseline/_build_fixture_entry actually read."""
    def __init__(self, **kwargs) -> None:
        defaults = dict(
            baseline_id=None, fixture_id=None, path=None, kind=None,
            summary=None, provenance=None, description=None,
            migration_target=None, migrated_by=None, components=None,
            declared_at=None, declared_by_issue=766, force=False,
            # Every EXISTING test below registers a fixture that was
            # never actually run through the real codec (they're plain
            # placeholder bytes) -- skip_validation defaults to True here
            # so they keep exercising the atomic bookkeeping in
            # isolation, without also needing a real cabal toolchain in
            # every environment this suite runs in. The validation path
            # itself is exercised separately below via a monkeypatched
            # _run_real_codec_validation, never a real subprocess.
            skip_validation=True)
        defaults.update(kwargs)
        self.__dict__.update(defaults)


def test_add_baseline_creates_a_new_baseline_and_fixture_atomically() -> None:
    print("--add-baseline creates a whole new baseline entry")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        fixture = make_fixture(tmp, "new.bin", b"new fixture bytes")
        summary = tmp / "new.expected.json"
        summary.write_text('{"ok": true}')
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps({"baselines": []}))
        old_path = sca.MANIFEST_PATH
        sca.MANIFEST_PATH = manifest_path
        try:
            rc = sca.cmd_add_baseline(_Args(
                baseline_id="new-baseline", fixture_id="new-fixture",
                path=str(fixture.relative_to(sca.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(sca.REPO_ROOT)),
                description="a test baseline", migration_target="current",
                migrated_by="test", components='[{"id":"metadata","version":1,"required":true}]'))
            expect(rc == 0, f"expected success, got exit code {rc}")
            written = json.loads(manifest_path.read_text())
            baselines = written.get("baselines", [])
            expect(len(baselines) == 1 and baselines[0]["id"] == "new-baseline",
                   f"expected exactly the new baseline, got {baselines}")
            if baselines:
                fixtures = baselines[0]["fixtures"]
                expect(len(fixtures) == 1 and fixtures[0]["id"] == "new-fixture"
                       and fixtures[0]["sha256"] == hashlib.sha256(b"new fixture bytes").hexdigest(),
                       f"expected the new fixture registered with a real checksum, got {fixtures}")
        finally:
            sca.MANIFEST_PATH = old_path


def test_add_baseline_refuses_new_baseline_missing_required_fields() -> None:
    print("--add-baseline refuses to create a new baseline missing description/migration-target/etc.")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        fixture = make_fixture(tmp, "new.bin", b"new fixture bytes")
        summary = tmp / "new.expected.json"
        summary.write_text('{"ok": true}')
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps({"baselines": []}))
        old_path = sca.MANIFEST_PATH
        sca.MANIFEST_PATH = manifest_path
        try:
            rc = sca.cmd_add_baseline(_Args(
                baseline_id="incomplete-baseline", fixture_id="new-fixture",
                path=str(fixture.relative_to(sca.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(sca.REPO_ROOT))))
            expect(rc == 1, f"expected refusal (missing baseline fields), got exit code {rc}")
            written = json.loads(manifest_path.read_text())
            expect(written.get("baselines", []) == [],
                   "expected the manifest to stay untouched on refusal")
        finally:
            sca.MANIFEST_PATH = old_path


def test_add_baseline_refuses_to_overwrite_without_force() -> None:
    print("--add-baseline refuses to silently overwrite an already-registered fixture")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        original = b"original bytes"
        fixture = make_fixture(tmp, "f.bin", original)
        summary = tmp / "f.expected.json"
        summary.write_text('{"ok": true}')
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps(base_manifest(tmp, fixture, original)))
        tampered = b"tampered bytes -- someone hand-regenerated without --force"
        fixture.write_bytes(tampered)
        old_path = sca.MANIFEST_PATH
        sca.MANIFEST_PATH = manifest_path
        try:
            rc = sca.cmd_add_baseline(_Args(
                baseline_id="test-baseline", fixture_id="test-fixture",
                path=str(fixture.relative_to(sca.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(sca.REPO_ROOT))))
            expect(rc == 1, f"expected refusal without --force, got exit code {rc}")
            rc2 = sca.cmd_add_baseline(_Args(
                baseline_id="test-baseline", fixture_id="test-fixture",
                path=str(fixture.relative_to(sca.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(sca.REPO_ROOT)), force=True))
            expect(rc2 == 0, f"expected --force to succeed, got exit code {rc2}")
            written = json.loads(manifest_path.read_text())
            expect(written["baselines"][0]["fixtures"][0]["sha256"]
                   == hashlib.sha256(tampered).hexdigest(),
                   "expected --force to record the NEW checksum")
        finally:
            sca.MANIFEST_PATH = old_path


def test_add_baseline_requires_summary_for_complete_session() -> None:
    print("--add-baseline refuses a complete-session fixture with no --summary")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        fixture = make_fixture(tmp, "f.bin", b"bytes")
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps({"baselines": []}))
        old_path = sca.MANIFEST_PATH
        sca.MANIFEST_PATH = manifest_path
        try:
            rc = sca.cmd_add_baseline(_Args(
                baseline_id="b", fixture_id="f",
                path=str(fixture.relative_to(sca.REPO_ROOT)), kind="complete-session",
                description="d", migration_target="current", migrated_by="m",
                components="[]"))
            expect(rc == 1, f"expected refusal (no --summary), got exit code {rc}")
        finally:
            sca.MANIFEST_PATH = old_path


def test_add_baseline_rolls_back_on_failed_real_codec_validation() -> None:
    print("--add-baseline rolls the manifest back if the real-codec validation fails")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        original = b"original bytes"
        fixture = make_fixture(tmp, "f.bin", original)
        summary = tmp / "f.expected.json"
        summary.write_text('{"ok": true}')
        manifest_path = tmp / "manifest.json"
        manifest_before = json.dumps({"baselines": []})
        manifest_path.write_text(manifest_before)
        old_manifest_path = sca.MANIFEST_PATH
        old_validate = sca._run_real_codec_validation
        sca.MANIFEST_PATH = manifest_path
        # Simulate the real `cabal test` gate failing, without spawning a
        # real subprocess -- _finalize_manifest_write only ever consumes
        # (bool, str), so substituting this is a faithful stand-in for a
        # genuinely broken fixture.
        sca._run_real_codec_validation = lambda: (False, "simulated hspec failure")
        try:
            rc = sca.cmd_add_baseline(_Args(
                baseline_id="new-baseline", fixture_id="new-fixture",
                path=str(fixture.relative_to(sca.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(sca.REPO_ROOT)),
                description="a test baseline", migration_target="current",
                migrated_by="test", components='[{"id":"metadata","version":1,"required":true}]',
                skip_validation=False))
            expect(rc == 1, f"expected the failed validation to fail the command, got {rc}")
            expect(manifest_path.read_text() == manifest_before,
                   "expected the manifest to be rolled back to its exact prior content")
        finally:
            sca.MANIFEST_PATH = old_manifest_path
            sca._run_real_codec_validation = old_validate


def test_add_baseline_keeps_registration_on_passed_real_codec_validation() -> None:
    print("--add-baseline keeps the registration if the real-codec validation passes")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        original = b"original bytes"
        fixture = make_fixture(tmp, "f.bin", original)
        summary = tmp / "f.expected.json"
        summary.write_text('{"ok": true}')
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps({"baselines": []}))
        old_manifest_path = sca.MANIFEST_PATH
        old_validate = sca._run_real_codec_validation
        sca.MANIFEST_PATH = manifest_path
        sca._run_real_codec_validation = lambda: (True, "simulated hspec pass")
        try:
            rc = sca.cmd_add_baseline(_Args(
                baseline_id="new-baseline", fixture_id="new-fixture",
                path=str(fixture.relative_to(sca.REPO_ROOT)), kind="complete-session",
                summary=str(summary.relative_to(sca.REPO_ROOT)),
                description="a test baseline", migration_target="current",
                migrated_by="test", components='[{"id":"metadata","version":1,"required":true}]',
                skip_validation=False))
            expect(rc == 0, f"expected the passed validation to keep the registration, got {rc}")
            written = json.loads(manifest_path.read_text())
            expect(len(written.get("baselines", [])) == 1,
                   "expected the new baseline to still be registered")
        finally:
            sca.MANIFEST_PATH = old_manifest_path
            sca._run_real_codec_validation = old_validate


def test_add_baseline_skips_validation_for_component_focused_kind() -> None:
    print("--add-baseline never runs the generic real-codec gate for a component-focused fixture")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        fixture = make_fixture(tmp, "f.bin", b"lua payload bytes")
        manifest_path = tmp / "manifest.json"
        manifest_path.write_text(json.dumps({"baselines": []}))
        old_manifest_path = sca.MANIFEST_PATH
        old_validate = sca._run_real_codec_validation
        sca.MANIFEST_PATH = manifest_path
        called = []
        sca._run_real_codec_validation = lambda: called.append(1) or (True, "")
        try:
            rc = sca.cmd_add_baseline(_Args(
                baseline_id="new-baseline", fixture_id="new-fixture",
                path=str(fixture.relative_to(sca.REPO_ROOT)), kind="component-focused",
                description="a test baseline", migration_target="current",
                migrated_by="test", components="[]", skip_validation=False))
            expect(rc == 0, f"expected success, got {rc}")
            expect(called == [],
                   "expected the real-codec validation to never be invoked for a "
                   "component-focused fixture")
        finally:
            sca.MANIFEST_PATH = old_manifest_path
            sca._run_real_codec_validation = old_validate


def test_detects_unknown_component_id_in_baseline() -> None:
    print("a baseline declares a component id the real registry doesn't know")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["baselines"][0]["components"].append(
            {"id": "totally-made-up-component", "version": 1, "required": True})
        violations = sca.audit(manifest)
        expect(any("no longer exists in the real component registry" in v
                    for v in violations),
               f"expected an unknown-component violation, got {violations}")


def test_detects_removed_input_version() -> None:
    print("a baseline declares a version the real codec no longer accepts")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        # craft-bills really accepts {1, 2} -- 99 has never existed.
        manifest["baselines"][0]["components"].append(
            {"id": "craft-bills", "version": 99, "required": True})
        violations = sca.audit(manifest)
        expect(any("currently accepted input versions" in v
                    and "craft-bills" in v for v in violations),
               f"expected a removed-decoder violation, got {violations}")


def test_detects_untracked_oldest_version() -> None:
    print("a real multi-version component is tracked, but not at its oldest version")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
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
        violations = sca.audit(manifest)
        expect(any("craft-bills" in v and "no manifest baseline declares" in v
                    for v in violations),
               f"expected an untracked-oldest-version violation, got {violations}")


def test_detects_required_component_with_zero_coverage() -> None:
    print("a required component (even single-version) has no baseline tracking it at all")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
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
        violations = sca.audit(manifest)
        expect(any("core-session" in v and "is REQUIRED" in v
                    and "not tracked by ANY" in v for v in violations),
               f"expected a required-zero-coverage violation, got {violations}")


def test_real_manifest_passes_the_audit() -> None:
    print("the real, checked-in manifest currently passes (regression guard)")
    manifest = sca.load_manifest()
    violations = sca.audit(manifest)
    expect(violations == [],
           f"expected the real manifest to be clean, got {violations}")


def main() -> int:
    for fn in [
        test_clean_manifest_has_no_violations,
        test_detects_missing_fixture_file,
        test_detects_checksum_drift,
        test_detects_size_mismatch_alone,
        test_decode_only_fixture_skips_checksum,
        test_detects_framing_version_mismatch,
        test_detects_frozen_dto_fingerprint_mismatch,
        test_detects_baseline_with_no_fixtures,
        test_frozen_dto_fingerprint_is_comment_insensitive,
        test_frozen_dto_fingerprint_changes_on_field_reorder,
        test_add_baseline_creates_a_new_baseline_and_fixture_atomically,
        test_add_baseline_refuses_new_baseline_missing_required_fields,
        test_add_baseline_refuses_to_overwrite_without_force,
        test_add_baseline_requires_summary_for_complete_session,
        test_add_baseline_rolls_back_on_failed_real_codec_validation,
        test_add_baseline_keeps_registration_on_passed_real_codec_validation,
        test_add_baseline_skips_validation_for_component_focused_kind,
        test_detects_unknown_component_id_in_baseline,
        test_detects_removed_input_version,
        test_detects_untracked_oldest_version,
        test_detects_required_component_with_zero_coverage,
        test_real_manifest_passes_the_audit,
    ]:
        fn()
    if FAILURES:
        print(f"\n{len(FAILURES)} failure(s)")
        return 1
    print("\nall tests passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
