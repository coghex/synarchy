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


def base_manifest(tmp: Path, fixture_path: Path, content: bytes) -> dict:
    return {
        "envelopeFramingVersion": sca.current_envelope_version(),
        "frozenDtoFingerprint": sca.frozen_dto_fingerprint(),
        "baselines": [
            {
                "id": "test-baseline",
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
        "baselines": [{"id": "empty-baseline", "fixtures": []}],
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
            declared_at=None, declared_by_issue=766, force=False)
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
