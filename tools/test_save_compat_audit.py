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
import subprocess
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
    oldest AND current accepted version (round-3 review: required
    components need coverage regardless of how many versions they
    accept, not just multi-version ones; round-10 review: a component's
    CURRENT version needs its own tracked coverage too, distinct from
    its oldest), satisfying audit_component_versions' full coverage
    check with entries that are trivially true of THIS repo's actual
    registry. audit_component_versions cross-checks against the real
    source unconditionally (there is no "test mode" -- that's the whole
    point), so a synthetic manifest aimed at ONE specific, unrelated
    violation class must still declare this or it would incidentally
    also fail on every real component's coverage check, which has
    nothing to do with what that test is exercising."""
    registry = sca.real_component_registry()
    entries = []
    for cid, info in registry.items():
        if not info.get("required"):
            continue
        oldest = min(info["inputVersions"])
        current = info["currentVersion"]
        entries.append({"id": cid, "version": oldest, "required": True})
        if current != oldest:
            entries.append({"id": cid, "version": current, "required": True})
    return entries


def base_manifest(tmp: Path, fixture_path: Path, content: bytes) -> dict:
    return {
        "envelopeFramingVersion": sca.current_envelope_version(),
        "frozenDtoFingerprint": sca.frozen_dto_fingerprint(),
        "envelopeFramingFingerprint": sca.envelope_framing_fingerprint(),
        "baselines": [
            {
                "id": "test-baseline",
                "migrationTarget": "current",
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


def test_detects_complete_session_fixture_missing_checksum() -> None:
    print("round-9 review: a \"kind\": \"complete-session\" fixture with "
          "sha256=null bypasses both this audit's checksum check and the "
          "hspec manifest gate's own fixture selection -- must be rejected")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        fpath = make_fixture(tmp, "fixture.bin", b"hello world")
        manifest = base_manifest(tmp, fpath, b"hello world")
        manifest["baselines"][0]["fixtures"][0]["kind"] = "complete-session"
        manifest["baselines"][0]["fixtures"][0]["sha256"] = None
        manifest["baselines"][0]["fixtures"][0]["sizeBytes"] = None
        manifest["baselines"][0]["fixtures"][0]["expectedCanonicalSummary"] = \
            "test-headless/data/save-compat/does-not-need-to-exist.json"
        violations = sca.audit(manifest)
        expect(any("sha256" in v and "complete-session" in v for v in violations),
               f"expected a checksum-less complete-session violation, got {violations}")


def test_detects_complete_session_fixture_missing_summary() -> None:
    print("round-9 review: a \"kind\": \"complete-session\" fixture with no "
          "expectedCanonicalSummary is never actually validated by the "
          "hspec manifest gate either -- must be rejected")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["baselines"][0]["fixtures"][0]["kind"] = "complete-session"
        violations = sca.audit(manifest)
        expect(any("expectedCanonicalSummary" in v and "complete-session" in v
                   for v in violations),
               f"expected a summary-less complete-session violation, got {violations}")


def test_component_focused_fixture_may_skip_checksum_and_summary() -> None:
    print("a \"kind\": \"component-focused\" fixture legitimately has "
          "neither sha256 nor expectedCanonicalSummary (its real coverage "
          "lives in a named hspec gate instead)")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        fpath = make_fixture(tmp, "fixture.hs", b"-- source file, not a binary blob")
        manifest = base_manifest(tmp, fpath, b"unused")
        manifest["baselines"][0]["fixtures"][0]["kind"] = "component-focused"
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
        "envelopeFramingFingerprint": sca.envelope_framing_fingerprint(),
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


def _synthetic_envelope_types_text(reordered: bool = False) -> str:
    descriptor_fields = (
        "    { cdVersion ∷ !Word32\n    , cdId ∷ !ComponentId\n"
        if reordered else
        "    { cdId ∷ !ComponentId\n    , cdVersion ∷ !Word32\n")
    return (
        "newtype ComponentId = ComponentId Text\n"
        "    deriving (Show, Eq, Ord)\n"
        "    deriving newtype (Hashable, Serialize)\n"
        "\n"
        "data ComponentDescriptor = ComponentDescriptor\n"
        + descriptor_fields +
        "    } deriving (Show, Eq, Generic, Serialize)\n"
        "\n"
        "newtype EnvelopeManifest = EnvelopeManifest\n"
        "    { emComponents ∷ [ComponentDescriptor]\n"
        "    } deriving stock (Show, Eq, Generic)\n"
        "      deriving anyclass (Serialize)\n"
        "\n"
        "envelopeMagic ∷ Word32\n"
        "envelopeMagic = 0x53595241\n"
        "\n"
        "fnv1a64 ∷ BS.ByteString → Word64\n"
        "fnv1a64 = BS.foldl' step 0\n"
        "  where\n"
        "    step acc byte = acc\n"
        "\n"
        "encodeW32 ∷ Word32 → BS.ByteString\n"
        "encodeW32 w = BS.pack [fromIntegral w]\n"
        "\n"
        "decodeW32 ∷ BS.ByteString → Word32\n"
        "decodeW32 = BS.foldl' (\\acc byte → acc) 0\n"
        "\n"
        "encodeW64 ∷ Word64 → BS.ByteString\n"
        "encodeW64 w = BS.pack [fromIntegral w]\n"
        "\n"
        "decodeW64 ∷ BS.ByteString → Word64\n"
        "decodeW64 = BS.foldl' (\\acc byte → acc) 0\n"
        "\n")


def test_envelope_framing_fingerprint_is_comment_insensitive() -> None:
    print("round-15 review: envelope framing fingerprint ignores whitespace/"
          "comment-only changes")
    with tempfile.TemporaryDirectory() as d:
        types_p = Path(d) / "Types.hs"
        codec_p = Path(d) / "Codec.hs"
        types_p.write_text(_synthetic_envelope_types_text())
        codec_p.write_text("-- the codec\nencodeEnvelope x = x\n")
        fp1 = sca.envelope_framing_fingerprint(types_p, codec_p)
        types_p.write_text(
            "-- a totally different, much longer comment\n"
            + _synthetic_envelope_types_text())
        codec_p.write_text(
            "-- the codec, now with a longer explanatory comment\n"
            "encodeEnvelope x = x\n")
        fp2 = sca.envelope_framing_fingerprint(types_p, codec_p)
        expect(fp1 == fp2,
               "expected envelope framing fingerprint to ignore comment-only changes")


def test_envelope_framing_fingerprint_changes_on_layout_change() -> None:
    print("round-15 review: envelope framing fingerprint changes when "
          "ComponentDescriptor's own field order changes -- exactly the "
          "byte-layout-change-with-no-version-bump scenario this fingerprint "
          "exists to catch")
    with tempfile.TemporaryDirectory() as d:
        types_p = Path(d) / "Types.hs"
        codec_p = Path(d) / "Codec.hs"
        codec_p.write_text("encodeEnvelope x = x\n")
        types_p.write_text(_synthetic_envelope_types_text(reordered=False))
        fp1 = sca.envelope_framing_fingerprint(types_p, codec_p)
        types_p.write_text(_synthetic_envelope_types_text(reordered=True))
        fp2 = sca.envelope_framing_fingerprint(types_p, codec_p)
        expect(fp1 != fp2,
               "expected envelope framing fingerprint to change when "
               "ComponentDescriptor's field order changes")


def test_detects_envelope_framing_fingerprint_mismatch() -> None:
    print("manifest envelopeFramingFingerprint disagrees with the real source")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["envelopeFramingFingerprint"] = "0" * 64
        violations = sca.audit(manifest)
        expect(any("envelopeFramingFingerprint" in v for v in violations),
               f"expected an envelope-framing-fingerprint violation, got {violations}")


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
            skip_validation=True,
            # --generate-session only -- cmd_generate reads these too;
            # every generate-session test below monkeypatches
            # generate_current_format_session itself, so the actual
            # values never reach a real engine boot.
            port=9999, page_id="test_page", seed=1, world_size=8,
            plate_count=3, spawn_building=None, spawn_unit=None)
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


def test_generate_session_refuses_when_summary_exists_without_force() -> None:
    print("--generate-session refuses when the SUMMARY (not just the fixture) already exists")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        fixture_path = tmp / "gen.bin"  # deliberately does NOT exist
        summary_path = tmp / "gen.expected.json"
        summary_path.write_text('{"already": "here"}')
        called = []
        old_gen = sca.generate_current_format_session
        sca.generate_current_format_session = lambda **kw: called.append(1)
        try:
            rc = sca.cmd_generate(_Args(
                baseline_id="b", fixture_id="f",
                path=str(fixture_path.relative_to(sca.REPO_ROOT)),
                summary=str(summary_path.relative_to(sca.REPO_ROOT))))
            expect(rc == 1, f"expected refusal, got exit code {rc}")
            expect(called == [],
                   "expected generation to never even start once refused")
            expect(summary_path.read_text() == '{"already": "here"}',
                   "expected the pre-existing summary to be left untouched")
        finally:
            sca.generate_current_format_session = old_gen


def test_generate_session_rolls_back_fixture_and_summary_on_dump_failure() -> None:
    print("--generate-session restores BOTH fixture and summary if canonical-summary derivation fails")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        fixture_path = tmp / "gen.bin"
        summary_path = tmp / "gen.expected.json"
        original_fixture = b"pre-existing fixture bytes"
        original_summary = '{"pre": "existing summary"}'
        fixture_path.write_bytes(original_fixture)
        summary_path.write_text(original_summary)
        old_gen = sca.generate_current_format_session
        old_dump = sca.dump_canonical_summary
        # Simulate a real generation that DID write new bytes (clobbering
        # the pre-existing fixture, exactly like --force would let it),
        # then a dump that fails -- both files must roll back to their
        # ORIGINAL content, not just get deleted or left half-written.
        sca.generate_current_format_session = (
            lambda **kw: kw["out_path"].write_bytes(b"newly generated bytes"))
        sca.dump_canonical_summary = lambda fp, sp: (False, "simulated dump failure")
        try:
            rc = sca.cmd_generate(_Args(
                baseline_id="b", fixture_id="f",
                path=str(fixture_path.relative_to(sca.REPO_ROOT)),
                summary=str(summary_path.relative_to(sca.REPO_ROOT)),
                force=True))
            expect(rc == 1, f"expected failure, got exit code {rc}")
            expect(fixture_path.read_bytes() == original_fixture,
                   "expected the fixture to be restored to its ORIGINAL bytes")
            expect(summary_path.read_text() == original_summary,
                   "expected the summary to be restored to its ORIGINAL content")
        finally:
            sca.generate_current_format_session = old_gen
            sca.dump_canonical_summary = old_dump


def test_generate_session_rolls_back_fixture_and_summary_on_validation_failure() -> None:
    print("--generate-session restores fixture+summary (not just the manifest) if real-codec validation fails")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        fixture_path = tmp / "gen.bin"
        summary_path = tmp / "gen.expected.json"
        manifest_path = tmp / "manifest.json"
        # Nothing pre-existing this time -- a first-ever generation for
        # a brand new baseline+fixture.
        manifest_path.write_text(json.dumps({"baselines": []}))
        old_gen = sca.generate_current_format_session
        old_dump = sca.dump_canonical_summary
        old_validate = sca._run_real_codec_validation
        old_manifest_path = sca.MANIFEST_PATH
        sca.MANIFEST_PATH = manifest_path
        sca.generate_current_format_session = (
            lambda **kw: kw["out_path"].write_bytes(b"newly generated bytes"))
        sca.dump_canonical_summary = (
            lambda fp, sp: (sp.write_text('{"ok": true}'), (True, ""))[1])
        sca._run_real_codec_validation = lambda: (False, "simulated hspec failure")
        try:
            rc = sca.cmd_generate(_Args(
                baseline_id="new-baseline", fixture_id="new-fixture",
                path=str(fixture_path.relative_to(sca.REPO_ROOT)),
                summary=str(summary_path.relative_to(sca.REPO_ROOT)),
                description="a test baseline", migration_target="current",
                migrated_by="test", components='[{"id":"metadata","version":1,"required":true}]',
                skip_validation=False))
            expect(rc == 1, f"expected failure, got exit code {rc}")
            expect(not fixture_path.exists(),
                   "expected the newly-generated fixture to be removed "
                   "(it did not exist before this invocation)")
            expect(not summary_path.exists(),
                   "expected the newly-generated summary to be removed "
                   "(it did not exist before this invocation)")
            written_manifest = json.loads(manifest_path.read_text())
            expect(written_manifest.get("baselines", []) == [],
                   "expected the manifest to also be rolled back (already "
                   "covered by _finalize_manifest_write, checked here for "
                   "full-transaction confidence)")
        finally:
            sca.generate_current_format_session = old_gen
            sca.dump_canonical_summary = old_dump
            sca._run_real_codec_validation = old_validate
            sca.MANIFEST_PATH = old_manifest_path


# The real, checked-in c3-typed-reference-v1-minimal fixture -- a genuine
# modern-shaped envelope with the SAME component set a real
# --generate-session run would also produce -- used below to build two
# envelopes that differ ONLY in their "metadata" component's smTimestamp
# (simulating what two engine.saveWorld calls at different wall-clock
# moments actually produce), so normalize_fixture_timestamp's
# reproducibility guarantee can be exercised against genuine envelope
# bytes rather than a hand-rolled binary fixture.
_C3_FIXTURE_PATH = (
    sca.REPO_ROOT / "test-headless/data/save-compat/c3-typed-reference-v1.bin")

_MAKE_TIMESTAMP_VARIANTS_GHCI = r"""
:set -XOverloadedStrings
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import World.Save.Envelope.Codec
import World.Save.Envelope.Types
import World.Save.Envelope (currentEnvelopeVersion, metadataComponentId)
import World.Save.Component (componentKnownIds)
import World.Save.Types (SaveMetadata(..))

bytes <- BS.readFile "__FIXTURE_PATH__"

:{
let knownAll = HS.insert metadataComponentId
                 (HS.insert (ComponentId "lua.unit_ai")
                    (HS.insert (ComponentId "lua.building_spawn") componentKnownIds))
    writeVariant ts outPath =
      case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion knownAll knownAll bytes of
        Left e -> putStrLn ("SETUP_FAILED: decode: " ++ show e)
        Right decoded ->
          case S.decode
                 (HM.lookupDefault BS.empty metadataComponentId (dePayloads decoded))
                 :: Either String SaveMetadata of
            Left e -> putStrLn ("SETUP_FAILED: metadata decode: " ++ e)
            Right meta -> do
              let variantMeta = meta { smTimestamp = ts }
                  newSpecs =
                    [ ( cdId d, cdVersion d, cdRequired d
                      , if cdId d == metadataComponentId
                           then S.encode variantMeta
                           else HM.lookupDefault BS.empty (cdId d) (dePayloads decoded) )
                    | d <- emComponents (deManifest decoded) ]
              case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion newSpecs of
                Left e -> putStrLn ("SETUP_FAILED: encode: " ++ show e)
                Right outBytes -> BS.writeFile outPath outBytes
in do
  writeVariant "2020-01-01T00:00:00.000000Z" "__VARIANT_A_PATH__"
  writeVariant "2099-12-31T23:59:59.999999Z" "__VARIANT_B_PATH__"
  putStrLn "SETUP_OK"
:}
"""


def test_normalize_fixture_timestamp_makes_generation_reproducible() -> None:
    print("round-11 review: two envelopes differing ONLY in engine.saveWorld's "
          "wall-clock smTimestamp converge to byte-identical fixtures after "
          "normalize_fixture_timestamp, proving --generate-session's output no "
          "longer depends on when the command happened to run")
    if not _C3_FIXTURE_PATH.exists():
        expect(False, f"expected the tracked fixture to exist at {_C3_FIXTURE_PATH}")
        return
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        variant_a = tmp / "variant_a.bin"
        variant_b = tmp / "variant_b.bin"
        setup_script = (_MAKE_TIMESTAMP_VARIANTS_GHCI
            .replace("__FIXTURE_PATH__", str(_C3_FIXTURE_PATH))
            .replace("__VARIANT_A_PATH__", str(variant_a))
            .replace("__VARIANT_B_PATH__", str(variant_b)))
        proc = subprocess.run(
            ["cabal", "repl", "test:synarchy-test-headless"],
            input=setup_script, cwd=sca.REPO_ROOT, capture_output=True,
            text=True, timeout=1800)
        setup_output = (proc.stdout or "") + (proc.stderr or "")
        if "SETUP_OK" not in setup_output or not (variant_a.exists() and variant_b.exists()):
            expect(False,
                   f"expected timestamp-variant setup to succeed, got tail: "
                   f"{setup_output.splitlines()[-30:]}")
            return
        expect(variant_a.read_bytes() != variant_b.read_bytes(),
               "expected the two variants to genuinely differ before "
               "normalization (otherwise this test proves nothing)")

        ok_a, tail_a = sca.normalize_fixture_timestamp(variant_a)
        expect(ok_a, f"expected normalization of variant A to succeed, got: {tail_a}")
        ok_b, tail_b = sca.normalize_fixture_timestamp(variant_b)
        expect(ok_b, f"expected normalization of variant B to succeed, got: {tail_b}")

        expect(variant_a.read_bytes() == variant_b.read_bytes(),
               "expected both variants to be byte-identical after "
               "normalize_fixture_timestamp, proving repeat generation over "
               "identical inputs is now reproducible regardless of wall-clock "
               "drift between runs")


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


def test_detects_untracked_current_version() -> None:
    print("round-10 review: a component's OLDEST version is tracked, but "
          "its CURRENT (bumped) version has no fixture coverage at all")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
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
        real = sca.real_component_registry()
        craft_bills_current = real["craft-bills"]["currentVersion"]
        manifest["baselines"][0]["components"] = [
            c for c in manifest["baselines"][0]["components"]
            if not (c["id"] == "craft-bills" and c["version"] == craft_bills_current)
        ]
        violations = sca.audit(manifest)
        expect(any("craft-bills" in v and "CURRENT version" in v
                    for v in violations),
               f"expected an untracked-current-version violation, got {violations}")
        expect(not any("craft-bills" in v and "a migration exists from" in v
                       for v in violations),
               f"did not expect an untracked-OLDEST-version violation too "
               f"(the oldest version 1 is still tracked), got {violations}")


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


def test_detects_modern_baseline_missing_required_component() -> None:
    print("a modern-shaped (non-session) baseline omits a required component from components[]")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
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
        violations = sca.audit(manifest)
        expect(any("is modern-shaped" in v and "world-pages" in v
                    for v in violations),
               f"expected a modern-baseline-incomplete violation, got {violations}")


def test_modern_baseline_check_skips_b1_shaped_baselines() -> None:
    print("a baseline that DOES declare session is exempt from the modern-completeness check")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        # base_manifest's components[] already includes "session" (every
        # required component, via _oldest_version_components) -- this is
        # the b1-shaped case, which can never declare the full modern set
        # and must not be flagged for that.
        violations = sca.audit(manifest)
        expect(not any("is modern-shaped" in v for v in violations),
               f"expected no modern-shape violation for a session-shaped baseline, got {violations}")


def test_detects_b1_migration_missing_apply_helper() -> None:
    print("migrateSessionV90's source no longer references a required apply* helper")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
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
        violations = sca.audit_b1_migration_covers_page_scoped_components(
            sca.real_component_registry(), p)
        expect(any("applyCraftBills" in v and "craft-bills" in v for v in violations),
               f"expected a missing-apply-helper violation, got {violations}")
        expect(len(violations) == 1,
               f"expected exactly one violation (only craft-bills' helper is missing), got {violations}")


def test_detects_unclassified_new_required_component_for_b1() -> None:
    print("round-13 review: a brand-new REQUIRED Haskell component that nobody "
          "added to SESSION_V90_APPLY_HELPER_FOR_COMPONENT or "
          "SESSION_V90_GLOBAL_OR_INPUT_COMPONENTS is its own violation, not a "
          "silent gap in B1 compatibility coverage")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        p = tmp / "SessionV90.hs"
        # The real source text, unmodified -- every REAL known component's
        # helper genuinely IS referenced here. The only injected fault is
        # a brand-new REQUIRED registry entry this dict/exemption set was
        # never told about.
        p.write_text(sca.SESSION_V90_SOURCE_PATH.read_text(encoding="utf-8"))
        registry = dict(sca.real_component_registry())
        registry["future-thing"] = {
            "currentVersion": 1, "inputVersions": [1], "required": True}
        violations = sca.audit_b1_migration_covers_page_scoped_components(registry, p)
        expect(any("future-thing" in v and "NO known migration-helper" in v
                   for v in violations),
               f"expected an unclassified-required-component violation, got {violations}")


def test_b1_migration_check_ignores_unrequired_new_component() -> None:
    print("a brand-new OPTIONAL Haskell component needs no B1 migration policy "
          "at all (requirement 9's legitimate absence case)")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        p = tmp / "SessionV90.hs"
        p.write_text(sca.SESSION_V90_SOURCE_PATH.read_text(encoding="utf-8"))
        registry = dict(sca.real_component_registry())
        registry["future-optional-thing"] = {
            "currentVersion": 1, "inputVersions": [1], "required": False}
        violations = sca.audit_b1_migration_covers_page_scoped_components(registry, p)
        expect(not any("future-optional-thing" in v for v in violations),
               f"expected no violation for an optional new component, got {violations}")


def test_real_manifest_passes_the_audit() -> None:
    print("the real, checked-in manifest currently passes (regression guard)")
    manifest = sca.load_manifest()
    violations = sca.audit(manifest)
    expect(violations == [],
           f"expected the real manifest to be clean, got {violations}")


def test_detects_manifest_version_claim_not_backed_by_real_fixture_bytes() -> None:
    print("round-12 review: a baseline's declared components[] version bump "
          "is rejected when NO real, tracked fixture's own decoded envelope "
          "actually carries a matching descriptor -- catches a manifest-only "
          "edit with no fixture ever re-encoded at the claimed version")
    manifest = sca.load_manifest()
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
    violations = sca.audit(manifest)
    expect(any("craft-bills" in v and "not backed by any tracked fixture's bytes" in v
               for v in violations),
           f"expected a fixture-backed-claim violation, got {violations}")


def main() -> int:
    for fn in [
        test_clean_manifest_has_no_violations,
        test_detects_missing_fixture_file,
        test_detects_checksum_drift,
        test_detects_size_mismatch_alone,
        test_decode_only_fixture_skips_checksum,
        test_detects_complete_session_fixture_missing_checksum,
        test_detects_complete_session_fixture_missing_summary,
        test_component_focused_fixture_may_skip_checksum_and_summary,
        test_detects_framing_version_mismatch,
        test_detects_frozen_dto_fingerprint_mismatch,
        test_detects_baseline_with_no_fixtures,
        test_frozen_dto_fingerprint_is_comment_insensitive,
        test_frozen_dto_fingerprint_changes_on_field_reorder,
        test_envelope_framing_fingerprint_is_comment_insensitive,
        test_envelope_framing_fingerprint_changes_on_layout_change,
        test_detects_envelope_framing_fingerprint_mismatch,
        test_add_baseline_creates_a_new_baseline_and_fixture_atomically,
        test_add_baseline_refuses_new_baseline_missing_required_fields,
        test_add_baseline_refuses_to_overwrite_without_force,
        test_add_baseline_requires_summary_for_complete_session,
        test_add_baseline_rolls_back_on_failed_real_codec_validation,
        test_add_baseline_keeps_registration_on_passed_real_codec_validation,
        test_add_baseline_skips_validation_for_component_focused_kind,
        test_generate_session_refuses_when_summary_exists_without_force,
        test_generate_session_rolls_back_fixture_and_summary_on_dump_failure,
        test_generate_session_rolls_back_fixture_and_summary_on_validation_failure,
        test_normalize_fixture_timestamp_makes_generation_reproducible,
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
        test_real_manifest_passes_the_audit,
        test_detects_manifest_version_claim_not_backed_by_real_fixture_bytes,
    ]:
        fn()
    if FAILURES:
        print(f"\n{len(FAILURES)} failure(s)")
        return 1
    print("\nall tests passed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
