#!/usr/bin/env python3
"""Manifest and frozen-DTO cases of the save-compat self-test (#2073).

The fifteen members that feed `save_compat_audit_manifest.audit` a
synthetic manifest and assert it detects one violation class: a missing,
drifted or wrong-sized fixture, a per-kind field requirement, a framing
or fingerprint claim that disagrees with the real source, and the
frozen-DTO fingerprint's own sensitivity to field order, comments and
transitively embedded leaf DTOs.

Two names here read as if they belonged elsewhere and do not:
`test_detects_framing_version_mismatch` asserts a MANIFEST field
mismatch rather than fingerprint computation, so it is a manifest case;
`test_frozen_dto_fingerprint_unaffected_by_pragma_normalization` is a
pragma-normalization case and lives with the envelope owner despite its
name (issue #2073's review pinned both placements).

Requirement 14: every case patches the module that OWNS the state it
moves -- `common.HASKELL_COMPONENT_SOURCE_PATHS` here -- never the
`save_compat_audit` façade, which no longer reads it.
"""
from __future__ import annotations

import re
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import save_compat_audit_common as common  # noqa: E402
import save_compat_audit_fingerprint as fingerprint  # noqa: E402
import save_compat_audit_manifest as manifest_audit  # noqa: E402

from selftestlib import expect  # noqa: E402
from test_save_compat_audit_support import (  # noqa: E402
    _oldest_version_components, base_manifest, make_fixture,
)


def test_clean_manifest_has_no_violations() -> None:
    print("clean manifest with a matching fixture")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(violations == [], f"expected no violations, got {violations}")


def test_detects_missing_fixture_file() -> None:
    print("missing fixture path")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = tmp / "does_not_exist.bin"
        manifest = base_manifest(tmp, fpath, content)
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("does not exist" in v for v in violations),
               f"expected a missing-path violation, got {violations}")


def test_detects_checksum_drift() -> None:
    print("fixture bytes changed after being recorded")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        original = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", original)
        manifest = base_manifest(tmp, fpath, original)
        fpath.write_bytes(b"HELLO WORLD -- tampered")
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("drifted" in v for v in violations),
               f"expected a drift violation, got {violations}")


def test_detects_size_mismatch_alone() -> None:
    print("recorded size disagrees even when sha256 is absent (n/a case skipped)")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["baselines"][0]["fixtures"][0]["sizeBytes"] = len(content) + 1
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("size" in v for v in violations),
               f"expected a size-mismatch violation, got {violations}")


def test_decode_only_fixture_skips_checksum() -> None:
    print("a fixture with sha256=null (decode-only/inline-source evidence) is not checksummed")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        fpath = make_fixture(tmp, "fixture.hs", b"-- source file, not a binary blob")
        manifest = base_manifest(tmp, fpath, b"unused")
        manifest["baselines"][0]["fixtures"][0]["sha256"] = None
        manifest["baselines"][0]["fixtures"][0]["sizeBytes"] = None
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(violations == [], f"expected no violations, got {violations}")


def test_detects_complete_session_fixture_missing_checksum() -> None:
    print("round-9 review: a \"kind\": \"complete-session\" fixture with "
          "sha256=null bypasses both this audit's checksum check and the "
          "hspec manifest gate's own fixture selection -- must be rejected")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        fpath = make_fixture(tmp, "fixture.bin", b"hello world")
        manifest = base_manifest(tmp, fpath, b"hello world")
        manifest["baselines"][0]["fixtures"][0]["kind"] = "complete-session"
        manifest["baselines"][0]["fixtures"][0]["sha256"] = None
        manifest["baselines"][0]["fixtures"][0]["sizeBytes"] = None
        manifest["baselines"][0]["fixtures"][0]["expectedCanonicalSummary"] = \
            "test-headless/data/save-compat/does-not-need-to-exist.json"
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("sha256" in v and "complete-session" in v for v in violations),
               f"expected a checksum-less complete-session violation, got {violations}")


def test_detects_complete_session_fixture_missing_summary() -> None:
    print("round-9 review: a \"kind\": \"complete-session\" fixture with no "
          "expectedCanonicalSummary is never actually validated by the "
          "hspec manifest gate either -- must be rejected")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["baselines"][0]["fixtures"][0]["kind"] = "complete-session"
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("expectedCanonicalSummary" in v and "complete-session" in v
                   for v in violations),
               f"expected a summary-less complete-session violation, got {violations}")


def test_component_focused_fixture_may_skip_checksum_and_summary() -> None:
    print("a \"kind\": \"component-focused\" fixture legitimately has "
          "neither sha256 nor expectedCanonicalSummary (its real coverage "
          "lives in a named hspec gate instead)")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        fpath = make_fixture(tmp, "fixture.hs", b"-- source file, not a binary blob")
        manifest = base_manifest(tmp, fpath, b"unused")
        manifest["baselines"][0]["fixtures"][0]["kind"] = "component-focused"
        manifest["baselines"][0]["fixtures"][0]["sha256"] = None
        manifest["baselines"][0]["fixtures"][0]["sizeBytes"] = None
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(violations == [], f"expected no violations, got {violations}")


def test_detects_framing_version_mismatch() -> None:
    print("manifest envelopeFramingVersion disagrees with the real source")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["envelopeFramingVersion"] = fingerprint.current_envelope_version() + 1
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("envelopeFramingVersion" in v for v in violations),
               f"expected a framing-version violation, got {violations}")


def test_detects_frozen_dto_fingerprint_mismatch() -> None:
    print("manifest frozenDtoFingerprint disagrees with the real source")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["frozenDtoFingerprint"] = "0" * 64
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("frozenDtoFingerprint" in v for v in violations),
               f"expected a fingerprint violation, got {violations}")


def test_detects_baseline_with_no_fixtures() -> None:
    print("a declared baseline with zero fixtures")
    manifest = {
        "envelopeFramingVersion": fingerprint.current_envelope_version(),
        "frozenDtoFingerprint": fingerprint.frozen_dto_fingerprint(),
        "envelopeFramingFingerprint": fingerprint.envelope_framing_fingerprint(),
        "baselines": [{"id": "empty-baseline", "components": _oldest_version_components(), "fixtures": []}],
    }
    violations = manifest_audit.audit(manifest)
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
        fp1 = fingerprint.frozen_dto_fingerprint(p)
        p.write_text(
            "-- a DIFFERENT comment, much longer, explaining fooA in depth\n"
            "data Foo = Foo\n"
            "    { fooA ∷ !Int    -- extra trailing comment\n"
            "    , fooB ∷ !Text\n"
            "    } deriving (Show, Generic, Serialize)\n")
        fp2 = fingerprint.frozen_dto_fingerprint(p)
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
        fp1 = fingerprint.frozen_dto_fingerprint(p)
        p.write_text(
            "data Foo = Foo\n"
            "    { fooB ∷ !Text\n"
            "    , fooA ∷ !Int\n"
            "    } deriving (Show, Generic, Serialize)\n")
        fp2 = fingerprint.frozen_dto_fingerprint(p)
        expect(fp1 != fp2,
               "expected fingerprint to change on field reorder")


def test_frozen_dto_fingerprint_changes_on_transitively_embedded_leaf_dto_reorder() -> None:
    print("round-16 review: fingerprint changes when a LEAF DTO SessionV90 "
          "embeds (not one of its own top-level blocks) has its OWN fields "
          "reordered in whatever OTHER file actually defines it -- the exact "
          "transitive-coverage gap this closes")
    with tempfile.TemporaryDirectory() as d:
        session_p = Path(d) / "SessionV90.hs"
        session_p.write_text(
            "data Foo = Foo\n"
            "    { fooLeaf ∷ !LeafDTO\n"
            "    } deriving (Show, Generic, Serialize)\n")
        leaf_p = Path(d) / "Leaf.hs"
        leaf_p.write_text(
            "data LeafDTO = LeafDTO\n"
            "    { leafA ∷ !Int\n"
            "    , leafB ∷ !Text\n"
            "    } deriving (Show, Generic, Serialize)\n"
            "\n")
        old_paths = common.HASKELL_COMPONENT_SOURCE_PATHS
        common.HASKELL_COMPONENT_SOURCE_PATHS = [leaf_p]
        try:
            fp1 = fingerprint.frozen_dto_fingerprint(session_p)
            leaf_p.write_text(
                "data LeafDTO = LeafDTO\n"
                "    { leafB ∷ !Text\n"
                "    , leafA ∷ !Int\n"
                "    } deriving (Show, Generic, Serialize)\n"
                "\n")
            fp2 = fingerprint.frozen_dto_fingerprint(session_p)
            expect(fp1 != fp2,
                   "expected fingerprint to change when a transitively-"
                   "embedded leaf DTO's own fields are reordered")
        finally:
            common.HASKELL_COMPONENT_SOURCE_PATHS = old_paths


def test_frozen_dto_fingerprint_covers_save_metadata_v90() -> None:
    print("round-17 review: SaveDataV90's own sd90Metadata field is now the "
          "frozen SaveMetadataV90 type (not the live, ever-evolving "
          "SaveMetadata) -- confirm its own data...deriving block is "
          "genuinely captured by the real frozen_dto_fingerprint scan, not "
          "merely present in the source file coincidentally")
    text = common.SESSION_V90_SOURCE_PATH.read_text(encoding="utf-8")
    own_blocks = re.findall(
        r"^data \w+ = \w+.*?deriving\s*\([^)]*\)", text,
        re.MULTILINE | re.DOTALL)
    expect(any("SaveMetadataV90" in b and "sm90Name" in b for b in own_blocks),
           "expected SaveMetadataV90's own block to be captured by the "
           "frozen_dto_fingerprint scan")


#: This owner's members, in the run order the façade concatenates
#: (issue #2073 requirement 12).
TESTS = [
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
    test_frozen_dto_fingerprint_changes_on_transitively_embedded_leaf_dto_reorder,
    test_frozen_dto_fingerprint_covers_save_metadata_v90,
]
