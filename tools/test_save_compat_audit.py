#!/usr/bin/env python3
"""Unit tests for save_compat_audit.py (issue #766, save-overhaul C4).

Feeds `audit()` synthetic manifests over a temporary directory tree --
never touches the real docs/save_compat/manifest.json or tracked
fixtures -- so these tests stay stable regardless of how the real
manifest grows, and prove the audit actually detects each violation
class it claims to (a "the audit detects an intentionally introduced
violation" gate, mirroring tools/test_persistence_inventory_audit.py's
own convention).

Selecting what runs (issue #1360)
--------------------------------
Exactly one member of this module,
`test_normalize_fixture_timestamp_makes_generation_reproducible`, spawns
a `cabal repl test:synarchy-test-headless` to build its two envelope
variants. That is ~26 s of a ~58 s module on a warm tree, and it
exercises fixture GENERATION, which only the save format, the fixture
set, or the audit tooling can move. So it -- and only it -- is selected
by changed paths rather than run on every pull request:

  python3 tools/test_save_compat_audit.py
      Everything, the reproducibility member included. The default, so a
      developer running this by hand still gets the whole module.
  python3 tools/test_save_compat_audit.py --without-reproducibility
      Every member EXCEPT the reproducibility one. This is what CI and
      `make ci` run unconditionally.
  python3 tools/test_save_compat_audit.py --only-reproducibility
      Just the reproducibility member. This is what CI and `make ci` run
      when the change touches a save-format, fixture, save-tooling or
      Cabal path -- `tools/ci_expensive_gates.py`'s `save-compat` gate,
      whose pattern table names every such path and whose --self-test
      pins both directions.

The two selective forms partition the module: `REPRODUCIBILITY_TESTS`
below is subtracted from the full list rather than duplicated, so a
member can never be in both or in neither. Nothing is skipped on a push
to master, where CI runs both forms as the post-merge backstop.

Usage:
  python3 tools/test_save_compat_audit.py [--without-reproducibility |
                                           --only-reproducibility]
Exit codes: 0 = all tests passed, 1 = one or more failed.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
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
        violations = sca.audit(manifest, fixture_dir=tmp)
        expect(violations == [], f"expected no violations, got {violations}")


def test_detects_missing_fixture_file() -> None:
    print("missing fixture path")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = tmp / "does_not_exist.bin"
        manifest = base_manifest(tmp, fpath, content)
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
        expect(violations == [], f"expected no violations, got {violations}")


def test_detects_framing_version_mismatch() -> None:
    print("manifest envelopeFramingVersion disagrees with the real source")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["envelopeFramingVersion"] = sca.current_envelope_version() + 1
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        old_paths = sca.HASKELL_COMPONENT_SOURCE_PATHS
        sca.HASKELL_COMPONENT_SOURCE_PATHS = [leaf_p]
        try:
            fp1 = sca.frozen_dto_fingerprint(session_p)
            leaf_p.write_text(
                "data LeafDTO = LeafDTO\n"
                "    { leafB ∷ !Text\n"
                "    , leafA ∷ !Int\n"
                "    } deriving (Show, Generic, Serialize)\n"
                "\n")
            fp2 = sca.frozen_dto_fingerprint(session_p)
            expect(fp1 != fp2,
                   "expected fingerprint to change when a transitively-"
                   "embedded leaf DTO's own fields are reordered")
        finally:
            sca.HASKELL_COMPONENT_SOURCE_PATHS = old_paths


def test_frozen_dto_fingerprint_covers_save_metadata_v90() -> None:
    print("round-17 review: SaveDataV90's own sd90Metadata field is now the "
          "frozen SaveMetadataV90 type (not the live, ever-evolving "
          "SaveMetadata) -- confirm its own data...deriving block is "
          "genuinely captured by the real frozen_dto_fingerprint scan, not "
          "merely present in the source file coincidentally")
    text = sca.SESSION_V90_SOURCE_PATH.read_text(encoding="utf-8")
    own_blocks = re.findall(
        r"^data \w+ = \w+.*?deriving\s*\([^)]*\)", text,
        re.MULTILINE | re.DOTALL)
    expect(any("SaveMetadataV90" in b and "sm90Name" in b for b in own_blocks),
           "expected SaveMetadataV90's own block to be captured by the "
           "frozen_dto_fingerprint scan")


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


# Issue #1416: the envelope framing fingerprint's ONE narrow pragma
# exception. `common lang` is where synarchy.cabal actually declares the
# extensions Codec.hs inherits; these fixtures reproduce the shape the
# real parser must read, including the negative form that makes bare-name
# comparison wrong.
def _synthetic_cabal_text(
        extensions: str = ("UnicodeSyntax\n"
                           "                      , NoImplicitPrelude\n"
                           "                      , OverloadedStrings"),
        library_imports: str = "warnings, lang, build-policy") -> str:
    return (
        "common warnings\n"
        "    ghc-options: -Wall\n"
        "\n"
        "common lang\n"
        "    default-language: GHC2024\n"
        f"    default-extensions: {extensions}\n"
        "\n"
        "common build-policy\n"
        "    ghc-options: -threaded\n"
        "\n"
        "library\n"
        f"    import: {library_imports}\n"
        "    exposed-modules: World.Save.Envelope.Codec\n"
        "    hs-source-dirs: src\n"
        "\n")


def _framing_fp(d: Path, codec_text: str, cabal_text: str | None = None) -> str:
    """envelope_framing_fingerprint over one scratch Codec.hs, holding
    Types.hs and the cabal file fixed unless a case varies them."""
    types_p = d / "Types.hs"
    codec_p = d / "Codec.hs"
    cabal_p = d / "synarchy.cabal"
    types_p.write_text(_synthetic_envelope_types_text())
    codec_p.write_text(codec_text)
    cabal_p.write_text(_synthetic_cabal_text()
                       if cabal_text is None else cabal_text)
    return sca.envelope_framing_fingerprint(types_p, codec_p, cabal_p)


_CODEC_BODY = (
    "module World.Save.Envelope.Codec (encodeEnvelope) where\n"
    "import qualified Data.ByteString as BS\n"
    "encodeEnvelope x = x\n")


def test_envelope_framing_fingerprint_ignores_inherited_language_pragma() -> None:
    print("issue #1416: removing a LANGUAGE declaration Codec.hs already "
          "inherits from synarchy.cabal's `common lang` leaves the envelope "
          "framing fingerprint unchanged -- PR #1001's UnicodeSyntax removal "
          "could not change the module's effective extension set, so it "
          "should never have moved this value")
    with tempfile.TemporaryDirectory() as tmp:
        d = Path(tmp)
        before = _framing_fp(
            d, "{-# LANGUAGE Strict, UnicodeSyntax #-}\n" + _CODEC_BODY)
        after = _framing_fp(d, "{-# LANGUAGE Strict #-}\n" + _CODEC_BODY)
        expect(before == after,
               "expected removing an already-inherited UnicodeSyntax "
               "declaration to leave the envelope framing fingerprint "
               "unchanged")
        separate = _framing_fp(
            d, "{-# LANGUAGE Strict #-}\n{-# LANGUAGE UnicodeSyntax #-}\n"
            + _CODEC_BODY)
        expect(separate == after,
               "expected an inherited extension declared on its OWN pragma "
               "line to be just as redundant as one sharing a pragma")
        # An inherited NEGATIVE inside the leading inert prefix is
        # provably a no-op: nothing ran before it, `No` propagates
        # nothing, and the extension was already off. No implication
        # table needed.
        silent = _framing_fp(d, _CODEC_BODY)
        lone_negative = _framing_fp(
            d, "{-# LANGUAGE NoImplicitPrelude #-}\n" + _CODEC_BODY)
        expect(lone_negative == silent,
               "expected a lone inherited NoImplicitPrelude to normalize "
               "away exactly like no LANGUAGE declaration at all")
        prefix_pair = _framing_fp(
            d, "{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}\n"
            + _CODEC_BODY)
        expect(prefix_pair == silent,
               "expected a header made up ENTIRELY of inherited "
               "declarations to normalize away whatever their polarity")

        # DECIDED TRADE-OFF (owner, 2026-08-19), pinned here so it reads
        # as intentional: an inherited negative PAST the prefix is
        # retained, even though NoImplicitPrelude merely restates an
        # inherited default and so leaves the effective set unchanged.
        # Proving that one redundant needs to know whether the preceding
        # `Strict` implies `ImplicitPrelude` -- GHC's implication table,
        # which this tool deliberately does not carry (see
        # test_envelope_framing_fingerprint_keeps_undecidable_headers).
        # Over-keeping costs one explicable fingerprint move; the other
        # direction hides a real change.
        negative = _framing_fp(
            d, "{-# LANGUAGE Strict, NoImplicitPrelude #-}\n" + _CODEC_BODY)
        expect(negative != after,
               "expected a NEGATIVE form past the inert prefix to be kept "
               "verbatim rather than normalized on name matching alone")


def test_envelope_framing_fingerprint_changes_on_effective_language_change() -> None:
    print("issue #1416: a LANGUAGE edit that changes Codec.hs's EFFECTIVE "
          "extension set still moves the fingerprint -- the exception is "
          "redundant declarations only, never LANGUAGE pragmas as a class")
    with tempfile.TemporaryDirectory() as tmp:
        d = Path(tmp)
        base = _framing_fp(d, "{-# LANGUAGE Strict #-}\n" + _CODEC_BODY)
        not_inherited = _framing_fp(
            d, "{-# LANGUAGE Strict, RebindableSyntax #-}\n" + _CODEC_BODY)
        expect(base != not_inherited,
               "expected adding a NON-inherited extension (RebindableSyntax "
               "can change do/numeric-literal desugaring while both versions "
               "compile) to move the envelope framing fingerprint")
        dropped_effective = _framing_fp(d, _CODEC_BODY)
        expect(base != dropped_effective,
               "expected removing the module's own effective `Strict` "
               "declaration to move the envelope framing fingerprint")
        # The bare-name trap: `ImplicitPrelude` appears inside the
        # inherited `NoImplicitPrelude`, but declaring it REVERSES that
        # default, so its effective state differs and it must be kept.
        reversed_default = _framing_fp(
            d, "{-# LANGUAGE Strict, ImplicitPrelude #-}\n" + _CODEC_BODY)
        expect(base != reversed_default,
               "expected a local ImplicitPrelude -- which REVERSES the "
               "inherited NoImplicitPrelude rather than restating it -- to "
               "move the envelope framing fingerprint")


def test_envelope_framing_fingerprint_changes_on_import_edit() -> None:
    print("issue #1416: an import-only Codec.hs edit still moves the envelope "
          "framing fingerprint -- an import can change which encoder the "
          "framing actually calls")
    with tempfile.TemporaryDirectory() as tmp:
        d = Path(tmp)
        base = _framing_fp(d, "{-# LANGUAGE Strict #-}\n" + _CODEC_BODY)
        extra_import = _framing_fp(
            d, "{-# LANGUAGE Strict #-}\n"
            + _CODEC_BODY.replace(
                "import qualified Data.ByteString as BS\n",
                "import qualified Data.ByteString as BS\n"
                "import qualified Data.ByteString.Lazy as BSL\n"))
        expect(base != extra_import,
               "expected an import-only Codec.hs edit to move the envelope "
               "framing fingerprint")


def test_envelope_framing_fingerprint_changes_on_options_ghc_edit() -> None:
    print("issue #1416: an OPTIONS_GHC pragma edit still moves the envelope "
          "framing fingerprint -- only LANGUAGE is in the exception's scope")
    with tempfile.TemporaryDirectory() as tmp:
        d = Path(tmp)
        base = _framing_fp(d, "{-# LANGUAGE Strict #-}\n" + _CODEC_BODY)
        with_options = _framing_fp(
            d, "{-# LANGUAGE Strict #-}\n"
            "{-# OPTIONS_GHC -fno-strictness #-}\n" + _CODEC_BODY)
        expect(base != with_options,
               "expected an OPTIONS_GHC pragma edit to move the envelope "
               "framing fingerprint")


def test_envelope_framing_fingerprint_keeps_pragma_shaped_source() -> None:
    print("issue #1416 review: only the module HEADER's pragma run is "
          "normalized -- pragma-SHAPED ordinary source (a string literal a "
          "codec could itself write) must stay fingerprinted, or two "
          "different literals naming different inherited extensions would "
          "collide")
    with tempfile.TemporaryDirectory() as tmp:
        d = Path(tmp)
        literal = ("{-# LANGUAGE Strict #-}\n"
                   "module World.Save.Envelope.Codec (encodeEnvelope) where\n"
                   "import qualified Data.ByteString as BS\n"
                   "marker = \"{-# LANGUAGE %s #-}\"\n"
                   "encodeEnvelope x = x\n")
        unicode_syntax = _framing_fp(d, literal % "UnicodeSyntax")
        overloaded = _framing_fp(d, literal % "OverloadedStrings")
        expect(unicode_syntax != overloaded,
               "expected two string literals naming DIFFERENT inherited "
               "extensions to keep different envelope framing fingerprints "
               "-- erasing pragma-shaped literals collides them")
        removed = _framing_fp(
            d, literal.replace("marker = \"{-# LANGUAGE %s #-}\"\n", ""))
        expect(unicode_syntax != removed,
               "expected deleting a pragma-shaped string literal outright to "
               "move the envelope framing fingerprint")
        # A LANGUAGE pragma past the header (which GHC rejects anyway) is
        # left alone rather than normalized: over-keeping is fail-safe.
        trailing = _framing_fp(
            d, "{-# LANGUAGE Strict #-}\n"
            "module World.Save.Envelope.Codec (encodeEnvelope) where\n"
            "import qualified Data.ByteString as BS\n"
            "{-# LANGUAGE UnicodeSyntax #-}\n"
            "encodeEnvelope x = x\n")
        expect(trailing != _framing_fp(d, "{-# LANGUAGE Strict #-}\n"
                                       + _CODEC_BODY),
               "expected a LANGUAGE pragma outside the module header to stay "
               "fingerprinted verbatim")
        # An OPTIONS_GHC pragma sitting IN the header run must not stop the
        # walk before a redundant LANGUAGE pragma that follows it.
        interleaved_with = _framing_fp(
            d, "{-# OPTIONS_GHC -Wall #-}\n"
            "{-# LANGUAGE Strict, UnicodeSyntax #-}\n" + _CODEC_BODY)
        interleaved_without = _framing_fp(
            d, "{-# OPTIONS_GHC -Wall #-}\n"
            "{-# LANGUAGE Strict #-}\n" + _CODEC_BODY)
        expect(interleaved_with == interleaved_without,
               "expected the header walk to see a redundant LANGUAGE pragma "
               "that follows an OPTIONS_GHC pragma")


def test_envelope_framing_fingerprint_keeps_undecidable_headers() -> None:
    print("issue #1416 review rounds 2 and 4: a header that turns an "
          "extension OFF -- directly, or by re-enabling one after a disable "
          "through GHC's implication graph -- is not decidable from the "
          "names alone, so the whole header is kept verbatim rather than "
          "normalized into a collision")
    with tempfile.TemporaryDirectory() as tmp:
        d = Path(tmp)
        plain = _framing_fp(d, "{-# LANGUAGE Strict #-}\n" + _CODEC_BODY)

        # Round 2: GHC applies repeated flags left to right, so ending on
        # ImplicitPrelude REVERSES the inherited NoImplicitPrelude while
        # ending on NoImplicitPrelude restates it. These compile
        # differently and must not share a fingerprint.
        reverses = _framing_fp(
            d, "{-# LANGUAGE Strict, ImplicitPrelude #-}\n" + _CODEC_BODY)
        restates = _framing_fp(
            d, "{-# LANGUAGE Strict, ImplicitPrelude, NoImplicitPrelude #-}\n"
            + _CODEC_BODY)
        expect(len({reverses, restates, plain}) == 3,
               "expected a conflicting flag list ending on ImplicitPrelude, "
               "one ending on NoImplicitPrelude, and a header declaring "
               "neither to keep three distinct fingerprints")
        split = _framing_fp(
            d, "{-# LANGUAGE Strict, ImplicitPrelude #-}\n"
            "{-# LANGUAGE NoImplicitPrelude #-}\n" + _CODEC_BODY)
        expect(split not in {reverses, plain},
               "expected the same conflict SPLIT across two header pragmas "
               "to stay distinct from both a header ending on "
               "ImplicitPrelude and one declaring neither")

        # Round 4: `common lang` inherits TypeFamilyDependencies, which
        # ENABLES TypeFamilies. Re-declaring it after NoTypeFamilies
        # reinstates TypeFamilies, so it is not redundant despite being
        # inherited -- the exact collision a name-only rule produces.
        disabled = _framing_fp(
            d, "{-# LANGUAGE NoTypeFamilies #-}\n" + _CODEC_BODY)
        reinstated = _framing_fp(
            d, "{-# LANGUAGE NoTypeFamilies, TypeFamilyDependencies #-}\n"
            + _CODEC_BODY)
        expect(disabled != reinstated,
               "expected an inherited TypeFamilyDependencies re-declared "
               "AFTER NoTypeFamilies to stay fingerprinted -- it re-enables "
               "the TypeFamilies the previous flag removed")

        # An extension the module does not inherit is untouched either
        # way, and a NEGATIVE form of one is distinct from both its
        # positive form and no declaration at all.
        no_strict = _framing_fp(d, "{-# LANGUAGE NoStrict #-}\n" + _CODEC_BODY)
        silent = _framing_fp(d, _CODEC_BODY)
        expect(len({no_strict, plain, silent}) == 3,
               "expected NoStrict, Strict and no declaration at all to keep "
               "three distinct fingerprints")

        # A negative form ANYWHERE in the header suppresses the exception
        # for the whole header, including an otherwise-redundant name
        # sitting beside it.
        beside = _framing_fp(
            d, "{-# LANGUAGE Strict, UnicodeSyntax, NoStrictData #-}\n"
            + _CODEC_BODY)
        beside_without = _framing_fp(
            d, "{-# LANGUAGE Strict, NoStrictData #-}\n" + _CODEC_BODY)
        expect(beside != beside_without,
               "expected an inherited UnicodeSyntax to be kept when a "
               "negative form shares its header")

        # So does naming one extension twice, and a token that is not a
        # bare extension name.
        duplicated = _framing_fp(
            d, "{-# LANGUAGE Strict, UnicodeSyntax, UnicodeSyntax #-}\n"
            + _CODEC_BODY)
        expect(duplicated != plain,
               "expected an extension named TWICE to leave the header "
               "verbatim rather than be deduplicated away")
        odd_a = _framing_fp(
            d, "{-# LANGUAGE Strict, UnicodeSyntax, -Wall #-}\n" + _CODEC_BODY)
        odd_b = _framing_fp(
            d, "{-# LANGUAGE Strict, -Wall #-}\n" + _CODEC_BODY)
        expect(odd_a != odd_b,
               "expected an unparseable LANGUAGE token to leave the header "
               "verbatim, so nothing beside it is normalized away either")


def test_envelope_framing_fingerprint_sees_past_header_block_comments() -> None:
    print("issue #1416 review round 3: _strip_line_comments removes only "
          "`--` comments, so a `{- ... -}` haddock block in the module "
          "header must not end the pragma walk and leave a redundant "
          "LANGUAGE declaration behind it fingerprinted")
    with tempfile.TemporaryDirectory() as tmp:
        d = Path(tmp)
        for label, header in [
                ("a leading block comment", "{- the codec -}\n"),
                ("a NESTED block comment",
                 "{- the codec {- an aside -} continued -}\n"),
                ("a block comment BETWEEN header pragmas",
                 "{-# OPTIONS_GHC -Wall #-}\n{- an aside -}\n")]:
            with_inherited = _framing_fp(
                d, header + "{-# LANGUAGE Strict, UnicodeSyntax #-}\n"
                + _CODEC_BODY)
            without = _framing_fp(
                d, header + "{-# LANGUAGE Strict #-}\n" + _CODEC_BODY)
            expect(with_inherited == without,
                   f"expected a redundant LANGUAGE declaration behind "
                   f"{label} to still be normalized away")

        # Block comments are only STEPPED OVER, never erased -- the
        # existing normalization strips `--` comments alone, and this
        # change must not quietly widen that.
        comment_a = _framing_fp(
            d, "{- one -}\n{-# LANGUAGE Strict #-}\n" + _CODEC_BODY)
        comment_b = _framing_fp(
            d, "{- two -}\n{-# LANGUAGE Strict #-}\n" + _CODEC_BODY)
        expect(comment_a != comment_b,
               "expected a header block comment's own text to stay in the "
               "hash -- stepping over one must not start excluding it")

        # An unterminated block comment leaves the header alone rather
        # than guessing where it ended.
        unterminated_with = _framing_fp(
            d, "{- never closed\n{-# LANGUAGE Strict, UnicodeSyntax #-}\n"
            + _CODEC_BODY)
        unterminated_without = _framing_fp(
            d, "{- never closed\n{-# LANGUAGE Strict #-}\n" + _CODEC_BODY)
        expect(unterminated_with != unterminated_without,
               "expected an unterminated header block comment to leave the "
               "header verbatim rather than normalize past it")


def test_inherited_extension_set_is_read_live_and_fails_loudly() -> None:
    print("issue #1416: the inherited extension set is derived from "
          "synarchy.cabal's `common lang`, and a stanza that cannot be read "
          "fails loudly rather than degrading to an empty/stale set")
    with tempfile.TemporaryDirectory() as tmp:
        d = Path(tmp)
        cabal_p = d / "synarchy.cabal"

        cabal_p.write_text(_synthetic_cabal_text())
        inherited = sca.inherited_default_extensions(cabal_p)
        expect(inherited == {"UnicodeSyntax": True, "ImplicitPrelude": False,
                             "OverloadedStrings": True},
               "expected the inherited set to be parsed from `common lang` "
               "as effective (name, enabled) states, negative forms included")

        commented = _synthetic_cabal_text(
            extensions=("UnicodeSyntax\n"
                        "                      -- a note about the next one\n"
                        "                      , NoImplicitPrelude\n"
                        "                      , OverloadedStrings"))
        cabal_p.write_text(commented)
        expect(sca.inherited_default_extensions(cabal_p) == inherited,
               "expected a cabal `--` comment inside the default-extensions "
               "continuation to be ignored, not read as an extension name")

        real = sca.inherited_default_extensions()
        expect(real.get("UnicodeSyntax") is True
               and real.get("ImplicitPrelude") is False
               and "Strict" not in real,
               "expected the REAL synarchy.cabal to parse, supplying "
               "UnicodeSyntax and NoImplicitPrelude but never Strict")

        for label, text in [
                ("no `common lang` stanza",
                 _synthetic_cabal_text().replace(
                     "common lang\n", "common langx\n", 1)),
                ("no default-extensions field",
                 re.sub(r"    default-extensions:.*?\n\n",
                        "\n", _synthetic_cabal_text(), flags=re.DOTALL)),
                ("a default-extensions entry that is not a bare name",
                 _synthetic_cabal_text(extensions="UnicodeSyntax, -Wall")),
                ("the library stanza no longer importing `lang`",
                 _synthetic_cabal_text(
                     library_imports="warnings, build-policy"))]:
            cabal_p.write_text(text)
            raised = False
            try:
                sca.inherited_default_extensions(cabal_p)
            except ValueError:
                raised = True
            expect(raised,
                   f"expected inherited_default_extensions to fail loudly on "
                   f"{label}")

        # A stanza edit must reach the fingerprint, not a hard-coded copy:
        # once `common lang` stops supplying UnicodeSyntax, a local
        # declaration of it becomes EFFECTIVE and must be fingerprinted.
        with_it = _framing_fp(d, "{-# LANGUAGE UnicodeSyntax #-}\n"
                              + _CODEC_BODY,
                              _synthetic_cabal_text(
                                  extensions="NoImplicitPrelude"))
        without_it = _framing_fp(d, _CODEC_BODY,
                                 _synthetic_cabal_text(
                                     extensions="NoImplicitPrelude"))
        expect(with_it != without_it,
               "expected a LANGUAGE declaration to become fingerprinted again "
               "once `common lang` stops supplying it -- the inherited set "
               "must track the cabal stanza, never a hard-coded list")


def test_frozen_dto_fingerprint_unaffected_by_pragma_normalization() -> None:
    print("issue #1416: the pragma step is scoped to the envelope framing "
          "fingerprint's codec text -- frozen_dto_fingerprint shares "
          "_normalize_haskell_block and its recorded manifest value must not "
          "move")
    manifest = sca.load_manifest()
    expect(sca.frozen_dto_fingerprint() == manifest["frozenDtoFingerprint"],
           "expected the real frozenDtoFingerprint to still match the "
           "manifest after the envelope pragma normalization was added")
    expect(sca.envelope_framing_fingerprint()
           == manifest["envelopeFramingFingerprint"],
           "expected the real envelopeFramingFingerprint to still match the "
           "manifest after the envelope pragma normalization was added")


def test_detects_envelope_framing_fingerprint_mismatch() -> None:
    print("manifest envelopeFramingFingerprint disagrees with the real source")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["envelopeFramingFingerprint"] = "0" * 64
        violations = sca.audit(manifest, fixture_dir=tmp)
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
            plate_count=3, spawn_building=None, spawn_unit=None,
            # #915: where to spawn --spawn-unit, how long to let ticks
            # run before saving, and a predicate that must hold before
            # the save -- for state a spawn verb never writes directly
            # (a per-unit location memory is INGESTED by the unit-AI
            # tick once the unit can SEE the location).
            spawn_unit_at="0,0", settle_seconds=0.0, setup_lua=None,
            require_lua=None,
            # #1101: the page's optional display identity and the
            # language provenance it was rendered from -- what makes a
            # generated fixture's placed locations carry real generated
            # names/glosses rather than definition labels.
            world_name=None, world_gloss=None, language_seed=None,
            language_version=None, name_expr=None)
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


def test_generate_session_rolls_back_on_generation_error_after_fixture_written() -> None:
    print("round-16 review: --generate-session restores the fixture even when "
          "GenerationError is raised AFTER the new bytes were already written "
          "(e.g. normalize_fixture_timestamp failing post-copyfile) -- not "
          "just when generation fails before ever touching the file")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        fixture_path = tmp / "gen.bin"
        summary_path = tmp / "gen.expected.json"
        original_fixture = b"pre-existing fixture bytes"
        fixture_path.write_bytes(original_fixture)
        old_gen = sca.generate_current_format_session

        def fake_gen(**kw):
            # Simulates generate_current_format_session's real shape since
            # round-11: engine.saveWorld/shutil.copyfile succeeds and
            # writes new bytes FIRST, then normalize_fixture_timestamp
            # (a separate, later step) fails.
            kw["out_path"].write_bytes(b"newly generated but un-normalized bytes")
            raise sca.GenerationError("simulated timestamp-normalization failure")

        sca.generate_current_format_session = fake_gen
        try:
            rc = sca.cmd_generate(_Args(
                baseline_id="b", fixture_id="f",
                path=str(fixture_path.relative_to(sca.REPO_ROOT)),
                summary=str(summary_path.relative_to(sca.REPO_ROOT)),
                force=True))
            expect(rc == 1, f"expected failure, got exit code {rc}")
            expect(fixture_path.read_bytes() == original_fixture,
                   "expected the fixture to be restored to its ORIGINAL "
                   "bytes, not left as the newly-written-but-failed content")
            expect(not summary_path.exists(),
                   "expected the summary (which never existed before) to "
                   "still not exist")
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


# A real, checked-in CURRENT-FORMAT fixture -- a genuine modern-shaped
# envelope with the SAME component set a real --generate-session run
# would also produce -- used below to build two envelopes that differ
# ONLY in their "metadata" component's smTimestamp (simulating what two
# engine.saveWorld calls at different wall-clock moments actually
# produce), so normalize_fixture_timestamp's reproducibility guarantee
# can be exercised against genuine envelope bytes rather than a
# hand-rolled binary fixture.
#
# It must track the CURRENT "metadata" schema version, because the setup
# below decodes that payload as the live SaveMetadata: an older baseline
# (c3, d1, e1 ...) carries a HISTORICAL metadata shape that only decodes
# through its frozen compat mirror, which is a different concern from the
# timestamp reproducibility this test is about. Re-point this at the
# newest current-format baseline whenever the metadata component's
# version is bumped again.
_CURRENT_FORMAT_FIXTURE_PATH = (
    sca.REPO_ROOT
    / "test-headless/data/save-compat/u1-generated-world-identity.bin")

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
      -- Structural re-encode only: knownAll widens what may APPEAR,
      -- while the reader-required set stays EMPTY. Reusing knownAll for
      -- both would demand that this already-tracked fixture carry every
      -- component the current build knows about -- including any
      -- OPTIONAL one added after the fixture was captured (#1087's
      -- container-knowledge), which by definition it does not.
      case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion knownAll HS.empty bytes of
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
    if not _CURRENT_FORMAT_FIXTURE_PATH.exists():
        expect(False, f"expected the tracked fixture to exist at {_CURRENT_FORMAT_FIXTURE_PATH}")
        return
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        variant_a = tmp / "variant_a.bin"
        variant_b = tmp / "variant_b.bin"
        setup_script = (_MAKE_TIMESTAMP_VARIANTS_GHCI
            .replace("__FIXTURE_PATH__", str(_CURRENT_FORMAT_FIXTURE_PATH))
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


def test_detects_registered_component_missing_from_source_scan() -> None:
    print("round-16 review: a component registered in saveComponentRegistry's "
          "own authoritative list, but whose codec definition can't be found "
          "anywhere the scan looked, fails loudly rather than silently "
          "vanishing from the entire registry with no error at all")
    with tempfile.TemporaryDirectory() as d:
        tmp = Path(d)
        registry_path = tmp / "Component.hs"
        registry_path.write_text(
            "saveComponentRegistry ∷ [RegisteredComponent]\n"
            "saveComponentRegistry =\n"
            "    [ registerComponent coreSessionCodec\n"
            "        (\\_ d snap -> Right snap)\n"
            "    , registerComponent totallyMissingCodec\n"
            "        (\\_ d snap -> Right snap)\n"
            "    ]\n"
            "  where\n"
            "    unused = ()\n"
            "\n"
            "-- next binding\n")
        component_dir = tmp / "Component"
        component_dir.mkdir()
        only_file = component_dir / "Session.hs"
        only_file.write_text(
            "coreSessionComponentId = ComponentId \"core-session\"\n"
            "coreSessionCodec = componentCodec ComponentSpec\n"
            "    { csComponent     = coreSessionComponentId\n"
            "    , csVersion       = 1\n"
            "    , csRequired      = True\n"
            "    , csDeps          = []\n"
            "    , csEncode        = encodeCore\n"
            "    , csDecode        = id\n"
            "    , csOlderVersions = []\n"
            "    , csValidate      = const []\n"
            "    }\n")
        old_registry_path = sca.COMPONENT_REGISTRY_SOURCE_PATH
        old_haskell_paths = sca.HASKELL_COMPONENT_SOURCE_PATHS
        sca.COMPONENT_REGISTRY_SOURCE_PATH = registry_path
        sca.HASKELL_COMPONENT_SOURCE_PATHS = [only_file]
        try:
            try:
                sca.real_component_registry()
                expect(False,
                       "expected real_component_registry() to raise for a "
                       "codec referenced in saveComponentRegistry but never "
                       "found by the source scan")
            except ValueError as e:
                expect("totallyMissingCodec" in str(e),
                       f"expected the error to name the missing codec, got: {e}")
        finally:
            sca.COMPONENT_REGISTRY_SOURCE_PATH = old_registry_path
            sca.HASKELL_COMPONENT_SOURCE_PATHS = old_haskell_paths


def test_discover_component_specs_derives_input_versions_from_one_declaration() -> None:
    print("issue #1093: the accepted-version set comes from the SAME single "
          "declaration the reader dispatches on -- csVersion plus each "
          "csOlderVersions `atVersion <n>` -- not a separately parsed list")
    specs = sca.discover_component_specs("""\
singletonCodec ∷ ComponentCodec OneDTO
singletonCodec = componentCodec ComponentSpec
    { csComponent     = oneComponentId
    , csVersion       = 1
    , csRequired      = True
    , csDeps          = [otherComponentId]
    , csEncode        = \\snap → OneDTO (map f (pages snap))
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = const []
    }

-- Field ORDER deliberately shuffled, and the older-version list spans
-- several lines: neither may change what this parser reads.
evolvedCodec ∷ ComponentCodec ThreeDTO
evolvedCodec = componentCodec ComponentSpec
    { csVersion       = 3
    , csOlderVersions = [ atVersion 2 migrateV2
                        , atVersion 1 migrateV1 ]
    , csComponent     = threeComponentId
    , csRequired      = False
    , csDeps          = []
    , csEncode        = encodeThree
    , csDecode        = buildThree
    , csValidate      = validateThree
    }
""")
    by_codec = {s["codec"]: s for s in specs}
    expect(set(by_codec) == {"singletonCodec", "evolvedCodec"},
           f"expected both codecs discovered, got {sorted(by_codec)}")
    expect(by_codec["singletonCodec"] == {
               "codec": "singletonCodec",
               "componentIdIdent": "oneComponentId",
               "currentVersion": 1, "inputVersions": [1], "required": True},
           f"expected the singleton spec parsed from one declaration, got "
           f"{by_codec['singletonCodec']}")
    expect(by_codec["evolvedCodec"] == {
               "codec": "evolvedCodec",
               "componentIdIdent": "threeComponentId",
               "currentVersion": 3, "inputVersions": [1, 2, 3],
               "required": False},
           f"expected inputVersions [1, 2, 3] derived from csVersion + "
           f"csOlderVersions, got {by_codec['evolvedCodec']}")


def test_discover_component_specs_ignores_commented_out_fields() -> None:
    print("issue #1093: a `--` comment inside a spec cannot contribute a "
          "phantom accepted version or a phantom field")
    specs = sca.discover_component_specs("""\
c ∷ ComponentCodec D
c = componentCodec ComponentSpec
    { csComponent     = cId
    , csVersion       = 2
    -- , csVersion       = 9   (an older value left in a comment)
    , csRequired      = True
    , csDeps          = []
    , csEncode        = enc
    , csDecode        = id
    , csOlderVersions = [ atVersion 1 migrateV1 ]  -- not atVersion 7 anything
    , csValidate      = const []
    }
""")
    expect(len(specs) == 1 and specs[0]["inputVersions"] == [1, 2],
           f"expected exactly [1, 2] from the live declarations, got {specs}")


def test_discover_component_specs_raises_on_a_spec_missing_a_needed_field() -> None:
    print("issue #1093: a spec whose fields this audit needs are absent "
          "raises loudly rather than silently matching into the NEXT codec's "
          "fields and reporting a wrong version for it")
    try:
        sca.discover_component_specs("""\
brokenCodec = componentCodec ComponentSpec
    { csComponent     = brokenComponentId
    , csRequired      = True
    , csDeps          = []
    , csEncode        = enc
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = const []
    }

laterCodec = componentCodec ComponentSpec
    { csComponent     = laterComponentId
    , csVersion       = 7
    , csRequired      = True
    , csDeps          = []
    , csEncode        = enc
    , csDecode        = id
    , csOlderVersions = []
    , csValidate      = const []
    }
""", "synthetic.hs")
        expect(False, "expected discover_component_specs to raise for a spec "
                      "with no csVersion field")
    except ValueError as e:
        expect("brokenCodec" in str(e) and "csVersion" in str(e),
               f"expected the error to name the codec and the missing field, "
               f"got: {e}")


def test_hand_rolled_component_codec_is_no_longer_silently_discovered() -> None:
    print("issue #1093: every gameplay codec now goes through the shared "
          "construction, so a codec that hand-rolls the ComponentCodec "
          "record is NOT discovered -- and the saveComponentRegistry cross-"
          "check then fails loudly naming it, rather than this tool quietly "
          "reporting the component as undiscovered")
    with tempfile.TemporaryDirectory() as d:
        tmp = Path(d)
        registry_path = tmp / "Component.hs"
        registry_path.write_text(
            "saveComponentRegistry ∷ [RegisteredComponent]\n"
            "saveComponentRegistry =\n"
            "    [ registerComponent handRolledCodec\n"
            "        (\\_ d snap -> Right snap)\n"
            "    ]\n"
            "\n"
            "-- next binding\n")
        component_dir = tmp / "Component"
        component_dir.mkdir()
        only_file = component_dir / "Session.hs"
        only_file.write_text(
            "handRolledComponentId = ComponentId \"hand-rolled\"\n"
            "handRolledCodec ∷ ComponentCodec D\n"
            "handRolledCodec = ComponentCodec\n"
            "    { ccId        = handRolledComponentId\n"
            "    , ccVersion   = 2\n"
            "    , ccInputVers = [1, 2]\n"
            "    , ccRequired  = True\n"
            "    }\n")
        old_registry_path = sca.COMPONENT_REGISTRY_SOURCE_PATH
        old_haskell_paths = sca.HASKELL_COMPONENT_SOURCE_PATHS
        sca.COMPONENT_REGISTRY_SOURCE_PATH = registry_path
        sca.HASKELL_COMPONENT_SOURCE_PATHS = [only_file]
        try:
            try:
                sca.real_component_registry()
                expect(False, "expected real_component_registry() to raise "
                              "for a hand-rolled ComponentCodec record")
            except ValueError as e:
                expect("handRolledCodec" in str(e),
                       f"expected the error to name the codec, got: {e}")
        finally:
            sca.COMPONENT_REGISTRY_SOURCE_PATH = old_registry_path
            sca.HASKELL_COMPONENT_SOURCE_PATHS = old_haskell_paths


def test_haskell_component_source_paths_discovers_new_files_automatically() -> None:
    print("round-16 review: HASKELL_COMPONENT_SOURCE_PATHS globs the "
          "Component/ directory rather than a fixed file list -- a brand-new "
          "file placed there is picked up with no code change needed")
    expect(len(sca.HASKELL_COMPONENT_SOURCE_PATHS) >= 4,
           f"expected at least the 4 known Component/*.hs files, got "
           f"{sca.HASKELL_COMPONENT_SOURCE_PATHS}")
    expect(all(p.suffix == ".hs" and p.parent.name == "Component"
               for p in sca.HASKELL_COMPONENT_SOURCE_PATHS),
           f"expected every discovered path to be a .hs file directly under "
           f"a Component/ directory, got {sca.HASKELL_COMPONENT_SOURCE_PATHS}")


def test_haskell_component_source_paths_is_the_whole_directory() -> None:
    print("issue #2098: HASKELL_COMPONENT_SOURCE_PATHS must equal the "
          "Component/ directory listing exactly -- the >= 4 check above "
          "cannot notice an owner DROPPED from discovery, and a dropped "
          "owner silently leaves its DTOs out of the B1 fingerprint")
    # _transitive_dto_blocks() `continue`s past a leaf it cannot resolve
    # rather than raising, so an owner module missing from the search
    # paths costs the fingerprint its declarations with no error at all.
    # Pinning the set to the real directory is what makes that
    # unrepresentable: the worldgen DTO graph's owners
    # (WorldGenClimate/WorldGenNaming/WorldGenCurrent/WorldGenHistory)
    # joined discovery by being placed in this directory, and any future
    # owner does too.
    directory = sca.REPO_ROOT / "src" / "World" / "Save" / "Component"
    expected = sorted(directory.glob("*.hs"))
    expect(expected, f"expected {directory} to contain component sources")
    expect(sca.HASKELL_COMPONENT_SOURCE_PATHS == expected,
           f"discovery does not match the directory listing.\n"
           f"  missing from discovery: "
           f"{sorted(set(expected) - set(sca.HASKELL_COMPONENT_SOURCE_PATHS))}\n"
           f"  discovered but absent from the directory: "
           f"{sorted(set(sca.HASKELL_COMPONENT_SOURCE_PATHS) - set(expected))}")
    # Every worldgen DTO owner is reached, by name, so a rename that
    # moved one out of this directory fails here rather than quietly
    # shrinking the fingerprint's input.
    discovered = {p.name for p in sca.HASKELL_COMPONENT_SOURCE_PATHS}
    for owner in ("WorldGen.hs", "WorldGenClimate.hs", "WorldGenNaming.hs",
                  "WorldGenCurrent.hs", "WorldGenHistory.hs"):
        expect(owner in discovered,
               f"worldgen DTO owner {owner} is not in discovery: "
               f"{sorted(discovered)}")


def directory_owner(name: str, paths: list) -> Path:
    """The single discovered path whose file name is `name`."""
    matches = [p for p in paths if p.name == name]
    expect(len(matches) == 1,
           f"expected exactly one discovered {name}, got {matches}")
    return matches[0]


def test_dropping_one_owner_from_discovery_changes_the_fingerprint() -> None:
    print("issue #2098 requirement 8: mutation-removing any single "
          "discovered owner from HASKELL_COMPONENT_SOURCE_PATHS must move "
          "the frozen B1 DTO fingerprint or fail -- proving discovery is "
          "load-bearing rather than incidentally complete")
    baseline = sca.frozen_dto_fingerprint()
    old_paths = sca.HASKELL_COMPONENT_SOURCE_PATHS
    # Only the owners the B1 closure actually reaches can move the hash;
    # the mutation is meaningful for those, and the directory-equality
    # test above is what covers the rest.
    reached = []
    try:
        for dropped in old_paths:
            sca.HASKELL_COMPONENT_SOURCE_PATHS = [
                p for p in old_paths if p != dropped]
            if sca.frozen_dto_fingerprint() != baseline:
                reached.append(dropped.name)
    finally:
        sca.HASKELL_COMPONENT_SOURCE_PATHS = old_paths
    expect(sca.frozen_dto_fingerprint() == baseline,
           "the fingerprint did not return to its unmutated value")
    # SessionV90 seeds the worldgen half of the B1 closure through ONE
    # field, `wp90GenParams ∷ !WorldGenParamsDTOv1`, so the closure
    # reaches the historical owner that now declares that shape and the
    # climate leaves it embeds. Dropping either must move the hash.
    for owner in ("WorldGenHistory.hs", "WorldGenClimate.hs"):
        expect(owner in reached,
               f"dropping {owner} left the B1 fingerprint unchanged, so its "
               f"DTOs are not actually contributing: moved by {reached}")
    # The naming owner is deliberately NOT asserted above: world-pages v1
    # predates the location instance table (it stored three chunk-keyed
    # sets), so no naming DTO is reachable from today's B1 seed at all --
    # that was already true of the pre-split module and is not something
    # this split changed. Its discovery is still load-bearing, and the
    # audit's OWN leaf resolver is what proves it: a location DTO
    # resolves through the globbed paths, and stops resolving the moment
    # the naming owner is dropped from them. `_transitive_dto_blocks`
    # `continue`s past an unresolvable leaf silently, so this is exactly
    # the failure mode a missing owner would produce.
    naming = directory_owner("WorldGenNaming.hs", old_paths)
    for dto in ("LocationInstanceDTOv1", "RiverNameDTO", "NameExprDTO"):
        expect(sca._find_type_definition(dto, old_paths) is not None,
               f"{dto} does not resolve through the discovered owners")
        expect(sca._find_type_definition(
                   dto, [p for p in old_paths if p != naming]) is None,
               f"{dto} still resolved with the naming owner dropped from "
               f"discovery -- this mutation proves nothing")


def _malformed_older_versions_source(current: int, older: str) -> str:
    """One synthetic ComponentSpec whose only interesting field is its
    csOlderVersions declaration."""
    return f"""\
evolvedCodec ∷ ComponentCodec D
evolvedCodec = componentCodec ComponentSpec
    {{ csComponent     = evolvedComponentId
    , csVersion       = {current}
    , csRequired      = True
    , csDeps          = []
    , csEncode        = enc
    , csDecode        = id
    , csOlderVersions = {older}
    , csValidate      = const []
    }}
"""


def _expect_older_versions_rejected(current: int, older: str,
                                    must_mention: list[str],
                                    what: str) -> None:
    try:
        specs = sca.discover_component_specs(
            _malformed_older_versions_source(current, older), "synthetic.hs")
        expect(False,
               f"expected discover_component_specs to raise for {what}, got "
               f"{specs}")
    except ValueError as e:
        missing = [m for m in must_mention if m not in str(e)]
        expect(not missing,
               f"expected the {what} error to mention {missing}, got: {e}")


def test_discover_component_specs_rejects_a_duplicate_older_version() -> None:
    print("issue #1275: csOlderVersions repeating a strictly OLDER version "
          "is rejected -- only the first decoder for a repeated version is "
          "ever reached by the dispatch table's lookup, so the second is "
          "silently unreachable. Set-normalizing it (the old "
          "`sorted({current} | set(older))`) erased exactly this evidence")
    _expect_older_versions_rejected(
        4, "[ atVersion 3 migrateV3, atVersion 2 migrateV2, "
           "atVersion 3 migrateV3Again ]",
        ["evolvedCodec", "v3", "more than once"],
        "a duplicate older version")


def test_discover_component_specs_rejects_the_current_version_as_older() -> None:
    print("issue #1275: csOlderVersions listing the CURRENT version is "
          "rejected -- the real current decoder shadows it (sortOn is stable "
          "and the current version is prepended), so its frozen DTO is never "
          "reached. Distinct from the duplicate case above, so uniqueness "
          "and strict ordering are demonstrated independently")
    _expect_older_versions_rejected(
        4, "[ atVersion 4 migrateV4, atVersion 1 migrateV1 ]",
        ["evolvedCodec", "v4", "csVersion"],
        "the current version declared as older")


def test_discover_component_specs_rejects_a_future_older_version() -> None:
    print("issue #1275: csOlderVersions listing a version NEWER than "
          "csVersion is rejected -- the reader would advertise and accept a "
          "version no writer has ever produced")
    _expect_older_versions_rejected(
        4, "[ atVersion 5 migrateV5, atVersion 1 migrateV1 ]",
        ["evolvedCodec", "v5", "NEWER"],
        "a future version declared as older")


def test_discover_component_specs_fails_closed_on_unreadable_older_entries() -> None:
    print("issue #1275: the parse fails CLOSED. A findall scan silently "
          "SKIPPED any element it did not recognize, deriving an accepted-"
          "version set missing real decoders; every one of these shapes must "
          "raise instead")
    for older, what in [
        ("[ atVersion legacyVersion migrateOld ]",
         "a non-literal atVersion argument"),
        ("[ ComponentVersion { cvVersion = 1, cvDecode = decodeV1 } ]",
         "a hand-built ComponentVersion"),
        ("[ frozenAt 1 migrateV1 ]", "a helper expression"),
        ("[ atVersion 1 migrateV1, someOtherEntry ]",
         "one unreadable element beside a readable one"),
        ("olderVersionsOf myComponent", "a non-list value"),
    ]:
        _expect_older_versions_rejected(
            4, older, ["evolvedCodec", "cannot enumerate"], what)


def test_discover_component_specs_exhausts_each_older_entry() -> None:
    print("issue #1275 (review round 1): matching only the HEAD of an entry "
          "is not fail-closed. An element whose readable `atVersion <n>` "
          "prefix is followed by more expression evaluates to a DIFFERENT "
          "ComponentVersion than the one recorded, so the entry must be "
          "consumed COMPLETELY or rejected")
    for older, what in [
        ("[ atVersion 1 migrateV1 `seq` atVersion futureVersion migrateFuture ]",
         "a trailing operator application hiding a non-literal version"),
        ("[ atVersion 1 (id ∷ D → D) `orElse` atVersion 9 migrateV9 ]",
         "a trailing operator application after a parenthesized build"),
        ("[ atVersion 1 migrateV1 extraArgument ]",
         "a second argument after the build"),
        ("[ atVersion 1 ]", "no build argument at all"),
        ("[ atVersion 1 $ migrateV1 ]",
         "an application operator this audit does not read"),
        ("[ atVersion 1 migrateV1 . fixup ]",
         "a composed build expression"),
    ]:
        _expect_older_versions_rejected(
            4, older, ["evolvedCodec", "cannot enumerate"], what)


def test_discover_component_specs_reads_the_real_build_argument_shapes() -> None:
    print("issue #1275 (review round 1): exhausting each entry must not "
          "reject the shapes actually shipped -- a bare identifier, a "
          "qualified name, and a balanced parenthesized expression "
          "(including nested parens and a string literal)")
    for older, expected in [
        ("[ atVersion 1 migrateWorldPagesV1 ]", [1, 4]),
        ("[ atVersion 1 Page.migrateV1 ]", [1, 4]),
        ("[ atVersion 1 (id ∷ WorldActivityDTO → WorldActivityDTO) ]", [1, 4]),
        ("[ atVersion 1 (fmap (dropTag \"v1\") . migrateV1) ]", [1, 4]),
        ("[ atVersion 2 (migrateV2 ∷ D2 → A), atVersion 1 migrateV1 ]",
         [1, 2, 4]),
    ]:
        specs = sca.discover_component_specs(
            _malformed_older_versions_source(4, older), "synthetic.hs")
        expect(len(specs) == 1 and specs[0]["inputVersions"] == expected,
               f"expected inputVersions {expected} for {older}, got {specs}")


def test_discover_component_specs_accepts_a_well_formed_multi_version_table() -> None:
    print("issue #1275: a well-formed table is completely unaffected -- "
          "descending, ascending, and single-entry declarations all still "
          "yield the same ascending inputVersions they always did")
    for older, expected in [
        ("[ atVersion 3 m3, atVersion 2 m2, atVersion 1 m1 ]", [1, 2, 3, 4]),
        ("[ atVersion 1 m1, atVersion 2 m2, atVersion 3 m3 ]", [1, 2, 3, 4]),
        ("[ atVersion 1 (id ∷ D → D) ]", [1, 4]),
        ("[]", [4]),
    ]:
        specs = sca.discover_component_specs(
            _malformed_older_versions_source(4, older), "synthetic.hs")
        expect(len(specs) == 1 and specs[0]["inputVersions"] == expected,
               f"expected inputVersions {expected} for {older}, got {specs}")


def test_discover_lua_save_modules_finds_the_real_two_modules() -> None:
    print("round-16 review: discover_lua_save_modules scans scripts/ rather "
          "than trusting a fixed 2-file list -- confirm it still finds the "
          "real, currently-registered unit_ai/building_spawn modules")
    discovered = sca.discover_lua_save_modules()
    discovered_ids = {lua_id for _path, lua_id in discovered}
    expect({"unit_ai", "building_spawn"} <= discovered_ids,
           f"expected both real modules discovered, got {discovered_ids}")


def test_detects_unknown_component_id_in_baseline() -> None:
    print("a baseline declares a component id the real registry doesn't know")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["baselines"][0]["components"].append(
            {"id": "totally-made-up-component", "version": 1, "required": True})
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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
        violations = sca.audit(manifest, fixture_dir=tmp)
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


def test_detects_orphaned_fixture_file() -> None:
    print("round-19 (post-approval) review: a file exists under the "
          "fixture directory but is not referenced by any baseline's "
          "fixture path or expectedCanonicalSummary")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        make_fixture(tmp, "orphaned.bin", b"nobody references me")
        violations = sca.audit(manifest, fixture_dir=tmp)
        expect(any("orphaned.bin" in v and "not referenced" in v
                   for v in violations),
               f"expected an orphaned-fixture violation, got {violations}")


def test_no_orphan_violation_when_every_file_is_referenced() -> None:
    print("a fixture's own path AND its expectedCanonicalSummary both "
          "count as references -- neither is misclassified as an orphan")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        summary_path = make_fixture(tmp, "fixture.expected.json", b"{}")
        manifest["baselines"][0]["fixtures"][0]["expectedCanonicalSummary"] = \
            str(summary_path.relative_to(sca.REPO_ROOT))
        violations = sca.audit(manifest, fixture_dir=tmp)
        expect(not any("not referenced" in v for v in violations),
               f"expected no orphan violation, got {violations}")


def test_orphan_check_is_skipped_when_fixture_dir_does_not_exist() -> None:
    print("a fixture_dir that doesn't exist yet (e.g. a from-scratch "
          "synthetic manifest with no directory at all) is not itself a "
          "violation -- the check has nothing to scan, not a missing dir")
    with tempfile.TemporaryDirectory(dir=sca.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        violations = sca.audit(manifest, fixture_dir=tmp / "does-not-exist")
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
    rendered = sca.render_setup_lua(table_stmt, 7, 3)
    expect(rendered == ("local t = {bandage = 1, [2] = 'x'}; "
                        "return unit.depositToCargo(3, 7, 'bandage')"),
           f"the table literal survives verbatim and only {{bid}}/{{uid}} "
           f"are substituted, got {rendered!r}")

    nested = sca.render_setup_lua("return f({a = {b = 1}})", 1, 2)
    expect(nested == "return f({a = {b = 1}})",
           f"a statement with NO placeholders is returned unchanged, "
           f"got {nested!r}")

    unspawned = sca.render_setup_lua("return g({bid}, {uid})", None, None)
    expect(unspawned == "return g(nil, nil)",
           f"an unspawned side substitutes the Lua literal nil, never the "
           f"Python string 'None', got {unspawned!r}")


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


#: The members that spawn a `cabal repl` and are therefore selected by
#: changed paths rather than run on every pull request (issue #1360).
#: Subtracted from ALL_TESTS below rather than listed twice, so the two
#: selective forms provably partition the module.
REPRODUCIBILITY_TESTS = [
    test_normalize_fixture_timestamp_makes_generation_reproducible,
]

#: Every member, in run order. `--without-reproducibility` runs this
#: minus REPRODUCIBILITY_TESTS; `--only-reproducibility` runs the
#: intersection; a bare run runs all of it.
ALL_TESTS = [
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
    test_envelope_framing_fingerprint_is_comment_insensitive,
    test_envelope_framing_fingerprint_changes_on_layout_change,
    test_envelope_framing_fingerprint_ignores_inherited_language_pragma,
    test_envelope_framing_fingerprint_changes_on_effective_language_change,
    test_envelope_framing_fingerprint_changes_on_import_edit,
    test_envelope_framing_fingerprint_changes_on_options_ghc_edit,
    test_envelope_framing_fingerprint_keeps_pragma_shaped_source,
    test_envelope_framing_fingerprint_keeps_undecidable_headers,
    test_envelope_framing_fingerprint_sees_past_header_block_comments,
    test_inherited_extension_set_is_read_live_and_fails_loudly,
    test_frozen_dto_fingerprint_unaffected_by_pragma_normalization,
    test_detects_envelope_framing_fingerprint_mismatch,
    test_add_baseline_creates_a_new_baseline_and_fixture_atomically,
    test_add_baseline_refuses_new_baseline_missing_required_fields,
    test_add_baseline_refuses_to_overwrite_without_force,
    test_add_baseline_requires_summary_for_complete_session,
    test_add_baseline_rolls_back_on_failed_real_codec_validation,
    test_add_baseline_keeps_registration_on_passed_real_codec_validation,
    test_add_baseline_skips_validation_for_component_focused_kind,
    test_generate_session_refuses_when_summary_exists_without_force,
    test_generate_session_rolls_back_on_generation_error_after_fixture_written,
    test_generate_session_rolls_back_fixture_and_summary_on_dump_failure,
    test_generate_session_rolls_back_fixture_and_summary_on_validation_failure,
    test_normalize_fixture_timestamp_makes_generation_reproducible,
    test_detects_registered_component_missing_from_source_scan,
    test_discover_component_specs_derives_input_versions_from_one_declaration,
    test_discover_component_specs_ignores_commented_out_fields,
    test_discover_component_specs_raises_on_a_spec_missing_a_needed_field,
    test_discover_component_specs_rejects_a_duplicate_older_version,
    test_discover_component_specs_rejects_the_current_version_as_older,
    test_discover_component_specs_rejects_a_future_older_version,
    test_discover_component_specs_fails_closed_on_unreadable_older_entries,
    test_discover_component_specs_exhausts_each_older_entry,
    test_discover_component_specs_reads_the_real_build_argument_shapes,
    test_discover_component_specs_accepts_a_well_formed_multi_version_table,
    test_hand_rolled_component_codec_is_no_longer_silently_discovered,
    test_haskell_component_source_paths_discovers_new_files_automatically,
    test_haskell_component_source_paths_is_the_whole_directory,
    test_dropping_one_owner_from_discovery_changes_the_fingerprint,
    test_discover_lua_save_modules_finds_the_real_two_modules,
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


def selected_tests(only_reproducibility: bool,
                   without_reproducibility: bool) -> list:
    """The members one invocation runs.

    The two flags partition ALL_TESTS: `--only-reproducibility` keeps
    exactly REPRODUCIBILITY_TESTS and `--without-reproducibility` keeps
    exactly the rest, so no member can be run twice or dropped by both.
    """
    expensive = set(REPRODUCIBILITY_TESTS)
    if only_reproducibility:
        return [fn for fn in ALL_TESTS if fn in expensive]
    if without_reproducibility:
        return [fn for fn in ALL_TESTS if fn not in expensive]
    return list(ALL_TESTS)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Unit tests for tools/save_compat_audit.py.",
        epilog="With no flag, every member runs.")
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--only-reproducibility", action="store_true",
        help="run ONLY the cabal-repl reproducibility member (#1360).")
    group.add_argument(
        "--without-reproducibility", action="store_true",
        help="run every member EXCEPT the cabal-repl reproducibility "
             "member (#1360).")
    args = parser.parse_args(argv)

    # A member listed as expensive but absent from the run order would
    # silently vanish from BOTH selective forms, which is exactly the
    # "coverage quietly stopped running" failure this selection exists
    # to avoid. Fail loudly instead.
    missing = [fn.__name__ for fn in REPRODUCIBILITY_TESTS
               if fn not in ALL_TESTS]
    if missing:
        print(f"REPRODUCIBILITY_TESTS members missing from ALL_TESTS: "
              f"{missing}")
        return 1

    tests = selected_tests(args.only_reproducibility,
                           args.without_reproducibility)
    if not tests:
        print("no tests selected -- refusing to report a vacuous pass")
        return 1
    for fn in tests:
        fn()
    if FAILURES:
        print(f"\n{len(FAILURES)} failure(s)")
        return 1
    print(f"\nall tests passed ({len(tests)} of {len(ALL_TESTS)} members)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
