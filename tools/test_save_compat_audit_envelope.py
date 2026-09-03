#!/usr/bin/env python3
"""Envelope-framing and normalization cases of the save-compat
self-test (issue #2073).

The twelve members that pin what
`save_compat_audit_fingerprint.envelope_framing_fingerprint` reacts to:
a real layout change, an effective-language change, an import or
`-optionsghc` edit -- and what it must stay blind to: comments, block
comments in the header, and a `{-# LANGUAGE #-}` pragma the Cabal file
already implies. It also holds the live inherited-extension read that
must fail LOUDLY rather than silently normalizing nothing, and the one
frozen-DTO member whose subject is pragma normalization rather than the
DTO graph, which is why it sits here and not with the manifest owner
(issue #2073's review pinned that placement).

The synthetic Haskell and Cabal sources these cases fingerprint are
built here rather than in the shared support owner: nothing outside this
owner fingerprints a synthetic envelope source.
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
    base_manifest, make_fixture,
)


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
        fp1 = fingerprint.envelope_framing_fingerprint(types_p, codec_p)
        types_p.write_text(
            "-- a totally different, much longer comment\n"
            + _synthetic_envelope_types_text())
        codec_p.write_text(
            "-- the codec, now with a longer explanatory comment\n"
            "encodeEnvelope x = x\n")
        fp2 = fingerprint.envelope_framing_fingerprint(types_p, codec_p)
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
        fp1 = fingerprint.envelope_framing_fingerprint(types_p, codec_p)
        types_p.write_text(_synthetic_envelope_types_text(reordered=True))
        fp2 = fingerprint.envelope_framing_fingerprint(types_p, codec_p)
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
    return fingerprint.envelope_framing_fingerprint(types_p, codec_p, cabal_p)


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
        inherited = fingerprint.inherited_default_extensions(cabal_p)
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
        expect(fingerprint.inherited_default_extensions(cabal_p) == inherited,
               "expected a cabal `--` comment inside the default-extensions "
               "continuation to be ignored, not read as an extension name")

        real = fingerprint.inherited_default_extensions()
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
                fingerprint.inherited_default_extensions(cabal_p)
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
    manifest = manifest_audit.load_manifest()
    expect(fingerprint.frozen_dto_fingerprint() == manifest["frozenDtoFingerprint"],
           "expected the real frozenDtoFingerprint to still match the "
           "manifest after the envelope pragma normalization was added")
    expect(fingerprint.envelope_framing_fingerprint()
           == manifest["envelopeFramingFingerprint"],
           "expected the real envelopeFramingFingerprint to still match the "
           "manifest after the envelope pragma normalization was added")


def test_detects_envelope_framing_fingerprint_mismatch() -> None:
    print("manifest envelopeFramingFingerprint disagrees with the real source")
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        content = b"hello world"
        fpath = make_fixture(tmp, "fixture.bin", content)
        manifest = base_manifest(tmp, fpath, content)
        manifest["envelopeFramingFingerprint"] = "0" * 64
        violations = manifest_audit.audit(manifest, fixture_dir=tmp)
        expect(any("envelopeFramingFingerprint" in v for v in violations),
               f"expected an envelope-framing-fingerprint violation, got {violations}")


#: This owner's members, in the run order the façade concatenates
#: (issue #2073 requirement 12).
TESTS = [
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
]
