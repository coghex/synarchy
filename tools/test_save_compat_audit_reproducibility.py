#!/usr/bin/env python3
"""The save-compat self-test's one expensive member (issue #2073).

`test_normalize_fixture_timestamp_makes_generation_reproducible` is the
sole member that spawns a `cabal repl test:synarchy-test-headless`, and
that is why it has an owner of its own rather than sitting in the
registration/generation owner it otherwise belongs to by subject.
Issue #1360 selects it by changed paths rather than running it on every
pull request, and the façade derives `REPRODUCIBILITY_TESTS` from this
module's `TESTS` so the expensive set and the run order can never
disagree about which member that is.

A second member added here becomes expensive by construction -- it joins
`REPRODUCIBILITY_TESTS` automatically -- so put a cheap case in the
owner that matches its subject instead.
"""
from __future__ import annotations

import subprocess
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
# It must track the CURRENT "metadata" schema version, because the setup
# below decodes that payload as the live SaveMetadata: an older baseline
# (c3, d1, e1 ...) carries a HISTORICAL metadata shape that only decodes
# through its frozen compat mirror, which is a different concern from the
# timestamp reproducibility this test is about. Re-point this at the
# newest current-format baseline whenever the metadata component's
# version is bumped again.
_CURRENT_FORMAT_FIXTURE_PATH = (
    common.REPO_ROOT
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
    with tempfile.TemporaryDirectory(dir=common.REPO_ROOT) as d:
        tmp = Path(d)
        variant_a = tmp / "variant_a.bin"
        variant_b = tmp / "variant_b.bin"
        setup_script = (_MAKE_TIMESTAMP_VARIANTS_GHCI
            .replace("__FIXTURE_PATH__", str(_CURRENT_FORMAT_FIXTURE_PATH))
            .replace("__VARIANT_A_PATH__", str(variant_a))
            .replace("__VARIANT_B_PATH__", str(variant_b)))
        proc = subprocess.run(
            ["cabal", "repl", "test:synarchy-test-headless"],
            input=setup_script, cwd=common.REPO_ROOT, capture_output=True,
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

        ok_a, tail_a = codec.normalize_fixture_timestamp(variant_a)
        expect(ok_a, f"expected normalization of variant A to succeed, got: {tail_a}")
        ok_b, tail_b = codec.normalize_fixture_timestamp(variant_b)
        expect(ok_b, f"expected normalization of variant B to succeed, got: {tail_b}")

        expect(variant_a.read_bytes() == variant_b.read_bytes(),
               "expected both variants to be byte-identical after "
               "normalize_fixture_timestamp, proving repeat generation over "
               "identical inputs is now reproducible regardless of wall-clock "
               "drift between runs")


#: This owner's members, in the run order the façade concatenates
#: (issue #2073 requirement 12). The façade also takes this list AS
#: `REPRODUCIBILITY_TESTS`, so requirement 13's "exactly the one
#: reproducibility test" is a property of where the member lives rather
#: than of a second list that could drift from it.
TESTS = [
    test_normalize_fixture_timestamp_makes_generation_reproducible,
]
