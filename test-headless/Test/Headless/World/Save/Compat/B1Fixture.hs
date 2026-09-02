-- | The real, tracked B1 envelope bytes the "save migrations" gate
--   (issue #766, save-overhaul C4) proves "World.Save.Compat.SessionV90"'s
--   frozen DTO tree against — REAL, historical bytes recovered from
--   git history, not merely a test's own encoder output — together
--   with the two helpers that decode them and the one metadata value
--   that agrees with them. Pure — no engine, no IO.
--
--   This is the ONE definition of every piece of fixture truth that
--   more than one contract family consumes (#2094): the historical-DTO
--   family ("Test.Headless.World.Save.Compat.Historical") and the
--   legacy-envelope family ("Test.Headless.World.Save.Compat.Legacy")
--   both decode these bytes, and neither carries a copy. Nothing here
--   is a contract of its own, and nothing beyond those three names is
--   exported. @docs/save_compat/manifest.json@'s
--   @historical-b1-session-recovered@ entry names this module as the
--   literal's home.
--
--   'trackedB1EnvelopeFixtureHex' below is byte-for-byte the SAME
--   fixture 'Test.Headless.World.Save.Envelope' tracked before #760
--   replaced it (commit 988c2727, "Introduce the tagged, checksummed
--   save envelope (#759, save-overhaul B1)") — recovered from git
--   history per the compatibility manifest's provenance field
--   (@docs/save_compat/manifest.json@, fixture id @b1-initial-session@).
--   It encodes a real @"session"@ component wrapping a genuine v90
--   'World.Save.Types.SaveData' value (single page @"main_world"@, seed
--   42, no entities) alongside a @"metadata"@ component whose OWN values
--   were hand-picked by that test's author independently of the actual
--   gameplay gen params (world size 64 / plate count 3, vs. the page's
--   REAL 'World.Generate.Types.defaultWorldGenParams' — world size 128 /
--   plate count 10) — B1 predates requirement 12's manifest/gameplay
--   agreement check entirely, so this is not a defect in the fixture,
--   just a pre-existing inconsistency requirement 12 (correctly)
--   still catches on migration. 'decodeSessionV90' alone (no
--   cross-validation) is what proves byte-compatibility; the full
--   'decodeSessionEnvelope' path is exercised separately in
--   "Test.Headless.World.Save.Components" against a self-consistent
--   hand-built fixture.
module Test.Headless.World.Save.Compat.B1Fixture
    ( fixtureBytes
    , extractSessionPayload
    , minimalSaveMetadataForExtra
    ) where

import UPrelude
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Numeric (readHex)

import World.Save.Envelope.Codec (decodeEnvelope, dePayloads)
import World.Save.Envelope.Types (defaultEnvelopeLimits, ComponentId(..))
import World.Save.Types (SaveMetadata(..))

hexDecode ∷ String → BS.ByteString
hexDecode = BS.pack . go
  where
    go (a:b:rest) = case readHex [a,b] of
        ((v,_):_) → v : go rest
        []        → error ("hexDecode: not a hex byte: " <> [a,b])
    go _          = []

-- | Extract the raw @"session"@ component payload from a structurally-
--   valid legacy envelope, bypassing 'World.Save.Envelope''s own
--   cross-validated migration entirely — exactly what this gate needs
--   to test 'decodeSessionV90' in isolation.
extractSessionPayload ∷ BS.ByteString → BS.ByteString
extractSessionPayload bytes =
    case decodeEnvelope defaultEnvelopeLimits 1
             (HS.fromList [ComponentId "metadata", ComponentId "session"])
             (HS.fromList [ComponentId "metadata", ComponentId "session"])
             bytes of
        Left err → error ("test setup: " <> show err)
        Right decoded → case HM.lookup (ComponentId "session")
                                (dePayloads decoded) of
            Just p  → p
            Nothing → error "test setup: session payload missing"

-- | A metadata value that agrees with the extracted fixture session's own
--   gameplay gen params (seed 42 / world size 128 / plate count 10 — see
--   the frozen v90 DTO test in
--   "Test.Headless.World.Save.Compat.Historical"), used by the
--   requirement-9 tests and the v90 migration checks: they are not
--   testing requirement 12's metadata-agreement check, so must not
--   trip over it.
minimalSaveMetadataForExtra ∷ SaveMetadata
minimalSaveMetadataForExtra = SaveMetadata
    { smName = "extra-test", smSeed = 42, smWorldSize = 128, smPlateCount = 10
    , smTimestamp = "2026-07-16T00:00:00.000000Z"
    , smWorldName = Nothing, smWorldGloss = Nothing, smAutosave = False
    , smGeneratedWorldIds = []
    }

-- | Byte-for-byte the SAME fixture 'Test.Headless.World.Save.Envelope'
--   tracked immediately after #759 landed (commit 988c2727), before #760
--   replaced it — see the module haddock for provenance and exactly what
--   it encodes. Never regenerate this from current code: HEAD's codec
--   can no longer produce a single-@"session"@-component envelope at
--   all (that is the whole point of this fixture).
fixtureBytes ∷ BS.ByteString
fixtureBytes = hexDecode trackedB1EnvelopeFixtureHex

trackedB1EnvelopeFixtureHex ∷ String
trackedB1EnvelopeFixtureHex =
    "535952410000000100000000000000610000000000000002000000000000\
    \00086d657461646174610000000101000000000000000000000000000000\
    \80b6ce951fb0e97917000000000000000773657373696f6e0000005a0100\
    \0000000000008000000000000003e3e5f920542dab08fab95b839d58d4e5\
    \290000000000000012656e76656c6f70655f746573745f73617665000000\
    \000000002a00000000000000400000000000000003000000000000001b32\
    \3032362d30372d31365430303a30303a30302e3030303030305a01000000\
    \000000000a5465737420576f726c6401000000000000000f612066697874\
    \75726520776f726c640000000000000012656e76656c6f70655f74657374\
    \5f73617665000000000000002a0000000000000040000000000000000300\
    \0000000000001b323032362d30372d31365430303a30303a30302e303030\
    \3030305a01000000000000000a5465737420576f726c6401000000000000\
    \000f61206669787475726520776f726c6400000000000000000100000000\
    \000000000000000000000000000000000000000000000000000000010000\
    \00000000000a6d61696e5f776f726c640000000000000001000000000000\
    \000a6d61696e5f776f726c640000000000000001000000000000000a6d61\
    \696e5f776f726c64000000000000002a0000000000000080000000000000\
    \000a0000000000000000000000000000001e000000000000000c00000000\
    \00000018000000000000003c3ecccccd3f000000000000000000001c0000\
    \000000000000000000000000000000000080000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000010000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000032\
    \3f8000003e99999a3f3333333fc000003f8000003f0000003f8333330000\
    \000000000000000000000000002000000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \0000000000000000000000000000000000003f800000000000003f800000\
    \3f3333333fa0000000000000000000060000000000000016000000000000\
    \000c3f8000003f8000003f80000000000000000000010000000000000002\
    \000000000000000100000000000000030000000000000001000000000000\
    \000300000000000000010000000000000003000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000003f80\
    \000000000000000000000c00000000000000000000000000000001000000\
    \000000000100000000000000013f80000000000000000000000000000000\
    \000000000000000000000000000000000000000000000000000000000000\
    \000000000000000000000000000000000000010000000000000000000000\
    \010000000000000000000000000000000000000000000000000000000000\
    \000000000000010000000000000000000000010000000000000000000000\
    \0000000000000000000000000000"

