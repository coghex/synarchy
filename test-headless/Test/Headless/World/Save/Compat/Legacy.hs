-- | The legacy-envelope family of the "save migrations" gate (issue
--   #766, save-overhaul C4; split out by #2094): what
--   "World.Save.Envelope" does with an envelope that is NOT the current
--   shape — refusing unknown optional data in a legacy envelope
--   (requirement 9), the overwrite guard's foreign-data verdicts,
--   exact recognition of the B1 and #760-era ("B2") shapes, the B2
--   fallback migration, and the classified component/envelope error
--   phases (#1919). Hand-built B1-shaped envelopes are assembled around
--   the REAL tracked B1 session payload from
--   "Test.Headless.World.Save.Compat.B1Fixture". Pure — no engine; the
--   only IO is read-only access to the tracked B2 fixture.
--
--   Each describe group is exported on its own so the aggregate
--   ("Test.Headless.World.Save.Compat") can sequence it among the other
--   families' groups in the order the suite has always run in; this
--   module registers nothing itself. The B2 envelope-tampering helpers
--   and the frozen v1 metadata value live here because only this
--   family builds envelopes by hand.
module Test.Headless.World.Save.Compat.Legacy
    ( unknownOptionalDataSpec
    , b2FallbackSpec
    ) where

import UPrelude
import Test.Hspec
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Serialize as S
import qualified Data.Text as T

import World.Save.Envelope
    ( decodeSessionEnvelope
    , metadataComponentId, metadataComponentVersion
    , legacyMetadataComponentVersion, currentEnvelopeVersion
    , foreignOptionalComponentIds
    , decodeSessionEnvelopeClassified, generationFailureProgress
    , LoadProgress(..) )
import World.Save.Serialize (loadPhaseFor)
import Engine.Load.Status (LoadPhase(..))
import World.Save.Envelope.Codec
    (decodeEnvelope, encodeEnvelope, dePayloads, deManifest)
import World.Save.Envelope.Types
    (defaultEnvelopeLimits, ComponentId(..), emComponents, cdId, cdVersion, cdRequired)
import World.Save.Component.Types
    (ComponentPhase(..), coreSessionComponentId, worldPagesComponentId)
import World.Save.Compat.SessionV90 (sessionComponentVersion)
import World.Save.Compat.MetadataV1 (SaveMetadataV1(..))
import World.Save.Component.Page (WorldPagesDTO(..))
import Test.Headless.World.Save.Compat.B1Fixture
    (fixtureBytes, extractSessionPayload, minimalSaveMetadataForExtra)

-- | Requirement 9: a legacy envelope carrying data beyond the exact
--   {metadata, session} shape is refused, and the overwrite guard
--   reports that data as foreign.
unknownOptionalDataSpec ∷ Spec
unknownOptionalDataSpec =
    describe "unknown optional data in a legacy envelope (requirement 9)" $ do
        it "refuses to migrate a legacy envelope carrying an extra \
           \optional component beyond {metadata, session}, rather than \
           \silently dropping it" $ do
            let extraSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, True
                      , extractSessionPayload fixtureBytes)
                    , (ComponentId "future-thing", 1, False, BS.pack [9, 9, 9])
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion extraSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Right _   → expectationFailure
                    "expected the extra optional component to be rejected, \
                    \not silently dropped"
                Left msg  → msg `shouldSatisfy` T.isInfixOf "future-thing"

        it "the overwrite guard recognizes a legacy {metadata, session} \
           \envelope as carrying NO foreign data (session itself is a \
           \recognized, migratable shape, not foreign)" $ do
            let plainSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, True
                      , extractSessionPayload fixtureBytes)
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion plainSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            foreignOptionalComponentIds HS.empty bytes `shouldBe` []

        it "the overwrite guard DOES flag a legacy envelope's genuinely \
           \extra optional component as foreign data" $ do
            let extraSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, True
                      , extractSessionPayload fixtureBytes)
                    , (ComponentId "future-thing", 1, False, BS.pack [9, 9, 9])
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion extraSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            foreignOptionalComponentIds HS.empty bytes
                `shouldBe` [ComponentId "future-thing"]

        it "the overwrite guard does NOT exempt an id merely spelled \
           \\"session\" when the envelope ISN'T the exact {metadata, \
           \session} legacy shape (round-4 review) -- a modern-shaped \
           \envelope carrying an unrelated optional component that \
           \happens to be named \"session\" is genuinely foreign, and \
           \exempting it just because of that name would silently drop \
           \it on the next save" $ do
            let modernShapedWithSessionNamedExtra =
                    [ (metadataComponentId, metadataComponentVersion, True
                      , S.encode minimalSaveMetadataForExtra)
                    , (ComponentId "world-pages", 1, True, BS.pack [1, 2, 3])
                    , (ComponentId "session", 1, False, BS.pack [4, 5, 6])
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion modernShapedWithSessionNamedExtra of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            foreignOptionalComponentIds HS.empty bytes
                `shouldBe` [ComponentId "session"]

        it "refuses to migrate an envelope shaped {metadata, session} \
           \whose \"session\" descriptor is marked OPTIONAL, not \
           \required (round-7 review) -- a genuine B1 envelope's writer \
           \always marks BOTH descriptors required; an envelope that \
           \merely matches the id set and version but not the required \
           \flag is not the real frozen shape, and must not be silently \
           \migrated as if it were" $ do
            let optionalSessionSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, False
                      , extractSessionPayload fixtureBytes)
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion optionalSessionSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Right _  → expectationFailure
                    "expected an envelope with an OPTIONAL session \
                    \descriptor to be rejected, not migrated"
                Left msg → msg `shouldSatisfy` T.isInfixOf "required"

        it "the overwrite guard does NOT exempt \"session\" when its OWN \
           \descriptor is marked optional (round-7 review) -- otherwise \
           \this exact envelope shape would be treated as \"no foreign \
           \data\" and get silently overwritten on the next save, \
           \discarding whatever the optional session payload actually \
           \was" $ do
            let optionalSessionSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, False
                      , extractSessionPayload fixtureBytes)
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion optionalSessionSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            foreignOptionalComponentIds HS.empty bytes
                `shouldBe` [ComponentId "session"]

        it "the overwrite guard does NOT exempt \"session\" when it is \
           \\"metadata\" (not \"session\") whose descriptor is marked \
           \optional (round-9 review) -- decodeLegacyStructureAndMetadata \
           \checks BOTH descriptors' required flag, so an envelope with a \
           \perfectly exact, required \"session\" alongside an OPTIONAL \
           \\"metadata\" is not real B1 shape either, and the guard must \
           \independently reach that same conclusion rather than exempt \
           \\"session\" merely because IT happens to be exact" $ do
            let optionalMetadataSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, False
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, True
                      , extractSessionPayload fixtureBytes)
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion optionalMetadataSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Right _  → expectationFailure
                    "expected an envelope with an OPTIONAL metadata \
                    \descriptor to be rejected, not migrated"
                Left msg → msg `shouldSatisfy` T.isInfixOf "required"
            foreignOptionalComponentIds HS.empty bytes
                `shouldBe` [ComponentId "session"]

        it "the overwrite guard does NOT exempt \"lua-state\" merely \
           \because the envelope alongside it LOOKS B1-shaped (round-10 \
           \review) -- an envelope {metadata required v1, session \
           \required v90, lua-state optional v1} is neither genuine B1 \
           \(B1 never carries \"lua-state\") nor genuine B2 (B2 never \
           \carries \"session\"), so \"lua-state\" must be reported as \
           \foreign data, not silently exempted just because it always \
           \rides along in the shared known-set the INITIAL decode needs" $ do
            let extraLuaStateSpecs =
                    [ (metadataComponentId, legacyMetadataComponentVersion, True
                      , S.encode minimalSaveMetadataV1ForExtra)
                    , (ComponentId "session", sessionComponentVersion, True
                      , extractSessionPayload fixtureBytes)
                    , (ComponentId "lua-state", 1, False, BS.empty)
                    ]
                bytes = case encodeEnvelope defaultEnvelopeLimits
                            currentEnvelopeVersion extraLuaStateSpecs of
                    Right b → b
                    Left e  → error ("test setup: " <> show e)
            case decodeSessionEnvelope HS.empty HS.empty bytes of
                Right _  → expectationFailure
                    "expected an envelope carrying \"lua-state\" alongside \
                    \{metadata, session} to be rejected -- it is neither \
                    \the exact B1 nor the exact B2 shape"
                Left _   → pure ()
            foreignOptionalComponentIds HS.empty bytes
                `shouldBe` [ComponentId "lua-state"]

-- | The #760-era ("B2") fallback: recognition, migration, and the
--   classified error phases its failures carry (#1919).
b2FallbackSpec ∷ Spec
b2FallbackSpec =
    describe "the #760-era (\"B2\") fallback (issue #766 requirement 3, \
             \round-7 review)" $ do
        it "migrates the real, tracked B2-shaped fixture (empty lua-state \
           \blob), and the overwrite guard recognizes it as carrying no \
           \foreign data" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames bytes of
                Left err → expectationFailure
                    ("expected the B2 fixture to migrate cleanly: "
                     <> T.unpack err)
                Right (_, _, luaComponents, isMigrated) → do
                    isMigrated `shouldBe` True
                    luaComponents `shouldBe` []
            foreignOptionalComponentIds HS.empty bytes `shouldBe` []

        -- Issue #1919, review round 1. Both legacy fallbacks run REAL
        -- component machinery (B1's decodeSessionV90/migrateSessionV90,
        -- B2's assembleSnapshot), so their failures carry
        -- 'ComponentPhase's just as the modern path's do. Those phases
        -- used to survive only because the load-status layer
        -- substring-matched them back out of the rendered text; now they
        -- must be transported structurally through
        -- 'decodeSessionEnvelopeClassified' or 'failedAtPhase' silently
        -- regresses for exactly these saves.
        it "carries a B2 assembly failure's COMPONENT phases through the \
           \classified path, not a flattened envelope-level guess" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            -- An empty page set: every component still decodes, and only
            -- validation/assembly rejects it -- the phases the pre-#1919
            -- substring parser reported as LoadComponentsMigrated.
            let tampered = replaceB2ComponentSpec bytes worldPagesComponentId
                               (versionOfB2Component bytes worldPagesComponentId)
                               True
                               (S.encode (WorldPagesDTO []))
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelopeClassified luaNames luaNames tampered of
                Right _ → expectationFailure
                    "expected an empty B2 page set to be refused"
                Left failure → do
                    let progress = generationFailureProgress failure
                    case progress of
                        ReachedComponents phases →
                            phases `shouldSatisfy`
                                all (\ph → ph ≡ ValidatePhase ∨ ph ≡ AssemblePhase)
                        other → expectationFailure
                            ("expected component progress, got " <> show other)
                    loadPhaseFor progress `shouldBe` LoadComponentsMigrated

        it "carries a B2 per-component DECODE failure's phase through the \
           \classified path" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2ComponentSpec bytes coreSessionComponentId
                               999 True
                               (payloadOfB2Component bytes coreSessionComponentId)
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelopeClassified luaNames luaNames tampered of
                Right _ → expectationFailure
                    "expected an unsupported core-session version to be refused"
                Left failure → do
                    generationFailureProgress failure
                        `shouldBe` ReachedComponents [DecodePhase]
                    loadPhaseFor (generationFailureProgress failure)
                        `shouldBe` LoadEnvelopeValidated

        it "still reports a genuinely NON-component B2 failure at the \
           \envelope level -- a malformed lua-state blob never reached a \
           \component phase, so it must not borrow one" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2LuaStateSpec bytes 1 True (BS.pack [1, 2, 3])
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelopeClassified luaNames luaNames tampered of
                Right _ → expectationFailure
                    "expected a malformed lua-state blob to be refused"
                Left failure → do
                    generationFailureProgress failure `shouldBe` ReachedEnvelope
                    loadPhaseFor (generationFailureProgress failure)
                        `shouldBe` LoadEnvelopeValidated

        it "refuses to migrate a B2-shaped envelope whose \"lua-state\" \
           \blob decodes to a WELL-FORMED but NON-EMPTY HashMap Text Text \
           \(round-18 review: the real pre-#761 sdLuaModules/ \
           \snapLuaModules shape, not a hand-wavy 'non-empty bytes' stand-\
           \in) -- the pre-#761 Lua deserializer that could interpret it \
           \was removed, so it cannot be honestly migrated, mirroring \
           \migrateSessionV90's identical policy for B1's own legacy Lua \
           \blob" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let realNonEmptyMap =
                    HM.fromList [("unit_ai", "some real persisted AI state")]
                tampered = replaceB2LuaStateSpec bytes 1 True (S.encode realNonEmptyMap)
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Right _  → expectationFailure
                    "expected a non-empty lua-state map to be refused"
                Left msg → msg `shouldSatisfy` T.isInfixOf "lua-state"

        it "refuses to migrate a B2-shaped envelope whose \"lua-state\" \
           \blob is genuinely MALFORMED -- not a valid HashMap Text Text \
           \at all (round-18 review: distinct from the well-formed-but-\
           \non-empty case above; malformed bytes must be refused as \
           \malformed, never silently treated as an acceptable empty \
           \state)" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2LuaStateSpec bytes 1 True (BS.pack [1, 2, 3])
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Right _  → expectationFailure
                    "expected a malformed lua-state blob to be refused"
                Left msg → msg `shouldSatisfy` T.isInfixOf "lua-state"

        it "migrates a B2-shaped envelope whose \"lua-state\" blob is the \
           \REAL cereal-encoded empty HashMap Text Text (round-18 review: \
           \8 bytes -- a Word64 zero length-prefix -- NOT a literal zero-\
           \byte BS.empty payload, which a genuine #760 writer's cereal \
           \encoder never actually produces for an empty map)" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let realEmptyMap = HM.empty ∷ HM.HashMap Text Text
                tampered = replaceB2LuaStateSpec bytes 1 True (S.encode realEmptyMap)
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Left err → expectationFailure
                    ("expected the real cereal-encoded empty map to migrate "
                     <> "cleanly: " <> T.unpack err)
                Right (_, _, luaComponents, isMigrated) → do
                    isMigrated `shouldBe` True
                    luaComponents `shouldBe` []

        it "refuses to migrate a B2-shaped envelope whose \"lua-state\" \
           \descriptor is marked OPTIONAL, not required -- mirrors the B1 \
           \fallback's identical precision (round-7 review): a genuine \
           \#760 writer always marked it required" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2LuaStateSpec bytes 1 False BS.empty
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Right _  → expectationFailure
                    "expected an optional lua-state descriptor to be refused"
                Left msg → msg `shouldSatisfy` T.isInfixOf "required"

        it "refuses to migrate a B2-shaped envelope whose \"lua-state\" \
           \descriptor claims a schema version OTHER than the one genuine \
           \#760 writers always used, even though it is required and \
           \EMPTY -- round-8 review: an unsupported/future lua-state \
           \schema must not be silently accepted (and then re-saved \
           \without ever recording that unknown version) just because it \
           \happens to share the required flag and an empty payload with \
           \the recognized v1 shape" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2LuaStateSpec bytes 2 True BS.empty
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Right _  → expectationFailure
                    "expected a wrong-version (v2) lua-state descriptor to \
                    \be refused rather than treated as the known v1 shape"
                Left msg → msg `shouldSatisfy` T.isInfixOf "lua-state"
            -- The overwrite guard must independently reach the same
            -- conclusion: this is NOT the recognized B2 shape, so
            -- "lua-state" is ordinary foreign data, not exempted.
            foreignOptionalComponentIds HS.empty tampered
                `shouldBe` [ComponentId "lua-state"]

        it "refuses to migrate a B2-shaped envelope whose \"core-session\" \
           \descriptor (a Haskell component OTHER than \"lua-state\") is \
           \marked OPTIONAL -- round-9 review: decodeB2StructureAndMetadata \
           \checks EVERY id in the B2 set for required, not merely \
           \\"lua-state\", and the overwrite guard must reach the \
           \identical conclusion rather than exempt the whole shape just \
           \because \"lua-state\" itself is exact" $ do
            bytes ← BS.readFile
                "test-headless/data/save-compat/b2-split-haskell-lua-state.bin"
            let tampered = replaceB2ComponentSpec bytes
                    (ComponentId "core-session") 1 False
                    (payloadOfB2Component bytes (ComponentId "core-session"))
                luaNames = HS.fromList ["unit_ai", "building_spawn"]
            case decodeSessionEnvelope luaNames luaNames tampered of
                Right _  → expectationFailure
                    "expected an optional \"core-session\" descriptor to \
                    \be refused, not treated as the genuine B2 shape"
                Left msg → msg `shouldSatisfy` T.isInfixOf "required"
            foreignOptionalComponentIds HS.empty tampered
                `shouldNotBe` []

-- | Rebuild the tracked B2 fixture's envelope with ONE component's
--   (version, required, payload) replaced -- every OTHER component's
--   id/version/required/payload carried over verbatim from the real
--   fixture -- so a test can exercise exactly one tampered descriptor at
--   a time against otherwise-genuine bytes.
replaceB2ComponentSpec
    ∷ BS.ByteString → ComponentId → Word32 → Bool → BS.ByteString
    → BS.ByteString
replaceB2ComponentSpec bytes targetCid ver req payload =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             knownAllB2Ids HS.empty bytes of
        Left e → error ("test setup: replaceB2ComponentSpec: decode: " <> show e)
        Right decoded →
            let otherSpecs =
                    [ (cdId d, cdVersion d, cdRequired d, payloadFor decoded (cdId d))
                    | d ← emComponents (deManifest decoded)
                    , cdId d ≢ targetCid ]
                newSpecs = otherSpecs ⧺ [(targetCid, ver, req, payload)]
            in case encodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion newSpecs of
                Right b → b
                Left e  → error ("test setup: replaceB2ComponentSpec: encode: " <> show e)
  where
    payloadFor decoded cid = HM.lookupDefault
        (error ("test setup: payload missing for " <> show cid)) cid
        (dePayloads decoded)

-- | The tracked B2 fixture's own already-encoded payload for one
--   component id, unchanged -- so a test tampering with only that
--   component's (version, required) flags can carry its real payload
--   forward verbatim rather than fabricate one.
payloadOfB2Component ∷ BS.ByteString → ComponentId → BS.ByteString
payloadOfB2Component bytes cid =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             knownAllB2Ids HS.empty bytes of
        Left e → error ("test setup: payloadOfB2Component: decode: " <> show e)
        Right decoded → HM.lookupDefault
            (error ("test setup: payload missing for " <> show cid)) cid
            (dePayloads decoded)

-- | The tracked B2 fixture's own declared schema version for one
--   component id -- so a test replacing that component's PAYLOAD keeps
--   its real historical version rather than hard-coding a number that
--   would silently drift into an unsupported-version test instead.
versionOfB2Component ∷ BS.ByteString → ComponentId → Word32
versionOfB2Component bytes cid =
    case decodeEnvelope defaultEnvelopeLimits currentEnvelopeVersion
             knownAllB2Ids HS.empty bytes of
        Left e → error ("test setup: versionOfB2Component: decode: " <> show e)
        Right decoded → case findDesc decoded of
            Just v  → v
            Nothing → error ("test setup: descriptor missing for " <> show cid)
  where
    findDesc decoded =
        listToMaybe [ cdVersion d | d ← emComponents (deManifest decoded)
                                  , cdId d ≡ cid ]

-- | The exact id set the tracked B2 fixture carries -- see its own
--   manifest entry's components[] list.
knownAllB2Ids ∷ HS.HashSet ComponentId
knownAllB2Ids = HS.fromList
    [ ComponentId "metadata", ComponentId "core-session"
    , ComponentId "texture-palette", ComponentId "world-pages"
    , ComponentId "world-edits", ComponentId "world-activity"
    , ComponentId "buildings", ComponentId "units"
    , ComponentId "unit-sim", ComponentId "craft-bills"
    , ComponentId "power-nodes", ComponentId "lua-state" ]

-- | 'replaceB2ComponentSpec' specialized to "lua-state", preserved as its
--   own name since every existing lua-state-focused test reads more
--   clearly calling it directly.
replaceB2LuaStateSpec
    ∷ BS.ByteString → Word32 → Bool → BS.ByteString → BS.ByteString
replaceB2LuaStateSpec bytes = replaceB2ComponentSpec bytes (ComponentId "lua-state")

-- | The SAME values as
--   'Test.Headless.World.Save.Compat.B1Fixture.minimalSaveMetadataForExtra'
--   in the frozen v1 metadata shape (#913). A hand-built
--   LEGACY envelope must carry v1 metadata, not the current one: a real
--   B1 file was written while metadata was still at v1, and the B1
--   recognizer pins that historical version deliberately
--   ('World.Save.Envelope.legacyMetadataComponentVersion') so a metadata
--   bump can never stop this build recognizing its own frozen baseline.
minimalSaveMetadataV1ForExtra ∷ SaveMetadataV1
minimalSaveMetadataV1ForExtra = SaveMetadataV1
    { sm1Name = "extra-test", sm1Seed = 42, sm1WorldSize = 128
    , sm1PlateCount = 10
    , sm1Timestamp = "2026-07-16T00:00:00.000000Z"
    , sm1WorldName = Nothing, sm1WorldGloss = Nothing
    }

